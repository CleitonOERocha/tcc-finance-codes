
library(tidyverse)
library(lubridate)
library(readxl)

options(scipen = 999)

pasta_graficos <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\Graficos\\'

pasta_dados <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\'

###############################################################################
############ Carregando a ajustando dados -------------------------------------
###############################################################################

setwd("C:\\Users\\cleit\\Desktop\\TCC\\Dados\\MarketCap")

# Carregando lista de ativos -----------------------
indicadores_setores_v2 <- read.csv2(paste0(pasta_dados, "indicadores_setores_v3.csv"))

indicadores_setores_v2$Ações <- ifelse(indicadores_setores_v2$Ações == "",
                                       NA,
                                       indicadores_setores_v2$Ações)

indicadores_setores_v2 <- indicadores_setores_v2 %>% drop_na(Ações)

num_acoes_df <- read.csv2("num_acoes_df.csv")

num_acoes_df <- num_acoes_df %>%
                dplyr::mutate(acoes_sa = paste0(symbol,".SA")) %>% dplyr::select(!symbol)


# Carregando dados de preços dos ativos até o dia 11/11/2021 ---------------------
price_data <- read.csv2(paste0(pasta_dados, "acoes_setores_2022_04_21.csv"))

price_data_num_acoes <- inner_join(num_acoes_df,
                                   price_data, by = c("acoes_sa" = "symbol"))

price_data_num_acoes <- price_data_num_acoes %>%
  dplyr::filter(num_acoes > 0) %>% 
  dplyr::filter(!acoes_sa %in% c("MMAQ3.SA", "MMAQ4.SA")) %>% 
  dplyr::mutate(ano = lubridate::year(date)) %>% 
  dplyr::group_by(ano, acoes_sa) %>%
  dplyr::mutate(media_volume = mean(volume, na.rm = TRUE)) %>% 
  as.data.frame() %>% 
  dplyr::filter(media_volume >= 50000) %>% 
  dplyr::mutate(marketcap = close*num_acoes) %>% 
  dplyr::relocate(num_acoes, .after = adjusted)

Market_cap <- price_data_num_acoes


###############################################################################
############ Calculando Fator SMB ---------------------------------------------
###############################################################################

# Obtendo o market cap do último dia do ano ------------------------
Market_cap <- Market_cap %>% 
  dplyr::group_by(ano, acoes_sa) %>% 
  slice(tail(row_number(), 1)) %>% 
  as.data.frame()


# Calculando os decis de cada ativo -----------------------
Market_cap <- Market_cap %>%
  dplyr::group_by(ano) %>% 
  dplyr::mutate(Decil = ntile(-adjusted, 10)) %>%
  as.data.frame() %>% 
  dplyr::select(acoes_sa, Decil, ano)


Market_cap_price <- dplyr::inner_join(price_data_num_acoes,
                                      Market_cap,
                                      by = c("acoes_sa", "ano"))


# Obtendo o preço médio de cada decil por data ------------------------
Decil_Price <- Market_cap_price %>%
  dplyr::group_by(date, Decil) %>%
  dplyr::summarise(Mean_Price = mean(adjusted)) 


# Criando as variáveis long e short -----------------------------
Decil_Price <- Decil_Price %>%
  dplyr::mutate(Buy_or_Short = case_when(Decil <= 3 ~ "Short", Decil >= 8 ~ "Long" )) 


# Agrupando por data e variável (long/short) e obtendo a média de preço -----------------
Decil_Price <- Decil_Price %>% 
  dplyr::group_by(date, Buy_or_Short) %>%
  dplyr::summarise(Mean_Price_Class = mean(Mean_Price))


# Removendo NA's (ativos que não são usados na estratégia long/short) --------------------
Decil_Price <- Decil_Price %>% drop_na(Buy_or_Short)


# Agrupando por variável e obtendo o retorno de cada estratégia -----------------
Decil_Price <- Decil_Price %>% 
  dplyr::group_by(Buy_or_Short) %>%
  dplyr::mutate(Return = Mean_Price_Class/lag(Mean_Price_Class)-1) %>%
  dplyr::select(date, Buy_or_Short, Return)


# Pivotando o banco ----------------------------
Decil_Price <- Decil_Price %>% pivot_wider(names_from = Buy_or_Short , values_from = Return) 


# Criando variável SMB ------------------------------
Decil_Price <- Decil_Price %>% dplyr::mutate(SMB = Long - Short)

#Decil_Price <- Decil_Price[-1,]

Decil_Price <- Decil_Price %>% drop_na(Long, SMB)

Decil_Price <- Decil_Price %>% dplyr::filter(SMB >= -1)
Decil_Price <- Decil_Price %>% dplyr::filter(SMB <= 1.5)

Decil_Price <- Decil_Price %>%
              dplyr::filter(!date %in% c('2014-01-02', '2015-01-02', '2016-01-02',
                                          '2017-01-02', '2018-01-02', '2019-01-02',
                                          '2020-01-02', '2018-05-02', '2018-05-11',
                                          '2020-04-16', '2021-01-04'))


Decil_Price <- Decil_Price %>% mutate(return_acum = cumprod(1 + SMB)-1)

Decil_Price$date <- lubridate::ymd(Decil_Price$date)

# Gráfico --------------------------------
smb_plot <- ggplot(data = Decil_Price, aes(x = date, y = return_acum, group = "all")) +
  geom_line(size = 1.2, color = "deepskyblue3") +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x="", y="Retorno Acumulado (%)",
       colour = "") +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_text(size = 10),
        plot.caption = element_text(size = 7),
        legend.text = element_text(size = 13),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size = 13, color = "dodgerblue4", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size = 14),
        axis.text.x = element_text(face="bold", color="#000000",size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months", expand = c(0,70)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(-0.9, 0.3, 0.15),
                     limits = c(-0.9,0.3))

### Salvando em PNG --------------------------------------------------
ggsave(plot = smb_plot,
       paste0(pasta_graficos, "smb_graph_ff.png"),
       width = 14, height = 7, dpi = 250, units = "in",type = "cairo-png")


fator_smb <- Decil_Price %>%
              dplyr::select(date, SMB, return_acum) %>% 
              `colnames<-`(c('date', 'SMB', 'SMB_acum'))

write.csv2(fator_smb, 
           'fator_smb.csv', row.names = F)


### Salvando em PNG --------------------------------------------------
ggsave(plot = smb_plot, "smb_graph_ff.png",
       width = 14, height = 7, dpi = 250, units = "in",type = "cairo-png")






