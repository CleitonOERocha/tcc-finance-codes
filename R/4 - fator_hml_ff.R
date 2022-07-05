

library(tidyverse)
library(lubridate)
library(readxl)

options(scipen = 999)

###############################################################################
############ Carregando dados do MarketCap ------------------------------------
###############################################################################

setwd("C:\\Users\\cleit\\Desktop\\TCC\\Dados\\MarketCap")

pasta_dados <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\'

pasta_graficos <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\Graficos\\'


# Carregando lista de ativos -----------------------
indicadores_setores_v2 <- read.csv2(paste0(pasta_dados, "indicadores_setores_v3.csv"))

indicadores_setores_v2$Ações <- ifelse(
                                indicadores_setores_v2$Ações == "",
                                NA,
                                indicadores_setores_v2$Ações)

indicadores_setores_v2 <- indicadores_setores_v2 %>% drop_na(Ações)

num_acoes_df <- read.csv2("num_acoes_df.csv")

num_acoes_df <- num_acoes_df %>%
                dplyr::mutate(acoes_sa = paste0(symbol,".SA")) %>% 
                dplyr::select(!symbol)


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


rm(price_data, num_acoes_df, indicadores_setores_v2)

###############################################################################
############ Carregando dados do Patrimonio Liq -------------------------------
###############################################################################

# csv com CNPJ e ticket da ação --------------------
cnpj_acoes <- read.csv2('C:\\Users\\cleit\\Desktop\\TCC\\Dados\\cnpj_acoes.csv')

# Ajuste no CNPJ da Tim
cnpj_acoes$CNPJ <- ifelse(cnpj_acoes$CNPJ == '02.421.421/0001-11', 
                          '02.558.115/0001-21', cnpj_acoes$CNPJ)
# Ajuste no CNPJ da Natura
cnpj_acoes$CNPJ <- ifelse(cnpj_acoes$CNPJ == '32.785.497/0001-97',
                          '71.673.990/0001-77', cnpj_acoes$CNPJ)
# Ajuste no CNPJ da Terra Santa
cnpj_acoes$CNPJ <- ifelse(cnpj_acoes$CNPJ == '40.337.136/0001-06',
                          '05.799.312/0001-20', cnpj_acoes$CNPJ)


# csv com CNPJ e cód CVM da ação ----------------------
cnpj_cod_cvm_acoes <- read.csv2(paste0(pasta_dados, 'cnpj_cod_cvm_acoes.csv'))

# csv com PL da ação e cód CVM -----------------------------
patrimonio_liq_empresas <- read.csv2(paste0(pasta_dados, 'patrimonio_liq_empresas.csv'))

# Unindo PL e CNPJ das ações ----------------------------
patrimonio_liq_empresas_v2 <- patrimonio_liq_empresas %>% 
                              left_join(cnpj_cod_cvm_acoes, by = 'CD_CVM')

# Removendo CNPJ's ausentes -------------------------------
patrimonio_liq_empresas_v2 <- patrimonio_liq_empresas_v2 %>% drop_na(CNPJ)

# Unindo PL com ticket da ação ------------------------------
patrimonio_liq_empresas_v3 <- left_join(patrimonio_liq_empresas_v2,
                                        cnpj_acoes, by = 'CNPJ')

# Removendo ticket ausente --------------------------------
patrimonio_liq_empresas_v3 <- patrimonio_liq_empresas_v3 %>% drop_na(Ticker)

patrimonio_liq_empresas_v3 <- patrimonio_liq_empresas_v3 %>%
  dplyr::select(DT_REFER, Ticker, VL_CONTA) %>% 
  dplyr::mutate(Ticker = paste0(Ticker, ".SA")) %>% 
  dplyr::rename(acoes_sa = Ticker,
                ano = DT_REFER,
                patrim_liq = VL_CONTA)



# Obtendo o market cap no meio do ano ------------------------
Market_cap <- price_data_num_acoes %>% 
  dplyr::mutate(mes = lubridate::month(date)) %>% 
  dplyr::mutate(mes_ano = paste0(mes,"_",ano)) %>% 
  dplyr::filter(mes_ano %in% c('6_2011', '6_2012', '6_2013',
                               '6_2014', '6_2015', '6_2016',
                               '6_2017', '6_2018', '6_2019',
                               '6_2020', '6_2021')) %>% 
  dplyr::group_by(ano, acoes_sa) %>% 
  slice(tail(row_number(), 1)) %>% 
  as.data.frame()


# Unindo dataframe do marketcap com df do PL -------------------
Market_cap_PL <- left_join(Market_cap,
                           patrimonio_liq_empresas_v3,
                           by = c('acoes_sa', 'ano'))

# Removendo ativos em PL -------------------------
Market_cap_PL <- Market_cap_PL %>% drop_na(patrim_liq)

###############################################################################
############ Criando Indice HML -----------------------------------------------
###############################################################################

# Criando indice BM --------------------------------
Market_cap_PL <- Market_cap_PL %>% dplyr::mutate(bm_index = patrim_liq/marketcap)

# Agrupando por ativo e ano para obter a média  do BM em cada ano --------------
Market_cap_PL_group <- Market_cap_PL %>%
  dplyr::group_by(ano, acoes_sa) %>%
  dplyr::summarise(bm_index = mean(bm_index, na.rm = TRUE)) %>% 
  as.data.frame()


# Criando grupos do HML --------------------------
Market_cap_PL_group_v2 <- Market_cap_PL_group %>%
  arrange(desc(bm_index)) %>% 
  dplyr::group_by(ano) %>% 
  dplyr::mutate(grupo = ntile(-bm_index, 10)) %>%
  as.data.frame() %>% 
  dplyr::select(acoes_sa, bm_index, grupo, ano) %>% 
  dplyr::mutate(grupo = case_when(grupo %in% 1:3 ~ 'High',
                                  grupo %in% 4:7 ~ 'Medium',
                                  grupo %in% 8:10 ~ 'Low'))



# Removendo ações intermediárias (Medium) -----------------------
Market_cap_PL_group_v2 <- Market_cap_PL_group_v2 %>% 
                          dplyr::filter(grupo != 'Medium')


# Unindo fator HML com os ativos reais ------------------------
Market_cap_PL_v3 <- dplyr::inner_join(price_data_num_acoes,
                                      Market_cap_PL_group_v2,
                                      by = c("acoes_sa", "ano"))


# Obtendo o preço médio de cada grupo por data ------------------------
grupo_Price <- Market_cap_PL_v3 %>%
  dplyr::group_by(date, grupo) %>%
  dplyr::summarise(Mean_Price = mean(adjusted)) %>% 
  as.data.frame()


# Removendo NA's (ativos que não são usados na estratégia long/short) --------------
grupo_Price <- grupo_Price %>% drop_na(grupo)


# Agrupando por variável e obtendo o retorno de cada estratégia ---------------
grupo_Price <- grupo_Price %>% 
  dplyr::group_by(grupo) %>%
  dplyr::mutate(Return = Mean_Price/lag(Mean_Price)-1) %>%
  as.data.frame() %>% 
  dplyr::select(date, grupo, Return)


# Pivotando o banco ----------------------------
grupo_Price <- grupo_Price %>% pivot_wider(names_from = grupo , values_from = Return) 


# Criando variável HML ------------------------------
grupo_Price <- grupo_Price %>% dplyr::mutate(HML = High - Low)


grupo_Price <- grupo_Price %>% drop_na(High, HML)

grupo_Price <- grupo_Price %>% dplyr::filter(HML >= -0.3)
grupo_Price <- grupo_Price %>% dplyr::filter(HML <= 0.3)


grupo_Price <- grupo_Price %>% dplyr::mutate(return_acum = cumprod(1 + HML)-1)

grupo_Price$date <- lubridate::ymd(grupo_Price$date)

hml_plot <- ggplot(data = grupo_Price, aes(x = date, y = return_acum, group = "all")) +
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
                     breaks = seq(-0.5,0.6,.1),
                     limits = c(-0.5,0.6))


### Salvando em PNG --------------------------------------------------
ggsave(plot = hml_plot, 
       paste0(pasta_graficos, "hml_graph_ff.png"),
       width = 14, height = 7, dpi = 250, units = "in",type = "cairo-png")


fator_hml <- grupo_Price %>% 
             dplyr::select(date, HML, return_acum) %>% 
             `colnames<-`(c('date', 'HML', 'HML_acum'))

write.csv2(fator_hml,
           paste0(pasta_dados, 'fator_hml.csv'), row.names = F)



