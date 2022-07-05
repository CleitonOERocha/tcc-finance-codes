


####################################################################
####################################################################
## ESCRITO POR: Cleiton Rocha - www.linkedin.com/in/cleitonoerocha/
## EMAIL:  cleitonotavio058@gmail.com 
## LICENÇA:  GPLv3
## DATA: 05/07/2022
## CÓDIGO DISPONÍVEL EM: 
## https://github.com/CleitonOERocha/tcc-finance-codes
####################################################################
####################################################################


library(tidyverse)
library(lubridate)
library(quantmod)

setwd('C:\\Users\\cleit\\Desktop\\TCC\\Dados')

pasta_graficos <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\Graficos\\'

pasta_dados <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\'


quantmod::getSymbols('^BVSP')

bovespa <- data.frame(date=index(BVSP), coredata(BVSP))

bovespa <- bovespa %>%
  dplyr::select(date, BVSP.Close, BVSP.Adjusted) %>% 
  `colnames<-`(c('date', 'close', 'adjusted')) %>% 
  dplyr::filter(date >= as.Date("2011-01-03") & date <= as.Date("2021-12-30"))


#########################################################################
######### Retorno Mensal ------------------------------------------------
#########################################################################

bovespa_mensal <- bovespa %>%
  dplyr::mutate(mes = lubridate::month(date),
                ano = lubridate::year(date)) %>% 
  dplyr::mutate(mes_ano = paste0(mes, "_", ano)) %>% 
  dplyr::group_by(mes_ano) %>% 
  dplyr::summarise(media_mes = mean(adjusted, na.rm = TRUE)) %>% 
  as.data.frame()

benchmark = 1.1465/nrow(bovespa_mensal)

bovespa_mensal <- bovespa_mensal %>%
  dplyr::mutate(mes_ano = lubridate::dmy(paste0('01_', mes_ano))) %>% 
  arrange(mes_ano) %>% 
  dplyr::mutate(Return = media_mes/lag(media_mes)-1) %>%
  drop_na(Return) %>%
  dplyr::mutate(Return_menos_txa_livre = Return - benchmark) %>% 
  dplyr::mutate(return_acum = cumprod(1 + Return)-1,
                return_acum_menos_txa_livre = cumprod(1 + Return_menos_txa_livre)-1)


ggplot(data = bovespa_mensal, aes(x = mes_ano,
                                  y = return_acum_menos_txa_livre, group = "all")) +
  geom_line(size = 1.2, color = "deepskyblue3") +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x="", y="Retorno Acumulado (%)",
       colour = "",
       title = "Fator Mercado - Fama French",
       caption = "Fonte: Yahoo! Finance | Elaboração: Cleiton Otavio da Exaltação Rocha") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 13),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size=17, color = "dodgerblue4", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size=10),
        axis.text.x = element_text(face="bold", color="#000000",size=10),
        axis.title.y = element_text(size = 10)) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 years", expand = c(0,50)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(-0.8,0.1,.1),
                     limits = c(-0.8,0.1))


#########################################################################
######### Retorno Diário ------------------------------------------------
#########################################################################


benchmark_diario = 1.1465/nrow(bovespa)

bovespa_diario <- bovespa %>%
  dplyr::mutate(Return = adjusted/lag(adjusted)-1) %>%
  drop_na(Return) %>%
  dplyr::mutate(Return_menos_txa_livre = Return - benchmark_diario) %>% 
  dplyr::mutate(return_acum = cumprod(1 + Return)-1,
                return_acum_menos_txa_livre = cumprod(1 + Return_menos_txa_livre)-1)


fator_mercado <- ggplot(data = bovespa_diario, aes(x = date,
                                                   y = return_acum_menos_txa_livre, group = "all")) +
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
                     breaks = seq(-0.8,0.1,.1),
                     limits = c(-0.8,0.1))


### Salvando em PNG --------------------------------------------------
ggsave(plot = fator_mercado,
       paste0(pasta_graficos, "fator_mercado_graph_ff.png"),
       width = 14, height = 7, dpi = 250, units = "in",type = "cairo-png")



fator_mercado <- bovespa_diario %>%
                 dplyr::select(date,
                               Return_menos_txa_livre,
                               return_acum_menos_txa_livre) %>%
  `colnames<-`(c('date', 'Market', 'Market_acum'))


write.csv2(fator_mercado, 
           paste0(pasta_dados, 'fator_mercado.csv'), row.names = F)
















