


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
library(timetk)
library(PerformanceAnalytics)

options(scipen = 999)

pasta_graficos <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\Graficos\\'

pasta_dados <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\'

setwd('C:\\Users\\cleit\\Desktop\\TCC\\Dados')

fator_hml <- read.csv2('fator_hml.csv')
fator_smb <- read.csv2('fator_smb.csv')
fator_mercado <- read.csv2('fator_mercado.csv')

fator_hml$date <- lubridate::ymd(fator_hml$date)
fator_smb$date <- lubridate::ymd(fator_smb$date)
fator_mercado$date <- lubridate::ymd(fator_mercado$date)

fatores_ff <- purrr::reduce(list(fator_hml,fator_smb,fator_mercado), 
                            dplyr::inner_join, by = 'date')

fatores_ff$ano <- lubridate::year(fatores_ff$date)

fatores_ff <- fatores_ff %>% dplyr::rename('Rm_minus_Rf' = 'Market')

fatores_ff <- fatores_ff %>% dplyr::select(-c("HML_acum", "SMB_acum", "Market_acum"))

rm(fator_hml, fator_mercado, fator_smb)

factor_df_train <- fatores_ff %>%
  dplyr::filter(date >= as.Date("2011-01-03") & date <= as.Date("2018-09-10"))

factor_df_test <- fatores_ff %>% 
  dplyr::filter(date > as.Date("2018-09-10"))


portfolio <- factor_df_test %>% dplyr::select(-c('ano'))

portfolio <- portfolio %>% timetk::tk_xts()

portfolio_retorno <- PerformanceAnalytics::Return.portfolio(portfolio,
                                                            weights = rep(.33,3))

portfolio_retorno <- data.frame(date=index(portfolio_retorno),
                                coredata(portfolio_retorno))

portfolio_retorno <- portfolio_retorno %>%
                     dplyr::mutate(return_acum = cumprod(1 + portfolio.returns)-1)


ff_portfolio <- portfolio_retorno %>% 
  ggplot(aes(x = date, y = return_acum, group = "all")) +
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
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 months", expand = c(0,50)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(-0.3,0.15,.05),
                     limits = c(-0.3,0.15))


### Salvando em PNG --------------------------------------------------
ggsave(plot = ff_portfolio,
       paste0(pasta_graficos, "ff_portfolio_retorno.png"),
       width = 14, height = 7, dpi = 250, units = "in",type = "cairo-png")


portfolio_retorno <- portfolio_retorno %>% 
                     dplyr::select(date, return_acum) %>%
                     `colnames<-`(c('date', 'fama_french'))

write.csv2(portfolio_retorno,
           paste0(pasta_dados, 'fama_french_portfolio.csv'), row.names = F)

