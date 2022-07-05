


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
library(Cairo)

setwd("C:\\Users\\cleit\\Desktop\\TCC\\Dados")

pasta_graficos <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\Graficos\\'


##############################################################
###### Carteiras de Markowitz e Ibovespa ---------------------
##############################################################

markowitz <- read.csv2("retorno_acum_ibov_markowitz_all.csv")

markowitz <- markowitz %>% 
             dplyr::mutate(date = lubridate::ymd(date)) %>%
             dplyr::filter(date <= "2021-12-31")

markowitz$date <- lubridate::ymd(markowitz$date)

##############################################################
###### Carteiras de Fama French ------------------------------
##############################################################

fama_french <- read.csv2('fama_french_portfolio.csv')

fama_french <- fama_french %>%
               dplyr::mutate(tipo = 'Fama-French') %>%
               dplyr::rename(media_ret = fama_french)

fama_french$date <- lubridate::ymd(fama_french$date)

# Unindo Markowitz e Fama-French -----------------------------

carteiras <- bind_rows(fama_french, markowitz)

##############################################################
###### Taxa Livre de Risco -----------------------------------
##############################################################

# https://calculadorarendafixa.com.br#
# https://www.contadordedias.com.br/contador-de-dias/

benchmark = (1.15793155-1)/1207

txa_livre_risco <- data.frame(date = seq(as.Date('2018-09-10'),as.Date('2021-12-30'),1))

txa_livre_risco <- txa_livre_risco %>%
  dplyr::mutate(txa_livre_risco = benchmark) %>%
  dplyr::mutate(txa_livre_risco = cumsum(txa_livre_risco)) %>% 
  `colnames<-`(c('date', 'media_ret')) %>% 
  dplyr::mutate(tipo = 'Taxa Livre de Risco')


todos_ativos <- bind_rows(carteiras, txa_livre_risco)


todos_ativos$tipo <- ifelse(
                      todos_ativos$tipo == 'Carteira Markowitz - SmallCaps', 'Markowitz - SmallCaps',
                      todos_ativos$tipo)


todos_ativos$tipo <- ifelse(
                     todos_ativos$tipo == 'Carteira Markowitz', 
                     'Markowitz',
                     todos_ativos$tipo)


ret_acum_graph <- ggplot(todos_ativos, aes(x = date, y = media_ret, colour = tipo)) +
  geom_line(size = 1.3) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(x="", y="Retorno Acumulado (%)",
       colour = "") +
  theme_classic() +
  theme(legend.position = "top",
        legend.title = element_text(size = 10),
        plot.caption = element_text(size = 7),
        legend.text = element_text(size = 15),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size = 13, color = "dodgerblue4", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size = 14),
        axis.text.x = element_text(face="bold", color="#000000",size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16)) +
  scale_x_date(date_labels = "%b\n%Y", breaks = "3 month", expand = c(0,15)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(-0.3,1.20,0.15),
                     limits = c(-0.3,1.20)) +
  scale_colour_manual(values = c("#ff6361", "#ffa600", "#003f5c", "#bc5090", "deepskyblue3"),
                      drop = FALSE,
                      guide = guide_legend(
                        direction = "horizontal",
                        keyheight = unit(2, units = "mm"), 
                        keywidth = unit(60/length(labels), units = "mm"),
                        title.position = 'top',
                        title.hjust = 0.5,
                        label.hjust = 0.5,
                        nrow = 1,
                        byrow = TRUE,
                        reverse = FALSE,
                        label.position = "top")) 



### Salvando em PNG ###
ggsave(plot = ret_acum_graph,
       paste0(pasta_graficos, "all_metodos.png"),
       width = 14, height = 7, dpi = 250, units = "in",type = "cairo-png")
