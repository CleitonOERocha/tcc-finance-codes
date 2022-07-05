


options(scipen = 999)

# pacotes R ------------------------

library(plotly)
library(tidyverse) 
library(gridExtra) 
library(ggplot2) 
library(PerformanceAnalytics) 
library(quantmod) 
library(fPortfolio) 
library(tseries) 
library(ggrepel)
library(actuar)
library(tseries)
library(reticulate)
library(xts)
library(lubridate)
library(viridis)
library(timetk)
library(tidyquant)

#install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)

setwd("C:\\Users\\pc\\Desktop\\TCC\\Dados")

pasta_graficos <- 'C:\\Users\\cleit\\Desktop\\TCC\\Dados\\Graficos\\'

###############################################################################
############ Carregando a ajustando dados -------------------------------------
###############################################################################


# Carregando lista de ativos -----------------------
indicadores_setores_v2 <- read.csv2("indicadores_setores_v3.csv")
small_caps <- indicadores_setores_v2 %>% dplyr::filter(Classificação == "Small Cap")

indicadores_setores_v2$Classificação

# Carregando dados de preços dos ativos até o dia 11/11/2021 ---------------
price_data <- read.csv2("acoes_setores_2022_04_21.csv")


# Transformando em date ---------------------------------------
price_data$date <- ymd(price_data$date)

price_data <- price_data %>% dplyr::filter(symbol %in% small_caps$acoes_sa)

datas <- data.frame(datas = unique(price_data$date)) %>% arrange(datas) %>% as.vector()

datas[round(length(datas$datas)*0.7,0),]


# Criando dataframe dos ativos únicos em cada ano de acordo com o maior volume -----
ativos_unique <- price_data %>%
  dplyr::mutate(ano = lubridate::year(date)) %>%
  dplyr::group_by(ano, symbol) %>%
  dplyr::summarise(media_vol = sum(volume, na.rm = TRUE)) %>% 
  as.data.frame() %>% 
  dplyr::mutate(ativo_no_number = stringr::str_extract(symbol, "^.{4}")) %>% 
  dplyr::group_by(ano, ativo_no_number) %>% 
  top_n(1, media_vol) %>% 
  as.data.frame() %>% 
  dplyr::mutate(id_ativo = paste0(symbol,"_",ano))


price_data <- price_data %>%
  dplyr::mutate(ano = lubridate::year(date)) %>%
  dplyr::mutate(id_ativo = paste0(symbol,"_",ano))


price_data <- price_data %>% dplyr::filter(symbol != 'CEGR3.SA')


price_data <- price_data %>%
  dplyr::filter(id_ativo %in% ativos_unique$id_ativo) %>% 
  dplyr::filter(adjusted >= 0) %>% 
  dplyr::filter(date <= datas[round(length(datas$datas)*0.7,0),]) %>% 
  as.data.frame() %>% 
  dplyr::select(symbol, date, adjusted) %>%
  spread(symbol,adjusted)


price_data <- price_data[,!is.na(price_data[1,])]

price_data <- price_data %>% as.data.frame()


price_data <- price_data%>% tk_xts()

# Se a ação n tiver preço, retorne o último valor dela ----------------------
price_data <- na.locf(price_data, fromLast = FALSE)


###############################################################################
# Calculando os retornos do ativos --------------------------------------------
###############################################################################


# Retorno pelo metódo 'Log' -----------------------------------
retornos = Return.calculate(price_data, method = "log")


# Retornos começam na primeira data; nenhuma posição anterior, retorno 0
retornos[1,] = 0 


###############################################################################
### Calculando o Benchmark: Taxa DI diária no período -------------------------
###############################################################################

# https://calculadorarendafixa.com.br/# 
# https://www.contadordedias.com.br/contador-de-dias/

# percentual em decimal/quantidade de dias

datas_split <- datas %>% dplyr::filter(datas >= as.Date("2011-01-03") & 
                                         datas <= as.Date("2018-09-10"))

nrow(datas_split)

benchmark = 1.1465/nrow(datas_split)


###############################################################################
### Calculando a média do retorno diário e a vol dos ativos -------------------
###############################################################################


# Matriz de covariância de todas as ações -------------------
Cov = cov(retornos)


# df to xts -------------------------------
retornos_df <- data.frame(date=index(retornos), coredata(retornos))


# Média do retorno e vol ------------------------------------------
retornos_df <- retornos_df %>%
  gather(acao, retorno_acao, -date) %>%
  dplyr::group_by(acao) %>%
  dplyr::summarise(media = mean(retorno_acao),
                   var = sd(retorno_acao)) 


# nome das colunas ---------------------------------
colnames(retornos_df) = c("Acao", "Retorno Medio", "Volatilidade")



# Calculando o Sharpe  ----------------------
retornos_df <- retornos_df %>% 
               dplyr::mutate(Sharpe = (`Retorno Medio` - benchmark)/Volatilidade)


# Trazendo o setor para o df -----------------------------------------
retornos_df <- left_join(retornos_df,
                         small_caps %>% dplyr::select(acoes_sa, Setor),
                         by = c("Acao" = "acoes_sa"))



###############################################################################
### Gráfico da volatilidade/retorno de cada ação ------------------------------
###############################################################################


retorno_plot <- ggplot(retornos_df, aes(x = Volatilidade,
                                        y = `Retorno Medio`, 
                                        label = Acao,
                                        colour = Setor)) +
  geom_point(size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Volatilidade",
       y = "Retorno Diário",
       colour = "Setor:") +
  theme_classic() +
  scale_x_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(0,0.1, 0.02),
                     limits = c(0,0.1)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size = 13, color = "dodgerblue4", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size = 14),
        axis.text.x = element_text(face="bold", color="#000000",size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16)) +
  scale_colour_discrete(
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"), 
      keywidth = unit(30/length(labels), units = "mm"),
      label.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      reverse = FALSE,
      label.position = "top")) +
  guides(col = guide_legend(nrow = 2))


### Salvando em PNG ###
ggsave(plot = retorno_plot, 
       paste0(pasta_graficos, "retorno_risco_smallcaps.png"),
       width = 14, height = 6, dpi = 250, units = "in",type = "cairo-png") 



x_date <- data.frame(date=index(retornos), coredata(retornos))
x_date[,1]

retornos_plot_df <-  data.frame(date=index(retornos), coredata(retornos))
retornos_plot_df <- retornos_plot_df %>% gather(ativo, retorno, -date)

ret_diario_plot <- ggplot(retornos_plot_df, aes(x = date, y = retorno, colour = ativo)) +
  geom_line() +
  theme_classic() +
  labs(x = "",
       y = "Retorno diário (%)",
       colour = "Setor:") +
  scale_x_date(date_labels = "%Y", breaks = "1 year", expand = c(0,50)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(-1.2, 1.8, 0.4),
                     limits = c(-1.2, 1.8)) +
  theme(legend.position = "none",
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size = 13, color = "dodgerblue4", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size = 14),
        axis.text.x = element_text(face="bold", color="#000000",size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))



### Salvando em PNG ###
ggsave(plot = ret_diario_plot,
       paste0(pasta_graficos, "retorno_diario_smallcaps.png"),
       width = 14, height = 6, dpi = 250, units = "in",type = "cairo-png") 



###############################################################################
### Montando a fronteira eficientede Markowitz --------------------------------
###############################################################################


# n observações para realizar Markowitz
n = 20000

# ativos para realizar a simulação
set.seed(1544)
x = nrow(retornos_df)

# Sorteando as ações --------------------
acoes_sorteadas <- sample(retornos_df$Acao, 
                          size = x, 
                          replace = FALSE)


print(acoes_sorteadas) 


# Retorno/Vol das ações escolhidas ----------------------

acoes_escolhidas_aleatorio <- retornos_df %>%
  dplyr::filter(Acao %in% acoes_sorteadas) %>%
  arrange(factor(Acao, levels = acoes_sorteadas))


retornos_sorteados <- retornos[,acoes_sorteadas]


# Matriz de covariância das ações -------------------
Cov_sorteados = cov(retornos_sorteados) # Matriz de covariância das ações

#View(cor(Cov_sorteados))

# Matriz de pesos -----------------------
w <- matrix(data = actuar::rpareto(n*x, 
                                   shape = 1,
                                   scale = 1),
            nrow = n, 
            ncol = x, 
            byrow = TRUE)

for(i in c(1:n)) {
  
  w[i,] = w[i,] / sum(w[i,])
  
}


# Montando a volatilidade para cada peso e cada carteira -----------------
# Usaremos a forma algébrica

vol_w = rep(NA, n)

for(i in c(1:n)) {
  
  w_momento = matrix(data = w[i,],
                     ncol = 1)
  
  # Transposta do vetor de peso da carteira i
  # multiplicação matricial matriz de covariância
  # multiplicação matricial do vetor de peso da carteira i
  
  
  vol_momento = sqrt( t(w_momento) %*% Cov_sorteados %*% w_momento )
  
  # Armazenando a volatilidade de cada carteira
  
  vol_w[i] = vol_momento
  
}

# Montando o retorno para cada peso -----------------

ret_w = rep(NA, n)

# Peso de cada ação no retorno; 
# será apagado a cada iteração

soma = rep(NA, x) 

for(i in c(1:n)) {
  
  for(j in c(1:x)) {
    
    # Peso aleatório vezes retorno médio de cada ação
    
    soma[j] = w[i,j] * acoes_escolhidas_aleatorio$`Retorno Medio`[j]
    
  }
  
  # Retorno da carteira é a soma dos retornos de cada ação
  
  ret_w[i] = sum(soma)
  
}

###############################################################################
### Gráfico da fronteira ------------------------------------------------------   
###############################################################################

# Unindo ambas informações em um dataframe apenas:

markowitz <- tibble(ret_w, vol_w)

markowitz <- markowitz %>%
  dplyr::mutate(Sharpe = (ret_w-benchmark)/vol_w)

names(markowitz) = c("Retorno Medio", "Volatilidade", "Sharpe")

min_var <- markowitz[which.min(markowitz$Volatilidade),]
max_sr <- markowitz[which.max(markowitz$Sharpe),]


p <- markowitz %>%
  ggplot(aes(x = Volatilidade, y = `Retorno Medio`*100, color = Sharpe)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ",")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size = 13, color = "dodgerblue4", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size = 14),
        axis.text.x = element_text(face="bold", color="#000000",size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16)) +
  labs(x = 'Risco Anualizado (%)',
       y = 'Retornos Anualizados (%)'
  ) +
  geom_point(aes(x = Volatilidade,
                 y = `Retorno Medio`), data = min_var, color = 'red', size = 3) +
  geom_point(aes(x = Volatilidade,
                 y = `Retorno Medio`*100), data = max_sr, color = 'red', size = 3) +
  annotate('text', x = 0.030, y = 0.13, label = "Portfólio com a melhor tangência", size = 5) +
  annotate('text', x = 0.01, y = -0.10, label = "Portfólio de\nvariação\nmínima", size = 5) +
  annotate(geom = 'segment', x = 0.030, xend = 0.0225,  y = 0.12, 
           yend = 0.07, color = 'black', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.009, xend = 0.0065,  y = -0.065, 
           yend = -0.01, color = 'black', arrow = arrow(type = "open")) +
  scale_colour_viridis(
    option = "viridis", 
    direction = -1,
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(100, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      label.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    ))


### Salvando em PNG ###
ggsave(plot = p, paste0(pasta_graficos, "fronteira_smallcaps.png"),
       width = 12, height = 5.5, dpi = 250, units = "in",type = "cairo-png") 



###################################################### 
# Montando a fronteira eficiente ---------------------
######################################################


# Pacote {fPortfolio}
# Monta a simulação apenas com esse código --------------------

fronteira = portfolioFrontier(as.timeSeries(retornos_sorteados)) 

# Gráfico da fronteira ----------------------------

frontierPlot(fronteira,
             pch = 20,
             cex = 1.5,
             type = "o",
             lwd = 2) 

singleAssetPoints(fronteira,
                  col = c(1:x),
                  pch = 20,
                  cex = 1.5) # Retorno/Vol de cada ação

# Quantidade de simulações ---------------------------------
monteCarloPoints(fronteira,
                 mcSteps = n, 
                 pch = 20,
                 cex = 0.1,
                 col = "steelblue") # Simulações com pesos aleatórios


# Distribuição/comportamento de retorno e vol das carteiras simuladas ----------------

vol_w_df <- data.frame(valor = vol_w)

vol_diaria_carteiras <- ggplot(data = vol_w_df, aes(x = valor)) +
  geom_histogram(bins = 200, fill = "#173F5F", colour = "white") +
  labs(x="Volatilidade Diária (%)", y="Frequência",
       colour = "",
       title = "Volatilidade das Carteiras Simuladas (%) - SmallCaps") + #,
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size = 15, hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size = 14),
        axis.text.x = element_text(face="bold", color="#000000",size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16)) +
scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(0, 0.07, .01),
                     limits = c(0,0.07))



ret_w_df <- data.frame(valor = ret_w)

ret_diaria_carteiras <- ggplot(data = ret_w_df, aes(x = valor)) +
  geom_histogram(bins = 200, fill = "#bc5090", colour = "white") +
  labs(x="Retornos Diários (%)", y="Frequência",
       colour = "",
       title = "Retornos Diários das Carteiras Simuladas (%) - SmallCaps") +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size = 15, hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size = 14),
        axis.text.x = element_text(face="bold", color="#000000",size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16)) +
scale_x_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","))


plot_vol_ret_diaria <- arrangeGrob(ret_diaria_carteiras, 
                                   vol_diaria_carteiras, ncol = 2)

### Salvando em PNG ###
ggsave(plot = plot_vol_ret_diaria,
       paste0(pasta_graficos, "retorno_vol_carteiras_simulacao_smallcaps.png"),
       width = 14, height = 6,
       dpi = 250, units = "in",type = "cairo-png") 


###############################################################
#### Obtendo carteiras de min var e carteira ótima ------------
###############################################################


# Carteira de mínima variância ------------------------------------

# mínima_variância = fronteira_by_hand %>%
#   arrange(Volatilidade) %>%
#   slice(1)


# Descobrindo a carteira ótima -------------------------

ótimo <- IntroCompFinR::tangency.portfolio(er = acoes_escolhidas_aleatorio$`Retorno Medio`,
                                           cov.mat = Cov_sorteados, 
                                           risk.free = 0.0001, 
                                           shorts = FALSE) 


ret_esp_ótimo = ótimo[[2]] # Retorno esperado da carteira ótima
vol_esp_ótimo = ótimo[[3]] # Volatilidade da carteira ótima

carteira_ótima = tibble(ret_esp_ótimo, vol_esp_ótimo)

names(carteira_ótima) = c("Retorno Medio", "Volatilidade")


###############################################################  
### Tabela com resultados -------------------------------------   
###############################################################


# Para os pesos ótimos para cada ação numa tabela -----------------

pesos_ótimos = ótimo[[4]] 

matriz_ótima = matrix(data = NA,
                      nrow = 3,
                      ncol = length(acoes_sorteadas))

matriz_ótima[1,] = paste0(round(pesos_ótimos*100, digits = 2))
matriz_ótima[2,] = paste0(round(acoes_escolhidas_aleatorio$`Retorno Medio`*100, digits = 2)) 
matriz_ótima[3,] = paste0(round(acoes_escolhidas_aleatorio$Volatilidade*100, digits = 2)) 

matriz_ótima = as.data.frame(matriz_ótima, 
                             row.names = c("Pesos",
                                           "Retorno",
                                           "Volatilidade"))

names(matriz_ótima) = acoes_escolhidas_aleatorio$Acao


matriz_otima_long <- matriz_ótima %>% gather(ativo, valor)

matriz_otima_long <- matriz_otima_long %>% 
                     mutate(info = rep(c("Pesos", 
                                         "Retorno", 
                                         "Volatilidade"), nrow(retornos_df)))

matriz_otima_long <- matriz_otima_long %>% spread(info, valor)

matriz_otima_long <- matriz_otima_long %>% dplyr::filter(Pesos != 0)


matriz_otima_long$Pesos <- as.numeric(matriz_otima_long$Pesos)
matriz_otima_long$Retorno <- as.numeric(matriz_otima_long$Retorno)
matriz_otima_long$Volatilidade <- as.numeric(matriz_otima_long$Volatilidade)


write.csv2(matriz_otima_long, "carteira_eficiente_smallcaps.csv", row.names = F)

######################################################################
############ Gerando dados ------------------------------------------
######################################################################


carteira_eficiente <- read.csv2("carteira_eficiente_smallcaps.csv")


carteira_eficiente$Pesos <- as.numeric(carteira_eficiente$Pesos)


cotacao_2 <- tq_get(carteira_eficiente$ativo,
                    from = "2018-09-10",
                    to = "2021-12-31",
                    get = "stock.prices")


# Removendo ações cujo volume no período seja menor que R$ 100.000,00 e transformando df em xts ------------------
cotacao_2 <- cotacao_2 %>%
  dplyr::select(symbol, date, adjusted) %>%
  spread(symbol,adjusted) %>%
  tk_xts()


# Se a ação n tiver preço, retorne o último valor dela ------------------------------
cotacao_2 <- na.locf(cotacao_2, fromLast = TRUE)


# calculando o retorno discreto
retornos = Return.calculate(cotacao_2, method = "log")


# Retornos começam na primeira data; nenhuma posição anterior, retorno 0
retornos[1,] = 0 

# pesos gerados do outro script
portReturns <- Return.portfolio(retornos,
                                (carteira_eficiente$Pesos/100),
                                rebalance_on = "years")

portReturns_est <- Return.portfolio(retornos,
                                    (carteira_eficiente$Pesos/100),
                                    rebalance_on = "years",
                                    verbose = TRUE,
                                    wealth.index = TRUE)


# transformando xts em dataframe
carteira <- data.frame(date=index(portReturns), coredata(portReturns))


# criando coluna com nome da carteira e mudando nome do retorno
carteira <- carteira %>%
  dplyr::mutate(tipo = "Carteira Eficiente Markowitz - SmallCaps") %>%
  dplyr::rename(retorno_diario = "portfolio.returns")



# retorno acumulado
port_cumulative_ret <- carteira %>% mutate(cr = cumprod(1 + retorno_diario))

port_cumulative_ret_xts <- port_cumulative_ret %>% 
  dplyr::select(date, cr) %>%
  dplyr::mutate(cr = cr - 1) %>% 
  dplyr::rename(`Carteira Eficiente Markowitz - SmallCaps` = "cr") %>%
  tk_xts()


######################################################################
##### Lendo dados da carteira 1 --------------------------------------
######################################################################

carteira_1 <- read.csv2("retorno_acum_ibov_markowitz_22042022.csv")

carteira_1$date <- lubridate::ymd(carteira_1$date)

carteira_1 <- carteira_1 %>% spread(tipo, media_ret) %>% tk_xts()

merge_port_full <- merge(port_cumulative_ret_xts,
                         carteira_1)[time(port_cumulative_ret_xts), ]

######################################################################
############## Unindo bancos e gerando gráfico -----------------------
######################################################################

merge_port_full_df <- data.frame(date=index(merge_port_full), 
                                 coredata(merge_port_full))
merge_port_full_df <- merge_port_full_df %>% gather(tipo, media_ret, -date)


merge_port_full_df$tipo <- ifelse(
                           merge_port_full_df$tipo == "Portfólio...Markowitz",
                           "Markowitz",
                            merge_port_full_df$tipo)

merge_port_full_df$tipo <- ifelse(
                            merge_port_full_df$tipo == "Carteira.Eficiente.Markowitz...SmallCaps",
                            "Markowitz - SmallCaps",
                             merge_port_full_df$tipo)

merge_port_full_df <- merge_port_full_df %>% drop_na(media_ret)

ret_acum_graph <- ggplot(merge_port_full_df, aes(x = date, y = media_ret, colour = tipo)) +
  geom_line(size = 1.3) +
  labs(x="", y="Retorno Acumulado",
       colour = "",
       title = "Carteira de Markowitz x Ibovespa") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 13),
        strip.text = element_text(face = "bold", size = "11"),
        plot.title = element_text(size=17, color = "dodgerblue4", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, face="bold", size = 12),
        axis.text.y = element_text(face="bold", color="#000000",size=10),
        axis.text.x = element_text(face="bold", color="#000000",size=10),
        axis.title.y = element_text(size = 10)) +
  scale_x_date(date_labels = "%b\n%Y", breaks = "2 months", expand = c(0,50)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = ".", decimal.mark = ","),
                     breaks = seq(-.3,1.3,.3),
                     limits = c(-.3,1.3)) +
  scale_colour_manual(values = c("#173F5F", "#bc5090", "#ffa600"),
                      drop = FALSE,
                      guide = guide_legend(
                        direction = "horizontal",
                        keyheight = unit(2, units = "mm"), 
                        keywidth = unit(50/length(labels), units = "mm"),
                        label.hjust = 0.5,
                        nrow = 1,
                        byrow = TRUE,
                        reverse = FALSE,
                        label.position = "top")) 


### Salvando csv --------------------------------------------------
write.csv2(merge_port_full_df, "retorno_acum_ibov_markowitz_all.csv", row.names = F)



### Salvando em PNG --------------------------------------------------
ggsave(plot = ret_acum_graph,
       "ret_acum_graph_all.png",
       width = 14, height = 7, dpi = 250, units = "in",type = "cairo-png")



