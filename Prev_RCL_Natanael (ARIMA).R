# Script para previsão de receita da RCL do Estado de Goiás
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)

# df <- readxl::read_excel(file.choose()) %>% 
#   dplyr::filter(ESPECIFICAÇÃO == "RECEITA CORRENTE LÍQUIDA (III) = (I-II)" | ...3 == "IPASGO") %>% 
#   select(2, 5:ncol(.)) %>% 
#   tidyr::gather("mes", "valores", 2:ncol(.)) %>% 
#   dplyr::mutate(ESPECIFICAÇÃO = tidyr::replace_na(ESPECIFICAÇÃO, "IPASGO")) %>%
#   tidyr::spread(key = ESPECIFICAÇÃO, value = valores) %>% 
#   dplyr::rename("RCL" = 3) %>% 
#   dplyr::mutate(RCL_adj = RCL - IPASGO)
# rcl_acum12 <- zoo::rollapply(df$RCL_adj, 12, cumsum, by = 1)


# df <- readxl::read_excel(file.choose()) %>%
#   dplyr::filter(ESPECIFICAÇÃO == "RECEITA CORRENTE LÍQUIDA (III) = (I-II)" | ESPECIFICAÇÃO == "AJUSTES" | ESPECIFICAÇÃO == "RCL_adj" | ESPECIFICAÇÃO == "RCL_acum12") %>%
#   select(2, 5:ncol(.)) %>%
#   tidyr::gather("mes", "valores", 2:ncol(.)) %>%
#   tidyr::spread(key = ESPECIFICAÇÃO, value = valores) %>%
#   dplyr::mutate(mes = as.Date(lubridate::ym(mes))) %>%
#   na.omit()


df <- readxl::read_excel('//sefazarquivos/AEMFPF/DIVERSOS/Previs-o-RCL/RCL2015_a_2024.xlsx', sheet = 1) %>% 
  tidyr::drop_na(RCL_AJUST) %>% 
  select(Data, RCL_AJUST) 

# Tue Jun 18 10:27:26 2024 ------------------------------
colnames(df)<-c("date", "rcl")
df$date<-as.Date(df$date, format="%Y-%m-%d")




# TRATAMENTO DO BANCO DE DADOS -------------------------------------------------
df.rcl <- RREORGFdataR::RREOdata(cod.ibge = 52, year = 2015:2023, period = 6, annex = 3) %>% 
  filter(conta == "RECEITA CORRENTE LÍQUIDA (III) = (I - II)" & !grepl('ÚLTIMOS|PREVISÃO', coluna)) %>% 
  mutate(mes = dplyr::case_when(coluna == "<MR-11>" ~ 01,
                                coluna == "<MR-10>" ~ 02,
                                coluna == "<MR-9>" ~ 03,
                                coluna == "<MR-8>" ~ 04,
                                coluna == "<MR-7>" ~ 05,
                                coluna == "<MR-6>" ~ 06,
                                coluna == "<MR-5>" ~ 07,
                                coluna == "<MR-4>" ~ 08,
                                coluna == "<MR-3>" ~ 09,
                                coluna == "<MR-2>" ~ 10,
                                coluna == "<MR-1>" ~ 11,
                                coluna == "<MR>" ~ 12),
         date = as.Date(paste(exercicio, mes, "01", sep = "-"))) %>% 
  select(date, "rcl" = valor)


df <- readxl::read_excel(file.choose()) %>% 
  dplyr::filter(ESPECIFICAÇÃO == "RECEITA CORRENTE LÍQUIDA (III) = (I-II)") %>% 
  select(2, (ncol(.)-11):ncol(.)) %>% 
  tidyr::gather("mes", "valores", 2:ncol(.)) %>% 
  dplyr::mutate(mes = as.Date(lubridate::ym(mes))) %>% 
  select(-1)


somas_moveis <- rollsum(valores, k = 12, fill = NA, align = "right")


# Definindo série de tempo -----------------------------------------------------
rcl <- ts(df$rcl, start = c(year(df$date)[1], month(df$date)[1]), frequency = 12)

# Verificação gráfica
autoplot(rcl) +
  labs(title = "RCL (sem IPASGO)", x = "Ano", y = "RCL") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, decimal.mark = ',', big.mark = '.'))


plot(decompose(rcl))
trend = decompose(rcl)$trend

# Definição da tendência
# rcl_trend=lm(rcl~c(1:length(rcl)))
# rcl_detrend=residuals(rcl_trend)
# plot.ts(rcl_detrend)
# rcl_detrend = rcl - predict(rcl_trend)
# plot.ts(rcl_detrend1)


# Identificando outliers (série diferenciada) -------------------------------
# out <- grDevices::boxplot.stats(diff(df$RCL_acum12))$out
# out_ind <- which(diff(df$RCL_acum12) %in% c(out))
# df[out_ind, ]
# 
# boxplot(diff(df$RCL_acum12),
#         ylab = "RCL",
#         main = "Boxplot da RCL")
# mtext(paste("Outliers: ", paste(round(out, 2), collapse = ", ")), side = 1)



# ------------------------------------------------------------------------------
ggtsdisplay(rcl, main = "Receita Corrente Líquida")

# ADF (stationary) test 
tseries::adf.test(rcl) # <- Não estacionária
tseries::adf.test(diff(rcl))

# ACF e PACF para RCL sem tendência
# ggtsdisplay(rcl_detrend)


# Realizando o ajuste da série temporal ----------------------------------------
# para que se possa comparar os dados gerados. 
# modelo_treino_td <- ts(rcl_detrend[1:49]) # Amostra treino
# modelo_teste_td <- ts(rcl_detrend[50:54]) # Amostra para confirmação

# Ajuste automaticamente vários modelos e encontre o AIC mais baixo.
# modelo_std <- auto.arima(modelo_treino_td, stepwise = FALSE, seasonal = F, xreg = rcl_trend$fitted.values,
#                      ic = c("aicc", "aic", "bic"),
#                      test = c("kpss", "adf", "pp"))
# modelo_std

# --------------------------------------------------------

# para que se possa comparar os dados gerados. 
modelo_treino <- window(rcl, end = c(2022,12)) # Amostra treino
modelo_teste <- window(rcl, start = c(2023)) # Amostra para confirmação


# Ajuste automaticamente vários modelos e encontre o AICc mais baixo.
modelo <- auto.arima(modelo_treino, stepwise = FALSE, seasonal = T,
                     ic = c("aicc", "aic", "bic"),
                     test = c("kpss", "adf", "pp"))
summary(modelo)
checkresiduals(modelo$residuals) 
# p <- ggtsdisplay(residuals(modelo), plot.type = "histogram")
# p <- p + labs(y = "value", x = "residuals")
# print(p, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))


accuracy(modelo$fitted, modelo_treino)
accuracy(modelo)
# O gráfico de resíduos apresenta média em torno de 0, o ACF não apresenta 
# autocorrelação significativa e o teste Ljung-Box apresenta valor de p 
# superior a 0,05. Esses fatores confirmam que os resíduos podem ser 
# considerados ruído branco. A distribuição também é geralmente normal, sem 
# valores discrepantes, o que significa que os intervalos de previsão serão 
# menos amplos e isso é bom.


# ------------------------------------------------------
# Modelo com base no auto.arima
# arima <- Arima(rcl, order=c(1,1,0), include.drift = F) 
# arima %>%
#   residuals() %>% ggtsdisplay()


# Modelos de previsão ----------------------------------------------------------
previsao.aut <- forecast(modelo, h = 17)
# previsao <- forecast(fit3, h = 24)
# head(previsao$lower) # Limite inferior
# head(previsao$upper) # Limite superior

kableExtra::kbl(previsao.aut) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))




autoplot(previsao.aut, fcol = "red", shadecols = c("#596DD5", "#D5DBFF")) + 
  autolayer(modelo_teste, color = 'blue')+
  # autolayer(modelo_teste)
  # autolayer(rcl) +
  labs(x = "Tempo", y = "RCL (sem IPASGO)", title = "Previsao usando o forecast") 
  # scale_x_continuous(breaks = seq(0,150,15)) +
  # scale_y_continuous(breaks = seq(100, 600, 50)) + 
  # geom_vline(xintercept = , lty = "dashed")
  
  
# Modelo Treino vs Modelo Ajustado

df.prev <- data.frame(
  mes = df$mes,
  rcl = df$RCL_acum12,
  rcl_mfit = c(modelo$fitted, rep(NA, length(df$RCL_acum12) - length(modelo$fitted))),
  prev = c(rep(NA, length(df$RCL_acum12) - length(previsao.aut$mean)), previsao.aut$mean)
  )
# 
#   ggplot(df.prev, aes(x = as.Date(mes))) +
#     geom_line(aes(y = rcl)) +
#     geom_line(aes(y = rcl_mfit), color = 'green') +
#     geom_line(aes(y = prev), color = 'red')
# 
#   autoplot(modelo$fitted, color = 'green') + 
#   autolayer(modelo$x, color = 'blue') +
#     autolayer(previsao.aut)

# Utilizando o modelo obtido no auto.arima para a projeção
modelo2 <- Arima(rcl, order=c(0,2,2), seasonal = c(2,0,0))
summary(modelo2)

previsao.aut2 <- forecast(modelo2, h = 43)
previsao.aut2

autoplot(previsao.aut2, fcol = "red", shadecols = c("#596DD5", "#D5DBFF")) + 
  autolayer(modelo_teste, color = 'blue')+
  labs(x = "Ano", y = "RCL (com ajustes)", title = "Previsao da RCL ajustada (jun. 2024 - dez. 2027)") 


