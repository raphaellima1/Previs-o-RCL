pacman::p_load(tidyverse, readxl, lubridate, glue, ggpubr,gridExtra,grid,
               ggthemes,officer, gt, flextable, kableExtra, magrittr,rvg, 
               mschart, janitor,sidrar, lemon,zoo, rlang, forecast,tseries, xlsx,
               mFilter)

tabela <-  read_excel("~/Trabalhos em R/previsao-receita/RTL E RESULTADO PRIMÁRIO/Receita_primaria_mensal_2018_2024.xlsx", skip = 4) |> 
  filter(...2 == 'RECEITA PRIMÁRIA TOTAL') |> 
  select(-1,-3,-4)|>  
  pivot_longer(cols = `201801`:`202406`) |> 
  mutate(data = ymd(paste0(name, '01'))) |> 
  select(data, value)


hp_filter <- hpfilter(tabela$value, freq = 14400)
tabela$hp <- hp_filter$trend
tabela |> 
  ggplot()+
  geom_line(aes(x  = data, y = value))+
  geom_line(aes(x = data, y = hp))

write.xlsx(tabela, './RTL E RESULTADO PRIMÁRIO/raceita_primaria.xlsx')


ts_rec_pri <- ts(tabela$value, start = c(2018, 1), end = c(2024, 6), frequency = 12)
ts_rec_pri



plot(decompose(ts_rec_pri))

ggtsdisplay(ts_rec_pri)

acf(ts_rec_pri)

adf_result <- adf.test(ts_rec_pri)
print(adf_result)

kpss_result <- kpss.test(ts_rec_pri)
print(kpss_result)


# # Diferenciação da série temporal
# diff_ts_RTL <- diff(ts_RTL)
# 
# # Verificação da estacionaridade após diferenciação
# adf_result_diff <- adf.test(diff_ts_RTL)
# kpss_result_diff <- kpss.test(diff_ts_RTL)
# 
# print(adf_result_diff)
# print(kpss_result_diff)


# para que se possa comparar os dados gerados. 
modelo_treino <- window(ts_rec_pri, end = c(2023,6)) # Amostra treino
modelo_teste <- window(ts_rec_pri, start = c(2023,7))




# Encontrar o melhor modelo ARIMA
best_arima_model <- auto.arima(modelo_treino, seasonal = T,
                               ic = c("aicc", "aic", "bic"),
                               test = c("kpss", "adf", "pp"))
summary(best_arima_model)
checkresiduals(best_arima_model)

accuracy(best_arima_model$fitted, modelo_treino)

arima <- forecast(best_arima_model, h = 12)


# 
# arima_model <- arima(modelo_treino, order = c(2,1,0))
# summary(arima_model)
# checkresiduals(arima_model)
# residuals <- residuals(arima_model)
# 

# tsdisplay(residuals, main="Resíduos do Modelo ARIMA(2,1,0)")

t <- 12

# metodo holt 
m_holt<-holt(modelo_treino, h=t)
accuracy(m_holt, modelo_teste)
checkresiduals(m_holt)

m_holtw<-hw(modelo_treino, seasonal="additive", h=t )
accuracy(m_holtw)
checkresiduals(m_holtw)

m_ets<-forecast(ets(modelo_treino, model = 'MMN'), h=t)
accuracy(m_ets)
checkresiduals(m_ets)

accuracy(m_ets)



autoplot(modelo_treino,size = 1)+
  autolayer(arima, series="ARIMA(0,1,0)", PI=F,size = 1)+
  autolayer(m_holt, series="Holt Method", PI=F,size = 1)+
  autolayer(m_holtw, series="Holt_Winters", PI=F,size = 1)+
  autolayer(modelo_teste, series="teste",size = 1)+
  autolayer(m_ets, series="ETS", PI=FALSE,size = 1)+
  xlab('Month/Year')+ylab('RCL em R$')+
  ggtitle('Forecasts RCL sem IPASGO')+
  guides(colour=guide_legend(title='Forecast'))
