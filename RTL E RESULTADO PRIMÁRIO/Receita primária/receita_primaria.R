pacman::p_load(tidyverse, readxl, lubridate, glue, ggpubr,gridExtra,grid,
               ggthemes,officer, gt, flextable, kableExtra, magrittr,rvg, 
               mschart, janitor,sidrar, lemon,zoo, rlang, forecast,tseries, xlsx,
               mFilter)

# tabela <-  read_excel("~/Trabalhos em R/previsao-receita/RTL E RESULTADO PRIMÁRIO/Receita primária/Receita_primaria_mensal_2018_2024.xlsx", skip = 4) |> 
#   filter(...2 == 'RECEITA PRIMÁRIA TOTAL (EXCETO FONTES RPPS)') |> 
#   select(-1,-3,-4)|>  
#   pivot_longer(cols = `201801`:`202406`) |> 
#   mutate(data = ymd(paste0(name, '01'))) |> 
#   select(data, value)
# 
# 
# hp_filter <- hpfilter(tabela$value, freq = 14400)
# tabela$hp <- hp_filter$trend
# tabela |> 
#   ggplot()+
#   geom_line(aes(x  = data, y = value))+
#   geom_line(aes(x = data, y = hp))
# 
# write.xlsx(tabela, './RTL E RESULTADO PRIMÁRIO/Receita primária/raceita_primaria.xlsx')
setwd('./RTL E RESULTADO PRIMÁRIO/Receita primária')

rec_pri <- read_excel("raceita_primaria.xlsx")


ts_rec_pri <- ts(rec_pri$Valor, start = c(2018, 1), end = c(2024, 6), frequency = 12)
ts_rec_pri



plot(decompose(ts_rec_pri))

ggtsdisplay(ts_rec_pri)

acf(ts_rec_pri)

adf_result <- adf.test(ts_rec_pri)
print(adf_result)

kpss_result <- kpss.test(ts_rec_pri)
print(kpss_result)




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

accuracy_ets <- accuracy(m_ets, modelo_teste)
accuracy_hw <- accuracy(m_holtw, modelo_teste)
accuracy_holt <- accuracy(m_holt, modelo_teste)
accuracy_arima <- accuracy(arima, modelo_teste)

# Criar uma tabela com as métricas de acurácia
accuracy_table <- data.frame(
  Model = c("ETS", "Holt", "Holt-Winters", "ARIMA(0,1,0)"),
  MAE = c(accuracy_ets["Training set", "MAE"], accuracy_holt["Training set", "MAE"], accuracy_hw["Training set", "MAE"], accuracy_arima["Training set", "MAE"]),
  RMSE = c(accuracy_ets["Training set", "RMSE"], accuracy_holt["Training set", "RMSE"], accuracy_hw["Training set", "RMSE"], accuracy_arima["Training set", "RMSE"]),
  MAPE = c(accuracy_ets["Training set", "MAPE"], accuracy_holt["Training set", "MAPE"], accuracy_hw["Training set", "MAPE"], accuracy_arima["Training set", "MAPE"])
)
accuracy_table


##### Resultados


hp_filter <- hpfilter(rec_pri$Valor, freq = 14400)
rec_pri$hp <- hp_filter$trend
rec_pri |>  ggplot()+
  geom_line(aes(x  = data, y = value))+
  geom_line(aes(x = data, y = hp))


write.xlsx(rec_pri, 'resultados_hp.xlsx', sheetName = 'filtro_hp')
write.xlsx(accuracy_table, 'resultados_prev.xlsx', sheetName = 'resultado_prev')

df <- as.data.frame(m_ets) %>% 
  setNames(c("Previsão_ETS", "lo80_ETS", "hi80_ETS", "lo95_ETS", "hi95_ETS")) %>% 
  bind_cols(as.data.frame(arima) %>% 
              setNames(c("Previsão_ARIMA", "lo80_ARIMA", "hi80_ARIMA", "lo95_ARIMA", "hi95_ARIMA"))) %>% 
  bind_cols(as.data.frame(m_holt) %>% 
              setNames(c("Previsão_Holt", "lo80_Holt", "hi80_Holt", "lo95_Holt", "hi95_Holt"))) %>% 
  bind_cols(as.data.frame(m_holtw) %>% 
              setNames(c("Previsão_HW", "lo80_HW", "hi80_HW", "lo95_HW", "hi95_HW"))) %>% 
  mutate(data = seq(from = as.Date("2023-07-01"), to = as.Date('2024-06-01'), by = "month"))


write.xlsx(df, 'resultados_teste.xlsx', sheetName = 'teste')



####### previsão até dez 2026

t <- 42

arima_model <- Arima(y = ts_rec_pri, order = c(1,1,1), seasonal = c(1,0,0))
summary(arima_model)
checkresiduals(arima_model)

arima <- forecast(arima_model, h = t)

# metodo holt 
m_holt<-holt(ts_rec_pri, h=t)
accuracy(ts_rec_pri, modelo_teste)
checkresiduals(ts_rec_pri)

m_holtw<-hw(ts_rec_pri, seasonal="additive", h=t)
accuracy(m_holtw)
checkresiduals(m_holtw)

m_ets<-forecast(ets(ts_rec_pri, model = 'ANN'), h=t)
accuracy(m_ets)
checkresiduals(m_ets)

accuracy(m_ets)

df <- as.data.frame(m_ets) %>% 
  setNames(c("Previsão_ETS", "lo80_ETS", "hi80_ETS", "lo95_ETS", "hi95_ETS")) %>% 
  bind_cols(as.data.frame(arima) %>% 
              setNames(c("Previsão_ARIMA", "lo80_ARIMA", "hi80_ARIMA", "lo95_ARIMA", "hi95_ARIMA"))) %>% 
  bind_cols(as.data.frame(m_holt) %>% 
              setNames(c("Previsão_Holt", "lo80_Holt", "hi80_Holt", "lo95_Holt", "hi95_Holt"))) %>% 
  bind_cols(as.data.frame(m_holtw) %>% 
              setNames(c("Previsão_HW", "lo80_HW", "hi80_HW", "lo95_HW", "hi95_HW"))) %>% 
  mutate(data = seq(from = as.Date("2024-07-01"), to = as.Date('2027-12-01'), by = "month"))


write.xlsx(df, 'resultados_previsão.xlsx', sheetName = 'teste')

