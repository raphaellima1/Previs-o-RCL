library(dplyr)
pacman::p_load(dplyr, ggplot2,readxl,forecast,xgboost,tidyr,xts,TSstudio, mFilter,BETS )


df <- read_excel('RCL2015_a_2024.xlsx', sheet = 1) %>% 
  drop_na(RCL_AJUST) %>% 
  select(Data, RCL_AJUST) 
  
# Tue Jun 18 10:27:26 2024 ------------------------------

colnames(df)<-c("date", "rcl")
df$date<-as.Date(df$date, format="%Y-%m-%d")
RCLXTS<- xts(df[-1], df[[1]])
RCLTS<- ts(df$rcl, start=c(2015,12),end=c(2024,5), frequency=12 )

RCLHP <- hpfilter(RCLTS, freq = 14400) 

rclhp <- RCLHP$trend |> 
  as.data.frame()

writexl::write_xlsx(rclhp, 'rfiltrohp.xlsx' )

ts_plot(RCLXTS, title="RCL sem IPASGO")


ts_decompose(RCLTS)

ts_heatmap(RCLTS)


ts_surface(RCLTS)

ts_seasonal(RCLTS, type="cycle")

ts_seasonal(RCLTS, type="box")

ggAcf(RCLTS)

# treino e teste----------------------------------------------------------------

# treino iniciando em jan de 20 a dez de 23
t <- 17
train <- window(RCLTS, start=c(2016,1), end=c(2022,12))

# teste iniciando em jan de 24
test <- window(RCLTS, start=c(2023))

# 
# m_mean<-meanf(train, h=20)
# accuracy(m_mean, test)
# 
# m_naive<-naive(train, h=20)
# accuracy(m_naive, test)
# 
# 
# m_snaive<-snaive(train, h=20)
# accuracy(m_snaive, test)
# 
# autoplot(train)+
#   autolayer(test, series="Test")+
#   autolayer(m_mean, series="Mean", PI=FALSE)+
#   autolayer(m_naive, series="Naive", PI=FALSE)+
#   autolayer(m_snaive, series="Seasonal Naive", PI=FALSE)+
#   xlab('Ano')+ylab('RCL')+
#   ggtitle('Forecasts RCL sem IPASGO')+
#   guides(colour=guide_legend(title='Forecast'))



#m_ses<-ses(train, h=t,exponential  = T)
#accuracy(m_ses, test)
#checkresiduals(m_ses)

m_holt<-holt(train, h=t)
accuracy(m_holt, test)
checkresiduals(m_holt)

m_holtw<-hw(train, seasonal="additive", h=t )
accuracy(m_holtw)
checkresiduals(m_holtw)

m_ets<-forecast(ets(train, model = 'MMN'), h=t)
accuracy(m_ets)
checkresiduals(m_ets)

accuracy(m_ets)


autoplot(train,size = 1)+

  #autolayer(m_ses, series="Simple Exponential", PI=F,size = 1)+
  autolayer(m_holt, series="Holt Method", PI=F,size = 1)+
  autolayer(m_holtw, series="Holt_Winters", PI=F,size = 1)+
  autolayer(test, series="Test",size = 1)+
  autolayer(m_ets, series="ETS", PI=FALSE,size = 1)+
  xlab('Month/Year')+ylab('RCL em R$')+
  ggtitle('Forecasts RCL sem IPASGO')+
  guides(colour=guide_legend(title='Forecast'))

accuracy_ses <- accuracy(m_ets, test)
accuracy_holt <- accuracy(m_holtw, test)
accuracy_hw <- accuracy(m_holt, test)

# Criar uma tabela com as métricas de acurácia
accuracy_table <- data.frame(
  Model = c("ETS", "Holt", "Holt-Winters"),
  MAE = c(accuracy_ses["Training set", "MAE"], accuracy_holt["Training set", "MAE"], accuracy_hw["Training set", "MAE"]),
  RMSE = c(accuracy_ses["Training set", "RMSE"], accuracy_holt["Training set", "RMSE"], accuracy_hw["Training set", "RMSE"]),
  MAPE = c(accuracy_ses["Training set", "MAPE"], accuracy_holt["Training set", "MAPE"], accuracy_hw["Training set", "MAPE"])
)
writexl::write_xlsx(accuracy_table, 'resultado.xlsx' )

# Mostrar a tabela
print(accuracy_table)

df <- as.data.frame(m_ets) %>% 
  setNames(c("Previsão_ETS", "lo80_ETS", "hi80_ETS", "lo95_ETS", "hi95_ETS")) %>% 
  bind_cols(as.data.frame(m_holt) %>% 
              setNames(c("Previsão_Holt", "lo80_Holt", "hi80_Holt", "lo95_Holt", "hi95_Holt"))) %>% 
  bind_cols(as.data.frame(m_holtw) %>% 
              setNames(c("Previsão_HW", "lo80_HW", "hi80_HW", "lo95_HW", "hi95_HW"))) %>% 
  mutate(data = seq(from = as.Date("2023-01-01"), to = as.Date('2024-05-01'), by = "month"))


writexl::write_xlsx(df, 'previsão.xlsx' )



m_ets<-forecast(ets(RCLTS, model = 'MMN'), h=43)
m_holtw<-hw(RCLTS, seasonal="additive", h=43)
m_holt<-holt(RCLTS, h=43)

autoplot(train,size = 1)+
  
  #autolayer(m_ses, series="Simple Exponential", PI=F,size = 1)+
  autolayer(m_holt, series="Holt Method", PI=F,size = 1)+
  autolayer(m_holtw, series="Holt_Winters", PI=F,size = 1)+
  autolayer(test, series="Test",size = 1)+
  autolayer(m_ets, series="ETS", PI=FALSE,size = 1)+
  xlab('Month/Year')+ylab('RCL em R$')+
  ggtitle('Forecasts RCL sem IPASGO')+
  guides(colour=guide_legend(title='Forecast'))

df <- as.data.frame(m_ets) %>% 
  setNames(c("Previsão_ETS", "lo80_ETS", "hi80_ETS", "lo95_ETS", "hi95_ETS")) %>% 
  bind_cols(as.data.frame(m_holt) %>% 
    setNames(c("Previsão_Holt", "lo80_Holt", "hi80_Holt", "lo95_Holt", "hi95_Holt"))) %>% 
  bind_cols(as.data.frame(m_holtw) %>% 
              setNames(c("Previsão_HW", "lo80_HW", "hi80_HW", "lo95_HW", "hi95_HW"))) %>% 
  mutate(data = seq(from = as.Date("2024-06-01"), to = as.Date('2027-12-01'), by = "month"))

  
writexl::write_xlsx(df, 'previsão1.xlsx')



