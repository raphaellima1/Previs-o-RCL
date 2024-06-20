library(dplyr)
pacman::p_load(dplyr, ggplot2,readxl,forecast,xgboost,tidyr,xts,TSstudio )


df <- read_excel('//sefazarquivos/AEMFPF/DIVERSOS/Previs-o-RCL/RCL2015_a_2024.xlsx', sheet = 1) %>% 
  drop_na(RCL_AJUST) %>% 
  select(Data, RCL_AJUST) 
  
# Tue Jun 18 10:27:26 2024 ------------------------------

colnames(df)<-c("date", "rcl")
df$date<-as.Date(df$date, format="%Y-%m-%d")
RCLXTS<- xts(df[-1], df[[1]])
RCLTS<- ts(df$rcl, start=c(2016,1),end=c(2024,5), frequency=12 )
RCLTS <- diff(RCLTS)

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



m_ses<-ses(train, h=t)
accuracy(m_ses, test)
checkresiduals(m_ses)

m_holt<-holt(train, h=t)
accuracy(m_holt, test)
checkresiduals(m_holt)

m_holtw<-hw(train, seasonal="additive", h=t)
accuracy(m_holtw, test)
checkresiduals(m_holtw)

m_ets<-forecast(ets(train, model = 'AAN'), h=t)
summary(m_ets)
checkresiduals(m_ets)

accuracy(m_ets, test)


autoplot(train,size = 1)+

  autolayer(m_ses, series="Simple Exponential", PI=F,size = 1)+
  autolayer(m_holt, series="Holt Method", PI=F,size = 1)+
  autolayer(m_holtw, series="Holt_Winters", PI=F,size = 1)+
  autolayer(test, series="Test",size = 1)+
  autolayer(m_ets, series="ETS", PI=FALSE,size = 1)+
  xlab('Month/Year')+ylab('RCL em R$')+
  ggtitle('Forecasts RCL sem IPASGO')+
  guides(colour=guide_legend(title='Forecast'))

