ts.plot(veri_ts,gpars=list(xlab="Zaman", ylab="Elektrik Tüketimi"))
    
library(fpp)
Acf(veri_ts,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Orijinal veri ACF Grafiği")
   
Pacf(veri_ts,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Orijinal seri PACF Grafiği")
    
   
veri_birinci_fark <- diff(veri_ts, differences = 1)
    
Acf(veri_birinci_fark,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Birinci Fark sonrası ACF Grafiği")
    
    
    
    Pacf(veri_birinci_fark,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Birinci Fark sonrası ACF Grafiği")
    
    veri_ts_mev1 <- diff(veri_birinci_fark,12)
    
    
    Acf(veri_ts_mev1,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Birinci Mevsimsel Fark sonrası ACF Grafiği")
    
    Acf(veri_ts_mev1,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Durağan Seri ACF Grafiği")
    ```
    
    
    Pacf(veri_ts_mev1,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Durağan Seri PACF Grafiği")
    
    library(forecast)
    en_iyi_model <- auto.arima(veri_ts,max.p = 2,max.q = 2,max.P = 3,max.Q = 3,d=1,D=1,seasonal = TRUE,ic = 'aicc')
    summary(en_iyi_model)
    

tahmin21<- en_iyi_model[["fitted"]]

hata21<- en_iyi_model[["residuals"]]

plot( window(veri_ts), 
      xlab="Zaman (Yıl)", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin21) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(Elektrik_Tüketimi)),
               expression(paste("ARIMA(2,1,1)(0,1,1)[12]"))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

Acf(hata21, lag.max = 42,  ylim=c(-1,1), lwd=5,main="Hatalar için ACF Grafiği")

ongoru21<- forecast(en_iyi_model, h=12)
knitr::kable(ongoru21[["mean"]],col.names = "ARIMA(2,1,1)(0,1,1)[12] Modelinin 2022 için aylık Elektrik Tüketim Tahminleri",align = "c")

plot(forecast(en_iyi_model,h=120),main = "ARIMA(2,1,1)(0,1,1)[12] modelinin İlerleyen Yıllar İçin Tahminleri")
