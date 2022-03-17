
Winters1<- forecast::ets(veri_ts, model = "AAA")
summary(Winters1)


library(forecast)
checkresiduals(Winters1, lag = 42)

tahmin_win1<- Winters1[["fitted"]]

ongoru1 <- forecast(Winters1,h=12)

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin_win1) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(Elektrik_Tuketimi)),
               expression(paste(Top.Winters_Tahmini))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

hata_win1<- Winters1[["residuals"]]
plot.ts(hata_win1)

Acf(hata_win1, lag.max = 42,  ylim=c(-1,1), lwd=5,main="Hatalar için ACF Grafiği")

Winters2<- forecast::ets(veri_ts, model = "MAM")
summary(Winters2)

library(forecast)
checkresiduals(Winters2, lag = 42)

tahmin_win2<- Winters2[["fitted"]]

ongoru2 <- forecast(Winters2,h=12)
knitr::kable(ongoru2[["mean"]],col.names = "2022 yılı ABD elektrik tüketimi - Çarpımsal Winters Tahminleri", align = "c")

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin_win2) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(Elektrik_Tuketimi)),
               expression(paste(Çar.Winters_Tahmini))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))



hata_win2<- Winters2[["residuals"]]
plot.ts(hata_win2)

Acf(hata_win2, lag.max = 42,  ylim=c(-1,1), lwd=5,main="Hatalar için ACF Grafiği")


ongoru2022 <- as.data.frame(cbind(ongoru1[["mean"]],ongoru2[["mean"]]))
colnames(ongoru2022) <- c("Toplamsal Winters", "Çarpımsal Winters")
knitr::kable(ongoru2022, align = "c")












