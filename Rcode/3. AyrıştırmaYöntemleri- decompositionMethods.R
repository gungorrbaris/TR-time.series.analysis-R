
veri_trend <- tslm(veri_ts~trend)

head(veri_trend[["fitted.values"]],n=36)

periyot_trend <- veri_ts - veri_trend[["fitted.values"]]

Acf(periyot_trend,lag.max = 42,  ylim=c(-1,1), lwd=5,plot=FALSE)

veri_trend_1 <- ma(veri_ts, order = 12, centre = TRUE)

mevsim3 <- veri_ts - veri_trend_1
head(mevsim3,n=36)

donemort3<-t(matrix(data=mevsim3, nrow = 12))
head(donemort3)

colMeans(donemort3, na.rm = T)

sum(colMeans(donemort3, na.rm = T))

mean(colMeans(donemort3, na.rm = T))

endeks1 <- colMeans(donemort3, na.rm = T) - mean(colMeans(donemort3, na.rm = T))

endeks1

indeks<-  matrix(data = endeks1, nrow = 587)
head(indeks)

trendhata <- veri_ts - indeks

plot.ts(trendhata)

trend1<-tslm(ts(trendhata)~trend)

head(trend1[["fitted.values"]],n=24)

tahmin<- indeks+trend1[["fitted.values"]]
tahmin <- ts(data = tahmin, start = c(1973, 01), end = c(2021, 11), frequency = 12 )
plot(tahmin,main = "Tahmin Serisi Grafiği")

hata <- ts(veri_ts) - ts(tahmin)
plot(hata,main = "Tahmin serisi için Hata Serisi Grafiği")

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2,main="Orijinal Seri ve Tahmin Serisinin Birlikte Grafiği")
lines( window(tahmin) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(OrijinalSeri )),
                   expression(paste(Tahmin ))),
       lwd=c(2,2),lty=c(1,3), cex=0.6, col=c(4,2))



Box.test(hata,type = "Ljung",lag = 42)

veri_trend2 <- tslm(veri_ts~trend)

head(veri_trend2[["fitted.values"]],n=36)

periyot_trend2 <- veri_ts - veri_trend2[["fitted.values"]]

Acf(periyot_trend2,lag.max = 42,  ylim=c(-1,1), lwd=5,plot=FALSE)

veri_trend_2 <- ma(veri_ts, order = 12, centre = TRUE)

mevsim4 <- veri_ts / veri_trend_2
head(mevsim4,n=36)

donemort4<-t(matrix(data=mevsim4, nrow = 12))
head(donemort4)

colMeans(donemort4, na.rm = T)

sum(colMeans(donemort4, na.rm = T))

mean(colMeans(donemort4, na.rm = T))

endeks2 <- colMeans(donemort4, na.rm = T) / mean(colMeans(donemort4, na.rm = T))

endeks2

mean(endeks2)

indeks2<-  matrix(data = endeks2, nrow = 587)
head(indeks2)

trendhata2 <- veri_ts / indeks2

plot.ts(trendhata2)

trend2<-tslm(ts(trendhata2)~trend)

head(trend2[["fitted.values"]],n=24)

tahmin2<- indeks2*trend2[["fitted.values"]]
tahmin2 <- ts(data = tahmin2, start = c(1973, 01), end = c(2021, 11), frequency = 12 )
plot(tahmin2,main = "Tahmin Serisi Grafiği")

hata2 <- ts(veri_ts) - ts(tahmin2)
plot(hata2,main = "Tahmin serisi için Hata Serisi Grafiği")

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2,main="Orijinal Seri ve Tahmin Serisinin Birlikte Grafiği")
lines( window(tahmin2) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(OrijinalSeri )),
                   expression(paste(Tahmin ))),
       lwd=c(2,2),lty=c(1,3), cex=0.6, col=c(4,2))


Acf(hata2, lag.max = 42,  ylim=c(-1,1), lwd=5,main="Hatalar için ACF Grafiği")

Box.test(hata2,type = "Ljung",lag = 42)


