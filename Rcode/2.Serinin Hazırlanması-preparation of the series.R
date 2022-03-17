


veri_ts <- ts(data = veri$Elektrik_Tuketimi, start = c(1973, 01), end = c(2021, 11), frequency = 12 )

library(foreach)

ts.plot(veri_ts,gpars=list(xlab="Zaman", ylab="Elektrik Tüketimi"))





library(fpp)
Acf(veri_ts,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Orijinal veri ACF Grafiği")




veri_birinci_fark <- diff(veri_ts, differences = 1)

Acf(veri_birinci_fark,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Birinci Fark sonrası ACF Grafiği")


Acf(veri_birinci_fark,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Birinci Fark sonrası ACF Grafiği",plot=FALSE)



veri_ts_mev1 <- diff(veri_birinci_fark,12)


Acf(veri_ts_mev1,lag.max = 42,  ylim=c(-1,1), lwd=5,main="Birinci Mevsimsel Fark sonrası ACF Grafiği")
