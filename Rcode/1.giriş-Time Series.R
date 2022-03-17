

veri <- read.csv("https://raw.githubusercontent.com/gungorrbaris/zaman-serisi-analizi-R/main/data/Table_7.6_Electricity_End_Use.csv")
knitr::kable(head(veri), align = "c")



tarih = seq(from = as.Date("1973-01-01"), to = as.Date("2021-11-01"), by = 'month')
veri$Month <- tarih
veri$Month <-format(veri$Month, "%m-%Y")
knitr::kable(head(veri), align = "c")



colnames(veri) <- c("Tarih","Elektrik_Tuketimi")
knitr::kable(head(veri), align = "c")





