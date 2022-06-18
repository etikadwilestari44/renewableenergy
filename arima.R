mydata <- read.csv("C:/newdata.csv", stringsAsFactors = F)
mydata$time <- as.Date(mydata$time,'%d/%m/%Y')
mydata$year <- as.integer(format(mydata$time,'%Y'))
mydata$month <- as.integer(format(mydata$time,'%m'))
totalperyear <- aggregate(cbind(bengkulu, sumsel, sumut, sumbar, riau,
                                aceh, lampung, kepri, babel, jambi) ~ year, data = mydata, FUN = sum)


library(tseries)
library(lmtest)
library(forecast)
library(ggplot2)
library(mgcv)
summary(totalperyear)

####################################################################################
sumsel_ts <- ts(totalperyear$sumsel, start = 1980, frequency = 1)
plot(sumsel_ts, main="Sumatera Selatan")
sumsel_train<-ts(totalperyear$sumsel[1:20])
acf(sumsel_ts, main='Grafik ACF Data Sumatera Selatan', lag.max = 36)
adf.test(sumsel_ts)

par(mfrow=c(2,1))
plot(sumsel_ts, lwd=2, main="Sumatera Selatan")
abline(h=mean(sumsel_ts),lwd=2,lty=2,col='red')
acf(sumsel_train, main='Grafik ACF Data Train Sumatera Selatan', lag.max = 36)
adf.test(sumsel_train)

sumsel_diff = diff(sumsel_ts)
par(mfrow=c(2,1))
plot(sumsel_diff,lwd=2, main='Plot Diferensiasi  Sumatera Selatan ')
abline(h=mean(sumsel_diff),lwd=2,lty=2,col='red')
acf(sumsel_diff, main='Grafik ACF  Sumatera Selatan ',lag.max=36)
adf.test(sumsel_diff)

sumsel_diff2 = diff(sumsel_diff)
par(mfrow=c(2,1))
plot(sumsel_diff2,lwd=2, main='Plot Diferensiasi  Sumatera Selatan ')
abline(h=mean(sumsel_diff2),lwd=2,lty=2,col='red')
acf(sumsel_diff2, main='Grafik ACF  Sumatera Selatan ',lag.max=36)
adf.test(sumsel_diff2)



mod_1sumsel = arima(sumsel_diff2,order=c(1,0,0))
mod_1sumsel

mod_2sumsel = arima(sumsel_diff2,order=c(0,1,0))
mod_2sumsel

mod_3sumsel = arima(sumsel_diff2,order=c(0,2,0))
mod_3sumsel

mod_4sumsel = arima(sumsel_diff2,order=c(3,2,3))
mod_4sumsel

mod_autosumsel = auto.arima(sumsel_diff2, max.p=4,max.q=4,
                      seasonal =FALSE, stationary = TRUE)
mod_autosumsel

coeftest(mod_autosumsel)
checkresiduals(mod_autosumsel)

sumsel_pred <- sumsel_train - residuals(mod_autosumsel)
ts.plot(sumsel_pred,sumsel_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_autosumsel)

fc = forecast(sumsel_ts, model=mod_autosumsel,h=10)
summary(fc)
plot(fc)
df_sumsel<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_sumsel) <- c("Forecast")
df_sumsel$year <- Year
head(df_sumsel)
ggplot(df_sumsel, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Sumatera Selatan")
#######################################################################################
aceh_ts <- ts(totalperyear$aceh, start = 1980, frequency = 1)
plot(aceh_ts, main="Aceh")
aceh_train<-ts(totalperyear$aceh[1:20])
acf(aceh_ts, main='Grafik ACF Data  Aceh', lag.max = 36)
adf.test(aceh_ts)
par(mfrow=c(2,1))
plot(aceh_ts, lwd=2, main="Aceh")
abline(h=mean(aceh_ts),lwd=2,lty=2,col='red')
acf(aceh_train, main='Grafik ACF Data Train Aceh', lag.max = 36)
adf.test(aceh_train)

aceh_diff = diff(aceh_ts)
par(mfrow=c(2,1))
plot(aceh_diff,lwd=2, main='Plot Diferensiasi  Aceh ')
abline(h=mean(aceh_diff),lwd=2,lty=2,col='red')
acf(aceh_diff, main='Grafik ACF  Aceh',lag.max=36)
adf.test(aceh_diff)

aceh_diff2 = diff(aceh_diff)
par(mfrow=c(2,1))
plot(aceh_diff2,lwd=2, main='Plot Diferensiasi  Aceh  ')
abline(h=mean(aceh_diff2),lwd=2,lty=2,col='red')
acf(aceh_diff2, main='Grafik ACF  Aceh  ',lag.max=36)
adf.test(aceh_diff2)


mod_1aceh = arima(aceh_train,order=c(3,1,1))
mod_1aceh

mod_2aceh = arima(aceh_train,order=c(0,1,0))
mod_2aceh

mod_3aceh = arima(aceh_train,order=c(0,2,0))
mod_3aceh

mod_4aceh = arima(aceh_train,order=c(3,2,3))
mod_4aceh

mod_autoaceh = auto.arima(aceh_train, max.p=4,max.q=4,
                      seasonal =FALSE, stationary = TRUE)
mod_autoaceh

coeftest(mod_autoaceh)
checkresiduals(mod_autoaceh)

aceh_pred <- aceh_train - residuals(mod_autoaceh)
ts.plot(aceh_pred,aceh_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_autoaceh)

fc = forecast(aceh_ts, model=mod_autoaceh,h=10)
summary(fc)
plot(fc)

df_aceh<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_aceh) <- c("Forecast")
df_aceh$year <- Year
head(df_aceh)
gamyearaceh <- gam(Forecast ~ s(year), data=df_aceh)
plot(gamyearaceh)
ggplot(df_aceh, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Aceh")

#################################################################################
bengkulu_ts <- ts(totalperyear$bengkulu, start = 1980, frequency = 1)
plot(bengkulu_ts, main="Bengkulu")
bengkulu_train<-ts(totalperyear$bengkulu[1:20])
acf(bengkulu_ts, main='Grafik ACF Data  Bengkulu', lag.max = 36)
adf.test(bengkulu_ts)
par(mfrow=c(2,1))
plot(bengkulu_ts, lwd=2, main="Bengkulu")
abline(h=mean(bengkulu_ts),lwd=2,lty=2,col='red')
acf(bengkulu_train, main='Grafik ACF Data Train bengkulu', lag.max = 36)
adf.test(bengkulu_train)

bengkulu_diff = diff(bengkulu_ts)
par(mfrow=c(2,1))
plot(bengkulu_diff,lwd=2, main='Plot Diferensiasi   Bengkulu')
abline(h=mean(bengkulu_diff),lwd=2,lty=2,col='red')
acf(bengkulu_diff, main='Grafik ACF  Bengkulu',lag.max=36)
adf.test(bengkulu_diff)

bengkulu_diff2 = diff(bengkulu_diff)
par(mfrow=c(2,1))
plot(bengkulu_diff2,lwd=2, main='Plot Diferensiasi  Bengkulu  ')
abline(h=mean(bengkulu_diff2),lwd=2,lty=2,col='red')
acf(bengkulu_diff2, main='Grafik ACF  Bengkulu  ',lag.max=36)
adf.test(bengkulu_diff2)

par(mfrow=c(2,2))
acf(bengkulu_train_diff, main='ACF Data Train bengkulu 1 Kali Differensiasi', lag.max=36)
pacf(bengkulu_train_diff, main='ACF Data Train bengkulu 1 Kali Differensiasi', lag.max=36)
acf(bengkulu_train_diff2, main='ACF Data Train bengkulu 2 Kali Differensiasi', lag.max=36)
pacf(bengkulu_train_diff2, main='ACF Data Train bengkulu 2 Kali Differensiasi', lag.max=36)

mod_1bengkulu = arima(bengkulu_train,order=c(3,1,1))
mod_1bengkulu

mod_2bengkulu = arima(bengkulu_train,order=c(0,1,0))
mod_2bengkulu

mod_3bengkulu = arima(bengkulu_train,order=c(0,2,0))
mod_3bengkulu

mod_4bengkulu = arima(bengkulu_train,order=c(3,2,3))
mod_4bengkulu

mod_autobengkulu = auto.arima(bengkulu_train, max.p=4,max.q=4,
                      seasonal =FALSE, stationary = TRUE)
mod_autobengkulu

coeftest(mod_1bengkulu)
checkresiduals(mod_1bengkulu)

bengkulu_pred <- bengkulu_train - residuals(mod_1bengkulu)
ts.plot(bengkulu_pred,bengkulu_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_1bengkulu)

fc = forecast(bengkulu_ts, model=mod_1bengkulu,h=10)
summary(fc)
plot(fc)

df_bengkulu<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_bengkulu) <- c("Forecast")
df_bengkulu$year <- Year
head(df_bengkulu)
gamyearbengkulu <- gam(Forecast ~ s(year), data=df_bengkulu)
plot(gamyearbengkulu)
ggplot(df_bengkulu, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Bengkulu")
################################################################################
sumut_ts <- ts(totalperyear$sumut, start = 1980, frequency = 1)
plot(sumut_ts, main="Sumatera Utara")
sumut_train<-ts(totalperyear$sumut[1:20])
acf(sumut_ts, main='Grafik ACF Data  Sumatera Utara', lag.max = 36)
adf.test(sumut_ts)
par(mfrow=c(2,1))
plot(sumut_ts, lwd=2, main="Sumatera Utara")
abline(h=mean(sumut_ts),lwd=2,lty=2,col='red')
acf(sumut_train, main='Grafik ACF Data Train Sumatera Utara', lag.max = 36)
adf.test(sumut_train)

sumut_diff = diff(sumut_ts)
par(mfrow=c(2,1))
plot(sumut_diff,lwd=2, main='Plot Diferensiasi   Sumatera Utara')
abline(h=mean(sumut_diff),lwd=2,lty=2,col='red')
acf(sumut_diff, main='Grafik ACF  Sumatera Utara',lag.max=36)
adf.test(sumut_diff)

sumut_diff2 = diff(sumut_diff)
par(mfrow=c(2,1))
plot(sumut_diff2,lwd=2, main='Plot Diferensiasi  Sumatera Utara  ')
abline(h=mean(sumut_diff2),lwd=2,lty=2,col='red')
acf(sumut_diff2, main='Grafik ACF  Sumatera Utara  ',lag.max=36)
adf.test(sumut_diff2)

mod_1sumut = arima(sumut_train,order=c(3,1,1))
mod_1sumut

mod_2sumut = arima(sumut_train,order=c(0,1,0))
mod_2sumut

mod_3sumut = arima(sumut_train,order=c(0,2,0))
mod_3sumut

mod_4sumut = arima(sumut_train,order=c(3,2,3))
mod_4sumut

mod_autosumut = auto.arima(sumut_train, max.p=4,max.q=4,
                              seasonal =FALSE, stationary = TRUE)
mod_autosumut

coeftest(mod_autosumut)
checkresiduals(mod_autosumut )

sumut_pred <- sumut_train - residuals(mod_autosumut )
ts.plot(sumut_pred,sumut_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_autosumut )

fc = forecast(sumut_ts, model=mod_autosumut ,h=10)
summary(fc)
plot(fc)

df_sumut<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_sumut) <- c("Forecast")
df_sumut$year <- Year
head(df_sumut)
gamyearsumut <- gam(Forecast ~ s(year), data=df_sumut)
plot(gamyearsumut)
ggplot(df_sumut, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Sumatera Utara")
################################################################################
sumbar_ts <- ts(totalperyear$sumbar, start = 1980, frequency = 1)
plot(sumbar_ts, main="Sumatera Barat")
sumbar_train<-ts(totalperyear$sumbar[1:20])
acf(sumbar_ts, main='Grafik ACF Data  Sumatera Barat', lag.max = 36)
adf.test(sumbar_ts)
par(mfrow=c(2,1))
plot(sumbar_ts, lwd=2, main="Sumatera Barat")
abline(h=mean(sumbar_ts),lwd=2,lty=2,col='red')
acf(sumbar_train, main='Grafik ACF Data Train Sumatera Barat', lag.max = 36)
adf.test(sumbar_train)

sumbar_diff = diff(sumbar_ts)
par(mfrow=c(2,1))
plot(sumbar_diff,lwd=2, main='Plot Diferensiasi   Sumatera Barat')
abline(h=mean(sumbar_diff),lwd=2,lty=2,col='red')
acf(sumut_diff, main='Grafik ACF  Sumatera Barat',lag.max=36)
adf.test(sumbar_diff)

sumbar_diff2 = diff(sumbar_diff)
par(mfrow=c(2,1))
plot(sumbar_diff2,lwd=2, main='Plot Diferensiasi  Sumatera Barat  ')
abline(h=mean(sumbar_diff2),lwd=2,lty=2,col='red')
acf(sumbar_diff2, main='Grafik ACF  Sumatera Barat  ',lag.max=36)
adf.test(sumbar_diff2)

mod_1sumbar = arima(sumbar_train,order=c(3,1,1))
mod_1sumbar

mod_2sumbar = arima(sumbar_train,order=c(0,1,0))
mod_2sumbar

mod_3sumbar = arima(sumbar_train,order=c(0,2,0))
mod_3sumbar

mod_4sumbar = arima(sumbar_train,order=c(3,2,3))
mod_4sumbar

mod_autosumbar = auto.arima(sumbar_train, max.p=4,max.q=4,
                           seasonal =FALSE, stationary = TRUE)
mod_autosumbar

coeftest(mod_autosumbar )
checkresiduals(mod_autosumbar )

sumbar_pred <- sumbar_train - residuals(mod_autosumbar )
ts.plot(sumbar_pred,sumbar_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_autosumbar)

fc = forecast(sumbar_ts, model=mod_1sumbar,h=10)
summary(fc)
plot(fc)

df_sumbar<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_sumbar) <- c("Forecast")
df_sumbar$year <- Year
head(df_sumbar)
gamyearsumbar <- gam(Forecast ~ s(year), data=df_sumbar)
plot(gamyearsumbar)
ggplot(df_sumbar, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Sumatera Barat")

################################################################################
riau_ts <- ts(totalperyear$riau, start = 1980, frequency = 1)
plot(riau_ts, main="riau")
riau_train<-ts(totalperyear$riau[1:20])
acf(riau_ts, main='Grafik ACF Data  Riau', lag.max = 36)
adf.test(riau_ts)
par(mfrow=c(2,1))
plot(riau_ts, lwd=2, main="riau")
abline(h=mean(riau_ts),lwd=2,lty=2,col='red')
acf(riau_train, main='Grafik ACF Data Train riau', lag.max = 36)
adf.test(riau_train)

riau_diff = diff(riau_ts)
par(mfrow=c(2,1))
plot(riau_diff,lwd=2, main='Plot Diferensiasi   riau')
abline(h=mean(riau_diff),lwd=2,lty=2,col='red')
acf(riau_diff, main='Grafik ACF  riau',lag.max=36)
adf.test(riau_diff)

riau_diff2 = diff(riau_diff)
par(mfrow=c(2,1))
plot(riau_diff2,lwd=2, main='Plot Diferensiasi  riau  ')
abline(h=mean(riau_diff2),lwd=2,lty=2,col='red')
acf(riau_diff2, main='Grafik ACF  riau ',lag.max=36)
adf.test(riau_diff2)

par(mfrow=c(2,2))
acf(riau_train_diff, main='ACF Data Train riau 1 Kali Differensiasi', lag.max=36)
pacf(riau_train_diff, main='ACF Data Train riau1 Kali Differensiasi', lag.max=36)
acf(riau_train_diff2, main='ACF Data Train riau 2 Kali Differensiasi', lag.max=36)
pacf(riau_train_diff2, main='ACF Data Train riau 2 Kali Differensiasi', lag.max=36)

mod_1riau = arima(riau_train,order=c(3,1,1))
mod_1riau

mod_2riau = arima(riau_train,order=c(0,1,0))
mod_2riau

mod_3riau = arima(riau_train,order=c(0,2,0))
mod_3riau

mod_4riau = arima(riau_train,order=c(3,2,3))
mod_4riau

mod_autoriau = auto.arima(riau_train, max.p=4,max.q=4,
                            seasonal =FALSE, stationary = TRUE)
mod_autoriau

coeftest(mod_autoriau)
checkresiduals(mod_autoriau)

riau_pred <- riau_train - residuals(mod_autoriau)
ts.plot(riau_pred,riau_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_autoriau)

fc = forecast(riau_ts, model=mod_autoriau,h=10)
summary(fc)
plot(fc)

df_riau<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_riau) <- c("Forecast")
df_riau$year <- Year
head(df_riau)
gamyearriau <- gam(Forecast ~ s(year), data=df_riau)
plot(gamyearriau)
ggplot(df_riau, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Riau")

################################################################################
lampung_ts <- ts(totalperyear$lampung, start = 1980, frequency = 1)
plot(lampung_ts, main="lampung")
lampung_train<-ts(totalperyear$lampung[1:20])
acf(lampung_ts, main='Grafik ACF Data  Lampung', lag.max = 36)
adf.test(lampung_ts)
par(mfrow=c(2,1))
plot(lampung_ts, lwd=2, main="lampung")
abline(h=mean(lampung_ts),lwd=2,lty=2,col='red')
acf(lampung_train, main='Grafik ACF Data Train lampung', lag.max = 36)
adf.test(lampung_train)

lampung_diff = diff(lampung_ts)
par(mfrow=c(2,1))
plot(lampung_diff,lwd=2, main='Plot Diferensiasi   lampung')
abline(h=mean(lampung_diff),lwd=2,lty=2,col='red')
acf(lampung_diff, main='Grafik ACF lampung',lag.max=36)
adf.test(lampung_diff)

lampung_diff2 = diff(lampung_diff)
par(mfrow=c(2,1))
plot(lampung_diff2,lwd=2, main='Plot Diferensiasi  lampung  ')
abline(h=mean(lampung_diff2),lwd=2,lty=2,col='red')
acf(lampung_diff2, main='Grafik ACF lampung  ',lag.max=36)
adf.test(lampung_diff2)


par(mfrow=c(2,2))
acf(lampung_train_diff, main='ACF Data Train lampung 1 Kali Differensiasi', lag.max=36)
pacf(lampung_train_diff, main='ACF Data Train lampung 1 Kali Differensiasi', lag.max=36)
acf(lampung_train_diff2, main='ACF Data Train lampung 2 Kali Differensiasi', lag.max=36)
pacf(lampung_train_diff2, main='ACF Data Train lampung 2 Kali Differensiasi', lag.max=36)

mod_1lampung = arima(lampung_train,order=c(3,1,1))
mod_1lampung

mod_2lampung = arima(lampung_train,order=c(0,1,0))
mod_2lampung

mod_3lampung = arima(lampung_train,order=c(0,2,0))
mod_3lampung

mod_4lampung= arima(lampung_train,order=c(3,2,3))
mod_4lampung

mod_autolampung = auto.arima(lampung_train, max.p=4,max.q=4,
                          seasonal =FALSE, stationary = TRUE)
mod_autolampung

coeftest(mod_1lampung)
checkresiduals(mod_1lampung)

lampung_pred <- lampung_train - residuals(mod_1lampung)
ts.plot(lampung_pred,lampung_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_1lampung)

fc = forecast(lampung_ts, model=mod_1lampung,h=10)
summary(fc)
plot(fc)

df_lampung<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_lampung) <- c("Forecast")
df_lampung$year <- Year
head(df_lampung)
gamyearlampung <- gam(Forecast ~ s(year), data=df_lampung)
plot(gamyearlampung)
ggplot(df_lampung, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Lampung")

################################################################################
jambi_ts <- ts(totalperyear$jambi, start = 1980, frequency = 1)
plot(jambi_ts, main="jambi")
jambi_train<-ts(totalperyear$jambi[1:20])
acf(jambi_ts, main='Grafik ACF Data  Jambi', lag.max = 36)
adf.test(jambi_ts)
par(mfrow=c(2,1))
plot(jambi_ts, lwd=2, main="jambi")
abline(h=mean(jambi_ts),lwd=2,lty=2,col='red')
acf(jambi_train, main='Grafik ACF Data Train jambi', lag.max = 36)
adf.test(jambi_train)

jambi_diff = diff(jambi_ts)
par(mfrow=c(2,1))
plot(jambi_diff,lwd=2, main='Plot Diferensiasi jambi')
abline(h=mean(jambi_diff),lwd=2,lty=2,col='red')
acf(jambi_diff, main='Grafik ACF jambi',lag.max=36)
adf.test(jambi_diff)

jambi_diff2 = diff(jambi_diff)
par(mfrow=c(2,1))
plot(jambi_diff2,lwd=2, main='Plot Diferensiasi  jambi')
abline(h=mean(jambi_diff2),lwd=2,lty=2,col='red')
acf(jambi_diff2, main='Grafik ACF jambi ',lag.max=36)
adf.test(jambi_diff2)

par(mfrow=c(2,2))
acf(jambi_train_diff, main='ACF Data Train jambi 1 Kali Differensiasi', lag.max=36)
pacf(jambi_train_diff, main='ACF Data Train jambi 1 Kali Differensiasi', lag.max=36)
acf(jambi_train_diff2, main='ACF Data Train jambi 2 Kali Differensiasi', lag.max=36)
pacf(jambi_train_diff2, main='ACF Data Train jambi 2 Kali Differensiasi', lag.max=36)

mod_1jambi = arima(jambi_train,order=c(3,1,1))
mod_1ljambi

mod_2jambi = arima(jambi_train,order=c(0,1,0))
mod_2jambi

mod_3jambi = arima(jambi_train,order=c(0,2,0))
mod_3jambi

mod_4jambi = arima(jambi_train,order=c(3,2,3))
mod_4jambi

mod_autojambi = auto.arima(jambi_train, max.p=4,max.q=4,
                             seasonal =FALSE, stationary = TRUE)
mod_autojambi

coeftest(mod_1jambi)
checkresiduals(mod_1jambi)

jambi_pred <- jambi_train - residuals(mod_1jambi)
ts.plot(jambi_pred,jambi_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_1jambi)

fc = forecast(jambi_ts, model=mod_1jambi,h=10)
summary(fc)
plot(fc)

df_jambi<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_jambi) <- c("Forecast")
df_jambi$year <- Year
head(df_jambi)
gamyearjambi<- gam(Forecast ~ s(year), data=df_jambi)
plot(gamyearjambi)
ggplot(df_jambi, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Jambi")

################################################################################
kepri_ts <- ts(totalperyear$kepri, start = 1980, frequency = 1)
plot(kepri_ts, main="Kepulauan Riau")
kepri_train<-ts(totalperyear$kepri[1:20])
acf(kepri_ts, main='Grafik ACF Data  Kepulauan Riau', lag.max = 36)
adf.test(kepri_ts)
par(mfrow=c(2,1))
plot(kepri_ts, lwd=2, main="Kepulauan Riau")
abline(h=mean(kepri_ts),lwd=2,lty=2,col='red')
acf(kepri_train, main='Grafik ACF Data Train Kepulauan Riau', lag.max = 36)
adf.test(kepri_train)

kepri_diff = diff(kepri_ts)
par(mfrow=c(2,1))
plot(kepri_diff,lwd=2, main='Plot Diferensiasi Kepulauan Riau')
abline(h=mean(kepri_diff),lwd=2,lty=2,col='red')
acf(kepri_diff, main='Grafik ACF Kepulauan Riau',lag.max=36)
adf.test(kepri_diff)

kepri_diff2 = diff(kepri_diff)
par(mfrow=c(2,1))
plot(kepri_diff2,lwd=2, main='Plot Diferensiasi  Kepulauan Riau')
abline(h=mean(kepri_diff2),lwd=2,lty=2,col='red')
acf(kepri_diff2, main='Grafik ACF Kepulauan Riau ',lag.max=36)
adf.test(kepri_diff2)

par(mfrow=c(2,2))
acf(kepri_train_diff, main='ACF Data Train Kepulauan Riau 1 Kali Differensiasi', lag.max=36)
pacf(kepri_train_diff, main='ACF Data Train Kepulauan Riau 1 Kali Differensiasi', lag.max=36)
acf(kepri_train_diff2, main='ACF Data Train Kepulauan Riau 2 Kali Differensiasi', lag.max=36)
pacf(kepri_train_diff2, main='ACF Data Train Kepulauan Riau 2 Kali Differensiasi', lag.max=36)

mod_1kepri = arima(kepri_train,order=c(3,1,1))
mod_1kepri

mod_2kepri = arima(kepri_train,order=c(0,1,0))
mod_2kepri

mod_3kepri = arima(kepri_train,order=c(0,2,0))
mod_3kepri

mod_4kepri = arima(kepri_train,order=c(3,2,3))
mod_4kepri 

mod_autokepri= auto.arima(kepri_train, max.p=4,max.q=4,
                           seasonal =FALSE, stationary = TRUE)
mod_autokepri

coeftest(mod_1kepri)
checkresiduals(mod_1kepri)

kepri_pred <- kepri_train - residuals(mod_1kepri)
ts.plot(kepri_pred,kepri_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_1kepri)

fc = forecast(kepri_ts, model=mod_1kepri,h=10)
summary(fc)
plot(fc)

df_kepri<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_kepri) <- c("Forecast")
df_kepri$year <- Year
head(df_kepri)
gamyearkepri<- gam(Forecast ~ s(year), data=df_kepri)
plot(gamyearkepri)
ggplot(df_kepri, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Kepulauan Riau")
################################################################################
babel_ts <- ts(totalperyear$babel, start = 1980, frequency = 1)
plot(babel_ts, main="Kepulauan Bangka Belitung ")
babel_train<-ts(totalperyear$babel[1:20])
acf(babel_ts, main='Grafik ACF Data  Kepulauan Bangka Belitung', lag.max = 36)
adf.test(babel_ts)
par(mfrow=c(2,1))
plot(babel_ts, lwd=2, main="Kepulauan Bangka Belitung ")
abline(h=mean(babel_ts),lwd=2,lty=2,col='red')
acf(babel_train, main='Grafik ACF Data Train Kepulauan Bangka Belitung ', lag.max = 36)
adf.test(babel_train)

babel_diff = diff(babel_ts)
par(mfrow=c(2,1))
plot(babel_diff,lwd=2, main='Plot Diferensiasi Kepulauan Bangka Belitung')
abline(h=mean(babel_diff),lwd=2,lty=2,col='red')
acf(babel_diff, main='Grafik ACF Kepulauan Bangka Belitung',lag.max=36)
adf.test(babel_diff)

babel_diff2 = diff(babel_diff)
par(mfrow=c(2,1))
plot(babel_diff2,lwd=2, main='Plot Diferensiasi  Kepulauan Bangka Belitung')
abline(h=mean(babel_diff2),lwd=2,lty=2,col='red')
acf(babel_diff2, main='Grafik ACF Kepulauan Bangka Belitung ',lag.max=36)
adf.test(babel_diff2)

par(mfrow=c(2,2))
acf(babel_train_diff, main='ACF Data Train Kepulauan Bangka Belitung  1 Kali Differensiasi', lag.max=36)
pacf(babel_train_diff, main='ACF Data Train Kepulauan Bangka Belitung  1 Kali Differensiasi', lag.max=36)
acf(babel_train_diff2, main='ACF Data Train Kepulauan Bangka Belitung  2 Kali Differensiasi', lag.max=36)
pacf(babel_train_diff2, main='ACF Data Train Kepulauan Bangka Belitung 2 Kali Differensiasi', lag.max=36)

mod_1babel = arima(babel_train,order=c(3,1,1))
mod_1babel

mod_2babel = arima(babel_train,order=c(0,1,0))
mod_2babel

mod_3babel = arima(babel_train,order=c(0,2,0))
mod_3babel

mod_4babel = arima(babel_train,order=c(3,2,3))
mod_4babel

mod_autobabel= auto.arima(babel_train, max.p=4,max.q=4,
                          seasonal =FALSE, stationary = TRUE)
mod_autobabel

coeftest(mod_1babel)
checkresiduals(mod_1babel)

babel_pred <- babel_train - residuals(mod_1babel)
ts.plot(babel_pred,babel_train, xlab = 'Waktu', ylab = 'Total Electricity', 
        col = c('red', 'blue'), main = 'Perbandingan Antara Data Asli dan dari Model')
accuracy(mod_1babel)

fc = forecast(babel_ts, model=mod_1babel,h=10)
summary(fc)
plot(fc)

df_babel<-as.data.frame(fc, null=TRUE)
Year <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029)
names(df_babel) <- c("Forecast")
df_babel$year <- Year
head(df_kepri)
gamyearbabel<- gam(Forecast ~ s(year), data=df_babel)
plot(gamyearbabel)
ggplot(df_babel, aes(year, Forecast)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x)) +
  ggtitle("GAM Model Kepulauan Bangka Belitung")

