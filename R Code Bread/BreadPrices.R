# chapter 1 code for bread price example

price<-c(5.8, 6.1, 5.4, 6.2, 5.0, 4.6, 5.8, 5.1, 5.3, 5.1, 4.8, 5.3, 6.8, 9.0, 8.6, 
9.0, 7.4, 6.4, 4.8, 3.9, 3.9, 5.6, 5.7, 7.5, 7.3, 7.4, 7.5, 9.7, 6.1, 6.0, 5.7, 5.0, 
4.2, 4.6, 5.9, 5.4, 5.4, 5.4, 5.6, 7.6, 7.4, 5.4, 5.1, 6.9, 7.5, 5.9, 6.2, 5.6, 5.8, 
5.6, 6.6, 4.8, 5.2, 4.5, 4.4, 5.3, 5.0, 6.4, 7.8, 8.5, 5.6, 7.1, 7.1, 8.0, 7.3, 5.7, 
4.8, 4.3, 4.4, 5.7, 4.7, 4.1, 4.1, 4.7, 7.0, 8.7, 6.2, 5.9, 5.4, 6.3, 4.9, 5.5, 5.4, 
4.7, 4.1, 4.6, 4.8, 4.5, 4.7, 4.8, 5.4, 6.0, 5.1, 6.5, 6.2, 4.6, 4.5, 4.0, 4.1, 4.7,
5.1, 5.2, 5.3, 4.8, 5.0, 6.2, 6.4, 4.7, 4.1, 3.9, 4.0, 4.9, 4.9, 4.8, 5.0, 4.9, 4.9, 
5.4, 5.6, 5.0, 4.5, 5.0, 7.2, 6.1)
BP.ts<-ts(price,start=1634)
summary(BP.ts)
BP.short.ts<-ts(price[1:(1+(1690-1634))],start=1634)
plot(BP.short.ts,xlab="Year",ylab="",
     main="Average Price of 4lb Loaf of Bread in London",las=1)
BP.acf<-acf(BP.ts,30)
BP.acf
 
# chapter 7 code: sample (partial) autocorrelation function

BP.acf<-acf(BP.ts, 15)
BP.pacf<-acf(BP.ts,15,type="partial")


# chapter 7 code: fitting an AR model to the data
## using the AIC criterion

BP.ar1m<-arima(BP.ts, order=c(1,0,0))
BP.ar1m

BP.ar1m$coef
BP.ar1m$var.coef
BP.ar1m$sigma2
BP.ar1m$aic


BP.ar2m<-arima(BP.ts, order=c(2,0,0))
BP.ar2m

BP.ar2m$coef
BP.ar2m$var.coef
BP.ar2m$sigma2
BP.ar2m$aic

# chapter 8

tsdiag(BP.ar1m)

# chapter 9
BP.fore <- predict(BP.ar1m, 10)
BP.fore
pred <- BP.fore$pred
L95 <- pred - 1.96 * BP.fore$se
U95 <- pred + 1.96 * BP.fore$se
year<-1758:1767
BP.PL95 <- data.frame(year,L95, pred, U95)
BP.PL95





 