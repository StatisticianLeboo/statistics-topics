library(TSA)
win.graph(width = 4.875, height=3,pointsize=8)
data(ar1.s); plot(ar1.s,ylab=expression(Y[t]),type='o', main="Time Plot of an AR(1) Series with ?? = 0.9")

win.graph(width=3, height=3,pointsize=8)
plot(y=ar1.s,x=zlag(ar1.s),ylab=expression(Y[t]), 
       xlab=expression(Y[t-1]),type='p', main = "Plot of Yt versus Yt ??? 1 for AR(1) Series")


## practical from lessons 1 and 2
library(readxl)
quakesdata <- scan("quakes.dat")


library(astsa)
#converting the data into a time series
quakests<- ts(quakesdata)
quakests
plot(quakests, type="b")

#plot for x versus lag 1 of quakests
lag1.plot(quakests,1)

#plot for the ACF os quakests for lag1 to 19
acf(quakests, xlim=c(1,19))
quakestslag1=lag(quakests,-1)# Creates a lag 1 of x variable
y=cbind(quakests, quakestslag1)
ar1fit=lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
summary(ar1fit)

plot(ar1fit$fit, ar1fit$residuals)#plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18

#sarima function
sarima(quakests, 1,0,0)
sarima.for(quakests,4,1,0,0)



mort=scan("cmort.dat")
plot(mort, type="o") # plot of mortality rate
mort_ts=ts(mort)
mortdiff=diff(mort_ts,1) # creates a variable = x(t) - x(t-1)
plot(mortdiff,type="o") # plot of first differences
acf(mortdiff,xlim=c(1,24)) # plot of first differences, for 24 lags
mortdifflag1=lag(mortdiff,-1)
y=cbind(mortdiff,mortdifflag1) # bind first differences and lagged first differences
mortdiffar1=lm(y[,1]~y[,2]) # AR(1) regression for first differences
summary(mortdiffar1) # regression results
acf(mortdiffar1$residuals, xlim = c(1,24)) # ACF of residuals for 24 lags.


#PACF 
ma1pacf=ARMAacf(ma=c(.7), lag.max=36, pacf = TRUE)
plot(ma1pacf, type="h", main="Theoretical PACF of MA(1) with theta= 0.7")
