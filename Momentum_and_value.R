---
  title: "Momentum"
author: "Brett Ryder"
date: "8 March 2016"
output: html_document
---
  
  ```{r, echo=FALSE}
setwd("~/Documents/Market_Timing")
library(readxl)
library(stringr)
library(zoo)
library(lubridate)
library(reshape2)
library(dynlm)
library(PerformanceAnalytics)
library(quantmod)
library(RQuantLib)
library(tseries)
```

```{r Getting Data, echo=FALSE}
YieldsHist<-read_excel("RBAYieldsHistory.xlsx",skip=9)
YieldsHist<-YieldsHist[,c("date","i2yr","i3yr","i5yr","i10yr","icash","i30d","i90d","i180d")]

YieldsHist$month<-month(as.Date(YieldsHist$date,origin="1899-12-30"))

#creating zoo object
dates <- as.Date(YieldsHist$date,origin="1899-12-30")
yields <- zoo(YieldsHist,dates)
yields <- na.locf(yields)
yields$i2yrL <- lag(yields$i2yr,k=-1,na.pad=TRUE)
yields$i3yrL <- lag(yields$i3yr,k=-1,na.pad=TRUE)
yields$i5yrL <- lag(yields$i5yr,k=-1,na.pad=TRUE)
yields$i10yrL <- lag(yields$i10yr,k=-1,na.pad=TRUE)
yields$icashL <- lag(yields$icash,k=-1,na.pad=TRUE)
yields$i30dL <- lag(yields$i30d,k=-1,na.pad=TRUE)
yields$i90dL <- lag(yields$i90d,k=-1,na.pad=TRUE)
yields$i180dL <- lag(yields$i180d,k=-1,na.pad=TRUE)

#adding equity price
SPI <- lag(get.hist.quote("^AORD", quote="Adj", start="1992-01-01", retclass="zoo"),k=-1,na.pad=TRUE) #lag by 1 day
colnames(SPI)<-"SPI"
spi <- na.locf(SPI) # Copy last traded price when NA

#adding 10 year USA bond yield
getSymbols.FRED("DGS10",env=.GlobalEnv,return.class = "zoo")
DGS10 <- na.locf(DGS10) # Copy last traded price when NA
usi10yr <- lag(DGS10,k=-1,na.pad=TRUE)

#30 day US$ LIBOR
getSymbols.FRED("USD1MTD156N",env=.GlobalEnv,return.class="zoo")
USD1MTD156N <- na.locf(USD1MTD156N)
usi30d <- lag(USD1MTD156N,k=-1,na.pad=TRUE)

#adding USD exchange rate
XR_hist<-read_excel("AUD.xlsx",skip=10)
dates<-as.Date(XR_hist$date,origin="1899-12-30")
XR<-zoo(XR_hist,dates)
XR$USD <- na.locf(XR$USD)
usd <- lag(XR$USD,k=-1,na.pad=TRUE)

#USA Standard and Poors Index
usspi <- lag(get.hist.quote("^GSPC",quote="Adj",start="1992-01-01",retclass="zoo"),k=-1,na.pad=TRUE)
usspi <- na.locf(usspi)


#windowing data to timeframe of yields
spi<-window(spi,start="1992-07-01",end="2016-02-04")
usd<-window(usd,start="1992-07-01",end="2016-02-04")
usi10yr <- window(usi10yr, start="1992-07-01",end="2016-02-04")
usi30d <- window(usi30d, start="1992-07-01",end="2016-02-04")
usspi <- window(usspi,start="1992-07-01",end="2016-02-04")

#merging data
yields1<-merge(yields,spi,usd,usi10yr,usi30d,usspi,all=TRUE)
yields <- na.locf(yields1)

```



```{r}
#calculating average interest rate and its change
yields$ndays<-diff(yields$date)

#turn of the month
yields$dmonth<-diff(yields$month,na.pad=TRUE)
yields$first<-ifelse(yields$dmonth==0,0,1)
yields$last<-lag(yields$first,na.pad=TRUE)

#Aggregating to end of month data
yieldsEOM<-yields[yields$last==1,]

#cleaning up
yieldsEOM <- yieldsEOM[, colnames(yieldsEOM) != "date"]
yieldsEOM <- yieldsEOM[, colnames(yieldsEOM) != "month"]
yieldsEOM <- yieldsEOM[, colnames(yieldsEOM) != "ndays"]
yieldsEOM <- yieldsEOM[, colnames(yieldsEOM) != "dmonth"]
yieldsEOM <- yieldsEOM[, colnames(yieldsEOM) != "first"]
yieldsEOM <- yieldsEOM[, colnames(yieldsEOM) != "last"]
```

```{r Bond Return Calculations}
yieldsEOM$r2yr<-NA
yieldsEOM$r3yr<-NA
yieldsEOM$r5yr<-NA
yieldsEOM$r10yr<-NA
yieldsEOM$r2yrL<-NA
yieldsEOM$r3yrL<-NA
yieldsEOM$r5yrL<-NA
yieldsEOM$r10yrL<-NA
yieldsEOM$usr10yr <- NA
yieldsEOM$price<-NA

price.fn<-function(years,series){
  for (i in 2:nrow(yieldsEOM)){
    
    bond <- list(settlementDays=0,
                 issueDate=index(yieldsEOM[i-1]),
                 faceAmount=100,
                 accrualDayCounter='ActualActual',
                 paymentConvention='Following',
                 paymentCalendar="Australia",
                 setEvaluationDate(index(yieldsEOM[i])) )
    
    schedule <- list(effectiveDate=index(yieldsEOM[i-1]),
                     maturityDate=index(yieldsEOM[i-1])+365.25*years,
                     period='Semiannual',
                     calendar='Australia',
                     businessDayConvention='Following')
    
    calc=list(dayCounter='ActualActual',
              compounding='Compounded',
              freq='Semiannual',
              durationType='Modified')
    
    coupon.rate <- series[i-1]/100
    yield <- series[i]/100
    
    bondPrice<-FixedRateBond(bond,
                             coupon.rate,
                             schedule,
                             calc,
                             yield=yield)
    
    yieldsEOM$price[i]<-bondPrice$dirtyPrice/100-1
  }
  yieldsEOM$price
}

yieldsEOM$r2yr<-price.fn(years=2,series=yieldsEOM$i2yr)
yieldsEOM$r3yr<-price.fn(years=3,series=yieldsEOM$i3yr)
yieldsEOM$r5yr<-price.fn(years=5,series=yieldsEOM$i5yr)
yieldsEOM$r10yr<-price.fn(years=10,series=yieldsEOM$i10yr)

yieldsEOM$r2yrL<-price.fn(years=2,series=yieldsEOM$i2yrL)
yieldsEOM$r3yrL<-price.fn(years=3,series=yieldsEOM$i3yrL)
yieldsEOM$r5yrL<-price.fn(years=5,series=yieldsEOM$i5yrL)
yieldsEOM$r10yrL<-price.fn(years=10,series=yieldsEOM$i10yrL)

yieldsEOM$usr10yr <- price.fn(years=10,series=yieldsEOM$usi10yr)

#Average Return on Australian Bonds
yieldsEOM$ravg<-(yieldsEOM$r2yr+yieldsEOM$r3yr+yieldsEOM$r5yr+yieldsEOM$r10yr)/4
yieldsEOM$ravgL<-(yieldsEOM$r2yrL+yieldsEOM$r3yrL+yieldsEOM$r5yrL+yieldsEOM$r10yrL)/4

#Average Interest Rate on Australian Bonds
yieldsEOM$iavg <- (yieldsEOM$i2yr+yieldsEOM$i3yr+yieldsEOM$i5yr+yieldsEOM$i10yr)/4
yieldsEOM$iavgL <- (yieldsEOM$i2yrL+yieldsEOM$i3yrL+yieldsEOM$i5yrL+yieldsEOM$i10yrL)/4

#cleaning up
yieldsEOM <- yieldsEOM[, colnames(yieldsEOM) != "price"]
```

```{r short term bill returns}
yieldsEOM$r30d <- lag(yieldsEOM$i30d,k=-1,na.pad=TRUE)/1200
yieldsEOM$r30dL <- lag(yieldsEOM$i30dL,k=-1,na.pad=TRUE)/1200
yieldsEOM$usr30d <- lag(yieldsEOM$usi30d,k=-1,na.pad=TRUE)/1200

```

```{r monthly spi and exchange rate returns}
yieldsEOM$rspi <- diff(log(yieldsEOM$spi)) 
yieldsEOM$rusd <- diff(log(yieldsEOM$usd))
yieldsEOM$russpi <- diff(log(yieldsEOM$usspi))
```

```{r exponential moving average of bond yields}
yieldsEOM$i10yrewa <- 8.40
for (i in 2:nrow(yieldsEOM)) {
  yieldsEOM$i10yrewa[i] <- 0.95*coredata(yieldsEOM$i10yrewa[i-1])+0.05*yieldsEOM$i10yrL[i]
}
```



```{r Preliminary Analysis}

movAvg<-function(series){
  
  ma3 <- rollapply(series,3,mean,align="right",fill=NA)
  ma6 <- rollapply(series,6,mean,align="right",fill=NA)
  ma9 <- rollapply(series,9,mean,align="right",fill=NA)
  ma12 <- rollapply(series,12,mean,align="right",fill=NA)
  
  s1 <- (series-mean(series,na.rm=TRUE))/(var(series,na.rm=TRUE)^0.5)
  s3 <- (ma3-mean(ma3,na.rm=TRUE))/(var(ma3,na.rm=TRUE)^0.5)
  s6 <- (ma6-mean(ma6,na.rm=TRUE))/(var(ma6,na.rm=TRUE)^0.5)
  s9 <- (ma9-mean(ma9,na.rm=TRUE))/(var(ma9,na.rm=TRUE)^0.5)
  s12 <- (ma12-mean(ma12,na.rm=TRUE))/(var(ma12,na.rm=TRUE)^0.5)
  list(s1,s3,s6,s9,s12)
}

#Long-term YC slope
yc <- lag(yieldsEOM$iavgL-yieldsEOM$i30dL,k=-1,fill=NA) 
yc_ma=as.data.frame.list(movAvg(yc))
colnames(yc_ma)<-c("ycs1","ycs3","ycs6","ycs9","ycs12")

#Short-term YC slope
syc <- lag(yieldsEOM$i90dL-yieldsEOM$i30dL,k=-1,fill=NA)
syc_ma=as.data.frame.list(movAvg(syc))
colnames(syc_ma)<-c("sycs1","sycs3","sycs6","sycs9","sycs12")

#spread to USA
susa <- lag(yieldsEOM$i10yrL-yieldsEOM$usi10yr,k=-1,fill=NA)
susa_ma=as.data.frame.list(movAvg(susa))
colnames(susa_ma)<-c("susas1","susas3","susas6","susas9","susas12")

#spread to trend
st <- lag(yieldsEOM$i10yrL-yieldsEOM$i10yrewa,k=-1,fill=NA) 
st_ma=as.zoo(as.data.frame.list(movAvg(st)))
colnames(st_ma)<-c("sts1","sts3","sts6","sts9","sts12")

#series <- diff(yieldsEOM$icashL,1,na.pad=TRUE) #change in cash rate
#series <- yieldsEOM$ravgL-yieldsEOM$r30dL #lagged momentum
#series <- -(yieldsEOM$rspi-yieldsEOM$r30dL) #spi momentum (negative)
#series <- -yieldsEOM$rusd+yieldsEOM$r30dL-yieldsEOM$usr30d #XR momentum
#series <- yieldsEOM$usr10yr-yieldsEOM$usr30d #US bond momentum

```

```{r}

#Returns
yieldsEOM$er <- yieldsEOM$ravg-yieldsEOM$r30d

#aggressiveness parameter
alpha <- 0.1

#trend deviation
st_ma_rets <- alpha*st_ma*yieldsEOM$er
j <- 1200*colMeans(st_ma_rets,na.rm=TRUE)
apply(st_ma_rets,2,var,na.rm=TRUE)


returnsTable <- matrix(data=NA,nrow=50,ncol=7)
returnsTable[1,3] <- "1mth"
returnsTable[1,4] <- "3mth"
returnsTable[1,5] <- "6mth"
returnsTable[1,6] <- "9mth"
returnsTable[1,7] <- "12mth"

returnsTable[19,1] <- "Momentum"




seriesName <- "US Bond M"
row_number <- 36
returnsTable[row_number,1] <- seriesName
returnsTable[(row_number+1),2] <- "car"
returnsTable[(row_number+2),2] <- "std"
returnsTable[(row_number+3),2] <- "sr"



#Calculating and plotting cumulative returns
yieldsEOM$r_port <- lag(series_std,k=-1,na.pad=TRUE)*(yieldsEOM$ravg-yieldsEOM$r30d)

yieldsEOM$r_lport[14:nrow(yieldsEOM)] <-
  cumsum(yieldsEOM$r_port[14:nrow(yieldsEOM)])

returnsTable[(row_number+1),3] <-
  round(1200*mean(yieldsEOM$r_port,na.rm=TRUE),3)
returnsTable[(row_number+2),3] <-
  round(100*(12*var(yieldsEOM$r_port,na.rm=TRUE))^0.5,3)
returnsTable[(row_number+3),3] <- 
  round(12*mean(yieldsEOM$r_port,na.rm=TRUE)
        /((12*var(yieldsEOM$r_port,na.rm=TRUE))^0.5),3)


yieldsEOM$r_port3 <- lag(series_ma3_std,k=-1,na.pad=TRUE)*(yieldsEOM$ravg-yieldsEOM$r30d)
yieldsEOM$r_lport3[14:nrow(yieldsEOM)]<-cumsum(yieldsEOM$r_port3[14:nrow(yieldsEOM)])

returnsTable[(row_number+1),4] <-
  round(1200*mean(yieldsEOM$r_port3,na.rm=TRUE),3)
returnsTable[(row_number+2),4] <-
  round(100*(12*var(yieldsEOM$r_port3,na.rm=TRUE))^0.5,3)
returnsTable[(row_number+3),4] <- 
  round(12*mean(yieldsEOM$r_port3,na.rm=TRUE)
        /((12*var(yieldsEOM$r_port3,na.rm=TRUE))^0.5),3)


yieldsEOM$r_port6 <- lag(series_ma6_std,k=-1,na.pad=TRUE)*(yieldsEOM$ravg-yieldsEOM$r30d)
yieldsEOM$r_lport6[14:nrow(yieldsEOM)]<-cumsum(yieldsEOM$r_port6[14:nrow(yieldsEOM)])
returnsTable[(row_number+1),5] <-
  round(1200*mean(yieldsEOM$r_port6,na.rm=TRUE),3)
returnsTable[(row_number+2),5] <-
  round(100*(12*var(yieldsEOM$r_port6,na.rm=TRUE))^0.5,3)
returnsTable[(row_number+3),5] <- 
  round(12*mean(yieldsEOM$r_port6,na.rm=TRUE)
        /((12*var(yieldsEOM$r_port6,na.rm=TRUE))^0.5),3)



yieldsEOM$r_port9 <- lag(series_ma9_std,k=-1,na.pad=TRUE)*(yieldsEOM$ravg-yieldsEOM$r30d)
yieldsEOM$r_lport9[14:nrow(yieldsEOM)]<-cumsum(yieldsEOM$r_port9[14:nrow(yieldsEOM)])

returnsTable[(row_number+1),6] <-
  round(1200*mean(yieldsEOM$r_port9,na.rm=TRUE),3)
returnsTable[(row_number+2),6] <-
  round(100*(12*var(yieldsEOM$r_port9,na.rm=TRUE))^0.5,3)
returnsTable[(row_number+3),6] <- 
  round(12*mean(yieldsEOM$r_port9,na.rm=TRUE)
        /((12*var(yieldsEOM$r_port9,na.rm=TRUE))^0.5),3)


yieldsEOM$r_port12 <- lag(series_ma12_std,k=-1,na.pad=TRUE)*(yieldsEOM$ravg-yieldsEOM$r30d)
yieldsEOM$r_lport12[14:nrow(yieldsEOM)]<-cumsum(yieldsEOM$r_port12[14:nrow(yieldsEOM)])

returnsTable[(row_number+1),7] <-
  round(1200*mean(yieldsEOM$r_port12,na.rm=TRUE),3)
returnsTable[(row_number+2),7] <-
  round(100*(12*var(yieldsEOM$r_port12,na.rm=TRUE))^0.5,3)
returnsTable[(row_number+3),7] <- 
  round(12*mean(yieldsEOM$r_port12,na.rm=TRUE)
        /((12*var(yieldsEOM$r_port12,na.rm=TRUE))^0.5),3)

plot(yieldsEOM$r_lport,ylim=c(-0.5,0.6))
abline(h=0)
lines(yieldsEOM$r_lport3,col="blue")
lines(yieldsEOM$r_lport6,col="green")
lines(yieldsEOM$r_lport9,col="red")
lines(yieldsEOM$r_lport12,col="orange")

chart.CumReturns(yieldsEOM$r_port12)
table.AnnualizedReturns(yieldsEOM$r_port12)

```


```{r}
acf(coredata(yieldsEOM$ravg-yieldsEOM$r30d),na.action=na.pass,lag.max=12)
acf(coredata(yieldsEOM$r10yr-yieldsEOM$r30d),na.action=na.pass,lag.max=12)
acf(coredata(yieldsEOM$r5yr-yieldsEOM$r30d),na.action=na.pass,lag.max=12)
acf(coredata(yieldsEOM$r3yr-yieldsEOM$r30d),na.action=na.pass,lag.max=12)
acf(coredata(yieldsEOM$r2yr-yieldsEOM$r30d),na.action=na.pass,lag.max=12)
acf(coredata(yieldsEOM$usr10yr-yieldsEOM$usr90d),na.action=na.pass,lag.max=12)
acf(coredata(yieldsEOM$rspi-yieldsEOM$r30d),na.action=na.pass,lag.max=12)
acf(coredata(yieldsEOM$rusd),na.action=na.pass,lag.max=12)
acf(coredata(yieldsEOM$russpi),na.action=na.pass,lag.max=12)
```


```{r, univariate cumulative returns}
ma_length<-3

yieldsEOM$r30da<-rollmean(yieldsEOM$r30d,ma_length,align="right",fill=NA)
yieldsEOM$r2yra<-rollmean(yieldsEOM$r2yr,ma_length,align="right",fill=NA)
yieldsEOM$r3yra<-rollmean(yieldsEOM$r3yr,ma_length,align="right",fill=NA)
yieldsEOM$r5yra<-rollmean(yieldsEOM$r5yr,ma_length,align="right",fill=NA)
yieldsEOM$r10yra<-rollmean(yieldsEOM$r10yr,ma_length,align="right",fill=NA)
yieldsEOM$usr10yra<-rollmean(yieldsEOM$usr10yr,ma_length,align="right",fill=NA)
yieldsEOM$ravga<-rollmean(yieldsEOM$ravg,ma_length,align="right",fill=NA)
yieldsEOM$rspia<-rollmean(yieldsEOM$rspi,ma_length,align="right",fill=NA)
yieldsEOM$rusda <- rollmean(yieldsEOM$rusd,ma_length,align="right",fill=NA)
yieldsEOM$russpia <- rollmean(yieldsEOM$russpi,ma_length,align="right",fill=NA)
```


```{r}
summary(dynlm((ravg-r30d)~L(i90d-i30d,1)+L(iavg-i30d,1)+L(ravga-r30da,1)+L(rspia-r30da,1)+L(rusd,1)+L(usr10yra,1)+L(usi10yr-usi90d,1)+L(i10yr-usi10yr,1)+L(i90d-usi90d,1)+L(russpia,1)+L(i10yr-i10yrewa,3),data=yieldsEOM))
```




```{r}
yieldsEOM$w<-NA

#Equities
yieldsEOM$w<- -lag(yieldsEOM$rspia-yieldsEOM$r30da,na.pad=TRUE,k=-1)/(var(yieldsEOM$rspia-yieldsEOM$r30da,na.rm=TRUE)^0.5)
yieldsEOM$w<-yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port<-yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)

# USAEquities
yieldsEOM$w<- -lag(yieldsEOM$russpia,na.pad=TRUE,k=-1)/(var(yieldsEOM$russpia,na.rm=TRUE)^0.5)
yieldsEOM$w<-yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port<-yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)

#Currency
yieldsEOM$w<- -lag(yieldsEOM$rusda,na.pad=TRUE,k=-1)/(var(yieldsEOM$rusda,na.rm=TRUE)^0.5)
yieldsEOM$w <-yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port<-yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)

#Bond Momentum
yieldsEOM$w<- lag(yieldsEOM$ravga-yieldsEOM$r30da,na.pad=TRUE,k=-1)/(var(yieldsEOM$ravga-yieldsEOM$r30da,na.rm=TRUE)^0.5)
yieldsEOM$w <- yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port<-yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)

#USA Bond Momentum
yieldsEOM$w<- lag(yieldsEOM$usr10yr,na.pad=TRUE,k=-1)/(var(yieldsEOM$usr10yr,na.rm=TRUE)^0.5)
yieldsEOM$w <- yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port<-yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)

#Yield Curve
yieldsEOM$w<- lag(yieldsEOM$iavg-yieldsEOM$i30d,na.pad=TRUE,k=-1)/(var(yieldsEOM$iavg-yieldsEOM$i30d,na.rm=TRUE)^0.5)
yieldsEOM$w <- yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port<-yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)

#USA Yield Curve
yieldsEOM$w<- lag(yieldsEOM$usi10yr-yieldsEOM$usi90d,na.pad=TRUE,k=-1)/(var(yieldsEOM$usi10yr-yieldsEOM$usi90d,na.rm=TRUE)^0.5)
yieldsEOM$w <- yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port<-yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)

#Yield Spread to USA
yieldsEOM$w<- lag(yieldsEOM$i10yr-yieldsEOM$usi10yr,na.pad=TRUE,k=-1)/(var(yieldsEOM$i10yr-yieldsEOM$usi10yr,na.rm=TRUE)^0.5)
yieldsEOM$w <- yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port<-yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)


#Short End Yield Curve
yieldsEOM$w <- lag(yieldsEOM$i90d-yieldsEOM$i30d,na.pad=TRUE,k=-1)/(var(yieldsEOM$i90d-yieldsEOM$i30d,na.rm=TRUE)^0.5)
yieldsEOM$w <- yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port <- yieldsEOM$w*(yieldsEOM$ravg-yieldsEOM$r30d)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)
```

```{r}
er1003 <- (yieldsEOM$r10yr-yieldsEOM$r30d)/3-(yieldsEOM$r3yr-yieldsEOM$r30d)

plot(er1003)
abline(h=0)
lines((yieldsEOM$i10yr-yieldsEOM$i3yr-.5)/100,col="red")

summary(dynlm(er1003~L(yieldsEOM$i10yr-yieldsEOM$i3yr,1)+L(yieldsEOM$rspia-yieldsEOM$r30da,1)+L(yieldsEOM$i90d-yieldsEOM$i30d,1)+L(er1003,1:4)+L(yieldsEOM$rusda,1)+L(yieldsEOM$usr10yra,1)+L(yieldsEOM$ravga-yieldsEOM$r30da,1)))

#Yield curve slope
yieldsEOM$w <- lag(yieldsEOM$rspia-yieldsEOM$r30da,na.pad=TRUE,k=-2)/(var(yieldsEOM$rspia-yieldsEOM$r30da,na.rm=TRUE)^0.5)
yieldsEOM$w <- yieldsEOM$w-mean(yieldsEOM$w,na.rm=TRUE)
yieldsEOM$r_port <- yieldsEOM$w*(yieldsEOM$r10yr/3-yieldsEOM$r3yr)
charts.PerformanceSummary(yieldsEOM$r_port)
table.AnnualizedReturns(yieldsEOM$r_port)



```





