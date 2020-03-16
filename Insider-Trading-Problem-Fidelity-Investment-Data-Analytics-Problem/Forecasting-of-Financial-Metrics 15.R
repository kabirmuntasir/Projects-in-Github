

library('quantmod')
library(data.table)
library(reshape)
library(plyr)
library(dplyr)
library(forecast)
library(ggplot2)
library(xts)
library(dygraphs)

# filter initially and take maximum dddate to ignore duplicate
#read the txt file , extract AtlanticCityElectricCo data and write CSV file
set.seed(101)

#2015
mydata2015q1=fread("C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/SEC DATA/2015q1/num2015q1.txt",header = TRUE)
head(mydata2015q1)

setDT(mydata2015q1)[, paste0("adsh", 1:3) := tstrsplit(adsh, "-", type.convert = TRUE, fixed = TRUE)]
sec2015q1=filter(mydata2015q1,qtrs=="1")
sec2015q1$d <- paste0(sec2015q1$tag,sep="_",sec2015q1$adsh1,sep="_",sec2015q1$coreg)
s2015q1=setDT(sec2015q1)[, .SD[which.max(ddate)], d]
dim(s2015q1)

mydata2015q2=fread("C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/SEC DATA/2015q2/num2015q2.txt",header = TRUE)
setDT(mydata2015q2)[, paste0("adsh", 1:3) := tstrsplit(adsh, "-", type.convert = TRUE, fixed = TRUE)]
sec2015q2=filter(mydata2015q2,qtrs=="2")
sec2015q2$d <- paste0(sec2015q2$tag,sep="_",sec2015q2$adsh1,sep="_",sec2015q2$coreg)
s2015q2=setDT(sec2015q2)[, .SD[which.max(ddate)], d]
dim(s2015q2)

mydata2015q3=fread("C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/SEC DATA/2015q3/num2015q3.txt",header = TRUE)
setDT(mydata2015q3)[, paste0("adsh", 1:3) := tstrsplit(adsh, "-", type.convert = TRUE, fixed = TRUE)]
sec2015q3=filter(mydata2015q3,qtrs=="3")
sec2015q3$d <- paste0(sec2015q3$tag,sep="_",sec2015q3$adsh1,sep="_",sec2015q3$coreg)
s2015q3=setDT(sec2015q3)[, .SD[which.max(ddate)], d]
dim(s2015q3)

mydata2015q4=fread("C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/SEC DATA/2015q4/num2015q4.txt",header = TRUE)
setDT(mydata2015q4)[, paste0("adsh", 1:3) := tstrsplit(adsh, "-", type.convert = TRUE, fixed = TRUE)]
sec2015q4=filter(mydata2015q4,qtrs=="4")
sec2015q4$d <- paste0(sec2015q4$tag,sep="_",sec2015q4$adsh1,sep="_",sec2015q4$coreg)
s2015q4=setDT(sec2015q4)[, .SD[which.max(ddate)], d]
dim(s2015q4)


#2016
mydata2016q1=fread("C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/SEC DATA/2016q1/num2016q1.txt",header = TRUE)
setDT(mydata2016q1)[, paste0("adsh", 1:3) := tstrsplit(adsh, "-", type.convert = TRUE, fixed = TRUE)]
sec2016q1=filter(mydata2016q1,qtrs=="1")
sec2016q1$d <- paste0(sec2016q1$tag,sep="_",sec2016q1$adsh1,sep="_",sec2016q1$coreg)
s2016q1=setDT(sec2016q1)[, .SD[which.max(ddate)], d]
dim(s2016q1)

mydata2016q2=fread("C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/SEC DATA/2016q2/num2016q2.txt",header = TRUE)
setDT(mydata2016q2)[, paste0("adsh", 1:3) := tstrsplit(adsh, "-", type.convert = TRUE, fixed = TRUE)]
sec2016q2=filter(mydata2016q2,qtrs=="2")
sec2016q2$d <- paste0(sec2016q2$tag,sep="_",sec2016q2$adsh1,sep="_",sec2016q2$coreg)
s2016q2=setDT(sec2016q2)[, .SD[which.max(ddate)], d]
dim(s2016q2)

mydata2016q3=fread("C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/SEC DATA/2016q3/num2016q3.txt",header = TRUE)
setDT(mydata2016q3)[, paste0("adsh", 1:3) := tstrsplit(adsh, "-", type.convert = TRUE, fixed = TRUE)]
sec2016q3=filter(mydata2016q3,qtrs=="3")
sec2016q3$d <- paste0(sec2016q3$tag,sep="_",sec2016q3$adsh1,sep="_",sec2016q3$coreg)
s2016q3=setDT(sec2016q3)[, .SD[which.max(ddate)], d]
class(s2016q3$value)



mydata2016q4=fread("C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/SEC DATA/2016q4/num2016q4.txt",header = TRUE)
setDT(mydata2016q4)[, paste0("adsh", 1:3) := tstrsplit(adsh, "-", type.convert = TRUE, fixed = TRUE)]
sec2016q4=filter(mydata2016q4,qtrs=="4")
head(sec2016q4)
sec2016q4$d <- paste0(sec2016q4$tag,sep="_",sec2016q4$adsh1,sep="_",sec2016q4$coreg)

s2016q4=setDT(sec2016q4)[, .SD[which.max(ddate)], d]
s2016q4$d1 <- paste0(s2016q4$value,sep="_",s2016q4$ddate)
s2016q4=s2016q4[,c(1:8,14,10:13)]
setnames(s2016q4, old = c('d1'), new = c('value'))
dim(s2016q4)
class(s2016q4$value)



#merge the file in to a single csv file

mydata20092016=rbind(s2015q1,s2015q2,s2015q3,s2015q4,s2016q1,s2016q2,s2016q3,s2016q4)
dim(mydata20092016)
head(mydata20092016)



#make subset of data by selecting imp column and reshape
mydata20092016$d1 <- paste0(mydata20092016$adsh2,sep="_",mydata20092016$qtrs)
distinct(mydata20092016)

mydata20092016_3v=mydata20092016[,c(1,9,14)]
head(mydata20092016_3v)
mydata20092016_3v_reshape <- reshape(mydata20092016_3v, v.names = "value", idvar = "d",timevar = "d1", direction = "wide")
head(mydata20092016_3v_reshape)
write.csv(mydata20092016_3v_reshape, file = "95kjustbeforereshape.csv")
sec=mydata20092016_3v_reshape
dim(sec)

#split d to 2 col
setDT(sec)[, paste0("d", 1:3) := tstrsplit(d, "_", type.convert = TRUE, fixed = TRUE)]
dim(sec)
head(sec)
sec$d4 <- paste0(sec$d2,sep="_",sec$d3)
head(sec)
dim(sec)
sec=sec[,c(2:10,13)]
dim(sec)
head(sec)
setnames(sec, old = c('d1','d4'), new = c('tag','adsh_coreg'))
head(sec)
dim(sec)

#data cleaning
#count na of each row and put in another column
sec$na_count <- apply(sec[,c(1:8)], 1, function(x) sum(is.na(x)))
head(sec)
dim(sec)
distinct(sec,na_count)
se=sec



#remove row where more than 20 na
s=se[se$na_count == 0]
dim(s)
head(s)


#make the data as numeric from charecter

#Do the forecast
sd=s
dim(sd)

sd$value.15_1=as.numeric(sd$value.15_1)
sd$value.15_2=as.numeric(sd$value.15_2)
sd$value.15_3=as.numeric(sd$value.15_3)
sd$value.15_4=as.numeric(sd$value.15_4)
sd$value.16_1=as.numeric(sd$value.16_1)
sd$value.16_2=as.numeric(sd$value.16_2)
sd$value.16_3=as.numeric(sd$value.16_3)

sd=sd[,c(10,9,1:8)]
head(sd)

sd=sd[-316,]
sd=sd[-625,]
sd=sd[-1449,]
sd=sd[-1744,]
sd=sd[-2908,]
sd=sd[-2971,]
sd=sd[-3703,]
sd=sd[-4164,]
sd=sd[-4798,]
sd=sd[-5030,]
sd=sd[-5021,]
sd=sd[-5117,]
sd=sd[-7400,]
sd=sd[-7709,]


dim(sd)



#to make null Datafram--- oo=data.frame() -- insert to only oo then 00[j,]
ooo=data.frame()
ooo[1,1]=1
ooo[1,2]=1
ooo[1,3]=1
ooo[1,4]=1
ooo[1,5]=1


##-----------------

for (i in 1:nrow(sd)){
  tryCatch({
    tt=sd[i,3:9]
    t=t(tt)
    ttt=ts(t,start=c(2000))
    skirtsseriesforecasts <- HoltWinters(ttt, gamma=FALSE)
    skkk<- forecast.HoltWinters(skirtsseriesforecasts, h=1)
    ooo[i,]=as.data.frame(skkk)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
    sd=sd[-i,]})
}


##----------------------



#bind the forecasted value with raw file
final=cbind(sd,ooo)
#write.csv(final, file = "HotwingeratR.csv")
head(final)

#split the adsh_coreg to get ticker value
setDT(final)[, paste0("adsh_coreg", 1:2) := tstrsplit(adsh_coreg, "_", type.convert = TRUE, fixed = TRUE)]
head(final)
setnames(final, old = c("V1","V2","V3","V4","V5",'adsh_coreg1','adsh_coreg2'), new = c("Point Forecast","Lo 80","Hi 80","Lo 95","Hi 95",'Adsh','coreg'))
ticker=read.csv('C:/Users/kabir/OneDrive/All Project/MGMT525 Fidelity Project/Data/12qtrs_verynewCompanyName and Ticker.csv', header=T)
tt=ticker[,c(15,18)]
ttt=(distinct(tt))


#merge ticker value with raw file
plyr7 <- left_join(final,ttt, by = c("Adsh"="Adsh"))
dim(plyr7)
plyr8=plyr7[,c(16:18,2:15)]
head(plyr8)
write.csv(plyr8, file = "9thmay8k.csv")
#now use the URL
plyr9=plyr8

plyr9=filter(plyr9,Ticker!="inct")
plyr9=filter(plyr9,Ticker!="cik0001586573")
plyr9=filter(plyr9,Ticker!="cik1077561")
plyr9=filter(plyr9,Ticker!="ncmllc")
plyr9=filter(plyr9,Ticker!="ocll")
plyr9=filter(plyr9,Ticker!="ugu")
plyr9=filter(plyr9,Ticker!="c803")
plyr9=filter(plyr9,Ticker!="cmds")
plyr9=filter(plyr9,Ticker!="nexg")
plyr9=filter(plyr9,Ticker!="imkt")
plyr9=filter(plyr9,Ticker!="wfsl")
plyr9=filter(plyr9,Ticker!="pmtc")
plyr9=filter(plyr9,Ticker!="lg")
plyr9=filter(plyr9,Ticker!="c730")
plyr9=filter(plyr9,Ticker!="mtga")

dim(plyr8)
class(plyr9)
head(plyr9)
symbols <- unique(toupper(plyr9$Ticker))
symbols

plyr4=data.frame()
plyr4[1,1]=1
plyr4[1,2]=1
plyr4[1,3]=1

s=0
for(s in seq_along(symbols)) {
  URL <- paste0("http://ichart.finance.yahoo.com/table.csv?s=",symbols[s])
  dat <- read.csv(URL)
  dat$Date <- as.Date(dat$Date, "%Y-%m-%d")
  #assign( paste0( symbols[s],"_data"), dat)
  #dat1=filter(dat,Date >= "2017-04-01" & Date <= "2017-08-31")
  #plyr4=mutate(plyr3[s,],Ticker=symbols[s])
  plyr4[s,1]=symbols[s]
  plyr4[s,2]=var(dat$Volume)
  plyr4[s,3]=sd(dat$Volume)
  dat <- NULL
  dat1 <- NULL
}

setnames(plyr4, old = c('V1','V2','V3'), new = c('Ticker','Stock_Volume_Variance','Stock_Price_STDDev'))
head(plyr4)


#merge the ticker value finding-URL with raw file
#make url file ready for combine
url01=as.data.frame(plyr4$Ticker) 
head(url01)
class(url01$`plyr4$Ticker`)
setnames(url01, old = c('plyr4$Ticker'), new = c('Ticker'))
url02=cbind(url01,plyr4[,2])
url03=cbind(url01,plyr4[,3])
class(url03$Ticker)

#make rawfile  file ready for combine

makeupper=as.data.frame(toupper(plyr9$Ticker)) 
names(makeupper)
setnames(makeupper, old = c('toupper(plyr9$Ticker)'), new = c('Ticker'))
rfile=cbind(plyr9,makeupper)
rfile1=rfile[,c(1,2,18,4:17)]


#get data from url file to raw file by left merge
head(rfile )
head(url02)
comb1=left_join(rfile1,url02, by= c("Ticker"="Ticker"))
comb2=left_join(comb1,url03, by= c("Ticker"="Ticker"))
head(comb2)
setnames(comb2, old = c('plyr4[, 2]','plyr4[, 3]'), new = c('Stock_variance','Stock_Std_dev'))
head(comb2)
#write.csv(comb2, file = "alldata_forecast_StockfromURL_calofstockvolvarstd.csv")

#Now create boolinger band and blah blah for flagged company
#get yes and no of line item 
comb3=comb2
setDT(comb3)[, paste0("value.16_4", 1:2) := tstrsplit(value.16_4, "_", type.convert = TRUE, fixed = TRUE)]
comb3=comb3[,c(1:11,20:21,13:19)]
setnames(comb3, old = c('value.16_41','value.16_42'), new = c('value.16_4','DDate'))
head(comb3)
comb3$value.16_4
comb4=mutate(comb3,chk2016q4intherangeornot=ifelse((comb3$value.16_4 >= comb3$`Lo 95`) & (comb3$value.16_4<=comb3$`Hi 95`), "yes", "no"))
head(comb4)
dim(comb4)

library(dplyr)
comb5=comb4 %>% group_by(Ticker) %>% mutate(count_total = n())
comb6=comb5 %>% group_by(Ticker,chk2016q4intherangeornot) %>% mutate(count_yesno = n())
comb7=as.data.frame(comb6)
head(comb7)

comb8=comb7 %>% group_by(Ticker) %>% mutate(pcntageofno = count_yesno/count_total)

comb8=as.data.frame(comb8)
dim(comb8)
head(comb8)

write.csv(comb8, file = "8thmay.csv")


comb9=filter(comb8,chk2016q4intherangeornot=="no",count_total>=10,pcntageofno>.2)
head(comb9)
dim(comb9)
comb10=as.data.frame(comb9$Ticker)
dt1=distinct(comb10)
dt1=as.character(dt1)
head(dt1)
dt2=unique(toupper(comb9$Ticker))
head(dt2)
class(dt2)

#---------------------------
#manual try
#r forecast
#ADSW
ADSW=getSymbols("ADSW",from = "2016-12-24", to="2017-05-24",src="yahoo", auto.assign=F)
chartSeries(ADSW$ADSW.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170224),"%Y%m%d"))),lwd=0.5,on=1,
      legend = "\nShare Price - green\nReport date 20170224",col="red")


#JCI
JCI=getSymbols("JCI",from = "2016-09-16", to="2017-01-08",src="yahoo", auto.assign=F)
chartSeries(JCI$JCI.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20161108),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20161108",col="red")

#MSCC
MSCC=getSymbols("MSCC",from = "2016-09-10", to="2017-01-10",src="yahoo", auto.assign=F)
chartSeries(MSCC$MSCC.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20161110),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20161110",col="red")

#MTOR
MTOR=getSymbols("MTOR",from = "2016-09-16", to="2017-01-16",src="yahoo", auto.assign=F)
chartSeries(MTOR$MTOR.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20161116),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20161116",col="red")

#RELL
RELL=getSymbols("RELL",from = "2016-02-16", to="2016-08-16",src="yahoo", auto.assign=F)
chartSeries(RELL$RELL.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20160527),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20160527",col="red")




#Nelly model
#AMAZ
AMAZ=getSymbols("AMAZ",from = "2016-09-18", to="2017-01-18",src="yahoo", auto.assign=F)
chartSeries(AMAZ$AMAZ.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20161118),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20161118",col="red")

#Excel forecast
#COHR
COHR=getSymbols("COHR",from = "2016-12-17", to="2017-05-07",src="yahoo", auto.assign=F)
chartSeries(COHR$COHR.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170207),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20170207",col="red")

#CHSCP
CHSCP=getSymbols("CHSCP",from = "2016-11-17", to="2017-03-17",src="yahoo", auto.assign=F)
chartSeries(CHSCP$CHSCP.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170112),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20170112",col="red")



#AMAT
AMAT=getSymbols("AMAT",from = "2016-12-15", to="2017-04-15",src="yahoo", auto.assign=F)
chartSeries(AMAT$AMAT.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170215),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20170215",col="red")

#TSN
TSN=getSymbols("TSN",from = "2016-12-06", to="2017-04-06",src="yahoo", auto.assign=F)
chartSeries(TSN$TSN.Adjusted,TA="addBBands();addVo()",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170206),"%Y%m%d"))),lwd=1,on=-1,
      legend = "\nShare Price - green\nReport date 20170206",col="red")




############################with moving average
#ADSW another try
ADSW=getSymbols("ADSW",from = "2016-07-24", to="2017-05-24",src="yahoo", auto.assign=F)
chartSeries(ADSW$ADSW.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170224),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date -  20170224\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")


#jci another try
JCI=getSymbols("JCI",from = "2016-05-16", to="2017-02-08",src="yahoo", auto.assign=F)
chartSeries(JCI$JCI.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20161108),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date - 20161108\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")

#MSCC another try
MSCC=getSymbols("MSCC",from = "2016-05-10", to="2017-01-10",src="yahoo", auto.assign=F)
chartSeries(MSCC$MSCC.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20161110),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date - 20161110\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")

#MTOR another try
MTOR=getSymbols("MTOR",from = "2016-07-16", to="2017-01-16",src="yahoo", auto.assign=F)
chartSeries(MTOR$MTOR.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20161116),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date -  20161116\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")

#RELL another try
RELL=getSymbols("RELL",from = "2016-03-28", to="2016-07-28",src="yahoo", auto.assign=F)
chartSeries(RELL$RELL.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20160527),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date -  20160528\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")

#AMAZ another try
AMAZ=getSymbols("AMAZ",from = "2016-06-18", to="2017-01-18",src="yahoo", auto.assign=F)
chartSeries(AMAZ$AMAZ.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20161118),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date -  20161118\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")


#COHR another try
COHR=getSymbols("COHR",from = "2016-06-17", to="2017-05-07",src="yahoo", auto.assign=F)
chartSeries(COHR$COHR.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170207),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date - 20170207\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")

#CHSCP another try
CHSCP=getSymbols("CHSCP",from = "2016-09-17", to="2017-03-17",src="yahoo", auto.assign=F)
chartSeries(CHSCP$CHSCP.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170112),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date -  20170112\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")


#AMAT another try
AMAT=getSymbols("AMAT",from = "2016-10-15", to="2017-04-15",src="yahoo", auto.assign=F)
chartSeries(AMAT$AMAT.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170215),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date - 20170215\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")


#TSN another try
TSN=getSymbols("TSN",from = "2016-09-06", to="2017-04-06",src="yahoo", auto.assign=F)
chartSeries(TSN$TSN.Close,TA="addRSI(n = 14, maType = SMA)",theme="white")
addTA(xts(TRUE,as.POSIXlt(as.Date(as.character(20170206),"%Y%m%d"))),on=-1,lwd=1,legend = "\nShare Price - green\nReport date -  20170206\nMA(10) - black\nMA(50) - red", col="red")
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE,col = "black")
addSMA(n = 50, on = 1, with.col = Cl, overlay = TRUE, col = "red")


#------------------------------------------------------------
#plot a forecast
tt=sd[3418,3:10]
tt
t=t(tt)
ttt=ts(t,start=c(2000))
ttt
skirtsseriesforecasts <- HoltWinters(ttt, gamma=FALSE)
skkk<- forecast.HoltWinters(skirtsseriesforecasts, h=1)

plot(t)
plot(skirtsseriesforecasts)
plot(skkk,main = "Holt-Winters forecasting of Sanmina Corp", ylab = "Value of Earning per share",xaxt = "n", xlab="Time-Quaters")
axis(1 ,at=2000:2007, labels=c("2015q1","2015q2","2015q3","2015q4","2016q1","2016q2","2016q3","2016q4"))
??points()
points(x=1.2, y=2007, type="p", pch=1, col="black", bg=NA, cex=1)

#----------------------------------------

#plot a forecast
tt=sd[3418,3:10]
tt
t=t(tt)
ttt=ts(t,start=c(2000))
ttt
skirtsseriesforecasts <- HoltWinters(t, gamma=FALSE)
skkk<- forecast.HoltWinters(skirtsseriesforecasts, h=1)

plot(t)
plot(skirtsseriesforecasts)
plot(skkk,main = "Holt-Winters forecasting of Sanmina Corp", ylab = "Value of Earning per share",xaxt = "n", xlab="Time-Quaters")
axis(1 ,at=2000:2008, labels=c("2015q1","2015q2","2015q3","2015q4","2016q1","2016q2","2016q3","2016q4","forecast"))
??points()
points(x=1.2, y=2007, type="p", pch=1, col="black", bg=NA, cex=1)


plot(-4:4, -4:4, type = "n")# setting up coord. system
points(rnorm(200), rnorm(200), col = "red")

setDT(tt)[, paste0("value.16_4", 1:2) := tstrsplit(value.16_4, "_", type.convert = TRUE, fixed = TRUE)]
tt=tt[,c(1:7,9)]
t
setnames(sec, old = c('value.16_41'), new = c('value.16_4'))
