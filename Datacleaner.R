setwd("C:/Users/Win7/Desktop/HVA TWK/JAAR 3/Minor/Project de waag")
rm(list = ls())
load("C:/Users/Win7/Desktop/HVA TWK/JAAR 3/Minor/Project de waag/.RData")
library(dplyr)
library(data.table)
library(lubridate)
library(psych)
library(tidyr)
library(mgcv)
library(ggplot2)
library(grid)
library(zoo)

# READING FILES----
SensData <- read.csv("sensormeasures.csv")
FinalData <- read.csv("./Github DATA/urbanairq_no2_final.csv")
SensLoc <- read.csv2("Senslonglat.csv",header=FALSE,sep=";",stringsAsFactors = FALSE)
colnames(SensLoc) <- c("id","lat","long","Adress")
SensData$srv_ts <- as.POSIXct(SensData$srv_ts)

# SPLITTING AND FORMING TIMESTAMPS----
DATA <- separate(SensData, col= "srv_ts",into=c("Date","Time"),sep=" ",remove=FALSE)
DATA$srv_ts <- as.POSIXct(DATA$srv_ts)
DATA$srv_ts <- DATA$srv_ts+(60*60)
DATA$Time <- format(trunc(strptime(DATA$Time, format="%H:%M:%S"), units="hours"),
                    format="%H")
DATA$Date <- ymd(DATA$Date)
DATA$Time <- as.numeric(DATA$Time)+1
DATA$Time[DATA$Time==24] <- 0
DATA$message[DATA$message==""] <- NA
DATA$message <- as.numeric(DATA$message)
FinalData <- separate(FinalData, col= "localTime",into=c("Date","Time"),sep=" ",remove=FALSE)
FinalData$Time <- format(trunc(strptime(FinalData$Time, format="%H:%M:%S"), units="hours"),
                         format="%H")
FinalData$Date <- ymd(FinalData$Date)
FinalData$Time <- as.numeric(FinalData$Time)

# GROUPING BY THE HOUR----
DATA <- DATA %>% 
  group_by(id, Date, Time) %>%
  summarise(temp=mean(temp, na.rm=TRUE), count=n(),rssi=mean(rssi, na.rm=TRUE),
            pm10=mean(pm10, na.rm=TRUE), pm25=mean(pm25, na.rm=TRUE),
            no2a=mean(no2a, na.rm=TRUE),no2b=mean(no2b, na.rm=TRUE),
            humidity=mean(humidity, na.rm=TRUE),message=sum(message,na.rm=T))

# FILTER (see sensor documentation)----
DATA <- DATA[order(DATA$Time, decreasing=FALSE),]
DATA <- DATA[order(DATA$Date, decreasing=FALSE),]
DATA <- DATA[order(DATA$id, decreasing=FALSE),]
DATA$message[DATA$message>1] <- 1
DATA$message[is.na(DATA$message)] <- 0
for(i in 1:length(DATA$message)){
  if(DATA$message[i]==1&&i<=14352){
    DATA$message[i+1] <- DATA$message[i+1]+2
    DATA$message[i+2] <- DATA$message[i+2]+2
    DATA$message[i+3] <- DATA$message[i+3]+2
  }
  if(DATA$message[i]==3&&i<=14352){
    DATA$message[i+1] <- DATA$message[i+1]+2
    DATA$message[i+2] <- DATA$message[i+2]+2
    DATA$message[i+3] <- DATA$message[i+3]+2
  }
  if(DATA$message[i]==5&&i<=14352){
    DATA$message[i+1] <- DATA$message[i+1]+2
    DATA$message[i+2] <- DATA$message[i+2]+2
    DATA$message[i+3] <- DATA$message[i+3]+2
  }
  if(DATA$message[i]==7&&i<=14352){
    DATA$message[i+1] <- DATA$message[i+1]+2
    DATA$message[i+2] <- DATA$message[i+2]+2
    DATA$message[i+3] <- DATA$message[i+3]+2
  }
}

DATA <- DATA %>% filter(message == 0)
# DATA <- DATA %>% filter(temp <= 30)
# DATA <- DATA %>% filter(humidity <= 85)
# DATA <- DATA %>% filter(humidity >= 18)
DATA <- DATA %>% filter(no2a <= 1.2*mean(no2a,na.rm=T))
DATA <- DATA %>% filter(no2a >= 0.8*mean(no2a,na.rm=T))
DATA <- DATA %>% filter(no2b <= 1.2*mean(no2b,na.rm=T))
DATA <- DATA %>% filter(no2b >= 0.8*mean(no2b,na.rm=T))

# slechte sensoren verwijderen
DATA <- DATA %>% filter(count >= 20)## MORE THAN 20 OBS. PER HOUR
DATA <- DATA %>% filter(id != 14560051)
DATA <- DATA %>% filter(id != 1184206)
DATA <- DATA %>% filter(id != 1184838)
DATA <- DATA %>% filter(id != 1183931)
DATA <- DATA %>% filter(id != 55300)
DATA <- DATA %>% filter(id != 55303)
DATA <- DATA %>% filter(Date >= "2016-06-02")

DATA$pm10 <- NULL ## pm measurments are simply not enough or too many zero values
DATA$pm25 <- NULL


# MERGING TOGETHER, MERGING ON TIMELINE----
DATA <- merge(DATA,SensLoc,by="id")
###
TotalData <- merge(FinalData[,c("Date","Time","localTime","ggd","ggd_os","ecn_AB46","ecn_AB47")],
                   DATA,by=c("Date","Time"),all=TRUE)
Timeline <- data.frame(localTime=seq.POSIXt(from=as.POSIXct("2016-06-02 00:00:00"),
                                            to=as.POSIXct("2016-08-29 00:00:00"),
                                            by="hour"))
TotalData$localTime <- as.POSIXct(TotalData$localTime)
TotalData$id <- as.factor(TotalData$id)

weerDat <- read.csv("weerdata.csv")
weerDat$Date <- ymd(weerDat$Date)
for(i in 1:nrow(weerDat)){
  if(weerDat$Time[i]==24){
    weerDat$Time[i] <- 0
    weerDat$Date[i] <- weerDat$Date[i]+1
  }}
TotalData <- merge(TotalData,weerDat,by=c("Date","Time"),all.x=TRUE)
# SPLITTING INTO DIFFERENT SENSORS----
for(i in 1:length(levels(TotalData$id))){
  assign(paste("SENS_",levels(TotalData$id)[i],sep=""),
         merge(Timeline,TotalData[TotalData$id==levels(TotalData$id)[i],],
               by="localTime",all.x=TRUE))
}
# SENS_55303 <- SENS_55303 %>% filter(Date < "2016-07-10")
TotalData <- rbind(SENS_1184453,SENS_1184527,SENS_1184739,SENS_1185325,SENS_13905017,
                   SENS_26296,SENS_53788,SENS_54200,SENS_54911,SENS_717780)
TotalData<- separate(TotalData, col= "localTime", into=c("Date","Time"), sep=" ",remove=FALSE)
TotalData <- TotalData[,-4:-5]
TotalData$Time <- format(trunc(strptime(TotalData$Time, format="%H:%M:%S"), units="hours"),
                    format="%H")
TotalData$Time <- as.numeric(TotalData$Time)
TotalData$Date <- ymd(TotalData$Date)
# TotalData$Windsnelheid <- TotalData$Windsnelheid/10
# TotalData$temp <- TotalData$temp/10

# write.csv(TotalData, "TotalData.csv",row.names=F)
# TotalData <- read.csv("TotalData.csv")

RMSE <-  matrix(nrow=2*length(levels(DATA$id)), ncol=8)
colnames(RMSE) <- c("Vondel periode 1","Vondel periode 2",
                    "OudeSchans periode 1","OudeSchans periode 2","Model",
                    "Sensor","Beste GAM performance","Beste LM performance")
k <- 1
RevModel <- NULL
RevModel2k <- NULL
GamModel <- NULL
GamModel2k <- NULL
Conv <- NULL
Conv2k<- NULL
Test <- NULL
SplitPred <- NULL
SplitPredGM <- NULL
DATA$id <- as.factor(DATA$id)
for(j in 1:length(levels(DATA$id))){
  Sw <- eval(parse(text=paste("SENS_",levels(DATA$id)[j], sep="")))
  Sw1 <- Sw %>% filter(Date<="2016-06-10")
  Sw2 <- Sw %>% filter(Date>="2016-08-18")
  sensp1 <- Sw %>% filter(Date>"2016-06-10")
  sensp2 <- Sw %>% filter(Date<"2016-08-18")
  lm.model <- lm(ggd~no2a+no2b+temp, data=Sw1)
  gam.model <- gam(ggd ~ s(no2a) + s(no2b) + s(temp), data=Sw1)
  lm.model2k <- lm(ggd~no2a+no2b+temp, data=Sw2)
  gam.model2k <- gam(ggd ~ s(no2a) + s(no2b) + s(temp), data=Sw2)
  RevModel[j] <- summary(lm.model)$r.squared
  RevModel2k[j] <- summary(lm.model2k)$r.squared
  GamModel[j] <- summary(gam.model)$r.sq
  GamModel2k[j] <- summary(gam.model2k)$r.sq
  Conv[j] <- gam.model$converged
  Conv2k[j] <- gam.model2k$converged
  sensp1$gam.pred <- predict(gam.model, newdata=sensp1)
  sensp1$lm.pred <- predict(lm.model, newdata=sensp1)
  sensp2$gam.pred2 <- predict(gam.model2k, newdata=sensp2)
  sensp2$lm.pred2 <- predict(lm.model2k, newdata=sensp2)
  SplitPred <- rbind(SplitPred,sensp1)
# p1 <- ggplot(data=sensp1, aes(x=gam.pred1, y=ggd))+
#   geom_point(alpha=0.2, color="black")+
#   geom_smooth(aes(x=gam.pred1, y=ggd), color="black")+
#   geom_line(aes(x=gam.pred1, y=gam.pred1), color="blue", linetype=2)+
#   ggtitle(paste("Pred over per 1--",levels(DATA$id)[j],"ggd"))
# p2 <- ggplot(data=sensp2, aes(x=gam.pred2, y=ggd))+
#   geom_point(alpha=0.2, color="black")+
#   geom_smooth(aes(x=gam.pred2, y=ggd), color="black")+
#   geom_line(aes(x=gam.pred2, y=gam.pred2), color="blue", linetype=2)+
#   ggtitle(paste("Pred over per 2--",levels(DATA$id)[j],"ggd"))


# terms <- predict(gam.model, type="terms")
# Sw <- Sw %>% filter(!is.na(ggd))
# tframe <- cbind(no2a = Sw1$no2a, no2b = Sw1$no2b, temp = Sw1$temp,
#                 y = Sw1$ggd, as.data.frame(terms))
# colnames(tframe) <- gsub('[()]', '', colnames(tframe))
# p3 <- plot(gam.model, pages=1, residuals=T, pch=19, cex=0.25,
#         scheme=1, col='#FF8000' , shade=T, shade.col='gray90',
#         main = paste("Splines",levels(DATA$id)[j],"ggd per 1"))
# p4 <- plot(gam.model2k, pages=1, residuals=T, pch=19, cex=0.25,
#         scheme=1, col='#FF8000' , shade=T, shade.col='gray90',
#         main = paste("Splines",levels(DATA$id)[j],"ggd per 2"))
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(p1, vp = vplayout(1, 1))
# print(p2, vp = vplayout(1, 2))
# print(p3, vp = vplayout(2, 1))
# print(p4, vp = vplayout(2, 2)) #pas op aangepast!
  RMSE[k,c(1,5,6)] <- c(round(sqrt(mean((sensp1$gam.pred - sensp1$ggd)^2,na.rm=TRUE)),2),"GAM",
                        levels(DATA$id)[j])
  RMSE[k+1,c(1,5,6)] <- c(round(sqrt(mean((sensp1$lm.pred - sensp1$ggd)^2,na.rm=TRUE)),2),"LM",
                          levels(DATA$id)[j])
  RMSE[k,c(2,5,6)] <- c(round(sqrt(mean((sensp2$gam.pred2 - sensp2$ggd)^2,na.rm=TRUE)),2),"GAM",
                        levels(DATA$id)[j])
  RMSE[k+1,c(2,5,6)] <- c(round(sqrt(mean((sensp2$lm.pred2 - sensp2$ggd)^2,na.rm=TRUE)),2),"LM",
                          levels(DATA$id)[j])
  RMSE[k,c(3,5,6)] <- c(round(sqrt(mean((sensp1$gam.pred - sensp1$ggd_os)^2,na.rm=TRUE)),2),"GAM",
                        levels(DATA$id)[j])
  RMSE[k+1,c(3,5,6)] <- c(round(sqrt(mean((sensp1$lm.pred - sensp1$ggd_os)^2,na.rm=TRUE)),2),"LM",
                          levels(DATA$id)[j])
  RMSE[k,c(4,5,6)] <- c(round(sqrt(mean((sensp2$gam.pred2 - sensp2$ggd_os)^2,na.rm=TRUE)),2),"GAM",
                        levels(DATA$id)[j])
  RMSE[k+1,c(4,5,6)] <- c(round(sqrt(mean((sensp2$lm.pred2 - sensp2$ggd_os)^2,na.rm=TRUE)),2),"LM",
                          levels(DATA$id)[j])
  RMSE[k,7] <- min(as.numeric(c(RMSE[k,1:4])))
  RMSE[k,8] <- min(as.numeric(c(RMSE[k+1,1:4])))
  RMSE[k+1,7] <- min(as.numeric(c(RMSE[k,1:4])))
  RMSE[k+1,8] <- min(as.numeric(c(RMSE[k+1,1:4])))
  k <- k+2
  }
Model.Fit <- data.frame(`Lineair periode 1`=round(RevModel,2),`Lineair periode 2`=round(RevModel2k,2),
                        `GAM periode 1`=round(GamModel,2), `GAM periode 2`=round(GamModel2k,2),
                        `Converged?`=Conv, `Converged? 2`=Conv2k,
                        Locatie=levels(TotalData$Adress))
colnames(Model.Fit) <- c("Lineair periode 1",
                         "Lineair periode 2",
                         "GAM periode 1",
                         "GAM periode 2",
                         "Converged?",
                         "Converged? 2",
                         "Locatie")
TotalData <- merge(TotalData,
                   SplitPred[,c("Date","Time","id","gam.pred","lm.pred")], by=c("Date","Time","id"),
                   all.x=TRUE)

colSel <- function(x){
  if(is.na(x)){return("blue")}
  else if(x>=40){return("red")}
  else if(x<40&&x>=30){return("yellow")}
  else if(x<30){return("green")}
}
TotalData$no2col <- sapply(TotalData$lm.pred,colSel)
TotalData$ggdcol <- sapply(TotalData$ggd,colSel)
TotalData$Adress <- as.factor(TotalData$Adress)
TotalData$lm.pred[TotalData$lm.pred<0] <- NA
TotalData$gam.pred[TotalData$gam.pred<0] <- NA
# write.csv(TotalData, "TotalData.csv",row.names=F)

save.image("C:/Users/Win7/Desktop/HVA TWK/JAAR 3/Minor/Project de waag/.RData")

# pairs.panels(SENS_1183931[,c(4,5,6,7,9,12,13,14)])
# pairs.panels(SENS_1183931[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_1184206[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_1184453[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_1184527[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_1184739[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_1184838[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_1185325[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_13905017[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_14560051[,c(4,5,6,7,12,13)])
# pairs.panels(SENS_26296[,c(4,5,6,7,12,13)])

#wat to do
# model valideren! splines RMSE etc
# Test.per1 <- TotalData %>% filter(Date>"2016-06-10")
# Test.per2 <- TotalData %>% filter(Date<"2016-08-18")
## Algemeen RMSE check over alle sensoren bij elkaar
# RMSEmod1 <- sqrt(mean((Test.per1$pred - Test.per1$ggd)^2,na.rm=TRUE))
# RMSElmmod1 <- sqrt(mean((Test.per1$lmpred - Test.per1$ggd)^2,na.rm=TRUE))
# RMSEmod2 <- sqrt(mean((Test.per2$pred2 - Test.per2$ggd)^2,na.rm=TRUE))
# RMSElmmod2 <- sqrt(mean((Test.per2$lmpred2 - Test.per2$ggd)^2,na.rm=TRUE))
# RMSEmod1OS <- sqrt(mean((Test.per1$pred - Test.per1$ggd_os)^2,na.rm=TRUE))
# RMSElmmod1OS <- sqrt(mean((Test.per1$lmpred - Test.per1$ggd_os)^2,na.rm=TRUE))
# RMSEmod2OS <- sqrt(mean((Test.per2$pred2 - Test.per2$ggd_os)^2,na.rm=TRUE))
# RMSElmmod2OS <- sqrt(mean((Test.per2$lmpred2 - Test.per2$ggd_os)^2,na.rm=TRUE))
# RMSEtot <- data.frame(RMSE=c(RMSEmod1,RMSElmmod1,RMSEmod2,RMSElmmod2,
#                           RMSEmod1OS,RMSElmmod1OS,RMSEmod2OS,RMSElmmod2OS),
#                    model=c("GAM","LM","GAM","LM","GAM","LM","GAM","LM"),
#                    periode=c("Periode 1","Periode 1","Periode 2","Periode 2",
#                              "Periode 1","Periode 1","Periode 2","Periode 2"),
#                    TOloc=c("Vondel","Vondel","Vondel","Vondel",
#                            "OudeSchans","OudeSchans","OudeSchans","OudeSchans"))
## Uitgesplitste RMSE check voor elke sensor appart twee periode 4 modellen

# save.image("C:/Users/Win7/Desktop/HVA TWK/JAAR 3/Minor/Project de waag/.RData")


# phase space reconstruction
# http://www.sciencedirect.com/science/article/pii/S0925400513002797
# 
# N.LMModel <- NULL
# N.LMModel2k <- NULL
# N.GamModel <- NULL
# N.GamModel2k <- NULL
# N.Conv <- NULL
# N.Conv2k <- NULL
# RMSEtr <-  matrix(nrow=2*length(levels(DATA$id)), ncol=8)
# colnames(RMSEtr) <- c("Vondel per1","Vondel per2",
#                     "OudeSchans per1","OudeSchans per2","Model",
#                     "Sensor","BestGAM","BestLM")
# m <- 1
# ## Experimentele code run at own risk
# for(j in 1:length(levels(DATA$id))){
#   obj1 <- eval(parse(text=paste("SENS_",levels(DATA$id)[j], sep="")))
#   obj1$no2a[is.na(obj1$no2a)] <- mean(obj1$no2a,na.rm=T)
#   obj1$no2b[is.na(obj1$no2b)] <- mean(obj1$no2b,na.rm=T)
#   
#   ts2a <- as.numeric(decompose(ts(obj1$no2a,
#                                   frequency = 200))$trend) #200
#   ts2b <- as.numeric(decompose(ts(obj1$no2b,
#                                   frequency = 200))$trend)
#   ts2a <- data.frame(TL=na.locf(ts2a),X=1:length(na.locf(ts2a)))
#   ts2b <- data.frame(TL=na.locf(ts2b),X=1:length(na.locf(ts2b)))
#   ts2a <- predict(loess(TL~X,data=ts2a,span=0.1))
#   ts2b <- predict(loess(TL~X,data=ts2b,span=0.1))
#   plot(xlab=levels(DATA$id)[j],decompose(ts(obj1$no2a, frequency = 200))$trend)
#   lines(ts2a,col="Red")
#   plot(xlab=levels(DATA$id)[j],decompose(ts(obj1$no2b, frequency = 200))$trend)
#   lines(ts2b,col="Blue")
#   obj1$Nno2a <- obj1$no2a-ts2a$y
#   obj1$Nno2b <- obj1$no2b-ts2b$y
#   obj1tr <- obj1 %>% filter(Date<="2016-06-10")
#   obj2tr <- obj1 %>% filter(Date>="2016-08-18")
#   obj1test <- obj1 %>% filter(Date>"2016-06-10")
#   obj2test <- obj1 %>% filter(Date<"2016-08-18")
# 
#   N.lm.model <- lm(ggd ~ Nno2a + Nno2b + temp, data=obj1tr)
#   N.gam.model <- gam(ggd ~ s(Nno2a) + s(Nno2b) + s(temp), data=obj1tr)
#   N.lm.model2 <- lm(ggd ~ Nno2a + Nno2b + temp, data=obj2tr)
#   N.gam.model2 <- gam(ggd ~ s(Nno2a) + s(Nno2b) + s(temp), data=obj2tr)
#   obj1test$GAMpred <- predict(N.gam.model, newdata=obj1test)
#   obj1test$LMpred <- predict(N.lm.model, newdata=obj1test)
#   obj2test$GAMpred <- predict(N.gam.model2, newdata=obj2test)
#   obj2test$LMpred <- predict(N.lm.model2, newdata=obj2test)
#   RMSEtr[m,c(1,5,6)] <- c(round(sqrt(mean((obj1test$GAMpred - obj1test$ggd)^2,na.rm=TRUE)),2),"GAM",
#                         levels(DATA$id)[j])
#   RMSEtr[m+1,c(1,5,6)] <- c(round(sqrt(mean((obj1test$LMpred - obj1test$ggd)^2,na.rm=TRUE)),2),"LM",
#                           levels(DATA$id)[j])
#   RMSEtr[m,c(2,5,6)] <- c(round(sqrt(mean((obj2test$GAMpred - obj2test$ggd)^2,na.rm=TRUE)),2),"GAM",
#                         levels(DATA$id)[j])
#   RMSEtr[m+1,c(2,5,6)] <- c(round(sqrt(mean((obj2test$LMpred - obj2test$ggd)^2,na.rm=TRUE)),2),"LM",
#                           levels(DATA$id)[j])
#   RMSEtr[m,c(3,5,6)] <- c(round(sqrt(mean((obj1test$GAMpred - obj1test$ggd_os)^2,na.rm=TRUE)),2),"GAM",
#                         levels(DATA$id)[j])
#   RMSEtr[m+1,c(3,5,6)] <- c(round(sqrt(mean((obj1test$LMpred - obj1test$ggd_os)^2,na.rm=TRUE)),2),"LM",
#                           levels(DATA$id)[j])
#   RMSEtr[m,c(4,5,6)] <- c(round(sqrt(mean((obj2test$GAMpred - obj2test$ggd_os)^2,na.rm=TRUE)),2),"GAM",
#                         levels(DATA$id)[j])
#   RMSEtr[m+1,c(4,5,6)] <- c(round(sqrt(mean((obj2test$LMpred - obj2test$ggd_os)^2,na.rm=TRUE)),2),"LM",
#                           levels(DATA$id)[j])
#   RMSEtr[m,7] <- min(as.numeric(c(RMSEtr[m,1:4])))
#   RMSEtr[m,8] <- min(as.numeric(c(RMSEtr[m+1,1:4])))
#   RMSEtr[m+1,7] <- min(as.numeric(c(RMSEtr[m,1:4])))
#   RMSEtr[m+1,8] <- min(as.numeric(c(RMSEtr[m+1,1:4])))
#   m <- m+2
#   N.LMModel[j] <- summary(N.lm.model)$r.squared
#   N.LMModel2k[j] <- summary(N.lm.model2)$r.squared
#   N.GamModel[j] <- summary(N.gam.model)$r.sq
#   N.GamModel2k[j] <- summary(N.gam.model2)$r.sq
#   N.Conv[j] <- N.gam.model$converged
#   N.Conv2k[j] <- N.gam.model2$converged
#   
#   # p1 <- ggplot(data=obj1test, aes(x=gam.pred1, y=ggd))+
#   #   geom_point(alpha=0.2, color="black")+
#   #   geom_smooth(aes(x=gam.pred1, y=ggd), color="black")+
#   #   geom_line(aes(x=gam.pred1, y=gam.pred1), color="blue", linetype=2)+
#   #   ggtitle(paste("Pred over per 1--",levels(DATA$id)[j],"ggd"))
#   # p2 <- ggplot(data=obj2test, aes(x=gam.pred2, y=ggd))+
#   #   geom_point(alpha=0.2, color="black")+
#   #   geom_smooth(aes(x=gam.pred2, y=ggd), color="black")+
#   #   geom_line(aes(x=gam.pred2, y=gam.pred2), color="blue", linetype=2)+
#   #   ggtitle(paste("Pred over per 2--",levels(DATA$id)[j],"ggd"))
# }
# N.Model.Fit <- data.frame(lm=round(N.LMModel,2),lm2=round(N.LMModel2k,2),
#                         gam=round(N.GamModel,2), gam2=round(N.GamModel2k,2),
#                         conv=N.Conv, conv=N.Conv2k,
#                         id=levels(DATA$id))




