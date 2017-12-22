#initializing all the libraries which might be required
library("ggplot2")
library("lubridate")
library(dplyr)
library(tidyr)
library(chron)

#loading the uber data set into a data frame
uberReqData<-read.csv("Uber Request Data.csv",stringsAsFactors = F)

#viewing the data frame to find any  quality issues
View(uberReqData)

#checking for duplicate Request id
uberReqData[which(duplicated(uberReqData$Request.id)),]
#no duplicates exist for request Id


#data cleaning
##The date and time are in an inconsistent format, using lubridate library to parse into common R format

uberReqData$Request.timestamp <- parse_date_time(uberReqData$Request.timestamp,
                                                     orders = c("%d-%m-%y %H:%M:%S","%d-%m-%y %H:%M"),
                                                     locale = "eng")
uberReqData$Drop.timestamp <- parse_date_time(uberReqData$Drop.timestamp,
                                                  orders=c("%d-%m-%y %H:%M:%S","%d-%m-%y %H:%M"),
                                                  locale = "eng")

#deriving Hours from the Request timestamp Column
Hours <- format(as.POSIXct(strptime(uberReqData$Request.timestamp,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

#derived metrics

#deriving date from request timestamp column for request Date column
#and adding both Hours and date column to Data frame
date <- (as.Date(uberReqData$Request.timestamp ))
uberReqData<-cbind(uberReqData,date)

uberReqData<-cbind(uberReqData,Hours)

#converting the Hours column to R time format for diving the day into segments
uberReqData$Hours<-chron(times=uberReqData$Hours)

#sorting teh data frame in ascending order of Request Id
uberReqData<-  uberReqData[order(uberReqData$Request.timestamp),]


#dividing the day into different Segments based on pickup time
uberReqData$Segment[uberReqData$Hours>=chron(times="00:00:00") & uberReqData$Hours<chron(times="03:00:00")]<-"Late Night"
uberReqData$Segment[uberReqData$Hours>=chron(times="03:00:00") & uberReqData$Hours<chron(times="07:00:00")]<-"Early Morning"
uberReqData$Segment[uberReqData$Hours>=chron(times="07:00:00") & uberReqData$Hours<chron(times="12:00:00")]<-"Morning"
uberReqData$Segment[uberReqData$Hours>=chron(times="12:00:00") & uberReqData$Hours<chron(times="16:00:00")]<-"Afternoon"
uberReqData$Segment[uberReqData$Hours>=chron(times="16:00:00") & uberReqData$Hours<chron(times="20:00:00")]<-"Evening"
uberReqData$Segment[uberReqData$Hours>=chron(times="20:00:00") & uberReqData$Hours<chron(times="23:59:59")]<-"Night"

#checking for NA values after the conversion
uberReqData[which(is.na(uberReqData$Segment)),]
uberReqData[which(is.na(uberReqData$Hours)),]

#Viewing the data frame after adding the segments
View(uberReqData)

#converting Hours into absolute hours i.e 00 , 01 for analysis by hour
uberReqData$Hours <- format(as.POSIXct(strptime(uberReqData$Hours,"%H:%M:%S",tz="")) ,format = "%H")


typeof(uberReqData$Hours)
View(uberReqData)

#univariate analysis
#plotting the frequency of cancelled, trip completed and No cars available by date and pickup point
ggplot(uberReqData,aes(x=factor(Status)))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1)+facet_wrap(~date)
ggplot(uberReqData,aes(x=factor(Pickup.point),fill=factor(Status)))+geom_bar()+facet_wrap(~date)

#segmented univariate
#plotting the number of trips with different statuses by segment and hour for both pickup points
ggplot(uberReqData,aes(x=factor(Status),fill=factor(Segment)))+geom_bar()+facet_wrap(~Pickup.point)
ggplot(uberReqData,aes(x=factor(Hours),fill=factor(Status)))+geom_bar()+facet_wrap(~Pickup.point)


#bivariate
#plotting the total nummber of requests by Status for all time segments to do demand supply analysis. 
ggplot(uberReqData,aes(x=factor(Status),fill=factor(Pickup.point)))+geom_bar()+facet_wrap(~Segment)

#plotting the requests bt Status by Hours and segment for both pickup points

ggplot(uberReqData,aes(x=factor(Status),fill=factor(Hours)))+geom_bar() + facet_wrap(~Pickup.point)
ggplot(uberReqData,aes(x=factor(Segment),fill=factor(Status)))+geom_bar() +facet_wrap(~Pickup.point)

#created a new data frame for supply demand analysis with an additional column "Supply "
#Supply is 1 when the Status is Trip Completed and 0 when Status is Cancelled or No Cabs available
UberSupplyDemandGAp<-uberReqData
View(UberSupplyDemandGAp)
UberSupplyDemandGAp$Supply[UberSupplyDemandGAp$Status=="Trip Completed"]<-1
UberSupplyDemandGAp$Supply[UberSupplyDemandGAp$Status=="Cancelled"|UberSupplyDemandGAp$Status=="No Cars Available"]<-0

#plotting the frequency of Supply based on hours and segments for both pickup points
ggplot(UberSupplyDemandGAp,aes(x=factor(Hours),fill=factor(Supply)))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1)+facet_wrap(~Pickup.point)
ggplot(UberSupplyDemandGAp,aes(x=factor(Segment),fill=factor(Supply)))+geom_bar()+facet_wrap(~Pickup.point)

UberSupplyDemandGAp$Hours<-as.numeric(UberSupplyDemandGAp$Hours)
airportpeakDemand<-nrow(UberSupplyDemandGAp[which(UberSupplyDemandGAp$Hours>=17 & UberSupplyDemandGAp$Hours<=21 &UberSupplyDemandGAp$Pickup.point=="Airport"),])
airportsupply<-nrow(UberSupplyDemandGAp[which(UberSupplyDemandGAp$Hours>=17 & UberSupplyDemandGAp$Hours<=21&UberSupplyDemandGAp$Pickup.point=="Airport"&UberSupplyDemandGAp$Status=="Trip Completed"),])

airportSupplyDemandgap<- airportpeakDemand-airportsupply

#similarly we can calculate the supply demand gap for city s
#we have the labels on the plot from which we can calculate the numerical value of supply
#demand gap which is done in the ppt and also in R code
