---
output: html_document
---

---
title: "PA1_template1"
author: "wenlarry"
date: "September 18, 2016"
output:

Loading and Processing the data

Unzip 'activity_zip in working directory
Library: dplyr, ggplot2


```{r setup,cache=F} 
knitr::opts_chunk$set(error=TRUE) 

```
```{r,load_process data} 
data<-read.csv("activity.csv")
dat1<-na.omit(data) 
as.numeric(dat1$month) 
View(dat1) 

```
Total number of steps taken each day

```{r, total steps} 
library(dplyr) 
Totalsteps<-dat1    %>% group_by (date)%>%summarize(totalsteps=sum(steps,na.rm=TRUE))
                                               View(Totalsteps)
                                              
```                                             
```{r, totalsteps hist, echo=FALSE} 
library(ggplot2)
ggplot(Totalsteps,aes(totalsteps))+geom_histogram(binwidth=1000,fill="red")

```
Mean and Median steps each day
```{r,mean,median}
mean(Totalsteps$totalsteps) 
median(Totalsteps$totalsteps) 

```
Time Series Plot _Average number of days averaged across all days
```{r,time series data}
library(dplyr) 
Avsteps<-dat1  %>%group_by(interval)%>% summarize(avsteps=
mean(steps,na.rm=TRUE)) 
View(Avsteps) 

```
```{r,time series plot, echo=FALSE} 
library(ggplot2)
ggplot(Avsteps,aes(interval,avsteps))+geom_line(color="orange")+labs(title="Time series at 5 min interval") 

```
5 min interval that on average contains the max number os steps is 835 with average steps of 206.2

This result can also be obtained from the above dataframe 'Avsteps'

```{r, max number of steps}
Avsteps[Avsteps$avsteps==max(Avsteps$avsteps),] 

```
Imputing missing values

NAs Total

```{r, NAs total}
sum(is.na(data)) 

```
Use the mean of 5 minute interval for imputation

Create a new dataset that is equal to the original dataset but with missing values substituted

```{r,NAs substitute}
library(dplyr) 
dat2<-mutate(data,steps=ifelse(is.na(steps),as.integer
(Avsteps$avsteps),steps)) 
View(dat2) 

```
Total number of steps taken each day
```{r, total number of steps data} 
library(dplyr) 
Totalsteps2<-dat2%>%group_by(date)%>%summarize(totalsteps2=sum(steps,na.rm=TRUE) )  
View(Totalsteps2) 

```
```{r,totalsteps2hist, echo=FALSE} 
library(ggplot2) 
ggplot(Totalsteps2,aes(totalsteps2))+geom_histogram(binwidth=1000,fill="purple") 

```
Mean and Madian of steps each day -Without Imputed Values vs With Imputed Values

Impact from imputing missing valueson the estimates of the total daily number of steps is in a mean difference of -16.1 and a median difference of 124.0 where the impuuted mean and median are lower

```{r, mean and median comparisons}
mean1<-mean(Totalsteps$totalsteps)
mean2<-mean(Totalsteps2$totalsteps2) 
mean2-mean1

median1<-median(Totalsteps$totalsteps)
median2<-median(Totalsteps2$totalsteps2) 
median2-median1 

```
Diffference in activities between weekdays and weekends

Create a new factor variable with 2 levels -'weekday' and 'weekend'

```{r, weekday,weekend}
dat2$date<-as.Date(dat2$date)
weekdays1<-c("Monday","Tuesday","Wednesday","Thursday",
            "Friday")
dat2$wday<-c('weekend','weekday')[weekdays    (dat2$date)%in%weekdays1+1L] 

table(dat2$wday) 
View(dat2) 

```
Time Series Panel Plot - interval(x axis) and Average Steps(y axis)

```{r, panel plot data}
library(dplyr) 
Avsteps2<-dat2%>%group_by(interval,wday)%>% summarize(avsteps2=mean(steps,na.rm=TRUE)) 
View(Avsteps2) 

```
```{r, panel plot,echo=FALSE} 
library(ggplot2) 
ggplot(Avsteps2,aes(interval,avsteps2))+geom_line(color="green")+facet_wrap(~wday       ,nrow=2) 

```
Time Series Single Plot _ An overlay of the time series to provide a clearer distinction

```{r,single plot,echo=FALSE} 
library(ggplot2) 
ggplot (Avsteps2,aes(interval,avsteps2, color=wday))+geom_line() 

```






