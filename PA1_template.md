---
title: "Reproducible Research_Peer Assignment 1"
author: "Hin-Weng Wan"
date: "September 16, 2016"
output: html_document
---
Loading and Processing Data

Unzip 'activity.zip in working directory
Library:dplyr, ggplot2in RStudio

```{r setup}
data<-read.csv("activity.csv")
dat1<-na.omit(data) 
as.numeric(dat1$month) 
View(dat1) 

```
Total number of steps each day _histogram

```{r, total steps_histogram}
totalsteps<-with(dat1,aggregate(steps,list(date=date),sum))
totalsteps<-rename (totalsteps,Total_Steps=x) 
View(totalsteps) 

ggplot(totalsteps,aes(x=Total_Steps ))+geom_histogram(binwidth=1000,fill="red") 

```
Mean and Median of steps each day

```{r, mean, median}
mean(totalsteps $Total_Steps) 
median(totalsteps $Total_Steps) 

```
Time Series Plot _ Average number of steps averaged across all days

```{r, time series plot} 
avsteps <- with(dat1,aggregate(steps,list(interval=interval),mean)) 
avsteps<-rename(avsteps,Average_Steps=x,Interval=interval )
View(avsteps) 

ggplot (avsteps,aes(Interval,Average_Steps))+geom_line(color="orange", size=0.8) + labs(title ="Time Series_5 min interval") 

``` 
5 minute interval that on average contains the max number of steps is 835 with average steps of 206.2 

You can also get the answer from the dataframe generated with the RCode 'View avsteps' above. 

```{r, max number of steps} 
avsteps[avsteps$Average_Steps==max(avsteps$Average_Steps),]

```
Imputing missing values

Total of NAs

```{r, total NAs}
sum(is.na(data) )

```
Use the mean of the 5 minute interval for imputation

Create a new dataset that is equal to the original data data set but with missing values substituted

```{r, NAs substitute} 
dat2<-mutate (data,steps=ifelse(is.na(steps),as.integer
(avsteps$Average_Steps)  ,steps )) 
View(dat2)    

```
Total number of steps taken each day _ histogram

```{r, total steps with imputed values_histogram}
totalsteps2<-with(dat2,aggregate(steps,list(date=date),sum)) 
totalsteps2<-rename (totalsteps2,Total_Steps=x) 
View(totalsteps2) 

ggplot (totalsteps2,aes(x=Total_Steps))+geom_histogram(binwidth=1000,fill="purple") 

```
Display the 2 histograms in one plot i.e the top histogram has imputed values and the bottom histogram has no imputed values

```{r, comparison of 2 histograms} 
Joint<-full_join (totalsteps2,totalsteps,by="date")  
Joint<-rename (Joint,Total_Imputed_Steps=Total_Steps.x)  
View(Joint) 

ggplot (Joint,aes(x=Total_Imputed_Steps,fill=Total_Imputed_Steps))+geom_histogram(binwidth=100000)+scale_size_identity()+facet_wrap(~Total_Imputed_Steps ,nrow=2,"free_x")  
 
```
Mean and Median of steps each day based on imputed values
```{r, mean, median of imputed values} 
mean(totalsteps2$Total_Steps) 
median(totalsteps2$Total_Steps) 

```
Mean and Median of steps each day _ Without Imputed Values vs With Imputed Values

Impact from imputing missing values on the estimates of the total daily number of steps is in a mean difference of -16.4and a median difference of -124, where both the imputed mean and median are lower 

```{r, mean and median comparisons}
mean1<-mean(totalsteps$Total_Steps) 
mean2<-mean(totalsteps2$Total_Steps) 
mean2-mean1

median1<-median(totalsteps$Total_Steps) 
median2<-median(totalsteps2$Total_Steps) 
median2-median1

```
Differences in activities between weekdays and weekends

Create a new factor variable with 2 levels -'weekday' and 
'weekend'

```{r.weekday,weekend}
dat2$date<-as.Date(dat2$date) 
weekdays1<-c("Monday","Tuesday","Wednesday","Thursday",
             "Friday")
dat2$wday<-c('weekend','weekday')[weekdays(dat2$date)%in%
            weekdays1+1L] 

table(dat2$wday) 
View(dat2) 

```
Time Series Panel Plot - interval(x axis) and Average Steps(y axis)

```{r, panel plot}  
avsteps2<- with(dat2,aggregate(steps,list(interval=interval,weekdays=dat2$wday),mean) )  
avsteps2<-rename (avsteps2,Average_Steps=x,Interval=interval)
View(avsteps2) 

ggplot (avsteps2,aes(Interval,Average_Steps))+geom_line(color="steelblue")+facet_wrap(~weekdays,nrow=2) 

```
Time Series Single Plot -interval (x axis) and Average Steps (y axis). This overlay of the time series provides a clearer distinction

```{r, single plot}
ggplot (avsteps2,aes(Interval,Average_Steps, 
color=weekdays))+geom_line () 

```