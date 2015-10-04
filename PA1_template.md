# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data<-read.csv("activity.csv")

clean_data<-data[!is.na(data$steps),c("steps","date","interval")]
```


## What is mean total number of steps taken per day?


```r
steps<-tapply(clean_data$steps,clean_data$date,sum)
hist(steps,col="blue", breaks = 61, main="Number of steps per day",xlab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
options(scipen=999) #disabling scientific notation

Mean<-round(mean(steps,na.rm=TRUE),digits=2)
Mean
```

```
## [1] 10766.19
```

```r
Median<-round(median(steps,na.rm=TRUE),digits=2)
Median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
intervalsteps<-tapply(clean_data$steps,clean_data$interval,mean)
intervals<-unique(clean_data$interval)

plot(intervals, intervalsteps, type="l", col="blue", xlab="Interval", ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The interval with maximum number of average steps:


```r
names(which.max(intervalsteps))
```

```
## [1] "835"
```

## Imputing missing values

Creating new data set with missing data for steps filled with the mean of interval.


```r
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

fdata <- ddply(data, ~ interval, transform, steps = impute.mean(steps))
```

Recalculating mean and median number of steps per day.


```r
fsteps<-tapply(fdata$steps,fdata$date,sum)
hist(fsteps,col="green", breaks = 61, main="Number of steps per day (NA's replaced by interval mean)",xlab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
NewMean<-round(mean(fsteps,na.rm=TRUE),digits=2)
NewMean
```

```
## [1] 10766.19
```

```r
NewMedian<-round(median(fsteps,na.rm=TRUE),digits=2)
NewMedian
```

```
## [1] 10766.19
```

The mean values of daily steps for new (10766.19) and old (10766.19) data sets **do not differ**.

Total number of steps **before** Imputing missing values:

```r
sum(data$steps,na.rm=TRUE)
```

```
## [1] 570608
```
Total number of steps **after** Imputing missing values:

```r
sum(fdata$steps,na.rm=TRUE)
```

```
## [1] 656737.5
```
The Imputing of missing values **increased** the total number of daily steps.

## Are there differences in activity patterns between weekdays and weekends?

```r
fdata$TimeOfWeek<-ifelse((weekdays(as.Date(fdata$date))=="Saturday"|weekdays(as.Date(fdata$date))=="Sunday"),"weekend", "weekday")

df1<-fdata[fdata$TimeOfWeek=="weekday",]
df2<-fdata[fdata$TimeOfWeek=="weekend",]

stepsdf<-tapply(df1$steps,df1$interval,mean)
intervals<-unique(df1$interval)
weekday<-rep("weekday",288)
df11<-data.frame(stepsdf,intervals,weekday)

stepsdf<-tapply(df2$steps,df2$interval,mean)
weekday<-rep("weekend",288)
df22<-data.frame(stepsdf,intervals,weekday)

dd<-rbind(df11,df22)

library(lattice)
xyplot(stepsdf~intervals | factor(weekday), data=dd,layout=c(1,2),type="l",xlab="Interval",ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

There is a difference in activity pattern between weekdays and weekends. During the weekends people are more active.  
