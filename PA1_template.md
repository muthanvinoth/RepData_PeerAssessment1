# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data


```r
library(ggplot2)
activity<-read.csv('activity.csv')
data<-na.omit(activity)
```




## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
px0<-aggregate(data$steps~data$date,data,sum)
colnames(px0)<- c("day","count")
```

2. Histogram of the total number of steps taken each day

```r
ggplot(data=px0, aes(px0$count)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(px0$count)
```

```
## [1] 10766.19
```

```r
median(px0$count)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
interval <- aggregate(steps ~ interval, data=data, FUN=mean)
plot(interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval$interval[which.max(interval$steps)]
```

```
## [1] 835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

Populate missing values with mean data.

3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
tdata<- activity
data <-na.omit(activity)

px0<-aggregate(data$steps~data$date,data,mean)
colnames(px0)<- c("date","count")
tdata<-merge(tdata, px0, by='date')
tdata[is.na(tdata[,1]), 1] <- tdata[is.na(tdata[,1]), 4]
px0<-aggregate(tdata$steps~tdata$date,tdata,sum)
colnames(px0)<- c("date","count")
```

4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
ggplot(data=px0, aes(px0$count)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(px0$count)
```

```
## [1] 10766.19
```

```r
median(px0$count)
```

```
## [1] 10765
```

The impact of the missing data seems rather low, at least when
estimating the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval,
                            data=activity,
                            subset=activity$daytype==type,
                            FUN=mean)
    plot(steps.type, type="l", main=type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
