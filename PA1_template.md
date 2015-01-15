# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

A histogram of the total number of steps taken each day


```r
stepsPerDay <- aggregate(steps ~ date, data, sum)$steps

hist(stepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean and median total number of steps taken per day


```r
mean(stepsPerDay)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

A time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsPerInterval <- aggregate(steps ~ interval, data, mean)
plot(stepsPerInterval$steps, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxStepsInterval = which.max(stepsPerInterval$steps)
stepsPerInterval$interval[maxStepsInterval]
```

```
## [1] 835
```


## Imputing missing values

Total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Fill in all of the missing values in the dataset using the mean for that 5-minute interval. To do this I used a for loop to iterate over the data frame and replace each NA with the mean data for that interval.


```r
dataImpute <- data
for (i in 1:nrow(dataImpute)) {
    if (is.na(dataImpute$steps[i])) {
        dataImpute$steps[i] <- stepsPerInterval$steps[stepsPerInterval$interval == dataImpute$interval[i]]
    }
}
```

Histogram of the total number of steps taken each day using data frame with imputed values for NAs


```r
stepsPerDayImpute <- aggregate(steps ~ date, dataImpute, sum)
hist(stepsPerDayImpute$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

Mean and median total number of steps taken per day using data frame with imputed values for NAs


```r
mean(stepsPerDayImpute$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDayImpute$steps)
```

```
## [1] 10766.19
```
These values differ from the estimates from the first part of the assignment. The mean and median are now the same. The impact of imputing missing data on the estimates of the total daily number of steps is small. We would expect that since we are filling the the missing values with mean data which would move the median closer to the mean.

## Are there differences in activity patterns between weekdays and weekends?

Add new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day


```r
dataImpute$day = ifelse(as.POSIXlt(dataImpute$date)$wday %in% 
    c(1:5), "weekday", "weekend")
```

Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
stepsPerInterval2 = aggregate(steps ~ interval + day, dataImpute, mean)

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0)) 

with(stepsPerInterval2, {
    plot(steps[day == "weekend"], main = "weekend", t = "l")
    plot(steps[day == "weekday"], main = "weekday", t = "l")
})
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
