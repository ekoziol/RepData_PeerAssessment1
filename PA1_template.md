# Reproducible Research: Peer Assessment 1
This document will explore relationships within walking data from an anonymous individual whom had their steps tracked in 5 minute intervals for a period of 61 days.

## Loading and preprocessing the data
In order to assess the data we first need to load the data into R.  The data has already been unzipped into the working directory and is named 'activity.csv'.  The data will be loaded into a variable called 'data'.


```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
In order to get the total number of steps per day, we first need to aggregate the data by date.

```r
stepsPerDay <- aggregate(data$steps,by=list((data$date)),sum)
names(stepsPerDay) <- c("day", "totalSteps")
```

Next, let us plot a histogram of the Total steps taken per day.  We will also produce the mean and median for the Total steps per day


```r
hist(stepsPerDay$totalSteps, col = "green", main = "Histogram of Total Steps Taken per Day", xlab = "Total steps per day", ylab = "Frequency of Total Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
median(stepsPerDay$totalSteps, na.rm=TRUE)
```

```
## [1] 10765
```

```r
mean(stepsPerDay$totalSteps, na.rm=TRUE)
```

```
## [1] 10766
```

## What is the average daily activity pattern?
In order to assess how many steps on average are taken during a given interval, we need to aggregate the data by interval.  


```r
stepsPerInterval <- aggregate(data$steps,by=list((data$interval)),mean, na.rm=TRUE)
names(stepsPerInterval) <- c("interval", "averageSteps")
plot(stepsPerInterval, type="l", main = "Average Steps Per Interval", xlab="Interval", ylab="Average Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Now we can identify the interval that has the largest number of average steps as well as the average number of steps for that interval.  This is given by:


```r
max(stepsPerInterval$averageSteps)
```

```
## [1] 206.2
```

```r
largestStepInterval <- stepsPerInterval[stepsPerInterval$averageSteps == max(stepsPerInterval$averageSteps),1]
largestStepInterval
```

```
## [1] 835
```
Thus the largest interval is at 835.  This likely takes place while the subject is going into work.

## Imputing missing values
First, let us determine the number of missing values (NA) in the data set:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

This amounts to the following percentage of all data:

```r
sum(is.na(data$steps))/nrow(data)*100
```

```
## [1] 13.11
```

#### Strategy for imputating NA values
With that proportion of data missing, we cannot just fill the na values in as zeros.  Therefore, it would be best to fill the data with the mean value for that interval.  We should fill by interval, since that is the level that the data is missing on.  We do not want to skew the data since we are not sure what the data actually is.  Let us create this 'fixed' data set.


```r
dataFixed <- data
dataFixed[is.na(dataFixed$steps),]$steps <- stepsPerInterval[match(dataFixed[is.na(dataFixed$steps),]$interval, stepsPerInterval$interval), ]$averageSteps
```

We now how to make a histogram of our new data:


```r
stepsPerDayFixed <- aggregate(dataFixed$steps,by=list((dataFixed$date)),sum)
names(stepsPerDayFixed) <- c("day", "totalSteps")
hist(stepsPerDayFixed$totalSteps, col = "purple", xlab = "Total steps per day", ylab = "Frequency of days for total steps", main = "Histogram of Total Steps per day with NA replacement" )
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
median(stepsPerDayFixed$totalSteps)
```

```
## [1] 10766
```

```r
mean(stepsPerDayFixed$totalSteps)
```

```
## [1] 10766
```

You can see that the median is now equal to the mean, while before adjusting for NA values the mean and median differed slightly. However, the mean remained the same from when we originally calculated it, not accounting for NA values.  The mean did not change because we set all the NA values equal to the mean for a given interval.  Thus, the NA values have no real impacting on adjusting the value of the calculated mean. As you can see, the frequency for the largest bar has increased dramatically in the histogram.  This is attributed to us changing all NA values to the mean for a given interval.

## Are there differences in activity patterns between weekdays and weekends?
We first need to create a separate column in the data that tells us whether or not a given observation is on a weekday or weekend.

```r
dataFixed$dayOfWeek <- weekdays(as.Date(dataFixed$date))
dataFixed$weekend <- dataFixed$dayOfWeek == "Sunday" | dataFixed$dayOfWeek == "Saturday"
dataFixed[dataFixed$weekend == TRUE, 'weekend'] <- "weekend"
dataFixed[dataFixed$weekend == FALSE, 'weekend'] <- "weekday"
```

Next we need to recreate the steps per interval dataframe that we made earlier except this time we will use the fixed data set.


```r
stepsPerIntervalFixed <- aggregate(dataFixed$steps,by=list((dataFixed$interval), (dataFixed$weekend)),mean, na.rm=TRUE)
names(stepsPerIntervalFixed) <- c("interval", "weekend", "averageSteps")
```


Finally we can plot the average number of steps taken per interval which is aggregated between weekdays and weekends. We will use the lattice library to accomplish this plot.

```r
library("lattice")
xyplot(stepsPerIntervalFixed$averageSteps ~ stepsPerIntervalFixed$interval | stepsPerIntervalFixed$weekend, data, type="l", layout = c(1,2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

