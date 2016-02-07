# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Install library
list_of_packages <- c('lattice', 'xts', 'miscTools')
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages, repos="http://cran.rstudio.com/")
library(lattice)
library(xts)
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library(miscTools)

# Read File
unzip("activity.zip", overwrite = TRUE)
dataset_complete <- read.csv(
    "activity.csv", 
    na.strings = "NA", 
    stringsAsFactors=FALSE, 
    header = TRUE
)

dataset_complete$date <- strptime(dataset_complete$date, "%Y-%m-%d")
dataset_complete$dateFactor <- cut(dataset_complete$date, breaks="days")
```

## What is mean total number of steps taken per day?

```r
# Ignore NA and 0 Steps
dataset <- dataset_complete[!is.na(dataset_complete$steps),]
dataset <- dataset[!dataset$steps == 0,]

# Draw Histogram
histogram(
    dataset$steps ~ dataset$dateFactor, 
    xlab='Days', 
    ylab='Steps', 
    main="Total steps taken per day"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Prepare Steps Time Series
steps.xts <- xts(dataset$steps, dataset$date)

# Steps Mean
steps_means <- apply.daily(steps.xts, colMeans)
plot(steps_means, main="Mean of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
steps_means
```

```
##                 [,1]
## 2012-10-02  63.00000
## 2012-10-03 140.14815
## 2012-10-04 121.16000
## 2012-10-05 154.58140
## 2012-10-06 145.47170
## 2012-10-07 101.99074
## 2012-10-09 134.85263
## 2012-10-10  95.19231
## 2012-10-11 137.38667
## 2012-10-12 156.59459
## 2012-10-13 119.48077
## 2012-10-14 160.61702
## 2012-10-15 131.67532
## 2012-10-16 157.12500
## 2012-10-17 152.86364
## 2012-10-18 152.36364
## 2012-10-19 127.19355
## 2012-10-20 125.24096
## 2012-10-21  96.93407
## 2012-10-22 154.71264
## 2012-10-23 101.34091
## 2012-10-24 104.43750
## 2012-10-25  56.63636
## 2012-10-26  77.02273
## 2012-10-27 134.92000
## 2012-10-28 110.17308
## 2012-10-29  80.93548
## 2012-10-30 110.32584
## 2012-10-31 179.23256
## 2012-11-02 143.24324
## 2012-11-03 117.45556
## 2012-11-05 141.06757
## 2012-11-06 100.40964
## 2012-11-07 135.61053
## 2012-11-08  61.90385
## 2012-11-11 132.71579
## 2012-11-12 156.01449
## 2012-11-13  90.56790
## 2012-11-15  20.50000
## 2012-11-16  89.19672
## 2012-11-17 183.83333
## 2012-11-18 162.47312
## 2012-11-19 117.88000
## 2012-11-20  95.14894
## 2012-11-21 188.04412
## 2012-11-22 177.62609
## 2012-11-23 252.30952
## 2012-11-24 176.56098
## 2012-11-25 140.88095
## 2012-11-26 128.29885
## 2012-11-27 158.67442
## 2012-11-28 212.14583
## 2012-11-29 110.10938
```

```r
# Steps Median
steps_medians <- apply.daily(steps.xts, colMedians)
plot(steps_medians, main="Median of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
steps_medians
```

```
##             [,1]
## 2012-10-02  63.0
## 2012-10-03  61.0
## 2012-10-04  56.5
## 2012-10-05  66.0
## 2012-10-06  67.0
## 2012-10-07  52.5
## 2012-10-09  48.0
## 2012-10-10  56.5
## 2012-10-11  35.0
## 2012-10-12  46.0
## 2012-10-13  45.5
## 2012-10-14  60.5
## 2012-10-15  54.0
## 2012-10-16  64.0
## 2012-10-17  61.5
## 2012-10-18  52.5
## 2012-10-19  74.0
## 2012-10-20  49.0
## 2012-10-21  48.0
## 2012-10-22  52.0
## 2012-10-23  56.0
## 2012-10-24  51.5
## 2012-10-25  35.0
## 2012-10-26  36.5
## 2012-10-27  72.0
## 2012-10-28  61.0
## 2012-10-29  54.5
## 2012-10-30  40.0
## 2012-10-31  83.5
## 2012-11-02  55.5
## 2012-11-03  59.0
## 2012-11-05  66.0
## 2012-11-06  52.0
## 2012-11-07  58.0
## 2012-11-08  42.5
## 2012-11-11  55.0
## 2012-11-12  42.0
## 2012-11-13  57.0
## 2012-11-15  20.5
## 2012-11-16  43.0
## 2012-11-17  65.5
## 2012-11-18  80.0
## 2012-11-19  34.0
## 2012-11-20  58.0
## 2012-11-21  55.0
## 2012-11-22  65.0
## 2012-11-23 113.0
## 2012-11-24  65.5
## 2012-11-25  84.0
## 2012-11-26  53.0
## 2012-11-27  57.0
## 2012-11-28  70.0
## 2012-11-29  44.5
```

## What is the average daily activity pattern?


```r
mean_steps_across_intervals <- aggregate(
    dataset$steps ~ dataset$interval, 
    dataset, 
    mean
)

names(mean_steps_across_intervals) <- c("interval","steps")

plot(
    mean_steps_across_intervals$interval, 
    mean_steps_across_intervals$steps,
    type="l",
    main="Average number of steps taken by daily interval",
    xlab='Interval',
    ylab='Avg Steps'
)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_steps_by_interval <- mean_steps_across_intervals[
    mean_steps_across_intervals$steps == max(mean_steps_across_intervals$steps)
    ,]

out <- paste(
    "Interval ", max_steps_by_interval$interval,
    " contains the maximum average"," of steps accros all the days. ",
    sep=""
)

print(out)
```

```
## [1] "Interval 835 contains the maximum average of steps accros all the days. "
```

## Imputing missing values

```r
dataset_na <- dataset_complete[is.na(dataset_complete$steps),]
out <- paste("There are ", nrow(dataset_na), "rows with no data", sep="")
print(out)
```

```
## [1] "There are 2304rows with no data"
```

## Are there differences in activity patterns between weekdays and weekends?

```r
##df1$date <- as.Date(df1$date)
#create a vector of weekdays
##weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
##df1$wDay <- factor((weekdays(df1$date) %in% weekdays1), 
##         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday') 
#Or
##df1$wDay <- c('weekend', 'weekday')[(weekdays(df1$date) %in% weekdays1)+1L]
```

