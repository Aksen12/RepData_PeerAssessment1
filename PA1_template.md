# Reproducible Research: Peer Assessment 1
Achintya Sen  


###This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  
##1. Loading and preprocessing the data  
#####Import the data using the read.csv function into the R environment.  

```r
activity_data <-  read.csv("activity.csv")
head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
  
##2. What is mean total number of steps taken per day?  
###For this part of the assignment, you can ignore the missing values in the dataset.  
### a) Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(steps~date,data = activity_data,sum,na.rm=TRUE)
```
### b) If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.  

```r
hist(steps_per_day$steps,main = "Histogram of the total number of steps taken each day",xlab = "Steps",breaks = 15)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  
### c) Calculate and report the mean and median of the total number of steps taken per day.  

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
mean_median_steps_per_day <- group_by(activity_data, date) %>%
    summarise(mean = mean(steps, na.rm = TRUE),
              median = median(steps, na.rm = TRUE))
head(mean_median_steps_per_day)
```

```
## # A tibble: 6 × 3
##         date     mean median
##       <fctr>    <dbl>  <dbl>
## 1 2012-10-01      NaN     NA
## 2 2012-10-02  0.43750      0
## 3 2012-10-03 39.41667      0
## 4 2012-10-04 42.06944      0
## 5 2012-10-05 46.15972      0
## 6 2012-10-06 53.54167      0
```
##3. What is the average daily activity pattern?
###a)   Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval <- activity_data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
```
Ploting the graph through the base plotting system.

```r
plot(steps~interval,data= interval,type="l", xlab="5-minute interval",ylab = "Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
  
###b)   Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
interval[which.max(interval$steps),]
```

```
## # A tibble: 1 × 2
##   interval    steps
##      <int>    <dbl>
## 1      835 206.1698
```
  
## Imputing missing values  
####    Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  
###a)   Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).  

```r
sum(is.na(activity_data))
```

```
## [1] 2304
```
  
#####There are 2304 NA Values.  
###b)   We will be using average values of the interval steps taken in the whole data  
###c)   Create a new dataset that is equal to the original dataset but with the missing data filled in.  

```r
activity_data_imputed <- activity_data
nas <- is.na(activity_data_imputed$steps)
average_steps <- tapply(activity_data_imputed$steps,activity_data_imputed$interval,mean,na.rm=TRUE,simplify = TRUE)
activity_data_imputed$steps[nas] <- average_steps[as.character(activity_data_imputed$interval[nas])]
```
#####Check if there are any NA values in the dataset.  

```r
sum(is.na(activity_data_imputed))
```

```
## [1] 0
```
####There are 0 NA values.  
###d)   Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```r
stepsByDayImputed <- tapply(activity_data_imputed$steps, activity_data_imputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
# Calculate mean and median of daily steps
paste("Mean daily steps =", mean(activity_data_imputed$steps, na.rm=TRUE))
```

```
## [1] "Mean daily steps = 37.3825995807128"
```

```r
paste("Median daily steps =", median(activity_data_imputed$steps, na.rm=TRUE))
```

```
## [1] "Median daily steps = 0"
```

## Are there differences in activity patterns between weekdays and weekends?

```r
activity_data_imputed$dateType <-  ifelse(as.POSIXlt(activity_data_imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activity_data_imputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
