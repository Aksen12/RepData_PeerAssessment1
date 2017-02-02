# Reproducible Research: Peer Assessment 1
Achintya Sen  

##1. Loading and preprocessing the data  
  

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
### a) Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(steps~date,data = activity_data,sum,na.rm=TRUE)
```
### b) If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.  

```r
hist(steps_per_day$steps,main = "Histogram of the total number of steps taken each day",xlab = "Steps",breaks = 15)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
  
### c) Calculate and report the mean and median of the total number of steps taken per day.  

##3. What is the average daily activity pattern?

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
