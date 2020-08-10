Loading and preprocessing the data
==================================

You can load the data with read.cvs function

    knitr::opts_chunk$set(echo = TRUE)
    data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")

And look the decriptive aspects of data with summary(), str() and head()

    summary(data)

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

    str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    head(data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

Process/transform the data into a format suitable for analysis - cleaning data analysis:
========================================================================================

Converting the “date” variable to Date classe and the “interval”
variable to a factor

    data$date <-as.Date(data$date, format = "%Y-%m-%d")
    data$interval <-factor(data$interval)

Question 01: What is mean total number of steps taken per day?
==============================================================

Make a histogram of the total number of steps taken each day

    steps_by_day<-aggregate(steps ~ date, data, sum)
    hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="thistle",xlab="Number of Steps", lwd=1)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)
Calculate and report the mean and median of the total number of steps
taken per day

    rmean <- mean(steps_by_day$steps)
    rmean

    ## [1] 10766.19

    rmedian <- median(steps_by_day$steps)
    rmedian

    ## [1] 10765

Question 02: What is the average daily activity pattern?
========================================================

Steps: 1.Calculating the average

    steps_by_interval <- aggregate(steps ~ interval, data, mean)

2.Ploting the average daily activity pattern

    plot(steps_by_interval$interval,steps_by_interval$steps, type="1", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png) 3.
The 5-minute interval that contains the maximum number of steps

    max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
    max_interval

    ## [1] 835
    ## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 125 ... 2355

Question 03: Imputing missing values
====================================

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
-----------------------------------------------------------------------------------------------------------------

    NATotal <- sum(!complete.cases(data))
    NATotal

    ## [1] 2304

2.Using Mean for the day compute missing values
-----------------------------------------------

### finding the indices of missing values (NAs)

    StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)

### Imputing missing values using the mean for that 5-minute interval

    fillNA <- numeric()
    for (i in 1:nrow(data)) {
      obs <- data[i, ]
      if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
      } else {
        steps <- obs$steps
      }
      fillNA <- c(fillNA, steps)
    }

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
--------------------------------------------------------------------------------------------------

    new_activity <- data
    new_activity$steps <- fillNA

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
-------------------------------------------------------------------------------------------------------------------------------------------------

    StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
    hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="tomato", xlab="Number of Steps")
    hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="peachpuff", xlab="Number of Steps", add=T)
    legend("topright", c("Imputed", "Non-imputed"), col=c("tomato", "peachpuff"), lwd=7)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)

### Calculating the mean and median

    rmeantotal <- mean(StepsTotalUnion$steps)
    rmeantotal

    ## [1] 10766.19

    rmediantotal <- median(StepsTotalUnion$steps)
    rmediantotal

    ## [1] 10766.19

### Do these values differ from the estimates from the first part of the assignment?

    rmediandiff <- rmediantotal - rmedian
    rmediandiff

    ## [1] 1.188679

    rmeandiff <- rmeantotal - rmean
    rmeandiff

    ## [1] 0

### The mean is the same however the median does have a small variance.The mean and median for the **complete dataset** are almost identical.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

### The impact of the missing data has the *biggest* effect on the 10000 - 150000 step interval according by the graph comparing the **imputed** and **non-imputed** analysis.

Question 04: There differences in activity patterns between weekdays and weekends?
==================================================================================

    weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
                  "Friday")

    new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))

    StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)

    library(lattice)

    xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l", bg = "white")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-16-1.png)
