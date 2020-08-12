Loading and preprocessing the data
----------------------------------

##### 1. Load the data (i.e. read.csv())

    if (!file.exists("activity.csv") )
        {
         dlurl <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
         download.file(dlurl,destfile='repdata%2Fdata%2Factivity.zip',mode='wb')  
         unzip('repdata%2Fdata%2Factivity.zip')
        }


    data <- read.csv("activity.csv")

------------------------------------------------------------------------

What is mean total number of steps taken per day?
-------------------------------------------------

    steps_by_day <- aggregate(steps ~ date, data, sum)

##### 1. Make a histogram of the total number of steps taken each day

    qplot(steps_by_day$steps, xlab='Total steps per day', ylab='Frequency using binwith 500',binwidth=450, geom= "histogram") +
      theme_bw()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

##### 2. Calculate and report the mean and median total number of steps taken per day

    stepsByDayMean <- mean(steps_by_day$steps)
    stepsByDayMedian <- median(steps_by_day$steps)
    stepsByDayMean

    ## [1] 10766.19

    stepsByDayMedian

    ## [1] 10765

-   Mean: 10766.19
-   Median: 10765

------------------------------------------------------------------------

What is the average daily activity pattern?
-------------------------------------------

    steps_by_interval <- aggregate(steps ~ interval, data, mean)

    plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)
\#\#\#\#\# 2. Which 5-minute interval, on average across all the days in
the dataset, contains the maximum number of steps?

    max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
    max_interval

    ## [1] 835

-   Most Steps are 835

------------------------------------------------------------------------

Imputing missing values
-----------------------

##### 1. Calculate and report the total number of missing values in the dataset

    NATotal <- sum(!complete.cases(data))
    NATotal

    ## [1] 2304

-   Total Number of Missing values are 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

    StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)
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

    new_activity <- data
    new_activity$steps <- fillNA

##### 4. Make a histogram of the total number of steps taken each day

    StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)

    hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="pink", xlab="Number of Steps")


    hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
    legend("topright", c("Imputed", "Non-imputed"), col=c("pink", "red"), lwd=10)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)
\#\#\#\#\# … and Calculate and report the mean and median total number
of steps taken per day.

    rmeantotal <- mean(StepsTotalUnion$steps)
    rmediantotal <- median(StepsTotalUnion$steps)
    rmeantotal

    ## [1] 10766.19

    rmediantotal

    ## [1] 10766.19

-   Mean (Imputed) are 10766.19
-   Median (Imputed) are 10766.19

#### Do these values differ from the estimates from the first part of the assignment?

    rmeandiff <- rmeantotal - stepsByDayMean

    rmediandiff <- rmediantotal - stepsByDayMedian
    rmeandiff

    ## [1] 0

    rmediandiff

    ## [1] 1.188679

-   The mean(Mean Var: 0) is the same, but the median does have a little
    variance(Median Var:1.1886792).

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

-   The impact of the missing data has the biggest effect on the 10000 -
    150000 step interval and changes frequency from 27.5 to 35, there is
    a variance of 7.5.

------------------------------------------------------------------------

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
                  "Friday")
    new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
    StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)

##### 2. Make a panel plot containing a time series plot

    xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)
