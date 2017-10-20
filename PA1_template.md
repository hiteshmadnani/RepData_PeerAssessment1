Downloading of Data
===================

Loading and preprocessing the data
==================================

Show any code that is needed to:

1.  Load the data (i.e. read.csv())

2.  Process/transform the data (if necessary) into a format suitable for
    your analysis

<!-- -->

    ##    steps       date interval
    ## 1     NA 2012-10-01        0
    ## 2     NA 2012-10-01        5
    ## 3     NA 2012-10-01       10
    ## 4     NA 2012-10-01       15
    ## 5     NA 2012-10-01       20
    ## 6     NA 2012-10-01       25
    ## 7     NA 2012-10-01       30
    ## 8     NA 2012-10-01       35
    ## 9     NA 2012-10-01       40
    ## 10    NA 2012-10-01       45
    ## 11    NA 2012-10-01       50
    ## 12    NA 2012-10-01       55
    ## 13    NA 2012-10-01      100
    ## 14    NA 2012-10-01      105
    ## 15    NA 2012-10-01      110
    ## 16    NA 2012-10-01      115
    ## 17    NA 2012-10-01      120
    ## 18    NA 2012-10-01      125
    ## 19    NA 2012-10-01      130
    ## 20    NA 2012-10-01      135

What is mean total number of steps taken per day?
=================================================

For this part of the assignment, you can ignore the missing values in
the dataset.

1.  Calculate the total number of steps taken per day

2.  If you do not understand the difference between a histogram and a
    barplot, research the difference between them. Make a histogram of
    the total number of steps taken each day

3.  Calculate and report the mean and median of the total number of
    steps taken per day

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

The mean is 1.076618910^{4}

The Median is 10765

What is the average daily activity pattern?
===========================================

1.  Make a time series plot (i.e. type = "l") of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all days (y-axis)

2.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    # What is the average daily activity pattern?
    stepbyinterval <- aggregate(steps ~ interval, activitydata, mean, na.rm=T)

    # Create a time plot
    plot(stepbyinterval$interval, stepbyinterval$steps,type = "l",
         xlab = "Average Number of Steps",
         ylab = "Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # find row with max number of steps
    maxStep <- which.max(stepbyinterval$steps)

    # find the interval which consist of maximum number of steps
    maxsteprow <- stepbyinterval[maxStep, ]

    # Max step Interval details
    maxsteprow

    ##     interval    steps
    ## 104      835 206.1698

The Max Number of steps is 206.1698113

Imputing missing values
=======================

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

2.  Devise a strategy for filling in all of the missing values in
    the dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

3.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

4.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    # Checking the number of NA values
    NaSum <- sum(is.na(activitydata))
    NaSum

    ## [1] 2304

    data_imputed <- activitydata
    for (i in 1:nrow(data_imputed)) {
            if (is.na(data_imputed$steps[i])) {
                    interval_value <- data_imputed$interval[i]
                    steps_value <- stepbyinterval[
                            stepbyinterval$interval == interval_value,]
                    data_imputed$steps[i] <- steps_value$steps
            }
    }

    # Check if there is any NA Value
    sum(is.na(data_imputed))

    ## [1] 0

    # imputed DF total number of step taken per interval
    imputedTotalSteps <- aggregate(steps ~ date, data_imputed, sum)
    head(imputedTotalSteps)

    ##         date    steps
    ## 1 2012-10-01 10766.19
    ## 2 2012-10-02   126.00
    ## 3 2012-10-03 11352.00
    ## 4 2012-10-04 12116.00
    ## 5 2012-10-05 13294.00
    ## 6 2012-10-06 15420.00

    # create histogram
    hist(imputedTotalSteps$steps, main = "Total number of step per day",
         xlab="Total number of steps in a day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    # Mean and Median of imputed data
    mean(imputedTotalSteps$steps)

    ## [1] 10766.19

    #median
    median(imputedTotalSteps$steps)

    ## [1] 10766.19

    #Mean and Median of orignal Data
    mean(stepsSplit$steps)

    ## [1] 10766.19

    #median
    median(stepsSplit$steps)

    ## [1] 10765

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.

2.  Make a panel plot containing a time series plot (i.e. type = "l") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    # Are there differences in activity patterns between weekdays and weekends?

    data_imputed$dayType <- weekdays(as.Date(data_imputed$date))

    data_imputed$dayType[data_imputed$dayType %in% c("Saturday", "Sunday")] <- "weekend"
    data_imputed$dayType[data_imputed$dayType != "weekend"] <- "weekday"

    data_imputed$dayType <- as.factor(data_imputed$dayType)

    # calculate average steps by interval across all days
    imputed_steps_by_interval <- aggregate(steps ~ interval + dayType, data_imputed, mean)

    # make the panel plot for weekdays and weekends
    library(lattice)

    # create the panel plot
    xyplot(steps ~ interval | dayType, imputed_steps_by_interval, type = "l", layout = c(1, 2), 
           xlab = "Interval", ylab = "Number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)
