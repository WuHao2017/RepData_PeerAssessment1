1. Introduction
---------------

A data set from a personal activity monitoring device will be analysis
in this course project. The device collects data at 5 minute intervals
through out the day. It consists of two month of data.

The dataset has three variables:

-   steps: Number of steps taking in a 5-minute interval(missing values
    are coded as NA)

-   data: The date on which the measurement was taken in YYYY-MM-DD
    format

-   interval: Identifier for the 5-minute in which measurement was taken

2. Locad library needed
-----------------------

    library(ggplot2)
    library(pander)
    library(tidyverse)
    library(data.table)

3. Read data
------------

    data_activity <- read.csv("activity.csv") %>%
            as.data.table()
    data_activity$date <- as.Date(data_activity$date)

4. Take a look at data
----------------------

### Summary of the data

    str(data_activity)

    ## Classes 'data.table' and 'data.frame':   17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

    summary(data_activity)

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

There were 288 observations for each day. In the attribute ‘step’, there
are NA values. There are altogether 2304 NA values.

### Summary the NA values by date

    SumNA <- data_activity[is.na(steps), .N, by = date]
    pander(SumNA)

<table style="width:26%;">
<colgroup>
<col style="width: 18%" />
<col style="width: 8%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: center;">date</th>
<th style="text-align: center;">N</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: center;">2012-10-01</td>
<td style="text-align: center;">288</td>
</tr>
<tr class="even">
<td style="text-align: center;">2012-10-08</td>
<td style="text-align: center;">288</td>
</tr>
<tr class="odd">
<td style="text-align: center;">2012-11-01</td>
<td style="text-align: center;">288</td>
</tr>
<tr class="even">
<td style="text-align: center;">2012-11-04</td>
<td style="text-align: center;">288</td>
</tr>
<tr class="odd">
<td style="text-align: center;">2012-11-09</td>
<td style="text-align: center;">288</td>
</tr>
<tr class="even">
<td style="text-align: center;">2012-11-10</td>
<td style="text-align: center;">288</td>
</tr>
<tr class="odd">
<td style="text-align: center;">2012-11-14</td>
<td style="text-align: center;">288</td>
</tr>
<tr class="even">
<td style="text-align: center;">2012-11-30</td>
<td style="text-align: center;">288</td>
</tr>
</tbody>
</table>

There are 8 days that do not have any observations. For the rest, no
missing values for step records.

5 Analysis without filling in missing value
-------------------------------------------

### Histogram of the total number of steps taken per day

    SumDaily <- data_activity[, .(steps = sum(steps)), by = date]
    ggplot(data = SumDaily) +
            geom_col(mapping = aes(x = date, y = steps))

    ## Warning: Removed 8 rows containing missing values (position_stack).

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### Mean and median number of steps taken each day

    print(paste("Mean number of steps taken eash day is ", mean(SumDaily$steps, na.rm = TRUE)))

    ## [1] "Mean number of steps taken eash day is  10766.1886792453"

    print(paste("Median number of steps taken eash day is ", median(SumDaily$steps, na.rm = TRUE)))

    ## [1] "Median number of steps taken eash day is  10765"

### Explore the average daily activity pattern: mean, median and max for each 5 minutes interval

    Sum5min <- data_activity[, 
                             .(Mean = mean(steps, na.rm = TRUE), 
                               Median = median(steps, na.rm = TRUE), 
                               Max = max(steps, na.rm = TRUE)), 
                             by =interval] %>%
            .[, Hour := (0:287)*5/60,] %>%
            pivot_longer(c('Mean', 'Median', 'Max'), names_to = 'StatType', values_to = 'steps')

    ggplot(data = Sum5min) +
            geom_point(mapping = aes(x = Hour, y = steps, colour = StatType)) +
            scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 3))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

1.  Most of the movement happened about from 6:00am to 9:00pm every
    day(see from the plot for max steps in each 5 minutes interval).

2.  About half of the total days, the movements mostly happened around
    8:00am ~ 9:00am and 6:00pm ~ 7:00pm(see from the plot for median
    steps in each 5 minutes interval).

The big variations in each 5 minutes interval during 6:00am ~ 9:00pm
indicate there might have different patterns on different week day.So
exploring the data based on week day is also needed.

    Sum5min_weekday <- data_activity[, weekday := wday(date), ] %>%
            .[, 
              .(Mean = mean(steps, na.rm = TRUE), 
                    Median = median(steps, na.rm = TRUE), 
                    Max = max(steps, na.rm = TRUE)), 
              by = c( 'weekday','interval')] %>%
            .[, Hour := rep((0:287)*5/60, times = 7)] %>%
            pivot_longer(c('Mean', 'Median', 'Max'), names_to = 'StatType', values_to = 'steps')

    ggplot(data = Sum5min_weekday[,,]) +
            geom_point(mapping = aes(x = Hour, y = steps, colour = StatType)) +
            scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 3)) +
            facet_wrap(~ weekday, nrow = 7)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

Sunday is the 1st day of the week.

From the plotting, it is clear that on each week day the distribution of
the steps in each 5 minutes interval are different.

6 Filling in all missing values and recalculated mean and median total number of steps taken perday
---------------------------------------------------------------------------------------------------

### Strategy for handling missing value

The general principal for choosing the values which will be used for
filling in the missing value is: not changing the distribution as much
as possible. In general, the distribution of step for each 5 minutes
interval are different on each week day. Regarding to this, will use the
median value of the step number based on each week day and each 5
minutes interval as the missing values.

### Filling in all missing values and check

    #### Create datatable, add additional column for weekday
    new_data_activity <- data_activity[, weekday := wday(date), ]

    #### Calculated median value of the steps by weekday and interval
    new_Sum5min_weekday <- new_data_activity[, .(Median = median(steps, na.rm = TRUE)), by = c('weekday', 'interval')]

    new_data_activity <- dplyr::full_join(new_data_activity, new_Sum5min_weekday, by = NULL) %>%
            as.data.table()

    new_data_activity$steps <- as.numeric(new_data_activity$steps)

    new_data_activity <- new_data_activity[is.na(steps), steps := Median, ] 

    #### Ploting the Mean, Median and Max based on week day and interval, thus to compare with the distribution withou filling in #### missing values
    new_Sum5min_weekday <- new_data_activity[, weekday := wday(date), ] %>%
            .[, 
              .(Mean = mean(steps, na.rm = TRUE), 
                    Median = median(steps, na.rm = TRUE), 
                    Max = max(steps, na.rm = TRUE)), 
              by = c('weekday', 'interval')] %>%
            .[, Hour := rep((0:287)*5/60, times = 7),] %>%
            pivot_longer(c('Mean', 'Median', 'Max'), names_to = 'StatType', values_to = 'steps')

    ggplot(data = new_Sum5min_weekday) +
            geom_point(mapping = aes(x = Hour, y = steps, colour = StatType)) +
            scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 3)) +
            facet_wrap(~ weekday, nrow = 7) 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Comparing the plotting before and after filling in missing value, it
seems no obvious difference. It verifys that the strategy used for
filling in missing value is fine.

### Make a histogram of the total number of steps taken per day, based on the new dataset

    new_SumDaily <- new_data_activity[, .(steps = sum(steps)), by = date]
    ggplot(data = new_SumDaily) +
            geom_col(mapping = aes(x = date, y = steps))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

### Mean and median number of steps taken each day, based on the new dataset

    print(paste("New mean number of steps taken eash day is ", mean(new_SumDaily$steps, na.rm = TRUE)))

    ## [1] "New mean number of steps taken eash day is  9705.23770491803"

    print(paste("New median number of steps taken eash day is ", median(new_SumDaily$steps, na.rm = TRUE)))

    ## [1] "New median number of steps taken eash day is  10395"

### Compare the mean and median before and after inputing missing value

    print(paste("Mean: ", mean(SumDaily$steps, na.rm = TRUE), "   ", "New mean: ", mean(new_SumDaily$steps, na.rm = TRUE)))

    ## [1] "Mean:  10766.1886792453     New mean:  9705.23770491803"

    print(paste("Median: ", median(SumDaily$steps, na.rm = TRUE), "   ", "New median: ", median(new_SumDaily$steps, na.rm = TRUE)))

    ## [1] "Median:  10765     New median:  10395"

Both mean and median are smaller than without inputing missing value.
Before inputing missing value, there was smaller difference between mean
and median vavlue. After inputing missing value with the strategy
defined, the difference between mean and median value increased a lot.

7 Explore activity patterns between weekdays and weekends
---------------------------------------------------------

Calculate average steps for each 5 minutes based on weekday and weekend.
Median is used as average here.

In order to helps better understand the pattern of activities, the unit
of 5 minute interval was changed from minutes to hours.

    #### Add additional variable which figure out the weekday and weekend for each day
    new_data_activity %>%
            .[,DayType := "weekday",] %>%
            .[weekday == 1 | weekday == 7, DayType := "weekend",]

    #### Calculate average steps based on weekday/weekend and 5 minutes interval
    new_Sum5min_DayType <- new_data_activity [,
                                              .(AvgSteps = median(steps)),
                                              by = c('DayType', 'interval')] %>%
                            .[, Hour := rep((0:287)*5/60, times = 2),]

    #### Plotting the based on the unit of 24 hours, instead of minutes. The reason is the unit of hours will helps to understand the activity pattern #### much better than the unit in minutes
    ggplot(data = new_Sum5min_DayType) +
            geom_line(mapping = aes(x = Hour, y = AvgSteps)) +
            scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 3)) +
            facet_wrap(~ DayType, nrow = 2) 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)

As we could see from the plotting for the patterns:

1.  For the week day, the movement of this guy in the morning are mostly
    during around 6:30am to around 9:00am. Most possible activities are
    getting up, washing, breakfast and going to work. From around 9:30am
    to 12:00pm, there are generally no movement at all. He or she was
    supposed to work, and the work was done in a way that has little
    movement(sitting by the table). There is about 20 to 30 minutes in
    the noon that has some movement, most possiblly for going to some
    place for lunch. In the afternoon, from around 12:30pm to around
    5:30pm, he or she was supposed to work and has little movement at
    all. From around 5:30 pm to 7:30 pm he or she is back to home,
    having dinner. After 7:30pm there is little movement, most possibly
    rest in home(watching TV in the Sofa and sleeping in bed).

2.  On the weekend, he or she get up later than the week day, get up
    around 8:00 am in teh morning. There are more frequent movements
    from 8:00 am to 9:00pm, mostly possible for the out door activities,
    or social events. After 9:30 pm, most possiblly rest in bed.
