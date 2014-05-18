# Reproducible Research: Peer Assessment 1
================

## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
library(lubridate)
# Here, we want to transform the date to an actual date object:
data$date <- ymd(data$date)
```



## What is mean total number of steps taken per day?
First, let's find the total number of steps taken per day:

```r
steps_per_day <- tapply(data$steps, data$date, sum, na.rm = TRUE)
```


Now, let's find the average over the days we've analyzed:

```r
mean_steps_per_day <- round(mean(steps_per_day))
median_steps_per_day <- round(median(steps_per_day))
```


The individual took a mean of 9354 and a median of 1.0395 &times; 10<sup>4</sup> steps per day.


## What is the average daily activity pattern?

First, we'll find the average number of steps taken at each point in the process:

```r
steps_per_interval <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
```


Let's plot the result:

```r
plot(dimnames(steps_per_interval), steps_per_interval, type = "l")
```

```
## Error: 'x' and 'y' lengths differ
```


There is a peak in the middle of the day, and we'd like to find out when this is:

```r
max_steps <- max(steps_per_interval)
max_index <- seq(along = steps_per_interval)[steps_per_interval == max_steps]
# retrieve time
times <- dimnames(steps_per_interval)[[1]]
max_time <- times[[1]][max_index]
```


We find out that the maximum number of steps is 206.1698.  This happens during the interval NA.  The interpretation here is that the person is most likely a morning walker or runner, and so take a lot of steps during this time in the mornings.



## Imputing missing values
We will first find the number of missing values:

```r
na_count <- length(data$steps[is.na(data$steps)])
```


This tells us that there are 2304 NA values in the dataset. We should find some way to estimate for these missing values.  We will choose to replace any NA values with the mean of that interval from the data above. 


```r
# copy data to a new variable, 'shiny_data'
shiny_data <- data.frame(data)
# loop through all dates
for (this_date in unique(shiny_data$date)) {
    # construct index for replacing values in 'data' variable
    big_idx <- is.na(shiny_data$steps) & (shiny_data$date == this_date)
    # construct index for pulling values from mean variable
    small_idx <- is.na(shiny_data$steps[shiny_data$date == this_date])
    # do the replacement
    shiny_data$steps[big_idx] <- steps_per_interval[small_idx]
}
```


Now, let's check these calculations against our previous calculations:

```r
s_steps_per_day <- tapply(shiny_data$steps, shiny_data$date, sum, na.rm = TRUE)
s_mean_steps_per_day <- round(mean(s_steps_per_day))
s_median_steps_per_day <- round(median(s_steps_per_day))
```


The mean number of steps per day is now 1.0766 &times; 10<sup>4</sup> steps, and the median is 1.0766 &times; 10<sup>4</sup> steps.  This differs from the previous values, which were 9354 and 1.0395 &times; 10<sup>4</sup>, respectively.  These values should strictly increase as we replace NA values, so we have gotten the expected result.

## Are there differences in activity patterns between weekdays and weekends?

First, we will need to add another variable to the dataset to see whether it was a weekday or weekend:

```r
# create a new column and fill with 'weekday' (since there are more of
# these)
shiny_data$wkdy <- "weekday"
# find all weekdays
wkdays <- weekdays(shiny_data$date)
# set weekends to correct display
shiny_data$wkdy[wkdays == "Sunday" | wkdays == "Sunday"] <- "weekend"
```


Second, we'll calculate mean steps per time interval for each of the categories.


```r
end_idx <- shiny_data$wkdy == "weekend"
weekend_steps <- tapply(shiny_data$steps[end_idx], shiny_data$interval[end_idx], 
    mean)
day_idx <- shiny_data$wkdy == "weekday"
weekday_steps <- tapply(shiny_data$steps[day_idx], shiny_data$interval[day_idx], 
    mean)
```


Now, let's take a look at what the differences between these two might be:

```r
par(mfrow = c(2, 1))
plot(times, weekend_steps, type = "l", main = "weekend")
plot(times, weekday_steps, type = "l", main = "weekday")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


This furthers our hypothesis that the person runs or walks in the mornings, since their highest spike of all occurs on weekdays at 8:35am.
