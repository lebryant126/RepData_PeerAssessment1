# Reproducible Research: Peer Assessment 1

## 1. Loading and preprocessing the data

We will read in the activity monitoring data. The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

We will add a new variable called "minute" which translates the interval identifier into the minute of the day at the beginning of the five-minute interval. This will be useful for better understanding time series plots with this data. Both the first and last five cases are diplayed below as an example of the dataset (although there are no recorded steps for these cases).


```r
act_df <- read.csv("activity.csv",colClasses = c("numeric","Date","numeric"))
act_df$minute <- 0.6 * (act_df$interval - (act_df$interval %% 100))+ 
        (act_df$interval %% 100) 
rbind(head(act_df,5),tail(act_df,5))
```

```
##       steps       date interval minute
## 1        NA 2012-10-01        0      0
## 2        NA 2012-10-01        5      5
## 3        NA 2012-10-01       10     10
## 4        NA 2012-10-01       15     15
## 5        NA 2012-10-01       20     20
## 17564    NA 2012-11-30     2335   1415
## 17565    NA 2012-11-30     2340   1420
## 17566    NA 2012-11-30     2345   1425
## 17567    NA 2012-11-30     2350   1430
## 17568    NA 2012-11-30     2355   1435
```

## 2. What is mean total number of steps taken per day?


```r
        # making histogram
sums_df <- aggregate(steps ~ date, data = act_df, FUN = sum)
hist(sums_df$steps,breaks = 15,xlab="Steps",main="Number of Steps Per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


```r
        # computing mean and median number of steps per day
        # numbers formatted for printing
stepMean <- format(mean(sums_df$steps),big.mark=",")
stepMedian <- format(median(sums_df$steps),big.mark=",")
```

The histogram above shows the total number of steps per day over a 53 day period. During this time, the mean number of steps per day was 10,766 steps and the median number of steps was 10,765 steps. Note that missing values were omitted from these calculations.

## 3. What is the average daily activity pattern?


```r
        # making time series plot
avg_df <- aggregate(steps ~ minute, data = act_df, FUN = mean)
plot(avg_df,type="l",main="Average Number of Steps During a Day",xlab="Minutes",ylab="Number of Steps",xaxt='n')
axis(1,60*(1:24))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


```r
        # making the five-minute interval print ready
maxInterval <- subset(avg_df,steps == max(avg_df$steps))
maxMinute <- maxInterval$minute %% 60
maxHour <- (maxInterval$minute - maxMinute)/60
```

The plot above shows the average number of steps for each five-minute interval. The five minute interval with the most steps on average is between 8:35 AM and 8:40 AM

## 4. Imputing missing values


```r
        # calculating the number of incomplete cases
        # formatting number for printing
missing <- format(nrow(act_df) - sum(complete.cases(act_df)), big.mark=",")
```

There are 2,304 cases containing incomplete data. We will impute the missing values to the average taken of over the five-minute interval for which the data is missing. We will remake the histogram is Section 2 with imputed dataset, as well as calculate the adjusted mean and median number of steps per day.



```r
        # imputing the missing values
                # initalize complete_df
                # carry out imputation using avg_df. 
                #       Note that modular arithmetic 
                #       is needed to match the five-minute 
                #       interval with the missing data to 
                #       the value contained in avg_df.
complete_df <- act_df  
for(i in 1:17568) { if (is.na(complete_df[i,1])) complete_df[i,1] = avg_df[((i -1) %% 288) + 1,2]}
```


```r
        # making the histogram
sumsComplete_df <- aggregate(steps ~ date, data = complete_df, FUN = sum)
hist(sumsComplete_df$steps,breaks = 15,xlab="Steps",main="Number of Steps Per Day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


```r
        # calculating the adjusted mean and median
        # formatting numbers for printing
stepMean_complete <- format(mean(sumsComplete_df$steps),big.mark=",")
stepMedian_complete <- format(median(sumsComplete_df$steps),big.mark=",")
```

For the imputed dataset, the mean number of steps per day was 10,766 steps and the median number of steps was 10,766 steps. We see that the results in this section using the imputed dataset are very close to those found in Section 2.

## 5. Are there differences in activity patterns between weekdays and weekends?

The time series plots below show the difference in activity patterns between weekdays and weedend days. In particular, they show the average number of steps for each five-minute interval.



```r
        # Add new variable to complete_df for weekday or weekend day
complete_df$type = ""
for(i in 1:17568) { complete_df[i,5] <- if(weekdays(complete_df[i,2]) %in% c("Saturday", "Sunday")) {"weekend"} else {"weekday"}}
complete_df$type <- factor(complete_df$type)
```


```r
        # make time series plots     
week_day <- subset(complete_df,type == "weekday")
week_end <- subset(complete_df,type == "weekend")

avg_df_week_day <- aggregate(steps ~ minute, data = week_day, FUN = mean)
avg_df_week_end <- aggregate(steps ~ minute, data = week_end, FUN = mean)

par(mfrow=c(2,1))
plot(avg_df_week_day,type="l",main="Average Number of Steps During a Weekday",
     xlab="Minutes",ylab="Number of Steps",xaxt='n')
axis(1,60*(1:24))
plot(avg_df_week_end,type="l",main="Average Number of Steps During a Weekend",
     xlab="Minutes",ylab="Number of Steps",xaxt='n')
axis(1,60*(1:24))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

