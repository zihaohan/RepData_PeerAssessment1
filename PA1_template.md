# Peer-graded Assignment: Course Project 1
Zihaohan Sang  
January 7, 2017  

This courses project is for coursera course:Reproducible Research, week 2.

The original data is from the course web site:activity.csv  
The variables included in this dataset are:  
1. **steps**: Number of steps taking in a 5-minute interval(missing values are coded as **NA**)  
2. **date**: The date on which the measurement was taken in "%Y-%m-%d" format  
3. **interval**: Identifier for the 5-minute interval in which measurement was taken  

##Loading and preprocessing the data
First, code for reading in the dataset and processing the data


```r
require(dplyr)
```

```
## Loading required package: dplyr
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
rawdata <- read.csv("activity.csv")
##casue the NA in rawdata set, and the date as variable, so the enxt step is to tidy the data set
 rawdata$date <- as.Date(rawdata$date)
 processed <- na.omit(rawdata)
 head(processed, 10)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
## 295     0 2012-10-02       30
## 296     0 2012-10-02       35
## 297     0 2012-10-02       40
## 298     0 2012-10-02       45
```

```r
 ##then we can use the processed data set to answer the following questions 
```


##What is mean total number of steps taken per day?
Next, Histogram of the total number of steps taken each day

```r
bydate <- processed %>% group_by(date) %>% summarise(sum = sum(steps))

head(bydate, 10)
```

```
## # A tibble: 10 × 2
##          date   sum
##        <date> <int>
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
```

```r
##plot the histogram
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
ggplot(aes(x = sum), data = bydate) + geom_histogram() + labs(x = "Total number of steps per day", y = "frequency", title = "Histogram of The Total Number of Steps \nTaken Each Day") + theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = rel(1.2), face = "bold"), panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(color = "black", size = 0.8))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/hist_total_steps_per_day-1.png)<!-- -->

Then, Mean and median number of steps taken each day

```r
summary(bydate$sum)[3:4]
```

```
## Median   Mean 
##  10760  10770
```


##What is the average daily activity pattern?



```r
byinterval <- processed %>% group_by(interval) %>% summarise(avg = mean(steps))

head(byinterval)
```

```
## # A tibble: 6 × 2
##   interval       avg
##      <int>     <dbl>
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
ggplot(aes(x = interval, y = avg), data =  byinterval) + geom_line() +labs(x = "Interval", y = "Average number of steps taken") + theme_classic()
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->



```r
byinterval[which.max(byinterval$avg), ]
```

```
## # A tibble: 1 × 2
##   interval      avg
##      <int>    <dbl>
## 1      835 206.1698
```

##Imputing missing values


```r
## caluculate the total number of NA 
sum(is.na(rawdata))
```

```
## [1] 2304
```

```r
## in this case, I will use the average steps of intervals to replace NA of original data set
na_set <-rawdata[which(is.na(rawdata$steps)),]

for (i in 1:nrow(na_set)){
        na_set$steps[i] <- byinterval$avg[byinterval$interval == na_set$interval[i]]
}

##form new dataset without NA
newdata <- rbind(na_set, processed)

##regroup the dataset based on date
newbyday <- newdata %>% group_by(date) %>% summarise(sum = sum(steps))

##histogram
hist(newbyday$sum, xlab = "total number of steps taken", main = "Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/imputing_na-1.png)<!-- -->

```r
## the mean and median of the total number of steps taken per day

summary(newbyday$sum)[3:4]
```

```
## Median   Mean 
##  10770  10770
```


##Are there differences in activity patterns between weekdays and weekends?

```r
byweekday <- newdata %>% mutate(wday = weekdays(date), weekday = 0)

for (i in 1:nrow(byweekday)){
        if (byweekday$wday[i] %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
                byweekday$weekday[i] <- "weekday"
        }else{
                byweekday$weekday[i] <- "weekend"
        }
}

head(byweekday, 10)
```

```
##        steps       date interval   wday weekday
## 1  1.7169811 2012-10-01        0 Monday weekday
## 2  0.3396226 2012-10-01        5 Monday weekday
## 3  0.1320755 2012-10-01       10 Monday weekday
## 4  0.1509434 2012-10-01       15 Monday weekday
## 5  0.0754717 2012-10-01       20 Monday weekday
## 6  2.0943396 2012-10-01       25 Monday weekday
## 7  0.5283019 2012-10-01       30 Monday weekday
## 8  0.8679245 2012-10-01       35 Monday weekday
## 9  0.0000000 2012-10-01       40 Monday weekday
## 10 1.4716981 2012-10-01       45 Monday weekday
```

```r
newbyinterval <- byweekday %>% group_by(interval, weekday) %>% summarise(avg = mean(steps))

ggplot(aes(x = interval, y = avg, col = weekday), data = newbyinterval) + geom_line() + facet_grid(weekday~.) + labs(x = "Interval", y = "Number of steps") + theme_classic()
```

![](PA1_template_files/figure-html/activity_weekday_pattern-1.png)<!-- -->
knit2html()

