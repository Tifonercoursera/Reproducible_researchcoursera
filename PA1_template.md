---
output: 
  html_document: 
    keep_md: yes
---
Assignemente week 2 
===================
## Download the data

the following code will download, unzip and read the database. Then the *date* variable is converted to the date class.

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data.zip")
unzip(zipfile = "./data.zip")
data <- read.csv("./activity.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## (first question) ,What is mean total number of steps taken per day?

Calculating the total number of steps by day,then I stored in variable labeled as *total*. The histogram is made on this *total* variable. The mean and median are report through the function summary()


```r
total <- tapply(data$steps, data$date, sum , na.rm = TRUE, simplify = TRUE)
head(total)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

```r
hist(total, col = "red", main = "total number of steps made with each day", xlab = "steps", ylab = "Number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

## (second question) What is the average daily activity pattern?

to make this plot I need the average steps by interval across all days. Therefore I generate a new variable labeled as *avge_step_by_intvl*, this variable has the average step for every interval (there are 288 intervals), and after this, I extract and store the 288 intervals inside a variable labeled as *intvl*. Finally a make the plot with these two variables.

```r
intvl <- data$interval[data$date == "2012-10-01"]
avge_step_by_intvl <-tapply(data$steps,data$interval, mean, na.rm = TRUE, simplify = TRUE)
plot(intvl, avge_step_by_intvl, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


The interval that has the maximum average step is the next one:



```r
intvl[avge_step_by_intvl == max(avge_step_by_intvl) ]
```

```
## [1] 835
```
## (third question) Imputing missing values.
First I report the total missing values that are inside the variable *steps*

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
After, I copy the data set. this data set will be the data that I will use to fill the missing values. The missing values will be filled with the mean from each interval.

```r
data_not_na <- cbind(data)
```

In the following code the *for*, is the one that fills each missing value with the average of steps per interval

```r
data$interval <- as.factor(data$interval)
data_not_na$interval <- as.factor(data_not_na$interval)
f <- as.numeric(levels(data_not_na$interval))
for (i in seq_along(f)) {
            data_not_na$steps[is.na(data_not_na$steps) & data_not_na$interval == f[i]] <- mean(data_not_na$steps[data_not_na$interval==f[i]], na.rm = TRUE)
}
```
Later, I report the median and mean, but these are calculated without any missing value. the mean and median are slightly higher than previously reported, the histogram plot also is plotted.

```r
total_sin_na <- tapply(data_not_na$steps, data_not_na$date, sum , na.rm = TRUE, simplify = TRUE)
summary(total_sin_na)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
hist(total_sin_na, col = "red", main = "total steps by day,without (NA) missing values", xlab = "total steps", ylab = "day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## (fourth question) Are there differences in activity patterns between weekdays and weekends?


```r
data_not_na$day <- weekdays(data_not_na$date)
data_not_na$week <- ifelse(data_not_na$day == "sábado" | data_not_na$day == "domingo", "weekend", "weekday" )
weekday <- subset(data_not_na, week == "weekday")
weekend <- subset(data_not_na, week == "weekend")

avge_step_by_intvl_weekday<-tapply(weekday$steps,weekday$interval, mean, simplify = TRUE)
avge_step_by_intvl_weekend<-tapply(weekend$steps,weekend$interval, mean, simplify = TRUE)

par(mfrow = c(2,1), mar = c(4,4,2,2))
plot(intvl, avge_step_by_intvl_weekend, type = "l",col ="blue", xlab = "", ylab = "Number of steps", main = "weekend")
plot(intvl, avge_step_by_intvl_weekday, type = "l",col ="blue", xlab = "interval", ylab = "Number of steps", main = "weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->









