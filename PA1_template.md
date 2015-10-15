REPRODUCIBLE RESEARCH COURSE PROJECT
=================================================================

#Prepared by Lindsay Mico 10/15/15 for Coursera Course Reproducible Research

##Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

##Loading and preprocessing the data

Load the libraries that I need for the project

```r
suppressWarnings(library(lubridate))
suppressMessages(suppressWarnings(library(plyr)))
suppressWarnings(library(knitr))
suppressWarnings(library(ggplot2))
suppressMessages(library(Hmisc))
```

Load the data (i.e. read.csv())

```r
setwd("C:/Users/LTM/Documents/New World Order/Reproducible Research")
file<-"activity.csv"
rawdata<-read.csv(file,header=TRUE,sep=",")
```

Process/transform the data (if necessary) into a format suitable for your analysis

```r
rawdata$date<-as.Date(rawdata$date,format="%Y-%m-%d")
rawdata<-transform(rawdata,day = wday(date))
```

##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

```r
data<-rawdata[complete.cases(rawdata)==TRUE,]
```

Calculate the total number of steps taken per day

```r
agg_dat_sum <-aggregate(steps~date, data, sum)
```

Make a histogram of the total number of steps taken each day

```r
qplot(steps, data=agg_dat_sum, xlab='Steps per day', ylab='Count',geom="histogram",binwidth=500)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
stepmean<-with(agg_dat_sum,mean(steps))
stepmean
```

```
## [1] 10766.19
```

```r
stepmedian<-with(agg_dat_sum,median(steps))
stepmedian
```

```
## [1] 10765
```

##What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
agg_dat_mean_interval <-aggregate(steps~interval, data, mean)
with(agg_dat_mean_interval,plot(steps~interval,type="l", main="Mean steps taken per 5 minute interval"))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
agg_dat_mean_interval[which.max(agg_dat_mean_interval$steps),1]
```

```
## [1] 835
```

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(which(is.na(rawdata$steps)))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in. 
Note that I am filling with the mean

```r
dataimputed<-rawdata
dataimputed$steps<-impute(dataimputed$steps,fun=mean)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
agg_dat_sum_i <-aggregate(steps~date, dataimputed, sum)
qplot(steps, data=agg_dat_sum_i, xlab='Steps per day', ylab='Count',geom="histogram",binwidth=500)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

```r
stepmean_i <- with(agg_dat_sum_i,mean(steps))
stepmean_i
```

```
## [1] 10766.19
```

```r
stepmedian_i <- with(agg_dat_sum_i,median(steps))
stepmedian_i
```

```
## [1] 10766.19
```

```r
stepmean_i-stepmean
```

```
## [1] 0
```

```r
stepmedian_i-stepmedian
```

```
## [1] 1.188679
```

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dataimputed$daytype<-"weekday"
dataimputed$daytype[dataimputed$day==1 | dataimputed$day==7]<-"weekend"
dataimputed$daytype<-as.factor(dataimputed$daytype)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
avgdataimputed <- aggregate(steps ~ interval + daytype, data=dataimputed, mean)
ggplot(avgdataimputed, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(daytype ~ .) +
        xlab("interval") + 
        ylab("Mean # of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 


