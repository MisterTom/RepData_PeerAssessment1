# Reproducible Research project 1
TomB  
Sunday, October 18, 2015  

## Introduction

This is an exploration of activity monitoring data for the Johns Hopkins Reproducible Data Peer assignment 1.  The original data measure the number of steps taken by a volunteer, as measured in 5 minute intervals over a couple of months, and may be found here: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip  

## Loading and preprocessing the data

First off, we'll load up the data into R, download the stat package we'll need, and set a global parameter to turn off warnings on my rudimentary plots.  (This code assumes that you have first downloaded the zipped data to R's active directory.)

```r
activity <- read.csv("~/R/activity.csv")
activity$steps=as.numeric(activity$steps)
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.2
```

```r
activity[,2]=as.Date(activity[,2])
options(warn=-1)
```

## Average number of steps per day

The distribution of total daily steps in the data looks like this:

```r
totalbyday= ddply(activity, .(date), summarise, sum = sum(steps))
plottitle = "Hist plot of total steps per day in the activity dataset"
xaxis = "total steps in a day"
histplot = hist(totalbyday[,2], main = plottitle, breaks = 20)
```

![](Reproducible_code_peer_1_files/figure-html/unnamed-chunk-2-1.png) 

And the mean and median of the data can be calculated thus. (The option NA.RM = TRUE means we ignore any invalid data when calculating our averages.)

```r
mean(totalbyday[,2],na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(totalbyday[,2],na.rm=TRUE)
```

```
## [1] 10765
```

## Average activity pattern

Next, we'll look at the average daily pattern.  The histogram below shows the average day as a series of five minute chunks- so 12 * 24 = 288 in all.


```r
totalbyperiod = ddply (activity, .(interval), summarise, sum = mean (steps,na.rm=TRUE))
timeseries = ts(data = totalbyperiod)

plot (timeseries[,2])
```

![](Reproducible_code_peer_1_files/figure-html/unnamed-chunk-4-1.png) 

The plot looks feasible - not much going on at night and some sort of a pattern in the daytime.

This code 

```r
subset (totalbyperiod, sum==max(totalbyperiod$sum))
```

```
##     interval      sum
## 104      835 206.1698
```
shows that peak activity occurs for the five minute interval starting at 8.35 AM- perhaps part of a daily commute.

## Adjusting for missing values

There are a lot of missing values in the data. Using the following code we can count the number of obviously invalid recordings of step intervals (expecting a positive number...)


```r
nrow(activity)- nrow (subset(activity,steps>=0))
```

```
## [1] 2304
```

... and this code generates a new data set "j" with a new column which has replaced any invalid recordings with the sample's daily average for the relevant interval - so for example, an invalid data point for 5 AM - 5.05 AM is replaced by the average number of steps recorded for all days between 5 AM and 5.05.


```r
j=merge(activity,totalbyperiod)
      j[5]=j[2]
        for (i in 1:nrow(j))
          {if(is.na(j[i,2]))
          {j[i,5]=j[i,4]}}
```

Because of this choice of replacement, the average daily pattern should be unaffected - but it's a good idea to check this. (R generates a column name "steps.1" for the cleaned data automatically.)

```r
totalbyperiod = ddply (j, .(interval), summarise, sum = mean (steps.1))
timeseries = ts(data = totalbyperiod)

plot (timeseries[,2])
```

![](Reproducible_code_peer_1_files/figure-html/unnamed-chunk-8-1.png) 


No change, yay. What's happened to the distribution, mean and median of total steps in a day, I hear you ask?

```r
totalbydayclean = ddply (j, .(date), summarise, sum = sum(steps.1))
plottitle = "Hist plot of total steps per day in the activity dataset - cleaned"
hist(totalbydayclean[,2], main = plottitle, xname = xaxis,breaks = 20) 
```

![](Reproducible_code_peer_1_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(totalbydayclean[,2],na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(totalbydayclean[,2],na.rm=TRUE) 
```

```
## [1] 10766.19
```


The mean hasn't changed, and the median now equals the mean (it was very similar before).  The most likely explanation here is that data were missing, or present, for complete days, and we've now added in 2,304/288 = 8 days of simulated average data into our data set.

## Hooray for the weekend

For some reason, we'll use this 'cleaned' data set to investigate whether there is any significant difference in the activity pattern between weekends and weekdays. 

First off we calculate a weekday for our data using a standard library function, and then calculate averages over not just the 288 five minute intervals, but also over a weekday / weekend indicator (in the new "weekend" column, where 1 = a weekend, 0 a weekday). this is stored in "timeseries".

```r
j[,3]= as.Date(j[,3])
j[,6]=weekdays(j[,3])
j[,7]=0
for (i in 1:nrow(j))
    {if (j[i,6]=="Saturday"|j[i,6]=="Sunday"){
      j[i,7]=1}}
colnames(j)[7]="weekend"

weekend = ddply (j, c("interval","weekend"), summarise, sum = mean (steps.1))

timeseries = ts(data = weekend)
timeseries = as.data.frame (timeseries)
```





The average weekday looks like this:

```r
weekday = subset (timeseries, weekend ==0 )
plot (weekday [,3],type = "l")
```

![](Reproducible_code_peer_1_files/figure-html/unnamed-chunk-11-1.png) 

...whereas a weekend looks like this. (And we're done, so turn warnings back on!)

```r
weekend = subset (timeseries, weekend ==1 )
plot (weekend [,3],type = "l")
```

![](Reproducible_code_peer_1_files/figure-html/unnamed-chunk-12-1.png) 

```r
options(warn=0)
```

Just from looking, there does appear to be a significant difference - although note there would be more fluctuation in the weekend data set due to randomness as the sample size is a bit smaller.  A regular pattern of commuting (but without a fixed going home time) might account for the weekday patten, with weekend activity being more variable.
