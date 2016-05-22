---
title: "Assignment Week 2 Reproducible Research"
author: "TG"
date: "Thursday, May 19, 2016"
output: html_document
---

This is an R Markdown document. 
It is the Content for the first assignment of the Reproducible Research course of the Coursera/JHSPH Data Science Specialization.

The source files are posted on GitHub.
=========================



An analysis on personal activity monitoring device data





#Reproducible Research with R

## Activity monitoring devices: Peer Assessment 1

## Introduction




It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a micoach <http://www.adidas.com/us/micoach > and Fit Smart <(http://www.adidas.de/micoach?grid=true)>, < (http://www.dcrainmaker.com/2014/07/hands-adidas-smart.html)>. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain underutilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## 0. Prepare the environment.



### clear workspace
```{r}


rm(list=ls())    
```
### Set the Working directory
```{r}

#setwd("~/repdata_data_activity")

```
### Prepare the input 


Note that the  data for this assignment has  been downloaded from the course web site:

.Dataset: [Activity monitoring data] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K] 

It has been downloaded and unzipped to the working directory.
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

The variables included in this dataset are:

.steps: Number of steps taking in a 5-minute interval (missing values are coded as  NA )


.date: The date on which the measurement was taken in YYYY-MM-DD format


.interval: Identifier for the 5-minute interval in which measurement was taken







### Load relevant Packages

```{r}

#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(readr)



```

##1. Loading and preprocessing the data



##### loading the  data
```{r}

input_activity <-read_csv("activity.csv")  

```
#### review the data dimensions & variables 
```{r}
names(input_activity)
dim(input_activity)
str(input_activity)
head(input_activity)
tail(input_activity)
```


##Process/transform the data (if necessary) into a format suitable for your analysis
###Define the tables

```{r}

tbl_activity0 <- tbl_df(input_activity)

```







##2. What is mean total number of steps taken per day?






*Note: For this part of the assignment, missing values will be ignored!*






###Calculate the total number of steps taken per day

```{r}
steps_sum_day<-tbl_activity0 %>%  group_by(date) %>%  summarise(steps_total = sum(steps))
```

###Plot a histogram of the total number of steps taken each day



*Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.*

```{r, echo=FALSE}
hist(steps_sum_day$steps_total, xlab = "sum of steps per day", main = "histogram of steps per day")
```


###Calculate and return the mean and median of the total number of steps taken per day






The mean of total number of steps taken per day is:






```{r, echo=TRUE}
mean_na<-round(mean(steps_sum_day$steps_total,na.rm=T))
print(mean_na)
```


The median of total number of steps taken per day is:






```{r, echo=TRUE}
med_na<-round(median(steps_sum_day$steps_total,na.rm=T))
print(med_na)
```


##3. What is the average daily activity pattern?






Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r, echo=TRUE}

x<-tbl_activity0 %>%     group_by(interval) %>%     summarise(avg_steps = mean(steps, na.rm=TRUE))

plot(x$interval,x$avg_steps,  type="l", xlab = "5-min interval", ylab = "average steps across all days",main ='Average number of steps by 5-minute interval  across all days')
 
```



 






### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo=TRUE}
avg_max <-x %>%  filter(min_rank(desc(avg_steps)) ==1) %>%select(interval)
print(avg_max)

```

The maximum number of steps across all days is performed at 8 : 35 AM









##4. Inputing missing values






###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)






The total number of missing values is:




```{r, echo=TRUE}


sum_na <- sum(is.na(input_activity[,1]))
print(sum_na)

```




###Devise a strategy for filling in all of the missing values in the dataset. 

The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r, echo=TRUE}

sum_na0 <- sum(is.na(tbl_activity0[,1]))
print(sum_na0)

y<-tbl_activity0 %>% group_by(interval) %>% summarise(avg_steps_int = mean(steps, na.rm=TRUE))
tmp_activity <-left_join(tbl_activity0, y, by = "interval")
filter_activity<-tmp_activity %>% filter(is.na (steps)) %>%group_by(date ,interval)%>% mutate(steps = avg_steps_int)
sum_check <- sum(is.na(filter_activity[,1]))





print(sum_check)

```

* NOTE: The missing values are filled by  using the mean of the 5-minute interval across all days (excluding the "NA days"")*






###Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r, echo=TRUE}



tmp_activity$steps <-as.numeric(tmp_activity$steps)
tmp2_activity<-tmp_activity %>% filter(!is.na (steps))
new_activity<- union(tmp2_activity,filter_activity)
new_activity<- new_activity %>%select(  steps,    date,    interval) %>%    arrange(desc(date, interval))



```




### Calculate the total number of steps taken per day using the data with filled NA's

```{r}
steps_new_day<-new_activity %>%  group_by(date) %>% select(steps) %>% summarise(steps_total = sum(steps))
```




### Make a histogram of the total number of steps taken each day 
 
 Histograms of the total number of steps taken each day using  missing data replaced with 5-minute average across all days



```{r, echo=FALSE}
hist(steps_new_day$steps_total, xlab = "sum of steps per day", main = "histogram of steps per day (na are replaced)")
```


###Calculate and return the mean and median of the total number of steps taken per day






The mean of total number of steps taken per day is:






```{r, echo=TRUE}
rep_mean<-round(mean(steps_new_day$steps_total))
print(rep_mean)
```


The median of total number of steps taken per day is:






```{r, echo=TRUE}
rep_med<-round(median(steps_new_day$steps_total))
print(rep_med)
```



###Calculate and return the mean and median difference of the total number of steps taken per day


#### Do these values differ from the estimates from the first part of the assignment?



The mean difference of total number of steps taken per day is:






```{r, echo=TRUE}
mean_dif<-(mean_na)-(rep_mean)
print(mean_dif)
```


The median of difference total number of steps taken per day is:



```{r, echo=TRUE}
med_dif<-(med_na)-(rep_med)
print(med_dif)
```






 
##5. What is the impact of imputing missing data on the estimates of the total daily number of steps?



The impact on the mean is none

The impact on the median is small




 The root cause of the relatively small difference is due to the inserting strategy. 
 
 Inputing missing data on the estimates of the total daily number of steps changes the median,  as the number of (for the calculation) relevant observations changed .
 
 Based on the method used for filling in missing values, we can get different mean  values. As it was decided for this exersice   to fill in the missing values with 5 min intervall averages of the means did not change.






##6. Are there differences in activity patterns between weekdays and weekends?






####Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.




```{r}




new_activity$date<-as.Date(new_activity$date)

new_activity$day<-weekdays(new_activity$date)


# weekdays
new_activity_weekdays<-new_activity[(!new_activity$day %in% c("Saturday","Sunday","Sonntag","Samstag")),]  

#  weekend
new_activity_weekend<-new_activity[(new_activity$day %in% c("Saturday","Sunday","Sonntag","Samstag")),]   


#  weekend and  weekdays mean across all days per interval
weekday_steps_mean<-new_activity_weekdays %>%     group_by(interval) %>%     summarise(avg_steps = mean(steps))

weekend_steps_mean<-new_activity_weekend %>%     group_by(interval) %>%     summarise(avg_steps = mean(steps))






```




####Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)



```{r}


par(mfrow=c(2,1))

plot(weekday_steps_mean$interval,weekday_steps_mean$avg_steps, type="l",xlab='Intervals',ylab="Number of steps",
     col='red',lwd=2, main="Weekday")

plot(weekend_steps_mean$interval,weekend_steps_mean$avg_steps, type="l", xlab='Intervals',ylab="number of steps",
     col='blue',lwd=2,main="Weekend")



```

