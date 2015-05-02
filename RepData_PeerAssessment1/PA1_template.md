# Reproducible Research: Peer Assessment 1


library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')

## 1) Loading and preprocessing the data

###  Loading the required data and packages
```
unzip("activity.zip")
dataset = read.csv("activity.csv", header = TRUE)
summary(dataset)
```

### Processing and tidying the data
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- as.factor(data$interval)

str(data)


## 2) What is mean total number of steps taken per day?
dailysteps <- aggregate(steps ~ date,data,sum)
colnames(dailysteps) <- c("dates","steps")
head(dailysteps)

library(ggplot2)

ggplot(dailysteps, aes(x=steps)) + 
        geom_histogram(fill = "blue", binwidth = 1000) +
        labs(title = "Daily Steps per Day",
        x="Number of steps per day", y= "Number of counts per day") + theme_bw()

avg_steps   <- mean(dailysteps$steps, na.rm=TRUE)
median_steps <- median(dailysteps$steps, na.rm=TRUE)

print(avg_steps)
print(median_steps)


## 3) What is the average daily activity pattern?
steps_per_interval <- aggregate(data$steps, by = list(interval = data$interval),FUN=mean, na.rm=TRUE)

steps_per_interval$interval <- as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])

colnames(steps_per_interval) <- c("interval", "steps")

ggplot(steps_per_interval, aes(x=interval, y=steps)) + geom_line(color="orange", size=1) + labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") + theme_bw()

max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]

print(max_interval)


## 4) Imputing missing values

### Total no of missing values
missvals <- sum(is.na(data$steps))

### Strategy for filling in all of the missing values in the dataset
na_fill <- function(data, pervalue) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

data_fill <- data.frame(  
        steps = na_fill(data, steps_per_interval),  
        date = data$date,  
        interval = data$interval)
str(data_fill)

sum(is.na(data_fill$steps)) #Final Check

### Histogram without missing values

fill_steps_per_day <- aggregate(steps ~ date, data_fill, sum)

colnames(fill_steps_per_day) <- c("date","steps")

ggplot(fill_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "red", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 

### Calculating the median and mean after removing values

steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)

steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)

print(steps_mean_fill)
print(steps_median_fill)

## 5) Are there differences in activity patterns between weekdays and weekends?

### To split and arrange steps by weekdays and weekends
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval), FUN=mean, na.rm=T)
    
    # convert to integers for plotting
    
    weekdays_steps$interval <- as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}

data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(data$date))
    weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))

    weekend_steps <- weekdays_steps(weekend_data)
    weekday_steps <- weekdays_steps(weekday_data)

    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}

data_weekdays <- data_by_weekdays(data_fill)

### Facetted histogram to show comparisons

ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="violet") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
