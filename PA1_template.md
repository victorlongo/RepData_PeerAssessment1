---
title: "Course Project 1"
author: "Victor Longo"
date: "May 26, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This is the Course Project 1 for John Hopkin's University's and Coursera's Reproducible Research course. Copy and paste the link below for further information. https://www.coursera.org/learn/reproducible-research/peer/gYyPt/course-project-1

# 1. Loading and preprocessing the data

The code below:
1.1 Loads the data (i.e. read.csv())
1.2 Processes/transforms the data into a format suitable for the analysis

```{r file.dl.unzip, cache=TRUE}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip")
unzip("activity.zip")
rawdata <- read.csv("activity.csv", sep = ",")
```

The code below:
1.3 Loads the required packages

```{r dplyr.magrittr, cache = TRUE}
if (!require("dplyr")){install.packages("dplyr");library(dplyr)}
if (!require("magrittr")){install.packages("magrittr");library(magrittr)}
if (!require("xtable")){install.packages("xtable");library(xtable)}
if (!require("ggplot2")){install.packages("ggplot2");library(ggplot2)}
```

# 2. What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

The code below: 
2.1 Calculates the total number of steps taken per day
2.2 Makes a histogram of the total number of steps taken each day

```{r daily.step.hist, cache = TRUE}
data1 <- na.omit(rawdata)
# Calculates the total number of steps taken per day
dailysteps <- select(data1, steps, date) %>%
  group_by(date) %>%
  summarise(dailysteps = as.numeric(sum(steps)))

# Makes a histogram of the total number of steps taken each day
hist(dailysteps$dailysteps, main = "Daily steps", xlab = "Total steps by day", ylab = "Frequency", col = "tomato4", xlim = c(0, 25000), breaks = length(dailysteps$dailysteps))

```

Te code below:
2.3 Calculates the mean and median of the total number of steps taken per day

```{r, results = "hide", cache = TRUE}
dailysteps.mean <- mean(dailysteps$dailysteps)
dailysteps.median <- median(dailysteps$dailysteps)
dailysteps.mean
dailysteps.median
```

- The mean of the total number of steps taken per day is `r dailysteps.mean`.
- The median of the total number of steps taken per day is `r dailysteps.median`.

# 3. What is the average daily activity pattern?

The code below:
3.1 Makes a time series plot of the average number of steps taken of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
agg <- aggregate(steps ~ interval, data = data1, FUN = "mean")
plot(agg$interval, agg$steps, type = "l", col = "steelblue", main = "Time series plot: average daily steps", xlab = "5-min interval", ylab = "Steps")
```

The code below answers:
3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
peak <- agg$interval[which.max(agg$steps)]
```

The peak in the plot above corresponds, on the x-axis, to the 5-min interval `r peak`, the daily interval that, on average, contains the maximum number of steps. 

# 4. Imputing missing values

The code below:
4.1 Calculates and reports the total number of missing values in the dataset (i.e. the total number of rows with NAs.

```{r}
missing <- sum(is.na(rawdata))
```

The total number of rows containing missing values in the original dataset is `r missing`.

The code below:
4.2 Devises a strategy for filling in all of the missing values in the dataset. I decided to use the mean for that 5-minute interval to replace the missing value.

4.3 Creates a new dataset (imputed_data) that is equal to the original dataset but with the missing data filled in.

```{r, cache = TRUE}
imputed_data <- transform(rawdata, steps = ifelse(is.na(rawdata$steps), agg$steps[match(rawdata$interval, agg$interval)], rawdata$steps))
```

The code below:
4.4 Makes a histogram of the total number of steps taken each day.

```{r, cache = TRUE}
dailysteps2 <- select(imputed_data, steps, date, interval) %>%
  group_by(date) %>%
  summarise(dailysteps = sum(steps))
hist(dailysteps2$dailysteps, main = "Daily steps", xlab = "Total steps by day", ylab = "Frequency", col = "tomato4", xlim = c(0, 25000), breaks = length(dailysteps2$dailysteps))
```

The code below:
4.5 Calculates the mean and median total number of steps taken per day.

```{r, cache = TRUE}
mean.imputed <- mean(dailysteps2$dailysteps)
median.imputed <- median(dailysteps2$dailysteps)
mean.imputed
median.imputed
```

- The imputed data mean is `r mean.imputed`.
- The imputed data mean is `r median.imputed`.

4.6 Do these values differ from the estimates from the first part of the assignment? 
- The mean with the imputed data does not differ from the first part of the assignment
- The median differs in 1.19, a closely irrelevant amount, taken the median values 

The code/histogram below answers:
4.7 What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
hist(dailysteps2$dailysteps, main = "Daily steps difference", xlab = "Total steps by day", ylab = "Frequency", col = "turquoise2", xlim = c(0, 25000), breaks = length(dailysteps2$dailysteps))

hist(dailysteps$dailysteps, main = "Daily steps difference", xlab = "Total steps by day", ylab = "Frequency", col = "papayawhip", xlim = c(0, 25000), breaks = length(dailysteps$dailysteps), add = T)

legend("topright", c("Imputed NAs", "Non-imputed NAs"), col=c("turquoise2", "papayawhip"), lwd=10)
```

- As the plot above suggests, the impact of imputing missing data on the estimates of the total daily number of steps is an increase in the frequency of days where around 10000 steps are taken.

# 5. Are there differences in activity patterns between weekdays and weekends?

The code below:
5.1 Creates a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
if (!require("dplyr")){install.packages("dplyr");library(dplyr)}
if (!require("magrittr")){install.packages("magrittr");library(magrittr)}
dailysteps3 <- select(imputed_data, steps, date, interval) %>%
  group_by(date)
dailysteps3$date <- as.Date(dailysteps3$date)
dailysteps3$daytype <- ifelse(weekdays(dailysteps3$date, abbreviate = T)=="Sat" | weekdays(dailysteps3$date, abbreviate = T)=="Sun", "weekend", "weekday")
```

The code below:
5.2 Makes a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken

```{r}
if (!require("ggplot2")){install.packages("ggplot2");library(ggplot2)}
aggweekday <- aggregate(steps ~ interval + daytype, dailysteps3, FUN="mean")
plot<- ggplot(aggweekday, aes(x = interval , y = steps, color = daytype)) +
       geom_line() +
       labs(title = "Daily steps by day type", x = "5-min interval", y = "Avg number of steps") +
       facet_wrap(~daytype, ncol = 1, nrow=2)
print(plot)
```












