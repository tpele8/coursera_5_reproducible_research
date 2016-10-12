###### Clearing workspace ######
rm(list=ls())
cat("\014")
################################

library(dplyr)
library(lattice)

# Reading in files
  setwd("/Users/i64425/GitHub/coursera_5_reproducible_research/Project1")
  df <- read.csv("activity.csv")

# Explore data
  #numDates = df %>% distinct(date)
  #numDates$date
  str(df)
  df$date <- as.Date(df$date, "%Y-%m-%d")

  #numSteps = df %>% distinct(steps)
  #sort(numSteps$steps)

# Calculating total number of steps per day
  stepsPerDay <- tapply(df$steps, df$date, FUN = sum, na.rm = TRUE)
  hist(stepsPerDay, xlab = "Steps", main = "Number of Steps Per Day")
  
  meanStepsPerDay <- mean(stepsPerDay)
  medianStepsPerDay <- median(stepsPerDay)
  
  meanStepsPerDay
  medianStepsPerDay
  
# Calculating Average Daily Activity Pattern
  meanStepsPerInterval <- tapply(df$steps, df$interval, FUN = mean, na.rm = TRUE)
  plot(names(meanStepsPerInterval), meanStepsPerInterval, type = "l", ylab = "Mean Number of Steps", 
       xlab = "5 Minute Intervals", main = "Mean Number of Steps At 5 Minute Intervals")
  
  maxSteps <- meanStepsPerInterval[[which.max(meanStepsPerInterval)]]
  maxInterval <- as.integer(names(meanStepsPerInterval[which.max(meanStepsPerInterval)]))/5
  
  tempStepTime <- as.integer(names(meanStepsPerInterval[which.max(meanStepsPerInterval)]))/60
  tempStepTime - floor(tempStepTime)
  if(floor(tempStepTime) > 12){
    stepTime <- paste(floor(tempStepTime)-12, ((tempStepTime - floor(tempStepTime))*60), sep = ":")
    amPM <- "PM"} else{
      stepTime <- paste(floor(tempStepTime), ((tempStepTime - floor(tempStepTime))*60), sep = ":")
      amPM <- "AM"}

  # It appears that, on average, the 835th 5 minute interval (which translates to 1:55 PM) contains 
  # the maximum number of 206.1698 steps which translates to 1:55 PM
  
# Imputing missing values
  naValues <- is.na(df$steps)
  naValuesTot <- length(naValues[naValues == TRUE])
  
  df2 <- df
  for (i in 1:nrow(df2)){
    if (is.na(df2$steps[i])){
      x <- sample(0:mean(df2$steps, na.rm = TRUE))[1]
      df2$steps[i] = x
      }
  }

  newStepsPerDay <- tapply(df2$steps, df2$date, FUN = "sum")
  hist(newStepsPerDay, xlab = "Steps Per Day", main = "Number of Steps Per Day")
  
  meanNewStepsPerDay <- mean(newStepsPerDay)
  medianNewStepsPerDay <- median(newStepsPerDay)
  
  meanNewStepsPerDay
  medianNewStepsPerDay
  
# Determining patters on weekdays vs. weekends
  df3 <- df2
  df3$day_of_week <- weekdays(df3$date)
  weekends <- c('Saturday', 'Sunday')
  df3$day_class <- factor(df3$day_of_week %in% weekends, levels = c(TRUE, FALSE), labels = c('Weekend', 'Weekday'))

  xyplot(steps~interval | day_class, data = df3, type = "l", layout = c(1,2),
         xlab = "Time Interval", ylab = "Number of Steps", 
        main = "Number of Steps by Day Classification")

  
  
  
  