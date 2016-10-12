###### Clearing workspace ######
rm(list=ls())
cat("\014")
################################

library(dplyr)

# Reading in files
  setwd("/Users/i64425/GitHub/coursera_5_reproducible_research/Project2")
  df <- read.csv("repdata%2Fdata%2FStormData.csv.bz2")

# Explore data
  #str(df)
  
  #distinctEventType <- df %>% distinct(EVTYPE)
  #distinctEventType$EVTYPE
  
# Question 1: Across the United States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 variable) 
# are most harmful with respect to population health?
  
  # Sum injuries and fatalaties into a single column, then sum all values of that column by disaster type
  
  # Create dataframe to be used in the analysis
    dfMod1 <- df[,c(8, 23, 24)]
  
  # Sum fatalaties and injuries into the same column
    dfMod1$TOTDEST <- apply(dfMod1[,2:3], 1, FUN = sum)
    
  # Add up total destruction by disaster type and sort, then only analyze the top most harmful disasters
    totByEvent <- sort(tapply(dfMod1$TOTDEST, dfMod1$EVTYPE, FUN = sum), decreasing = TRUE)
    
    # Used the disasters that had at least 100 injuries/fatalities
    totByEventReduce <- as.data.frame(totByEvent[totByEvent >= 100])
    names(totByEventReduce) <- c("MAGNITUDE")
    
    # Plot to see how they relate
    plot(totByEventReduce, ylab = "Magnitude Of Injuries/Fatalities", xlab = "Disaster Index", main = "Magnitude Of Disasters Across the United States")

    # Print
    print(totByEventReduce)

# Question 2: Across the United States, which types of events have the greatest economic consequences?
