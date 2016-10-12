###### Clearing workspace ######
rm(list=ls())
cat("\014")
################################

library(dplyr)

# Reading in files
  setwd("/Users/i64425/GitHub/coursera_5_reproducible_research/Project2")
  df <- read.csv("repdata%2Fdata%2FStormData.csv.bz2")
  View(df)

# Explore data
  str(df)
  
  propdmgexpdistinct <- df %>% distinct(PROPDMGEXP)
  propdmgexpdistinct$PROPDMGEXP
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
    totByEventReduce <- head(as.data.frame(totByEvent), n = 10)
    names(totByEventReduce) <- c("MAGNITUDE")
    
    # Plot to see how things relate
    plot(totByEventReduce, ylab = "Magnitude of Injuries/Fatalities", xlab = "Disaster Index", 
         main = "Health Impact of Top 10 Disasters Across the United States")

    # Print
    print(totByEventReduce)

# Question 2: Across the United States, which types of events have the greatest economic consequences?
  # Create dataframe to be used in analysis
    dfMod2 <- df[,c(8, 25, 27)]
    dfMod2$TOTDMG <- apply(dfMod2[,2:3], 1, FUN = sum)

  # Add property damages up by each event
    totByDmg <- sort(tapply(dfMod2$TOTDMG, dfMod2$EVTYPE, FUN = sum), decreasing = TRUE)

  # Create data frame for all property damages greater than $100.00  
    totByDmgReduce <- head(as.data.frame(totByDmg), n = 10)
    names(totByDmgReduce) <- c("MAGNITUDE")

  # Plot to see how things relate  
    plot(totByDmgReduce, ylab = "Magnitude of Economical Damages", xlab = "Disaster Index", 
         main = "Economic Impact From Top 10 US Disasters")
    
  # Print
    print(totByDmgReduce)
      