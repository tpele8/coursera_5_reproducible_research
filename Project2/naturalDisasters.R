###### Clearing workspace ######
rm(list=ls())
cat("\014")
################################

library(dplyr)

# Reading in files
  setwd("/Users/i64425/GitHub/coursera_5_reproducible_research/Project2")
  df <- read.csv("repdata%2Fdata%2FStormData.csv.bz2")

# Explore data
  #View(df)
  #str(df)
  #propdmgexpdistinct <- df %>% distinct(PROPDMGEXP)
  #propdmgexpdistinct$PROPDMGEXP
  #distinctEventType <- df %>% distinct(EVTYPE)
  #distinctEventType$EVTYPE
  
# Question 1: Across the United States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 variable) 
# are most harmful with respect to population health?
  
# Question 2: Across the United States, which types of events have the greatest economic consequences?
  
  # Create dataframe to be used in the analysis
  dfMod <- df[,c(8, 23, 24, 25, 27)]
  
  # Sum injuries and fatalaties into a single column, then so the same for the property and crop damage
    dfMod$TOTDEST <- apply(dfMod[,2:3], 1, FUN = sum)
    dfMod$TOTDMG <- apply(dfMod[,4:5], 1, FUN = sum)
    
  # Add up total destruction by disaster type and sort, then only analyze the top most harmful disasters
    totByEvent <- sort(tapply(dfMod$TOTDEST, dfMod$EVTYPE, FUN = sum), decreasing = TRUE)
    totByDmg <- sort(tapply(dfMod$TOTDMG, dfMod$EVTYPE, FUN = sum), decreasing = TRUE)
    length(totByEvent)
    length(totByDmg)
    
    # Used the disasters that had at least 100 injuries/fatalities
    totByEventReduce <- head(as.data.frame(totByEvent), n = 10)
    totByDmgReduce <- head(as.data.frame(totByDmg), n = 10)
    names(totByEventReduce) <- c("MAGNITUDE")
    names(totByDmgReduce) <- c("MAGNITUDE")
    
    # Plot to see how things relate
    dev.off()
    plot(totByEventReduce, ylab = "Magnitude of Injuries/Fatalities", xlab = "Disaster Index", 
         main = "Health Impact of Top 10 Disasters Across the United States")
    plot(totByDmgReduce, ylab = "Magnitude of Economical Damages", xlab = "Disaster Index", 
         main = "Economic Impact From Top 10 US Disasters")

    # Print
    cat(sprintf("List totByEvent contains %d total incidents.", length(totByEvent)))
    cat(sprintf("List totByDmg contains %d total incidents.", length(totByDmg)))
