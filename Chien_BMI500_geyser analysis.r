library(readxl)
library(tidyverse)

geyserdf <- read.csv("~/Desktop/Geyser/geyser.csv",header=TRUE,skip=30,skipNul = TRUE)
rows_to_remove <- which(geyserdf$eruptions=="NaN")
geyserdf <- geyserdf[-rows_to_remove,]
geyserdf[eruptions]=as.numeric(geyserdf$eruptions) #remove NANs
geyserdf[waiting]=as.numeric(geyserdf$waiting) #remove NANs

#redo data as number
geyser <- data.frame(erupt= as.numeric(geyserdf$eruptions),
                      wait= as.numeric(geyserdf$waiting)) 
#this will introduce some NAs where data was entered incorrectly
#Will remove, since the intentions of the person entering data is conjecture
#for example, row 89 "7l" was entered, presumably instead of 71.
geyser <- na.omit(geyser) 
# removes wait times <0 - entered in error
geyser <- geyser[-which(geyser$erupt<0),] # removes rows where eruption <0
# removes outliers
upperbound <- mean(geyser$erupt)+3*sd(geyser$erupt)
lowerbound <- mean(geyser$wait)-3*sd(geyser$wait)
geyser <-geyser[-which(geyser$erupt>upperbound),] #removes rows where eruption > mean + 3x std dev
geyser <-geyser[-which(geyser$wait<lowerbound),] #removes row where wait < mean - 3x std dev

#Bad plot
plot(geyser$wait,geyser$erupt, 
     xlab = "", ylab="", 
     type="l", main = "Wait Times Effect Eruption Duration")

#Better plot
plot(geyser$wait,geyser$erupt, xlab = "Interval Between Eruptions (min)", ylab= "Eruption Time (min)",
     #xlim = c(0,100), ylim=c(0,5),
     type='p', pch=20)
