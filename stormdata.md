# The Most Harmful Weather Event
---
title: "Untitled"
author: "Shan Huang"
date: "Thursday, July 24, 2014"
output: html_document
---
## 1. Synopsis
In US, storm and other weather events cause a large loss for both population 
health and economy every year.In order to reduce the loss and damage from these
disaster, it is important to find out which of them are the most harmful events.
History time and geographic information for each weather event occurrence data 
were collected by U.S. National Oceanic and Atmospheric Administration's (NOAA) 
storm database. In this paper, the database was downloaded and analysed to answer
two question:
1. Across the United States, which types of events (as indicated in the EVTYPE 
variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic 
consequences?

## 2. Getting the data


```r
Sys.setlocale('LC_ALL', 'English')
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
setwd("./StormDataNOAA")
```

```
## Error: cannot change working directory
```

```r
download.file("http://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2", 
              "repdata-data-StormData.csv.bz2")
stormData <- read.csv("repdata-data-StormData.csv.bz2", sep=",", header=TRUE, 
                      stringsAsFactors=FALSE)
```

Take a look at the dataset, there are 4 columns are corresponding to health and 
economy loss:
1. FATALTIES ~ The number of fatalities caused by the events
2. INJURIES ~ The number of people injured by the events
3. PROPDMG ~ The amount of property loss by the events
4. CROPDMG ~ The amount of crop damage by the weather


```r
str(stormData)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ BGN_TIME  : chr  "0130" "0145" "1600" "0900" ...
##  $ TIME_ZONE : chr  "CST" "CST" "CST" "CST" ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
##  $ STATE     : chr  "AL" "AL" "AL" "AL" ...
##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : chr  "" "" "" "" ...
##  $ BGN_LOCATI: chr  "" "" "" "" ...
##  $ END_DATE  : chr  "" "" "" "" ...
##  $ END_TIME  : chr  "" "" "" "" ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : chr  "" "" "" "" ...
##  $ END_LOCATI: chr  "" "" "" "" ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: chr  "" "" "" "" ...
##  $ WFO       : chr  "" "" "" "" ...
##  $ STATEOFFIC: chr  "" "" "" "" ...
##  $ ZONENAMES : chr  "" "" "" "" ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : chr  "" "" "" "" ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```


## 3. Data Processing
### 3.1 The most harmful events for public health

```r
fatalities <- aggregate(stormData$FATALITIES, list(stormData$EVTYPE), sum)
injuries <- aggregate(stormData$INJURIES, list(stormData$EVTYPE), sum)
health <- merge(fatalities, injuries, by="Group.1")
health[,4] <- health[,2] + health[,3]
colnames(health) <- c("type", "fatalities", "injuries", "total")
```


```r
healthtop <- health[order(health$total, decreasing=TRUE),][1:10,]

library(reshape2)
healthtop_1 <- melt(healthtop, id.vars="type")

library(ggplot2)
ggplot(healthtop_1, aes(x = reorder(type, -value), y = value)) + 
    geom_bar(stat = "identity", aes(fill = variable), position = "dodge") + 
    scale_y_sqrt("Number of Cases") + xlab("Event Type") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
    ggtitle("Top 10 Health Threat Events")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

### 3.2 The most harmful events for economy

```r
propertydmgs <- aggregate(stormData$PROPDMG, list(stormData$EVTYPE), sum)
corpdmgs <- aggregate(stormData$CROPDMG, list(stormData$EVTYPE), sum)
colnames(propertydmgs) <- c("type", "propertydmgs")
colnames(corpdmgs) <- c("type", "corpdmgs")

propertydmgstop <- propertydmgs[order(propertydmgs$propertydmgs, decreasing=TRUE),][1:10,]
corpdmgstop <- corpdmgs[order(corpdmgs$corpdmgs, decreasing=TRUE),][1:10,]

library(gridExtra)
```

```
## Loading required package: grid
```

```r
plot2 <- ggplot(propertydmgstop, aes(x=reorder(type, -propertydmgs), y=propertydmgs)) + 
    geom_bar(stat = "identity",  position = "dodge") + 
    scale_y_sqrt("Property Damage(US Dollar)") + xlab("Event Type") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
    ggtitle("Top 10 Property Threat Events")

plot3 <- ggplot(corpdmgstop, aes(x=reorder(type, -corpdmgs), y=corpdmgs)) + 
    geom_bar(stat = "identity",  position = "dodge") + 
    scale_y_sqrt("Crop Damage(US Dollar)") + xlab("Event Type") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
    ggtitle("Top 10 Crop Threat Events")

grid.arrange(plot2, plot3, ncol = 2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

### 3.3 Which part of US suffer most from the weather disaster


## 4. Result 
