Sys.setlocale('LC_ALL', 'English')
setwd("./Storm")
download.file("http://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2", 
              "repdata-data-StormData.csv.bz2")
stormData <- read.csv("repdata-data-StormData.csv.bz2", sep=",", header=TRUE, 
                      stringsAsFactors=FALSE)

str(stormData)
fatalities <- aggregate(stormData$FATALITIES, list(stormData$EVTYPE), sum)
injuries <- aggregate(stormData$INJURIES, list(stormData$EVTYPE), sum)
health <- merge(fatalities, injuries, by="Group.1")
health[,4] <- health[,2] + health[,3]
colnames(health) <- c("type", "fatalities", "injuries", "total")

healthtop <- health[order(health$total, decreasing=TRUE),][1:10,]

library(reshape2)
healthtop_1 <- melt(healthtop, id.vars="type")

library(ggplot2)
ggplot(healthtop_1, aes(x = reorder(type, -value), y = value)) + 
    geom_bar(stat = "identity", aes(fill = variable), position = "dodge") + 
    scale_y_sqrt("Number of Cases") + xlab("Event Type") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
    ggtitle("Top 10 Health Threat Events")

propertydmgs <- aggregate(stormData$PROPDMG, list(stormData$EVTYPE), sum)
corpdmgs <- aggregate(stormData$CROPDMG, list(stormData$EVTYPE), sum)
colnames(propertydmgs) <- c("type", "propertydmgs")
colnames(corpdmgs) <- c("type", "corpdmgs")

propertydmgstop <- propertydmgs[order(propertydmgs$propertydmgs, decreasing=TRUE),][1:10,]
corpdmgstop <- corpdmgs[order(corpdmgs$corpdmgs, decreasing=TRUE),][1:10,]

library(gridExtra)

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
