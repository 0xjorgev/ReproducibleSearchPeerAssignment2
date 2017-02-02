---
title: "Coursera's Reproducible Search: Peer Assessment 2"
author: "Jorge Mendoza"
date: "01/29/2016"
output: html_document
---

##### - Load required libraries to proccess data and Graphs

library(ggplot2)
library(imputeMissings)
library(utils)
library(plyr)
library(grid)

##### - Set current working directory

setwd("~/dataScience/Course V/Assigment-2/")

##### - Download and Load the project's data located at "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists('./data/repdata-data-StormData.csv')){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url=url, destfile='./data/repdata-data-StormData.csv.bz2', method='curl')
  filePath <- "./data/repdata-data-StormData.csv.bz2"
  destPath <- "./data/repdata-data-StormData.csv"
  bunzip2(filePath,destPath,overwrite=TRUE, remove=FALSE)
}

csvStromData = read.csv("./data/repdata-data-StormData.csv", sep = ",")
requiredColNames <- c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
dataSource <- csvStromData[, requiredColNames]

dataSource$YEAR <- as.numeric(format(as.Date(dataSource$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))

sortUtil <- function(fieldName, top, dataset) {
  index <- which(colnames(dataset) == fieldName)
  field <- aggregate(dataset[, index], by = list(dataset$EVTYPE), FUN = "sum")
  names(field) <- c("EVTYPE", fieldName)
  field <- arrange(field, field[, 2], decreasing = T)
  field <- head(field, n = top)
  field <- within(field, EVTYPE <- factor(x = EVTYPE, levels = field$EVTYPE))
  return(field)
}

unitsConververUtil <- function(dataset, fieldName, newFieldName) {
  totalLen <- dim(dataset)[2]
  index <- which(colnames(dataset) == fieldName)
  dataset[, index] <- as.character(dataset[, index])
  logic <- !is.na(toupper(dataset[, index]))
  dataset[logic & toupper(dataset[, index]) == "B", index] <- "9"
  dataset[logic & toupper(dataset[, index]) == "M", index] <- "6"
  dataset[logic & toupper(dataset[, index]) == "K", index] <- "3"
  dataset[logic & toupper(dataset[, index]) == "H", index] <- "2"
  dataset[logic & toupper(dataset[, index]) == "", index] <- "0"
  dataset[, index] <- as.numeric(dataset[, index])
  dataset[is.na(dataset[, index]), index] <- 0
  dataset <- cbind(dataset, dataset[, index - 1] * 10^dataset[, index])
  names(dataset)[totalLen + 1] <- newFieldName
  return(dataset)
}

fatalities <- sortUtil("FATALITIES", 20, dataSource)
injuries <- sortUtil("INJURIES", 20, dataSource)

dataSource <- unitsConververUtil(dataSource, "PROPDMGEXP", "PROPERTYDAMAGE")
dataSource <- unitsConververUtil(dataSource, "CROPDMGEXP", "CROPDAMAGE")

names(dataSource)
options(scipen=999)

property <- sortUtil("PROPERTYDAMAGE", 20, dataSource)
crop <- sortUtil("CROPDAMAGE", 20, dataSource)

ggplot(data=fatalities, aes(x=EVTYPE, y=FATALITIES)) + 
  geom_bar(stat="identity") + xlab("Event type") + ylab("Total fatalities") + 
  ggtitle("Fatalities By Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data=property, aes(x=EVTYPE, y=INJURIES)) + 
  geom_bar(stat="identity") + xlab("Event type") + ylab("Total fatalities") + 
  ggtitle("Fatalities By Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data=property, aes(x=EVTYPE, y=PROPERTYDAMAGE)) + 
  geom_bar(stat="identity") + xlab("Event type") + ylab("Total fatalities") + 
  ggtitle("Fatalities By Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data=crop, aes(x=EVTYPE, y=CROPDAMAGE)) + 
  geom_bar(stat="identity") + xlab("Event type") + ylab("Total fatalities") + 
  ggtitle("Fatalities By Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
