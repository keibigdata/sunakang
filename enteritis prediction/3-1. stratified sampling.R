rm(list=ls())

setwd("D:\\업무\\빅데이터팀\\전염성질환 예측\\data")

##################sampling#####################################
install.packages("doBy")

library(sampling)
library(doBy)
library(plyr)

data <- read.csv("D:\\test_1\\air_weather_population_disease_merge_1.csv", header = T, sep = ",")

training_data <- sampleBy(~year+month+SIDO, frac=0.7, replace=FALSE, data = data)

data1 <- rbind(data, training_data)
test_data <- subset(count(data1), freq==1)

write.csv(training_data, "D:\\test_1\\training_data.csv")
write.csv(test_data, "D:\\test_1\\test_data.csv")