rm(list=ls())
#install.packages("lubridate")
#install.packages("plyr")
#install.packages("Amelia")
#install.packages("sqldf")
library("plyr")
library("lubridate")
library("Amelia")
library("sqldf")
setwd("D:\\업무\\빅데이터팀\\전염성질환 예측\\data\\기후데이터\\데이터 전처리")

SIDO <- read.csv("시군구매칭_code기준.csv", header=T, sep=",")  
weather <- read.csv("month_0016.csv", header=T, sep=",")

head(SIDO_weather)
str(weather)

ymd <- weather[,2]
ymd_character <- as.character(ymd)
weather_1 <- cbind(weather, ymd_character)
weather_final <- weather_1[,-2]

SIDO_weather <- merge(SIDO, weather_final, by='code')
a <- substr(SIDO_weather$ymd_character, 1,4)
b <- substr(SIDO_weather$ymd_character, 6,7)

weather_matching <- cbind(SIDO_weather, a, b)

write.csv(weather_matching, "D:\\업무\\빅데이터팀\\전염성질환 예측\\data\\기후데이터\\데이터 전처리\\SIDO_weather_matching.csv")

