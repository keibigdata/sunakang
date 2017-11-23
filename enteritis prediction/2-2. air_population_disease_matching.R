rm(list=ls())
#install.packages("lubridate")
#install.packages("plyr")
#install.packages("Amelia")
#install.packages("sqldf")
#library("plyr")
#library("lubridate")
#library("Amelia")
#library("sqldf")

#인구-대기 데이터 매칭

setwd("D:\\업무\\빅데이터팀\\전염성질환 예측\\data")

populationData <- read.csv("인구데이터\\인구수_인구밀도_SGG_0913.csv",header = T, sep = ",")
airpolutionData <- read.csv("대기오염 데이터\\월별 통계\\pm10_sig_matching_75_mean_max_agg_weather_merged.csv", header = T, sep = ",")

yearSGGpopulation <- paste(populationData$year, populationData$SGG)
yearSGGair <- paste(airpolutionData$year, airpolutionData$SIG_CD)

populationYear <- cbind(populationData, day=yearSGGpopulation)
airYear <- cbind(airpolutionData, day=yearSGGair)

mergeData <- merge(x=airYear, y=populationYear, by='day', all.x=TRUE)

write.csv(mergeData, "air_weather_population_merge.csv")

#인구/대기-질병데이터 매칭

diseaseData <- read.csv("건강데이터\\5년연속질병_장감염\\SGGmonthresult_final.csv", header=T, sep=",")
AirPopulationData <- read.csv("air_weather_population_merge.csv", header=T, sep=",")

yearmonthSGGDisease <- paste(diseaseData$year, diseaseData$month, diseaseData$SGG)
yearmonthSGGAir <- paste(AirPopulationData$year, AirPopulationData$month, AirPopulationData$SIG_CD)

bindDisease <- cbind(diseaseData, day=yearmonthSGGDisease)
bindAir <- cbind(AirPopulationData, day=yearmonthSGGAir)

mergeData <- merge(x=bindAir, y=bindDisease, by='day', all.x=TRUE)
write.csv(mergeData, "air_weather_population_disease_merge.csv")


