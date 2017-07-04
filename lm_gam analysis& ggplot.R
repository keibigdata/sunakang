#gam 분석

rm(list=ls())


#install.packages("fareway")
#install.packages("mgcv")
#install.packages("corrplot")
#install.packages("car")
#install.packages("tidyverse")
#install.packages("ggplot")
#install.packages("gcookbook")
#install.packages("reshape2")
#install.packages("plyr")
#install.packages("gam")

library("gam")


library("corrplot")
library("fareway")
library("mgcv")
library("car")
library("tidyverse")
library("ggplot2")
library("dplyr")
library("gcookbook")
library("reshape2")

setwd("D:\\업무\\빅데이터팀\\전염성질환 예측\\data")



#정규화
data <- read.csv("total_data_final.csv", header = T, sep = ",", stringsAsFactor = FALSE)
str(data)
scaled_data1 <- scale(data[5:49])
scaled_data1 <- as.data.frame(scaled_data1)
scaled_data <- cbind(data1, scaled_data1)
write.csv(scaled_data, "total_data_final_scaled.csv")

#LM분석
data1 <- read.csv("lm_gam.csv", header = T, sep = ",", stringsAsFactor = FALSE)

full_model <- lm(data1$A00_09~data1$SO2_mean+data1$CO_mean+data1$O3_mean+data1$NO2_mean+data1$PM10_mean+
                   data1$SO2_max+data1$CO_max+data1$O3_max+data1$NO2_max+data1$PM10_max+
                   data1$평균기온..C._mean+data1$평균최고기온..C.+data1$평균최저기온..C.+
                   data1$최고기온..C.+data1$최저기온..C.+data1$평균현지기압.hPa.+data1$평균상대습도...+
                   data1$최소상대습도...+data1$일최다강수량.mm.+data1$평균풍속.m.s.+data1$최대풍속.m.s.+
                   data1$일조시간합_mean+data1$일조시간합_max+data1$일조시간합_min+data1$일조율_mean+
                   data1$일조율_max+data1$일조율_min+data1$인구수+data1$인구밀도, data=data1)
summary(full_model)

anova(full_model)


tmp_x <- paste(colnames(data1)[7:35], collapse=" + ")
tmp_xy <- paste("A00_09 ~ ", tmp_x, collapse = "")
tmp_xy
as.formula(tmp_xy)

forward_model <- step(lm(A00_09~SO2_mean + CO_mean + O3_mean + NO2_mean + PM10_mean + 
                           SO2_max + CO_max + O3_max + NO2_max + PM10_max + 평균기온..C._mean + 
                           평균최고기온..C. + 평균최저기온..C. + 최고기온..C. + 최저기온..C. + 
                           평균현지기압.hPa. + 평균상대습도... + 최소상대습도... + 일최다강수량.mm. + 
                           평균풍속.m.s. + 최대풍속.m.s. + 일조시간합_mean + 일조시간합_max + 
                           일조시간합_min + 일조율_mean + 일조율_max + 일조율_min + 
                           인구수 + 인구밀도, data=data1), scope=list(upper=as.formula(tmp_xy), lower=data1$A00_09~1), direction="backward", trace=1)

#GAM분석

gam_ob <- gam(data1$A00_09~data1$SO2_mean + data1$CO_mean + data1$O3_mean + data1$NO2_mean + data1$PM10_mean + 
                data1$SO2_max + data1$CO_max + data1$O3_max + data1$NO2_max + data1$PM10_max + data1$평균기온..C._mean + 
                data1$평균최고기온..C. + data1$평균최저기온..C. + data1$최고기온..C. + data1$최저기온..C. + 
                data1$평균현지기압.hPa. + data1$평균상대습도... + data1$최소상대습도... + data1$일최다강수량.mm. + 
                data1$평균풍속.m.s. + data1$최대풍속.m.s. + data1$일조시간합_mean + data1$일조시간합_max + 
                data1$일조시간합_min + data1$일조율_mean + data1$일조율_max + data1$일조율_min , data=data1, family=poisson(link=log))

summary(gam_ob)
par(mfrow=c(5,5))
plot(gamobj)



# 시계열 그래프(ggplot)
setwd("D:\\Users\\KEI\\Desktop\\year")
data1 <- read.csv("year_2009.csv", header = T, sep = ",", stringsAsFactor = FALSE)
data2 <- read.csv("year_2010.csv", header = T, sep = ",", stringsAsFactor = FALSE)
data3 <- read.csv("year_2011.csv", header = T, sep = ",", stringsAsFactor = FALSE)
data4 <- read.csv("year_2012.csv", header = T, sep = ",", stringsAsFactor = FALSE)
data5 <- read.csv("year_2013.csv", header = T, sep = ",", stringsAsFactor = FALSE)




a2 <- ggplot(data3, aes(x=data3$date, y=data3$A00_09, color=as.factor(data3$SIDO_NM), group=as.factor(data3$SIDO_NM)))+ geom_line() + geom_point(position = position_dodge(0.1), size = 2)+theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"))
geom_point(shape = 22, size = 3, fill = "white")

a1 <- ggplot(data2, aes(x=data2$date, y=data2$A00_09, color=as.factor(data2$SIDO_NM), group=as.factor(data2$SIDO_NM)))+ geom_line() + geom_point(position = position_dodge(0.1), size = 2)+theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"))
geom_point(shape = 22, size = 3, fill = "white")

a3 <- ggplot(data4, aes(x=data4$date, y=data4$A00_09, color=as.factor(data4$SIDO_NM), group=as.factor(data4$SIDO_NM)))+ geom_line() + geom_point(position = position_dodge(0.1), size = 2)+theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"))
geom_point(shape = 22, size = 3, fill = "white")

a4 <- ggplot(data5, aes(x=data5$date, y=data5$A00_09, color=as.factor(data5$SIDO_NM), group=as.factor(data5$SIDO_NM)))+ geom_line() + geom_point(position = position_dodge(0.1), size = 2)+theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"))
geom_point(shape = 22, size = 3, fill = "white")



ggplot(data1, aes(x=data1$date, y=data1$A00_09, color=as.factor(data1$SIDO), group=as.factor(data1$SIDO)))+geom_line()+facet_wrap(~data1$SIDO)


