#gam 분석

rm(list=ls())

install.packages("leaps")
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
library("leaps")


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

#################################################################################
################################ OLS분석 ########################################
training_data <- read.csv("D:\\test_1\\minmax정규화\\training_data_min_maxnorm.csv", header = T, sep = ",")
test_data <- read.csv("D:\\test_1\\minmax정규화\\test_data_norm.csv", header = T, sep = ",")

trn_data_ols <- training_data
val_data_ols <- test_data

month <- as.factor(training_data$month)
trn_data_ols <- cbind(month, trn_data_ols[,-c(2)])

month <- as.factor(test_data$month)
val_data_ols <- cbind(month, val_data_ols[,-c(2)])


full_model_step <- step(lm(A08~month+X_COORD+Y_COORD+SO2_mean + CO_mean + O3_mean + NO2_mean + PM10_mean + SO2_max + CO_max + O3_max + NO2_max + PM10_max + 
                           temp_mean+temp_highmean+temp_lowmean+temp_max+temp_min+ atmosphere_mean+sea_pressure_mean+sea_pressure_max+sea_pressure_min+
                          water_pressure_mean+water_pressure_max+water_pressure_min+dewpoint_mean+humidity_mean+humidity_min+rain_sum+day_rain_max+wind_mean+
                          wind_max+maximum_instantaneous_wind+sun_time+sun_percentage+snow_sum+grass_temp_mean+grass_temp_min+ground_temp_mean+population+population_density
                           , data=trn_data_ols), direction="both", trace=1)

summary(full_model_step)
plot(full_model_step)

confint(full_model_step)
vif(full_model_step)

full_model_predict_step <- predict(full_model_step, val_data_ols)
target.y <- val_data_ols[,"A08"]
predictData_ols <- as.data.frame(cbind(target.y, full_model_predict_step))

perf_mat_ols <- matrix(0,4,1)
perf_mat_ols[1,1] <- mean((val_data_ols$A0-full_model_predict_step)^2)
perf_mat_ols[2,1] <- sqrt(mean((val_data_ols$A0-full_model_predict_step)^2))
perf_mat_ols[3,1] <- mean(abs(val_data_ols$A0-full_model_predict_step))
perf_mat_ols[4,1] <- mean(abs((val_data_ols$A0-full_model_predict_step)/val_data$A0_ols))*100

write.csv(predictData_ols, "D:\\test_1\\result\\predict_OLS_1.csv")


# z-score
training_data <- read.csv("D:\\test_1\\zscore정규화\\training_data_znorm.csv", header = T, sep = ",")
test_data <- read.csv("D:\\test_1\\zscore정규화\\test_data_znorm.csv", header = T, sep = ",")

trn_data_ols <- training_data
val_data_ols <- test_data

month <- as.factor(training_data$month)
trn_data_ols <- cbind(month, trn_data_ols[,-c(2)])

month <- as.factor(test_data$month)
val_data_ols <- cbind(month, val_data_ols[,-c(2)])

full_model_step <- step(lm(A0~month+X_COORD+Y_COORD+SO2_mean + CO_mean + O3_mean + NO2_mean + PM10_mean + SO2_max + CO_max + O3_max + NO2_max + PM10_max + 
                             temp_mean+temp_highmean+temp_lowmean+temp_max+temp_min+ atmosphere_mean+sea_pressure_mean+sea_pressure_max+sea_pressure_min+
                             water_pressure_mean+water_pressure_max+water_pressure_min+dewpoint_mean+humidity_mean+humidity_min+rain_sum+day_rain_max+wind_mean+
                             wind_max+maximum_instantaneous_wind+sun_time+sun_percentage+snow_sum+grass_temp_mean+grass_temp_min+ground_temp_mean+population+population_density
                           , data=trn_data_ols), direction="both", trace=1)

summary(full_model_step)
plot(full_model_step)

confint(full_model_step)
vif(full_model_step)

full_model_predict_step <- predict(full_model_step, val_data_ols)
target.y <- val_data_ols[,"A0"]
predictData_ols <- as.data.frame(cbind(target.y, full_model_predict_step))

perf_mat_ols <- matrix(0,4,1)
perf_mat_ols[1,1] <- mean((val_data_ols$A0-full_model_predict_step)^2)
perf_mat_ols[2,1] <- sqrt(mean((val_data_ols$A0-full_model_predict_step)^2))
perf_mat_ols[3,1] <- mean(abs(val_data_ols$A0-full_model_predict_step))
perf_mat_ols[4,1] <- mean(abs((val_data_ols$A0-full_model_predict_step)/val_data$A0_ols))*100

write.csv(predictData_ols, "D:\\test_1\\result\\predict_OLS.csv")


##########################################################################
########################### lasso ########################################
rm(list=ls())


#install.packages("lars")
#install.packages("glmnet")
library(lars)
library(glmnet)
library(ggplot2)

training_data.lasso <- read.csv("D:\\test_1\\minmax정규화\\training_data_min_maxnorm.csv", header = T, sep = ",")
test_data.lasso <- read.csv("D:\\test_1\\minmax정규화\\test_data_norm.csv", header = T, sep = ",")


month <- as.factor(training_data.lasso$month)
trn_data.lasso <- cbind(month, training_data.lasso[,-c(2)])

month <- as.factor(test_data.lasso$month)
val_data.lasso <- cbind(month, test_data.lasso[,-c(2)])

data3 <- rbind(trn_data.lasso, test_data.lasso)


#data4 <- data3[,c(7:46)]
data4 <- data3[,c(1, 7:46)]
data5 <- data3[,49]
data6 <- as.data.frame(cbind(data4, data5))
str(data6)
dim(data3)

x1 <- model.matrix(data5~., data6)
str(x1)
y1 <- data6$data5

grid <- 10^seq(5,-5,length=100)

train1 <- c(1:7051)
test1 <- (-train1)
y.test1 <- y1[test1]

#test.lasso <- lars(x1[train1,], y1[train1], type="lasso")
#plot(test.lasso)
#plot(test.lasso, plottype = "Cp")
#round(test.lasso$beta, 4)
#coef(test.lasso, s=40)

#plot(table(y1[train1]))


lasso.mod1 <- glmnet(x1[train1,], y1[train1], alpha = 1, lambda = grid)
plot(lasso.mod1)

cv.out1 <- cv.glmnet(x1[train1,], y1[train1], alpha = 1, lambda = grid)
plot(cv.out1)
bestlam1 <- cv.out1$lambda.min   # 에러가 최소로 나타나는 람다값을 찾아
bestlam3 <- cv.out1$lambda.1se   #standard error가 가장 regularized 된 모델이 된 람다값
lasso.pred1 <- predict(lasso.mod1, s=bestlam1, newx = x1[test1,])
lasso.pred3 <- predict(lasso.mod1, s=bestlam3, newx = x1[test1,])


mean((lasso.pred1-y.test1)^2)
sqrt(mean((lasso.pred1-y.test1)^2))
mean((lasso.pred3-y.test1)^2)
out1 <- glmnet(x1, y1, alpha=1, lambda = grid)
lasso.coef1 <- predict(out1, type="coefficients", s=bestlam1)[1:53,]

test_data_y <- y.test1
pred_data_y <- lasso.pred1

predict_data <- as.data.frame(pred_data_y)
merged_test_pred_y <- cbind(test_data_y, pred_data_y)

write.csv(merged_test_pred_y, "D:\\test_1\\result\\predict_lasso_A08.csv")


################################################################################
################################## deep NN #####################################
setwd("D:\\test_1\\minmax정규화")
install.packages("h2o")
library(h2o)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)


#install.packages("deepnet")
#install.packages("data.table")
library(data.table)
library(deepnet) 



#train <- h2o.importFile(path="D:\\test\\training_data_dnn_variableremove.csv")
#test <-  h2o.importFile(path="D:\\test\\test_data_dnn_variableremove.csv")


#train <- fread("D:\\test_1\\zscore정규화\\training_data_znorm.csv", header = T, sep = ",")
#test <- fread("D:\\test_1\\zscore정규화\\test_data_znorm.csv", header = T, sep = ",")

train <- fread("D:\\test_1\\minmax정규화\\training_data_min_maxnorm.csv", header = T, sep = ",")
test <- fread("D:\\test_1\\minmax정규화\\test_data_norm.csv", header = T, sep = ",")
str(train)
colnames(train)
month <- as.factor(train$month)
trn_data.dnn <- cbind(month, train[,-c(2)])

month <- as.factor(test$month)
val_data.dnn <- cbind(month, test[,-c(2)])


train.h2o <- as.h2o(trn_data.dnn)
test.h2o <- as.h2o(val_data.dnn)

x.indep <- c(1, 7:46)
#x.indep <- c(7:46)
y.dep <- c(49)
#y.dep <- c(47)
#a <- c(10, 50, 100, 200, 300, 400, 500, 700, 1000)
dlearning.model <- h2o.deeplearning(y=y.dep, x=x.indep, training_frame = train.h2o, 
                            epochs = 1000, hidden = c(100,10,10), l1=0.0001,
                            activation="Rectifier", seed=1234)

h2o.performance(dlearning.model)
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
test.y <- test$A08

sqrt(mean((test.y-predict.dl2)^2))
mseResult <- rbind(mseResult, perf_mat)

write.csv(mseResult,  "D:\\test\\DNNpredict_epoch500_hidden3_100.csv")


################################################################################
######################### optimized dnn ########################################

setwd("D:\\test\\")

rm(list=ls())

#install.packages("doBy")
#library(sampling)
#library(doBy)
#library(plyr)

#data <- read.csv("D:\\test\\training_data_dnn_variableremove.csv", header = T, sep = ",")

#training_data <- sampleBy(~year+month+SIDO, frac=0.7, replace=FALSE, data = data)

#data1 <- rbind(data, training_data)
#valid_data <- subset(count(data1), freq==1)

#write.csv(training_data, "D:\\test\\training_data_dnn_grid.csv")
#write.csv(valid_data, "D:\\test\\valid_data_dnn_grid.csv")


# 1. 기존 H2O 제거
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# 2. H2O 의존성 설치
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# 3. H2O 설치
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
install.packages("h2oEnsemble")
library(devtools)
install_github("https://github.com/h2oai/h2o-3")
install.packages("https://h2o-release.s3.amazonaws.com/h2o-ensemble/R/h2oEnsemble_0.1.8.tar.gz", repos = NULL)

#-------------------------------------------------------------------------
# 01.1. H2O 클러스터 환경설정
#-------------------------------------------------------------------------

library(h2o)
library(h2oEnsemble)  # This will load the `h2o` R package as well
h2o.init(nthreads = -1)  # Start an H2O cluster with nthreads = num cores on your machine
h2o.removeAll() # Clean slate - just in case the cluster was already running
#h2o.shutdown()



#install.packages("deepnet")
#install.packages("data.table")
library(data.table)
library(deepnet) 

h2o.init( nthreads = -1 )
h2o.no_progress()

train <- h2o.importFile(path="D:\\test\\training_data_dnn_variableremove.csv")
train <- h2o.importFile(path="D:\\test\\training_data_dnn_grid.csv")
valid <- h2o.importFile(path="D:\\test\\valid_data_dnn_grid.csv")
test <-  h2o.importFile(path="D:\\test\\test_data_dnn_variableremove.csv")

#train <- fread("D:\\test\\training_data_dnn_변수제거.csv", header = T, sep = ",")
#test <- fread("D:\\test\\test_data_dnn_변수제거.csv", header = T, sep = ",")
str(train)
colnames(train)


train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
valid.h2o <- as.h2o(valid)

x.indep <- c(12:33)
#x.indep <- c(7:46)
y.dep <- c(34)
#y.dep <- c(47)

y <- "A0"
x <- setdiff(names(train.h2o[,c(7:31, 33)]), c(y, "int_rate"))

activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout", "Tanh", "TanhWithDropout")
l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
epochs_opt<-c(100,500,1000,1500,2000) 
hyper_params <- list(epochs=epochs_opt, activation=activation_opt, l1=l1_opt, l2=l2_opt)


dl_grid <- h2o.grid("deeplearning", x=x, y=y,
                    grid_id = "dl_grid",
                    training_frame = train.h2o,
                    seed=1,
                    hidden = c(10, 10, 10),
                    hyper_params = hyper_params)

dl_gridperf <- h2o.getGrid(grid_id = "dl_grid", 
                           sort_by = "mse", 
                           decreasing = TRUE)
print(dl_gridperf)

best_dl_model_id <- dl_gridperf@model_ids[[1]]
best_dl <- h2o.getModel(best_dl_model_id)

# 모형 성능평가
best_dl_perf <- h2o.performance(model = best_dl, 
                                newdata = test.h2o)
h2o.auc(best_dl_perf)

