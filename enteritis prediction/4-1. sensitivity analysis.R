##################sensitivity analysis#########################

rm(list=ls())
setwd("D:\\test_1\\minmax정규화")

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o.ai/download")))
library(h2o)
localH2O = h2o.init()

# Finally, let's run a demo to see H2O at work.
demo(h2o.glm)

library(h2o)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

library(data.table)
library(deepnet)

train <- fread("training_data_min_maxnorm.csv", header = T, sep = ",")
test <- fread("test_data_norm.csv", header = T, sep = ",")

str(train)
colnames(train)
month <- as.factor(train$month)
trn_data.dnn <- cbind(month,train[,-c(2)])

month <- as.factor(test$month)
val_data.dnn <- cbind(month, test[,-c(2)])

val_data.dnn[,2]
train.h2o <- as.h2o(trn_data.dnn)
test.h2o <- as.h2o(val_data.dnn)

x.indep <- c(1, 7:46)
#x.indep <- c(7:46)

y.dep <- c(47)
set.seed(1234)
dlearning.model <- h2o.deeplearning(y=y.dep, x=x.indep, training_frame = train.h2o, 
                                    epochs = 30, hidden = c(500,500,500), l1=0.00001, 
                                    activation="Rectifier")

h2o.performance(dlearning.model)

result = rep(0, length = nrow(test))
test1 = test

test_name = colnames(test1)

for(i in 7:46){
  
  test1 = data.frame(month = as.factor(test1$month), test1[,-2])
  test1[, which(colnames(test1)==test_name[i])] = test1[, which(colnames(test1)==test_name[i])]*1.1
  test.h2o <- as.h2o(test1)
  predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
  result = cbind(result, predict.dl2)
  
}


write.csv(result,  "D:\\test_1\\counterfatual\\A0_counterfactualadd30.csv")

data <- read.csv("D:\\test_1\\counterfactual\\A0_counterfactual.csv")

result2 <- data.frame()
yhat0 <- data$result

for(j in 3:42){
  #predict.y <- data[,j]
  dy <-(mean((data[,j]-yhat0)/yhat0))*100
  result2 <- rbind(result2, dy)
}

write.csv(result2,  "D:\\test_1\\counterfactual\\A0_counterfactual_result.csv")





