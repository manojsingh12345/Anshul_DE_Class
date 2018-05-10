####################################################################################################
# This script will build the model
####################################################################################################

rm(list=ls())                                       ### Remove all variables from WS ####
#install.packages("randomForest")
#setwd("/Users/anshul/Downloads")                    # Setting the wd ####
library(randomForest)                               ### Libraries requiured ### 
input_data = read.csv("BoF_clean.csv",header = T)   ### Load the dataset ####
summary(input_data)                                 #### See the summary ####
smp_size <- floor(0.70 * nrow(input_data))          ### Splitting the dataset into 70% training and 30% testing ####
set.seed(1123)
train_row <- sample(seq_len(nrow(input_data)), size = smp_size)
train_data <- input_data[train_row,]
test_data <- input_data[-train_row,]

plot(input_data$O2_Blow,input_data$Delta_HM_Bath_Temp)

#### Building RF model on the train dataset ###
yy = train_data$Delta_HM_Bath_Temp
xx = train_data[,-12]
colnames(train_data)
colnames(test_data)
rf1 = randomForest(xx,yy,ntree=500,mtry=10,nodesize =5,importance = TRUE)
########
varImpPlot(rf1,type=2)
########
pred_rf1 = predict(rf1,test_data)             #### Using test set to predict ###
########
c(test_data[1,])  #check a single record data
one_test_record <- test_data[1,]
######## Test 1 prediction
predict(rf1,one_test_record)   
##### Function to calculate MAE ####
MAE = function(m, o){
  mean(abs(m-o),na.rm = T)
}
########
MAE(train_data$Delta_HM_Bath_Temp,rf1$predicted)  #### Model performance ####
cor(test_data$Delta_HM_Bath_Temp,pred_rf1)^2      ### R-Square between predictor and actual
save(rf1, file='Bath_Temp_predict_model.RData')     ## saving the model
