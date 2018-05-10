
rm(list=ls())                                       ### Remove all variables from WS ####
#install.packages("randomForest")
#setwd("/Users/anshul/Downloads")                    # Setting the wd ####
library(randomForest)                               ### Libraries requiured ### 
input_data_retrain = read.csv("BoF_clean_retrain.csv",header = T)   ### Load the dataset ####
summary(input_data_retrain)                                 #### See the summary ####

#### Building RF model on the train dataset ###
yy = input_data_retrain$Delta_HM_Bath_Temp
xx = input_data_retrain[,-12]
rf1_retrain = randomForest(xx,yy,ntree=500,mtry=10,nodesize =5,importance = TRUE)
########
varImpPlot(rf1_retrain,type=2)
########
pred_rf1_retrain = predict(rf1_retrain,input_data_retrain)             #### Using test set to predict ###
########
##### Function to calculate MAE ####
MAE = function(m, o){
  mean(abs(m-o),na.rm = T)
}
########
MAE(input_data_retrain$Delta_HM_Bath_Temp,rf1_retrain$predicted)  #### Model performance ####
cor(input_data_retrain$Delta_HM_Bath_Temp,pred_rf1_retrain)^2      ### R-Square between predictor and actual
save(rf1_retrain, file='Bath_Temp_predict_model_retrain.RData')     ## saving the model
