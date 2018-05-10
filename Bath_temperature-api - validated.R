#setwd("/Users/anshul/Downloads")
library(rpart)
library(jsonlite)
#install.packages("gmailr")
library(gmailr)
load("Bath_Temp_predict_model.RData")

validate_feature_inputs <- function(rangevar) {
  range_valid <- (rangevar >=0 & rangevar<=1)
  tests <- c("input value should be between 0 and 1")
  test_results <- c(range_valid)
  
  if(!all(test_results)) {
    failed <- which(!test_results)
      email_msg <- mime(
        To = "verma.anshul31@gmail.com",
        Subject = "Range validation failed - check input values",
        body = "Can you hear me now?")
      send_message(email_msg)
    return(tests[failed])
  } else {
    return("OK")
  }
}


#* @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}


#* @post /predict
#* @get /predict
predict_DeltaTemp <-function(
  HM_Si=1, O2_Blow=1, BLOW_LIME_MATWEIGHT_Combined=1
  , BLOW_IRONOR_MATWEIGHT_Combined=1, BLOW_DOLO_MATWEIGHT_Combined=1
  , BIN2_RELATIVE_LANCE_HEIGHT=1, BIN3_RELATIVE_LANCE_HEIGHT=1
  , Delta_Carbon_Weight=1 , BIN2_SECVENTURIOPENING=1
  , HM.Temp_Combined=1, RefractoryLife_Combined=1
  , Delta_HM_Bath_Temp=1 , ACTOXYGENMOMENT_DOLO=1
  , PreviousHeat_BathTemp=1 , HM_Weight=1
  , Total_Scrap=1 , loss_radiation_hm=1
  , loss_convection_hm=1 , loss_radiation_vessel=1
  , loss_convection_vessel=1
) {
  
  # validations
  validate_HM_Si <- validate_feature_inputs(HM_Si)
  if (validate_HM_Si[1] == "OK") 
    {
        var1=as.integer(HM_Si)
        var2=as.integer(O2_Blow)
        var3=as.integer(BLOW_LIME_MATWEIGHT_Combined)
        var4=as.integer(BLOW_IRONOR_MATWEIGHT_Combined)
        var5=as.integer(BLOW_DOLO_MATWEIGHT_Combined)
        var6=as.integer(BIN2_RELATIVE_LANCE_HEIGHT)
        var7=as.integer(BIN3_RELATIVE_LANCE_HEIGHT)
        var8=as.integer(Delta_Carbon_Weight)
        var9=as.integer(BIN2_SECVENTURIOPENING)
        var10=as.integer(HM.Temp_Combined)
        var11=as.integer(RefractoryLife_Combined)
        var12=as.integer(Delta_HM_Bath_Temp)
        var13=as.integer(ACTOXYGENMOMENT_DOLO)
        var14=as.integer(PreviousHeat_BathTemp)
        var15=as.integer(HM_Weight)
        var16=as.integer(Total_Scrap)
        var17=as.integer(loss_radiation_hm)
        var18=as.integer(loss_convection_hm)
        var19=as.integer(loss_radiation_vessel)
        var20=as.integer(loss_convection_vessel)
        
        test_ds <- data.frame(HM_Si=var1, O2_Blow=var2,
                              BLOW_LIME_MATWEIGHT_Combined=var3, BLOW_IRONOR_MATWEIGHT_Combined=var4,
                              BLOW_DOLO_MATWEIGHT_Combined=var5,BIN2_RELATIVE_LANCE_HEIGHT=var6,
                              BIN3_RELATIVE_LANCE_HEIGHT=var7,Delta_Carbon_Weight=var8,
                              BIN2_SECVENTURIOPENING=var9,HM.Temp_Combined=var10,
                              RefractoryLife_Combined=var11,Delta_HM_Bath_Temp=var12,
                              ACTOXYGENMOMENT_DOLO=var13,PreviousHeat_BathTemp=var14,
                              HM_Weight=var15,Total_Scrap=var16,
                              loss_radiation_hm=var17,loss_convection_hm=var18,
                              loss_radiation_vessel=var19,loss_convection_vessel=var20)
        prediction <- predict(rf1, test_ds)
        write.table(c(test_ds,prediction),file ="test_ds.csv", append=TRUE, col.names = FALSE, sep = ",")
        paste("The predicted delta temerature is: ", list(Delta_BathTemp=unbox(prediction)))
        #return(list(Delta_BathTemp=unbox(prediction)))
  
  } 
  else 
    {
    result <- list(
      input = list(HM_Si = HM_Si),
      response = list(input_error = validate_HM_Si),
      status = 400,
      model_version = 1)
  }
}

