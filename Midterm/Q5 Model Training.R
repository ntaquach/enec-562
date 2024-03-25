```{r message=F, warning=F, results=F}
# Question 5

library(gtools)
pastePerm<- function(row, names){
  keep<- which(row==1)
  if(length(keep)==0){
    return('1')
  }else{
    return(paste(names[keep],collapse='+'))
  }
}
dredgeform<- function(pred, covars, alwaysIn=''){
  p<- length(covars)
  perm.tab<- permutations(2, p, v=c(0,1), repeats.allowed=T)
  myforms<- NULL
  for(j in 1:nrow(perm.tab)){
    myforms[j]<- pastePerm(perm.tab[j,], covars)
  }
  myforms<- paste0(pred, '~ 1', alwaysIn,'+', myforms)
  return(myforms)
}

allformulas<- dredgeform(pred = "log.ozone", covars = c("rad", "temp", "wind", "rad*wind", "rad*temp", "wind*temp"))
allformulas <- allformulas[2:length(allformulas)] 


#run model
set.seed(123)
compare_var <- as.data.frame(matrix(ncol = 4, nrow = 0))
colnames(compare_var) <- c("formula", "RMSE", "R2", "MAE")

for ( i in 1:length(allformulas)) {
  
  train.control <- trainControl(method = "repeatedcv", number = 3, repeats = 100)
  # Train the full model
  model_full <- train(as.formula(allformulas[i]), data = ozone, method = "lm",
                      trControl = train.control)
  # Summarize the results
  compare_var[i, 1] <- allformulas[i]
  compare_var[i, 2] <- mean(model_full$resample$RMSE)
  compare_var[i, 3] <- mean(model_full$resample$Rsquared, na.rm = T)
  compare_var[i, 4] <- mean(model_full$resample$MAE)
  
  
}

compare_var$prediction_error_rate <- compare_var$RMSE/mean(ozone$ozone) 

compare_var %>% arrange(prediction_error_rate)

compare_var %>% arrange(RMSE)


```