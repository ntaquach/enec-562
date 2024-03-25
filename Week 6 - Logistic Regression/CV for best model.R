
sdat <- sdat %>% 
  mutate(stat_name = ifelse(stat == 0, "Zero_Label", "One_Label"))

set.seed(234)
library(caret)
library(gtools)
pastePerm<- function(row, names){
  keep<- which(row==1)
  if(length(keep)==0){
    return('1')
  }else{
    return(paste(names[keep],collapse='+'))
  }
}
my_sqrt <- function(var1){
  sqrt(var1)
}
dredgeform<- function(pred, covars, alwaysIn=''){
  p<- length(covars)
  perm.tab<- permutations(2, p, v=c(0,1), repeats.allowed=T)
  myforms<- NULL
  for(j in 1:nrow(perm.tab)){
    myforms[j]<- pastePerm(perm.tab[j,], covars)
  }
  myforms<- paste0(pred, '~',myforms)
  return(myforms)
}
allformulas<- dredgeform(pred = "stat_name", covars = c("AE", "AG", "BH", "FL", "HL",
                                                        "KL", "SK", "TL", "TT", "WT"))

compare_var <- as.data.frame(matrix(ncol = 4, nrow = 0))
colnames(compare_var) <- c("formula", "AUC", "sensitivity", "specificity")

for ( i in 2:length(allformulas)) {
  
  train.control <- trainControl(method = "repeatedcv", number = 3, repeats = 10, 
                                summaryFunction=twoClassSummary, 
                                classProbs=T,
                                savePredictions = T)
  
  # Train the full model
  model <- train(as.formula(allformulas[i]), data = sdat, method = "glm", family = "binomial", trControl = train.control, metric = "ROC")
  
  # Summarize the results
  compare_var[i, 1] <- allformulas[i]
  compare_var[i, 2] <- model$results$ROC
  compare_var[i, 3] <- model$results$Sens
  compare_var[i, 4] <- model$results$Spec
  
  
}

compare_var %>% arrange(-AUC)
