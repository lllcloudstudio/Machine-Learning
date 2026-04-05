library(boot)
table=read.csv('/Users/guest/Desktop/Github/ML/data/kolachalama_data.csv',head=TRUE)
attach(table)

# Recursive function to drop variables and assess via CV
recursive_cv_assessment <- function(data, response, predictors) {
  # 1. Define model formula
  formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
  model_form <- as.formula(formula_str)
  
  # 2. Fit model and perform 10-fold CV
  model <- glm(model_form, data = data)
  cv_res <- cv.glm(data, model, K = 10)
  cv_error <- cv_res$delta[1]
  cv_K <- cv_res$K

  cat(formula_str, ",", round(cv_error, 4), ",", cv_K, file="/Users/Guest/Desktop/Github/ML/summary/2/out.file",append=TRUE)
  #cat(formula_str, ",", round(cv_error, 4), ",", cv_K,"\n", file="/Users/Guest/Desktop/Github/ML/summary/2/out.file",append=TRUE)
  


  # 3. Base case: if only one predictor left, stop
  if (length(predictors) == 1) {
    return(NULL)
  }
  
  # 4. Recursive Step: Remove one predictor and recurse
  for (i in 1:length(predictors)) {
    new_predictors <- predictors[-i]
    cat(i,"\n", file="/Users/Guest/Desktop/Github/ML/summary/2/out.file",append=TRUE)
    # For demonstration, only recurse by dropping the last added, 
    # otherwise, this becomes combinatorial.
  }
  # Simple recursion example: Drop the last predictor in the list
  recursive_cv_assessment(data, response, predictors[-length(predictors)])
}

# --- Example Usage ---
# Use mtcars dataset, predicting mpg
#data(mtcars)
# Initial variables
#vars <- c("wt", "hp", "disp")

# Run recursive function
#recursive_cv_assessment(mtcars, "mpg", vars)
###########################



vars=c("ph","SBP","DBP","Hb","WBC","Platelet","BUN","Creatinine","HCO3","HbA1c","LDL","HDL","TG","Tsat","Ferritin")

# No 2 loops
for (i in 1:length(vars)){
recursive_cv_assessment(table,vars[i],vars[-i])

}
########################################################################################################


table_kval_assessment <- function(data,kval,start) {
  #formula_str <- paste(length(colnames(data)),",K=",kval,",nstart=",start)
  #model_form <- as.formula(formula_str)
  kmodel=kmeans(data,kval,start)
  kmodel.withinss=kmodel$withinss
  kmodel.tot.withinss=kmodel$tot.withinss

  cat(kval,",",start,",",length(colnames(table)),",",kmodel.withinss,",",kmodel.tot.withinss,"\n",  file="/Users/Guest/Desktop/Github/ML/summary/2/out_1.file",append=TRUE)
    } #formula_str,",",

start=c(1,20,50)
kval=c(2,3,4,5,6)
vars=c("ph","SBP","DBP","Hb","WBC","Platelet","BUN","Creatinine","HCO3","HbA1c","LDL","HDL","TG","Tsat","Ferritin")
table=table[,vars]

for (i in 1:length(kval)){
table_kval_assessment(table,kval[i],20)
}
for (i in 1:length(kval)){
table_kval_assessment(table,kval[i],50)
}
for (i in 1:length(kval)){
table_kval_assessment(table,kval[i],1)
}

########################################################################################################
start=c(1,20,50)
kval=c(2,3,4,5,6)
vars=c("ph","SBP","DBP","Hb","WBC","Platelet","BUN","Creatinine","HCO3","HbA1c","LDL","HDL","TG","Tsat","Ferritin")



recursive_kmeans_assessment <- function(data, kval,start, predictors) {
  table=data[,predictors]
  # 1. Define model formula
  #formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
  #model_form <- as.formula(formula_str)
  
  # 2. Fit model and perform 10-fold CV
  #model <- glm(model_form, data = data)
  #cv_res <- cv.glm(data, model, K = 10)
  #cv_error <- cv_res$delta[1]
  #cv_K <- cv_res$K
  kmodel=kmeans(table,kval,start)
  kmodel.withinss=kmodel$withinss
  kmodel.tot.withinss=kmodel$tot.withinss

  cat(kval,",",start,",",length(colnames(table)),",",kmodel.withinss,",",kmodel.tot.withinss,"\n",  file="/Users/Guest/Desktop/Github/ML/summary/2/out_1.file",append=TRUE)
     #formula_str,",",

  #cat(formula_str, ",", round(cv_error, 4), ",", cv_K, file="/Users/Guest/Desktop/Github/ML/summary/2/out_1.file",append=TRUE)
  
  # 3. Base case: if only one predictor left, stop
  if (length(predictors) == 1) {
    return(NULL)
  }
  
  # 4. Recursive Step: Remove one predictor and recurse
  #for (i in 1:length(predictors)) {
    #new_predictors <- predictors[-i]
    #cat(i,"\n", file="/Users/Guest/Desktop/Github/ML/summary/2/out_1.file",append=TRUE)
    # For demonstration, only recurse by dropping the last added, 
    # otherwise, this becomes combinatorial.
  
  # Simple recursion example: Drop the last predictor in the list
  recursive_kmeans_assessment(table, kval,start, predictors[-length(predictors)])
}
}
#####
recursive_kmeans_assessment(table,2,1,vars)
recursive_kmeans_assessment(table,3,1,vars)
recursive_kmeans_assessment(table,4,1,vars)
recursive_kmeans_assessment(table,5,1,vars)
recursive_kmeans_assessment(table,6,1,vars)

recursive_kmeans_assessment(table,2,20,vars)
recursive_kmeans_assessment(table,3,20,vars)
recursive_kmeans_assessment(table,4,20,vars)
recursive_kmeans_assessment(table,5,20,vars)
recursive_kmeans_assessment(table,6,20,vars)

recursive_kmeans_assessment(table,2,50,vars)
recursive_kmeans_assessment(table,3,50,vars)
recursive_kmeans_assessment(table,4,50,vars)
recursive_kmeans_assessment(table,5,50,vars)
recursive_kmeans_assessment(table,6,50,vars)

