library(boot)
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

table=read.csv('/Users/guest/Desktop/Github/ML/data/kolachalama_data.csv',head=TRUE)
attach(table)

vars=c("ph","SBP","DBP","Hb","WBC","Platelet","BUN","Creatinine","HCO3","HbA1c","LDL","HDL","TG","Tsat","Ferritin")

# No 2 loops
for (i in 1:length(vars)){
recursive_cv_assessment(table,vars[i],vars[-i])

}

#list of combinations
# Example list/vector
#my_list <- c("A", "B", "C", "D")

# Generate combinations of 2
#pairs <- combn(my_list, 2)

# Display result
#print(pairs)
# To see as a list of pairs:
#pair_list <- combn(my_list, 2, simplify = FALSE)


########################################################################################################
table_kval_assessment <- function(data,kval,start) {
  # 1. Define model formula
  formula_str <- paste(length(colnames(data)),",K=",kval,",nstart=",start)
  model_form <- as.formula(formula_str)
  
  # 2. Fit model and perform 10-fold CV
  kmodel=kmeans(data,kval,start)
  kmodel.withinss=kmodel$withinss
  kmodel.tot.withinss=kmodel$tot.withinss
  
  #model <- glm(model_form, data = data)
  #cv_res <- cv.glm(data, model, K = 10)
  #cv_error <- cv_res$delta[1]
  #cv_K <- cv_res$K

  cat(formula_str, ",", kval,",",start, file="/Users/Guest/Desktop/Github/ML/summary/2/out_1.file",append=TRUE)
    }
  #cat(formula_str, ",", round(cv_error, 4), ",", cv_K,"\n", file="/Users/Guest/Desktop/Github/ML/summary/2/out.file",append=TRUE)
  


  # 3. Base case: if only one predictor left, stop
  #if (length(predictors) == 1) {
    #return(NULL)
  #}
  
  # 4. Recursive Step: Remove one predictor and recurse
  #for (i in 1:length(predictors)) {
    #new_predictors <- predictors[-i]
    #cat(i,"\n", file="/Users/Guest/Desktop/Github/ML/summary/2/out_1.file",append=TRUE)
    # For demonstration, only recurse by dropping the last added, 
    # otherwise, this becomes combinatorial.

  # Simple recursion example: Drop the last predictor in the list
  
table_kval_assessment(table,m)

# 20

20,50
3,4,5,6



