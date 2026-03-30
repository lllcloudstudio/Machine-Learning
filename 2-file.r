2-file
table=read.csv('/Users/guest/Desktop/Github/ML/data/kolachalama_data.csv',head=TRUE)
attach(table)

set.seed(3) # for k-means k=2 initial obs.
kmodel=kmeans(table[,c(7,14)],2)
plot(table[,c(7,14)],col=(kmodel$cluster),main="K-means Clustering Results with K=2",pch=20,cex=2)

# loocv
library(boot)
train=table[sample(nrow(table),40,replace=FALSE),]

model_initial=glm(ph~SBP+DBP+Hb+WBC+Platelet+BUN+Creatinine+HCO3+HbA1c+LDL+HDL+TG+Tsat+Ferritin,data=table) #
model_1=glm(ph~SBP+DBP+Hb+WBC+Platelet+BUN+Creatinine+HCO3+HbA1c+LDL+HDL+TG+Tsat+Ferritin,data=table,subset=train) # table_reduction_1

----------
----------
table_reduced=table[,4:20]
n <- nrow(table_reduced)
squared_errors <- numeric(n)

for (i in 1:n) {
  # Training data: all observations except the i-th
  train_data <- table_reduced[-i, ]
  # Test data: only the i-th observation
  test_data <- table_reduced[i, ]

  # Fit the model (e.g., linear regression) on the training data
  fit <- lm(ph ~ Hb + LDL, data = train_data) #mpg hp LDL literature --OK

  # Predict the value for the left-out observation
  prediction <- predict(fit, newdata = test_data)

  # Calculate the squared error and store it
  actual_value <- test_data$ph 
  squared_errors[i] <- (actual_value - prediction)^2
}

# Calculate the Mean Squared Error (MSE)
mean_mse <- mean(squared_errors)
print(paste("Manual LOOCV Mean Squared Error:", mean_mse)) #[1] 10.47764
----------
----------
----------
----------
----------
----------
----------
# Using the built-in 'mtcars' dataset
#data(mtcars)
#n <- nrow(mtcars)
#squared_errors <- numeric(n)

#for (i in 1:n) {
  # Training data: all observations except the i-th
  #train_data <- mtcars[-i, ]
  # Test data: only the i-th observation
  #test_data <- mtcars[i, ]

  # Fit the model (e.g., linear regression) on the training data
  #fit <- lm(mpg ~ hp + wt, data = train_data)

  # Predict the value for the left-out observation
  #prediction <- predict(fit, newdata = test_data)

  # Calculate the squared error and store it
  #actual_value <- test_data$mpg
  #squared_errors[i] <- (actual_value - prediction)^2
#}

# Calculate the Mean Squared Error (MSE)
#mean_mse <- mean(squared_errors)
#print(paste("Manual LOOCV Mean Squared Error:", mean_mse))
----------

# same
glm.fit=glm(ph~Hb,data=table)
coef(glm.fit)
lm.fit=lm(ph~Hb,data=table)
coef(lm.fit) #just a check since 1 variable, use coef, same model
----------
library(boot)
table=read.csv('/Users/guest/Desktop/Github/ML/data/kolachalama_data.csv',head=TRUE)
attach(table)


glm.fit=glm(ph~Hb,data=table) #  see line 17
cv.err=cv.glm(table,glm.fit)
cv.err$delta 
# [1] 10.31443 10.30631 2 values differ

glm.fit=glm(ph~Hb,data=table)
cv.error=cv.glm(table,glm.fit,K=5) # not poly, 
cv.error$delta 
#[1] 10.88180 10.73039

glm.fit=glm(ph~Hb,data=table)
cv.error=cv.glm(table,glm.fit,K=10) # not poly, 
cv.error$delta 
# [1] 10.43601 10.38702

--

# cv.glm() limits total names to 4: K call delta seed
glm.fit=glm(ph~Hb+LDL,data=table)
cv.error=cv.glm(table,glm.fit,K=5) # not poly, 
cv.error$delta  
#[1] 9.910736 9.827394

glm.fit=glm(ph~Hb+LDL,data=table)
cv.error=cv.glm(table,glm.fit,K=10) # not poly, 
cv.error$delta  
#[1] 10.78344 10.69450

glm.fit=glm(ph~Hb+LDL+HDL,data=table)
cv.error=cv.glm(table,glm.fit,K=10) # not poly, 
cv.error$delta  
# [1] 11.56192 11.42872

glm.fit=glm(ph~Hb+LDL+HDL,data=table)
cv.error=cv.glm(table,glm.fit,K=5) # not poly, 
cv.error$delta 
#[1] 13.77369 13.18647 

glm.fit=glm(ph~SBP+DBP+Hb+WBC+Platelet+BUN+Creatinine+HCO3+HbA1c+LDL+HDL+TG+Tsat+Ferritin,data=table)
cv.error=cv.glm(table,glm.fit,K=5)
cv.error$delta 
#[1] 254.1249 204.3427

glm.fit=glm(ph~SBP+DBP+Hb+WBC+Platelet+BUN+Creatinine+HCO3+HbA1c+LDL+HDL+TG+Tsat+Ferritin,data=table)
cv.error=cv.glm(table,glm.fit,K=10)
cv.error$delta 
# [1] 62.44892 56.79505
----------
# no need to set seed
cv.error=rep(0,5)
for(i in 1:5){
	glm.fit=glm(ph~poly(Hb,i),data=table) # 
	cv.error[i]=cv.glm(table,glm.fit)$delta[1]
}
cv.error
# [1] 10.31443 10.73992 11.45616 11.69210 13.87564 Higher MSE poly()


----------

> set.seed(3) # for k-means k=2 initial obs.
> kmodel=kmeans(table[,c(7,14)],2)
> kmodel$tot.withinss
[1] 117.2138
> kmodel$withinss
[1] 93.01711 24.19669

> #plot(table[,c(7,14)],col=(kmodel$cluster+1),main="K-means Clustering Results with K=2",pch=20,cex=2)
> kmodel=kmeans(table[,c(7,14)],2,nstart=1)
> kmodel$withinss
[1] 93.01711 24.19669
> kmodel$tot.withinss
[1] 117.2138

> kmodel=kmeans(table[,c(7,14)],2,nstart=20)
> kmodel$tot.withinss
[1] 117.2138
> kmodel$withinss
[1] 93.01711 24.19669

> kmodel=kmeans(table[,c(7,14)],2,nstart=50)
> kmodel$withinss
[1] 24.19669 93.01711
> kmodel$tot.withinss
[1] 117.2138

-------------------------
> set.seed(3) # for k-means k=2 initial obs.
> kmodel=kmeans(table[,c(7,14)],2)
> kmodel$tot.withinss
[1] 117.2138
> kmodel$withinss
[1] 93.01711 24.19669

> #plot(table[,c(7,14)],col=(kmodel$cluster+1),main="K-means Clustering Results with K=2",pch=20,cex=2)
> kmodel=kmeans(table[,c(7,14)],3,nstart=1)
> kmodel$withinss
[1] 16.10500 24.19669 18.95800
> kmodel$tot.withinss
[1] 59.25969

> kmodel=kmeans(table[,c(7,14)],4,nstart=20)
> kmodel$tot.withinss
[1] 41.38811
> kmodel$withinss
[1] 18.958000  3.718114  2.607000 16.105000

> kmodel=kmeans(table[,c(7,14)],5,nstart=50)
> kmodel$withinss
[1] 9.552308 2.607000 3.718114 6.322222 3.077500
> kmodel$tot.withinss
[1] 25.27714

> kmodel=kmeans(table[,c(7,14)],6,nstart=50)
> kmodel$withinss
[1] 0.245000 3.718114 2.607000 1.827500 3.592143 5.311111
> kmodel$tot.withinss
[1] 17.30087

> kmodel=kmeans(table[,c(7,14)],6,nstart=20)
> kmodel$tot.withinss
[1] 17.65165
> kmodel$withinss
[1] 6.322222 0.245000 1.681818 3.077500 2.607000 3.718114
> 

-----------------------------
# scaled start
scaled_table=scale(table)

pdf('/Users/Guest/Desktop/Github/ML/summary/2/hclust.pdf', width=8, height=6)
plot(hclust(dist(scaled_table)),main="Scaled HC Complete Linkage",xlab="",sub="",cex=.9)
dev.off()

hc.complete=hclust(dist(table[,c(7,14)]),method="complete")
hc.average=hclust(dist(table[,c(7,14)]),method="average")
hc.single=hclust(dist(table[,c(7,14)]),method="single")






> #plot(table[,c(7,14)],col=(kmodel$cluster),main="K-means Clustering Results with K=2",pch=20,cex=2)
> #names(kmodel)
[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
[6] "betweenss"    "size"         "iter"         "ifault"      



-----------------------------

library(boot)

# Recursive function to drop variables and assess via CV
recursive_cv_assessment <- function(data, response, predictors,k) {  # or no k
  # 1. Define model formula
  formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
  model_form <- as.formula(formula_str)
  
  # 2. Fit model and perform 10-fold CV
  model <- glm(model_form, data = data)
  cv_res <- cv.glm(data, model, K = k) # 10
  cv_error <- cv_res$delta[1]


  cat( formula_str, ",", round(cv_error, 4),",","K=",cv_res$K, "\n", file="/Users/Guest/Desktop/Github/ML/summary/2/out.file",append=TRUE)
  #cat("Formula:", formula_str, "| CV Error:", round(cv_error, 4), "\n", file="/Users/Guest/Desktop/Github/ML/summary/2/out.file",append=TRUE)


  # 3. Base case: if only one predictor left, stop
  if (length(predictors) == 1) {
    return(NULL)
  }
  
  # 4. Recursive Step: Remove one predictor and recurse
  for (i in 1:length(predictors)) {
    new_predictors <- predictors[-i]
    # For demonstration, only recurse by dropping the last added, 
    # otherwise, this becomes combinatorial.
  }
  # Simple recursion example: Drop the last predictor in the list
  recursive_cv_assessment(data, response, predictors[-length(predictors)],k)
}

# --- Example Usage ---
# Use mtcars dataset, predicting mpg
data(mtcars)
# Initial variables
vars <- c("wt", "hp", "disp")

# Run recursive function
recursive_cv_assessment(mtcars, "mpg", vars)










