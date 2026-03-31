x=matrix(data=c(1,2,3,4),nrow=2,ncol=2)
x=matrix(data=c(1,2,3,4),2,2) #byrow=false
sqrt(x)

##### 2 var

x=rnorm95)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)

##### y var

set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

##### to plot

x=rnorm(100)
y=rnorm(100)
plot(x,y)

plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs y")
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()

##### datasets library data
library(datasets)
data()
LoadLibraries=function{
	library(datasets)
	print("The libraries have been loaded")
}

LoadLibraries()

# Loblolly, Orange, ToothGrowth # attach() 

attach(ToothGrowth)
lmodel=lm(dose~len)
lmodel
summary(lmodel)

names(lmodel)

 [1] "coefficients"  "residuals"     "effects"       "rank"         
 [5] "fitted.values" "assign"        "qr"            "df.residual"  
 [9] "xlevels"       "call"          "terms"         "model"

> lmodel$coefficients # of list
> lmodel$coefficients[1] 
> lmodel$coefficients[2]
> lmodel$coefficients[[1]]
> lmodel$coefficients[[2]]

##### pg 112-
confint()
predict()
plot()
abline()
cor()
glm()
contrasts()
coef()

##### google ai
sample()
nrow()
round()
class() # or 
is.character()
as.character()
mean()
var()
sqrt()
sd()
scale()
seq()
rnorm() #mean 0 sd 1
pairs() # 2 variable plots of data
summary() #quartiles incl min or max of each variable
cat()
combn() #package:utils
##### Stock market data

> dim(Smarket)
[1] 1250    9
> library(ISLR)
> names(Smarket)
cor(Smarket[,-9])
attach(Smarket)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial) # consider levels
glm.probs=predict(glm.fit,type="response") # log model (use model data or test data)
contrasts(Direction) # Levels


# confusion matrix for smarket data mod >.5 probability 
glm.pred=rep("Down",1250) # vector
glm.pred[glm.probs>.5]="Up" # vector selection from log model criteria probs >.5 considered up

table(glm.pred,Direction) # table of new vector and variable Direction
507+145/1250

##### Additions to data
# random sample label selection
# Example 1: Randomly select 3 letters from "A", "B", "C"
random_vec <- sample(c("A", "B", "C"), 3, replace = TRUE)
print(random_vec)

# Example 2: Select 3 random letters from the built-in alphabet (subset)
random_letters <- sample(letters[1:3], 3, replace = TRUE)
print(random_letters)

######################################
table=read.csv('/Users/guest/Desktop/Github/ML/data/kolachalama_data.csv',head=TRUE) # numerical
attach(table)
set.seed(20) # arbitrary number # category vector sample()
dim(table) #[1] 50 20 (20-80 test-train)

class(table$Gender)
sum(table$Gender==1) # count of gender 1
sum(table$Gender==2) # count of gender 2


table$Gender=ifelse(table$Gender ==1, 0,table$Gender)
table$Gender=ifelse(table$Gender ==2, 1,table$Gender) # as.numeric issue ??


#table$Gender=as.character(table$Gender)# numerical to character string
#table$Gender=gsub("1","0",table$Gender)
#table$Gender=gsub("2","1",table$Gender)

category <- sample(c(0, 1), 50, replace = TRUE) # random vector not attached to table
table[["category"]] <- category # add category with random labels

table_reduction=table[,c(2,5:11,13:21)] # reduction 1 
#table_reduction=table[,c(2,4:21)] 


#sample_size <- round(0.8 * nrow(table))
#sampled_data <- table_reduction[sample(nrow(table_reduction), size = sample_size), ]
train = (ph < 7.35) # criteria 

table_reduction_1=table_reduction[!train,] # reduction 2 
Direction=category[!train]


glm.fit=glm(Gender~SBP+DBP+Hb+WBC+Platelet+BUN+Creatinine+HCO3+ph+HbA1c+LDL+HDL+TG+Tsat+Ferritin,family="binomial",data=table_reduction,subset=train) # table_reduction_1
capture.output(summary(glm.fit), file = "/Users/guest/Desktop/Github/ML/summary/1/glm.model.txt")
glm.probs=predict(glm.fit,table_reduction_1,type="response")






glm.fit.1=glm(category~SBP+DBP+Hb+WBC+Platelet+BUN+Creatinine+HCO3+ph+HbA1c+LDL+HDL+TG+Tsat+Ferritin,family="binomial",data=table_reduction,subset=train) # table_reduction_1
#capture.output(summary(glm.fit.1), file = "/Users/guest/Desktop/Github/ML/summary/1/glm.model.1.txt")


#glm.pred=rep("0",7) # "Down"
#glm.pred[glm.probs>0.5]="1" # "Up" val:
#table(glm.pred,Direction)
# table_criteria <- table_reduction[table_reduction$Hb < 12, ] # criteria hemoglobin level < 12 to predict
# 1 or 2 category for gender
#glm.probs=predict(glm.fit,,type="response")
# consider males or females recommendations for hb or ph levels
# linear algebra PCA
######################################

pca_result=prcomp(table_reduction[,1:16],center=TRUE,scale.=TRUE)

pdf(file="/Users/guest/Desktop/Github/ML/summary/1/multiplot.pdf", width=8, height=6)
par(mfrow=c(1,2))
plot(pca_result,type="l",main="Principal Component Vectors")
plot(pca_result,main="Principal Component Vectors") # bar plot
dev.off()

pdf(file="/Users/guest/Desktop/Github/ML/summary/1/biplot.pdf",width=8,height=6)
biplot(pca_result,main="Two Principal Components of the Kolachalama Lab Data")
dev.off()

#capture.output(summary(pca_result),file='/Users/guest/Desktop/Github/ML/summary/1/pca.summary.txt')


######################################
#pca_result <- prcomp(iris.pca.data, center = TRUE, scale. = TRUE)
#names(pca_result)
#plot(pca_result,type="l")
#biplot(pca_result)
#plot(pca_result)
######################################


pca_result_1=prcomp(table_reduction[,2:17],center=TRUE,scale.=TRUE)

pdf(file = "/Users/guest/Desktop/Github/ML/summary/1/multiplot.1.pdf", width = 8, height = 6)
par(mfrow=c(1,2))
plot(pca_result_1,type="l",main="Principal Component Vectors")
plot(pca_result_1,main="Principal Component Vectors") # bar plot
dev.off()

pdf(file="/Users/guest/Desktop/Github/ML/summary/1/biplot.1.pdf",width=8,height=6)
biplot(pca_result_1,main="Two Principal Components of the Kolachalama Lab Data")
dev.off()

capture.output(summary(pca_result_1),file='/Users/guest/Desktop/Github/ML/summary/1/pca.summary.1.txt')

