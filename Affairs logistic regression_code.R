Affairs <- read.csv(file.choose())
View(Affairs)
sum(is.na(Affairs))
colnames(Affairs)
library(dplyr)
Affairs["affairs"] <- if_else(Affairs$affairs <= 0, 0, 1)
summary(Affairs)


install.packages('AER')
data(Affairs,package="AER")
dim(Affairs)

Affairs$gender= as.factor(Affairs$gender)
str(Affairs)

# Preparing a linear regression 
mod_lm <- lm(affairs~.,data=Affairs)
pred1 <- predict(mod_lm,Affairs)
pred1
plot(Affairs$gender,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)




# GLM function use sigmoid curve to produce desirable results 

model <- glm(affairs~.,data=Affairs,family = "poisson")

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,Affairs,type="response")
summary(model)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,Affairs$affairs)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
Affairs[,"prob"] <- prob
Affairs[,"pred_values"] <- pred_values
Affairs[,"yes_no"] <- yes_no

View(Affairs[,c(1,10:12)])

table(Affairs$affairs,Affairs$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
