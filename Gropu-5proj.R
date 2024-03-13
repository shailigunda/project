#Group-5 BA with R 6356.502
# Clear the workspace
rm(list=ls())
cat("\014")
# Libraries
library(dplyr)
library(ggplot2)
library(GGally)
library(lattice)
library(rpart)
library(rpart.plot)
library(caret)
library(car)
library(data.table)

# Setting the working directory
setwd("C:/Users/spars/Desktop/SEM 1/BA with R")

# Loading the data
ospi_df <- read.csv('online_shoppers_intention.csv')


### Data Pre-Processing ###

ospi_df <- na.omit(ospi_df)
df <- ospi_df


# Changing Month Names
old_names <- c("Feb", "Mar", "May", "Oct",  "June", "Jul", "Aug", "Nov", "Sep", "Dec" )
new_names <- c("February", "March", "May", "October", "June", "July", "August", "November", "September",  "December")
df$Month <- factor(df$Month, old_names, new_names)
df$Month <- factor(df$Month,levels = month.name)


### Converting To Factor Levels For Remaining Variable ###
df$OperatingSystems = as.factor(df$OperatingSystems)
df$Browser = as.factor(df$Browser)
df$Region = as.factor(df$Region)
df$TrafficType = as.factor(df$TrafficType)
df$VisitorType = as.factor(df$VisitorType)
df$Weekend = as.factor(df$Weekend)
df$Revenue = as.factor(df$Revenue)

library(ggplot2)

### Revenue Wise Count ####
rev <- ggplot(df, aes(Revenue)) + 
  geom_bar(stat="count", fill="blue")+ 
  geom_text(aes(label = ..count..), stat = "Count",colour = "black", size = 3, 
            vjust = 1.5, position = position_dodge(0.9)) 
rev

### Visitor Wise Count ###
vis <- ggplot(df, aes(VisitorType))+
  geom_bar(stat="count", fill="light blue")+
  geom_text(aes(label = ..count..), stat = "Count",colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(0.9))
vis

### Month Wise Count ###
mon <- ggplot(df, aes(factor(Month, levels = month.name)))+
  geom_bar(stat="count", fill=" dark blue")+
  geom_text(aes(label = ..count..), stat = "Count",colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(0.9))
mon

library(dplyr)

### Month Wise Visitors Count ###
mon_vis <- df%>%group_by(Month,VisitorType)%>%summarise(total=n())
vis_mon <- ggplot(mon_vis, aes( y=total, x=factor(Month), fill = VisitorType)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = total), size = 2, color = "black",
            vjust = 1.5, position = position_dodge(0.9))
vis_mon

### Month Wise Revenue Count ###
mon_rev <- df%>%group_by(Month,Revenue)%>%summarise(total=n())
rev_mon <- ggplot(mon_rev, aes( y=total, x=factor(Month), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = total), size = 2, color = "black",
            vjust = 1.5, position = position_dodge(0.9))
rev_mon

### Browser Wise Revenue Count ###
brow_rev <- df%>%group_by(Browser,Revenue)%>%summarise(total=n())
rev_brows <- ggplot(brow_rev, aes( y=total, x=factor(Browser), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = total), size = 2, color = "black",
            vjust = 1.5, position = position_dodge(0.9))
rev_brows

### Operating Systems Wise Revenue Count ###
os_rev<- df%>%group_by(OperatingSystems,Revenue)%>%summarise(total=n())
rev_os<- ggplot(os_rev, aes( y=total, x=factor(OperatingSystems), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = total), size = 2, color = "black",
            vjust = 2, position = position_dodge(0.9))
rev_os

### Region Wise Revenue Count ###
reg_rev <- df%>%group_by(Region,Revenue)%>%summarise(total=n())
rev_reg<- ggplot(reg_rev, aes( y=total, x=factor(Region), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = total), size = 2, color = "black",
            vjust = 2, position = position_dodge(0.9)) 
rev_reg

### Traffic Wise Revenue Count ###
traff_rev <- df%>%group_by(TrafficType,Revenue)%>%summarise(total=n())
rev_traff <- ggplot(traff_rev, aes( y=total, x=factor(TrafficType), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = total), size = 2, color = "black",
            vjust = 2, position = position_dodge(0.9))
rev_traff

### Weekend-Weekday Wise Revenue Count ###
wkn_rev<- df%>%group_by(Weekend,Revenue)%>%summarise(total=n())
rev_wkn_df <- ggplot(wkn_rev, aes( y=total, x=factor(Weekend), fill = Revenue)) + 
  geom_col(position = position_dodge()) +
  geom_text(aes(label = total), size = 2, color = "black",
            vjust = 2, position = position_dodge(0.9))
rev_wkn_df

### Monthly Weekend-Weekday Traffic ###
mntly_wknd_weekdy_rev <- df%>%group_by(Month,Revenue)%>%summarise(total=n())

rev_mntly_wknd_weekdy <- ggplot(mntly_wknd_weekdy_rev ,aes(x=Month, y=total, group=Revenue , fill = Revenue)) +
  geom_line(position = position_dodge(width = 0.9)) +
  ggtitle("Monthly Weekend Weekday Traffic") +
  geom_text(aes(label = total), size = 3, color = "black",
            vjust = 2, position = position_dodge(0.9))+
  ylab("Customers Who Purchased The Products")
rev_mntly_wknd_weekdy

########## Classification ##########

# Partitioning the data
set.seed(2)

# Splitting the data into training and test data sets
train.index <- sample(1:nrow(df), nrow(df)*(2/3))

# Using the train index set to split the data set
#  Rev.train for building the model
#  Rev.test for testing the model
Rev.train<- df[train.index, ]
Rev.test <- df[-train.index, ]

########## Decision Tree ##########

library(rpart)

### Growing tree and finding the nodes
fit.big <- rpart(Revenue ~ ., 
                 data=Rev.train,
                 control=rpart.control(xval=10, minsplit=500, cp=0))
nrow(fit.big$frame) #7

# Plotting tree using built-in function
plot(fit.big, uniform=TRUE,  
     branch=0.5,         
     main="Classification Tree for Revenue Prediction",   
     margin=0.1)         
text(fit.big,  use.n=TRUE,   
     all=TRUE,           
     fancy=FALSE,            
     pretty=TRUE,           
     cex=0.8)   

# Plotting a prettier tree using rpart.plot
#install.packages('rpart.plot')
library(rpart.plot)
prp(fit.big, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10, main="Classification Tree for Revenue Prediction")
length(fit.big$frame$var[fit.big$frame$var == "<leaf>"])

### Classifying records for training data
# Extracting the vector of predicted class for each observation in Rev.train_df
rev.pred_train <- predict(fit.big, Rev.train, type="class")
# Extracting the actual class of each observation in Rev.train_df
rev.actual_train <- Rev.train$Revenue

# Generating confusion matrix for training data
library(lattice)
library(caret)
confusionMatrix(rev.pred_train, rev.actual_train)


### Classifying records for test data
# Extracting the vector of predicted class for each observation in Rev.test_df
rev.pred_test <- predict(fit.big, Rev.test, type="class")
# Extracting the actual class of each observation in Rev.test_df
rev.actual_test <- Rev.test$Revenue

# Generating confusion matrix for training data
confusionMatrix(rev.pred_test, rev.actual_test)


########## Logistic Regression ##########

# Using glm() (general linear model) with family = "binomial" to fit a logistic regression.

logit_reg <- glm(Revenue ~ ., data = Rev.train, family = "binomial") 

summary(logit_reg)

# Using predict() with type = "response" to compute predicted probabilities. 
logit_predict <- predict(logit_reg, Rev.test, type = "response")
# we choose 0.5 as the cutoff here for 1 vs. 0 classes
logit_predictclass <- ifelse(logit_predict > 0.5, "TRUE", "FALSE")
logit_predictclass <- as.factor(logit_predictclass)

# evaluating classifier performance on testing data
library(caret)
actual <- Rev.test$Revenue
predict <- logit_predictclass
confusionMatrix(predict, actual)


### ROC curve for Logistic Regression
library(ROCit)
roc_emp <- rocit(score = logit_predict, class = actual) 

# check AUC, Cutoff, TPR, FPR(=1-Specificity)
result = data.frame(cbind(AUC=roc_emp$AUC, Cutoff=roc_emp$Cutoff, 
                          TPR=roc_emp$TPR, FPR=roc_emp$FPR))
head(result)
tail(result)

# Finding the optimal point
result$diff = result$TPR - result$FPR
result[which.max(result[, c("diff")]), ]

# Plotting ROC 
plot(roc_emp, values = F, col = c(2,4))

