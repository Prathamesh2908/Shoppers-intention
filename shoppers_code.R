
#Read data 
shoppers <- read.csv("C:/Users/prath/Desktop/DMML dataset/online_shoppers_intention/online_shoppers_intention.csv")

#structure
str(shoppers)
summary(shoppers)


#missing value analysis
sapply(shoppers, function(x) sum(is.na(x)))
shoppers <- na.omit(shoppers)
str(shoppers)
unique(shoppers$Month)

#fix the structure
shoppers$Revenue <- gsub(FALSE, 0, shoppers$Revenue)
shoppers$Revenue <- gsub(TRUE, 1, shoppers$Revenue)
shoppers$Weekend <- gsub(TRUE, 1, shoppers$Weekend)
shoppers$Weekend <- gsub(FALSE, 0, shoppers$Weekend)

shoppers$Month <- factor(shoppers$Month, levels = c("Feb", "Mar", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE)
shoppers$OperatingSystems <- factor(shoppers$OperatingSystems)
shoppers$Browser <- factor(shoppers$Browser)
shoppers$Region <- factor(shoppers$Region)
shoppers$TrafficType <- factor(shoppers$TrafficType)
shoppers$VisitorType <- factor(shoppers$VisitorType)
shoppers$Revenue <- factor(shoppers$Revenue)
shoppers$Weekend <- factor(shoppers$Weekend)
str(shoppers)

#Performing basics statistical analysis
summary(shoppers[,c(1:10)])
table(shoppers$Revenue)
table(shoppers$Weekend)
table(shoppers$VisitorType)
table(shoppers$TrafficType)
table(shoppers$Region)
table(shoppers$Browser)
table(shoppers$OperatingSystems)
table(shoppers$Month)



#correlation plot
library(corrplot)
correlation <- cor(shoppers[,c(1:10)])
corrplot(correlation, method = "square", type = "lower", diag = TRUE)


#Relationship between Exit Rates and Bounce Rates 
library(ggplot2)
options(repr.plot.width = 8, repr.plot.height = 5)
ggplot(data = shoppers, mapping = aes(x = BounceRates, y = ExitRates)) + geom_point(mapping = aes(color = Revenue)) + geom_smooth(se = TRUE, alpha = 0.5) + theme_light() + ggtitle("Relationship between Exit Rates and Bounce Rates") + xlab("Bounce Rates") + ylab("Exit Rates") + geom_text(mapping = aes(x = 0.15, y = 0.05, label = "Correlation = 0.913"))


#Preprocessing the data
library(plyr)
#converting into factors
shoppers$Month <- factor(shoppers$Month, order = TRUE, levels =c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'))
#mapping the values to the months 
shoppers$Month_num <- mapvalues(shoppers$Month, from = c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'), to = c(1,2,3,4,5,6,7,8,9,10))
#converting into factors
shoppers$VisitorType <- factor(shoppers$VisitorType, order = TRUE, levels = c('Returning_Visitor', 'Other', 'New_Visitor'))
#mapping the values to returning visitor, other and new visitor
shoppers$VisitorType_Num <-mapvalues(shoppers$VisitorType, from = c("Returning_Visitor", "Other", "New_Visitor"), to = c(1,2,3))
#converting into factors
shoppers$OperatingSystems <- factor(shoppers$OperatingSystems, order = TRUE, levels = c(6,3,7,1,5,2,4,8))
#converting into factors
shoppers$Browser <- factor(shoppers$Browser, order = TRUE, levels = c(9,3,6,7,1,2,8,11,4,5,10,13,12))
#converting into factors
shoppers$Region <- factor(shoppers$Region, order = TRUE, levels = c(8,6,3,4,7,1,5,2,9))
#converting into factors
shoppers$TrafficType <- factor(shoppers$TrafficType, order = TRUE, levels = c(12,15,17,18,13,19,3,9,1,6,4,14,11,10,5,2,20,8,7,16))
#converting into factors
shoppers$Weekend <- ifelse(shoppers$Weekend == TRUE, 1, 0)
str(shoppers)


# Splitting the data in test and train
library(caret)
set.seed(777)
split  <- createDataPartition(shoppers$Revenue, p = 0.8, list = FALSE)
train_shop <- shoppers[split,]
test_shop <- shoppers[-split,]

train_shop$Revenue <- as.factor(train_shop$Revenue)
test_shop$Revenue <- as.factor(test_shop$Revenue)


split  <- createDataPartition(shoppers$Revenue, p = 0.8, list = FALSE)
train_copy <- shoppers[split,]
test_copy <- shoppers[-split,]

test_copy$Revenue[test_copy$Revenue == "FALSE"] <- "0"
test_copy$Revenue[test_copy$Revenue == "TRUE"] <- "1"

train_copy$Revenue[train_copy$Revenue == "FALSE"] <- "0"
train_copy$Revenue[train_copy$Revenue == "TRUE"] <- "1"

train_copy$Revenue <- as.factor(train_copy$Revenue)
test_copy$Revenue <- as.factor(test_copy$Revenue)

train_copy <- select(train_copy, -Browser)
test_copy <- select(test_copy, -Browser)

 
#logistic Regression model

library(ISLR)
model_glm <- glm(Revenue ~., family=binomial(link='logit'),data=train_copy)
summary(model_glm)

pred_glm <- predict(model_glm, newdata=test_copy )

pred_glm[pred_glm < 0.5] <- 0
pred_glm[pred_glm > 0.5] <- 1

pred_glm <- as.factor(pred_glm)

accuracy_glm <- confusionMatrix(table(test_copy$Revenue , pred_glm))
accuracy_glm

library(pROC)
library(ROCR)



#For training set
roc(train_copy$Revenue,model_glm$fitted.values,plot=T,col="red",print.auc=T,legacy.axes=TRUE,percent = T,
    xlab="False Positive percentage",ylab="True Positive percentage",lwd=5,main="ROC Curve")


#Naivebayes model

library(arules)
library(data.tree)
library(caret)
library(e1071)

library(naivebayes)


model_nb <- naiveBayes(Revenue ~ .,data = test_copy)
pred_nb <- predict(model_nb,test_copy)


accuracy_nb <- confusionMatrix(table(test_copy$Revenue, pred_nb))
accuracy_nb




