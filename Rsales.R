#Reading the dataset
SalesOpportunity<- read.csv("Salesopportunity.csv",header = T)
str(SalesOpportunity)

#Checking for the missing vaules
sapply(SalesOpportunity,function(x) sum(is.na(x)))
sapply(SalesOpportunity,function(x) sum(is.null(x)))

prop.table(table(SalesOpportunity$Opportunity.Result))
table(SalesOpportunity$Opportunity.Result)

library(DMwR)

library(pROC)
install.packages("grid")
install.packages("pROC")

SalesOpportunity_n<- SMOTE(Opportunity.Result ~ .,SalesOpportunity, perc.over = 100, perc.under = 200)


write.csv(file="Finalsale.csv",SalesOpportunity_n, row.names = F)

Sales<- read.csv("Finalsale.csv",header = T)


#Filtering the numeric data for the finding the correlation between the independent varaible 
num_sata<-sapply(Sales,function(x){is.numeric(x)})
Sales_cor_input<-Sales[,num_sata]

#Filtering the independent varibles which has high correlation 
library(caret)
highlyCorDescr <- findCorrelation(cor(Sales_cor_input), cutoff = .5)
filteredDescr <- Sales[,-highlyCorDescr]

#write csv
write.csv(file="Finalsale.csv", filteredDescr, row.names = F)


str(filteredDescr)

#Removing the unwanted variable 
SalesOpportunity$Opportunity.Number <- NULL


library(caret)
sample <- createDataPartition(filteredDescr$Opportunity.Result, p = .75, list = FALSE)
train <- SalesOpportunity[sample, ]
prop.table(table(SalesOpportunity$Opportunity.Result))
test <- SalesOpportunity[-sample, ]


library(randomForest)
rf <- randomForest(Opportunity.Result ~ ., data=train,importance=TRUE, ntree=200)
varImpPlot(rf)

#Re-running 
#rf_new <- randomForest(status~ MDVP.Fo.Hz.+ PPE + spread1 + MDVP.Fhi.Hz.+spread2, data=train,importance=TRUE, ntree=100)
final <- predict(rf, test, type = "class")
caret::confusionMatrix(final, test$Opportunity.Result, positive = "Won")
install.packages("e1071")
