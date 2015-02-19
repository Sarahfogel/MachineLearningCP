data<-read.table("../pml-training.csv", sep=",", header=T)
test.set<-read.table("../pml-testing.csv", sep=",", header=T)

summary(data$X)
tail(data$X)
max(data$X)
summary(data$user_name)
head(summary(data))
grep("classe", names(data))
summary(data[,160])
names(data)
names(test.set)


install.packages("caret")
install.packages("kernlab")
install.packages("e1071")
library(caret)
library(kernlab)

library(e1071)

set.seed(100)
modelFit<-train(data$classe ~ ., data=data, method="glm")
modelFit

summary(data[,c(6:12)])

data(spam)
set.seed(100)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit.spam <- train(type ~.,data=training, method="glm")
modelFit.spam
modelFit.spam$finalModel
predictions.spam<- predict(modelFit.spam, newdata=testing)
predictions.spam
confusionMatrix(predictions.spam, testing$type)


str(data)
numeric.data<-data

for (i in 1:length(names(data))) {
    numeric.data[,i]<-as.numeric(as.character(data[,i]))
}

numeric.data[,160]<-data[,160]
    
    modelFit<-train(data$classe ~ ., data=numeric.data, method="glm")
nas<-numeric()
for (i in 1:length(names(data))) {
    nas[i]<-sum(is.na(numeric.data[,i]))    

}
for (i in 1:length(names(data))) {
    nas[i]<-sum(is.na(data[,i]))    
    
}
sum(is.na(numeric.data[,2]))
sapply(numeric.data[,2], function(x) sum(length(which(is.na(x)))))
pca<- preProcess(data[,-160], method="pca", pcaComp=2)




#====================Pre Process Data============================

    processed.data<-data

# Remove any column that is mostly NAs
    remove.index<-numeric()

    for (i in 1:length(names(data))){
        if (sum(is.na(data[,i]))>=19000){
            remove.index<-c(remove.index, i)
        }
    }

    processed.data<-processed.data[,-remove.index]

    summary(processed.data)
    dim(processed.data)
    sapply()