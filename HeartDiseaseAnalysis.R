library(tidyverse)
data <- read.csv("C:/Users/amans/Downloads/heart.csv")
head(data)
tail(data)
glimpse(data)
ncol(data)
nrow(data)
colnames(data)
summary(data)

data2 <- data %>% mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
         fbs = if_else(fbs == 1, ">120", "<=120"),
         exang = if_else(exang == 1, "YES" ,"NO"),
         cp = if_else(cp == 1, "ATYPICAL ANGINA",
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target == 1, "YES", "NO")
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())


ggplot(data2, aes(x=data2$target, fill=data2$target))+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("count")+
  ggtitle("Presence & Absence of Heart Disease")+
  scale_fill_discrete(name= 'Heart Disease', labels =c("Absence", "Presence"))

prop.table(table(data2$target))

data2 %>%
  group_by(ï..age) %>%
  count() %>%
  filter(n>10) %>%
  ggplot()+
  geom_col(aes(ï..age, n), fill = 'green')+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Agecount")

data2 %>%
  ggplot(aes(x=sex, y=trestbps))+
  geom_boxplot(fill ='purple')+
  xlab('sex')+
  ylab('BP')+
  facet_grid(~cp)

data %>%
  ggplot(aes(x=sex, y=trestbps))+
  geom_boxplot(fill ='purple')+
  xlab('sex')+
  ylab('BP')+
  facet_grid(~cp)


data2 %>%
  ggplot(aes(x=sex, y=chol))+
  geom_boxplot(fill ='orange')+
  xlab('sex')+
  ylab('Chol')+
  facet_grid(~cp)


install.packages("corrplot")
library(corrplot)
library(ggplot2)

cor_heart <- cor(data2[, 10:14])
cor_heart

corrplot(cor_heart, method ='square', type='upper')

??method
s = sum(is.na(data2))
s

library(caret)
set.seed(10)
colnames(data2)

inTrainRows <- createDataPartition(data2$target,p=0.7,list=FALSE)
trainData <- data2[inTrainRows,]
testData <-  data2[-inTrainRows,]
nrow(trainData)/(nrow(testData)+nrow(trainData))

AUC = list()
Accuracy = list()

install.packages('e1071', dependencies=TRUE)

library(e1071)

set.seed(10)
logRegModel <- train(target ~ ., data=trainData, method = 'glm', family = 'binomial')
logRegPrediction <- predict(logRegModel, testData)
logRegPredictionprob <- predict(logRegModel, testData, type='prob')[2]
logRegConfMat <- confusionMatrix(logRegPrediction, testData[,"target"])
#ROC Curve
install.packages("pROC")
library(pROC)
AUC$logReg <- roc(as.numeric(testData$target),as.numeric(as.matrix((logRegPredictionprob))))
logReg <- logRegConfMat$overall['Accuracy']  #foun











set.seed(10)
svmModel <- train(target ~ ., data = trainData,
                  method = "svmRadial",
                  trControl = fitControl,
                  preProcess = c("center", "scale"),
                  tuneLength = 8,
                  metric = "ROC")
svmPrediction <- predict(svmModel, testData2)
svmPredictionprob <- predict(svmModel, testData2, type='prob')[2]
svmConfMat <- confusionMatrix(svmPrediction, testData2[,"target"])
#ROC Curve
AUC$svm <- roc(as.numeric(testData2$target),as.numeric(as.matrix((svmPredictionprob))))
svm <- svmConfMat$overall['Accuracy']  

install.packages('kernlab')

library(kernlab)
fitControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          ## Estimate class probabilities
                          classProbs = TRUE,
                          ## Evaluate performance using 
                          ## the following function
                          summaryFunction = twoClassSummary)

library(randomForest)
set.seed(10)
RFModel <- randomForest(target ~ .,
                        data=trainData, 
                        importance=TRUE, 
                        ntree=200)
#varImpPlot(RFModel)
RFPrediction <- predict(RFModel, testData)
RFPredictionprob = predict(RFModel,testData,type="prob")[, 2]

RFConfMat <- confusionMatrix(RFPrediction, testData[,"target"])

AUC$RF <- roc(as.numeric(testData$target),as.numeric(as.matrix((RFPredictionprob))))
RF <- RFConfMat$overall['Accuracy']  

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 3, ncol = 2,
                           dimnames = list(row.names, col.names))))

summary(logRegModel)$coeff


logRegConfMat

RFConfMat

svmConfMat
