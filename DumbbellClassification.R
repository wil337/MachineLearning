train<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",stringsAsFactors = FALSE,na.strings=c("NA","#DIV/0!"))
test<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
str(train)
head(train)
feat<-names(train)
names(train)
names(test)
cbind(names(train),names(test))
ggpairs(train)
library(GGally)
ggpairs(train)
library(caret)

train(classe~.,method="glmnet",data=train2)
library(manipulate)
library(ggplot2)
manipulate(
  ggplot(data=train,aes(y=classe))+geom_point(aes_string(x=pick)),
  pick=picker("X","user_name"))
names(train)
test$X
finalfeatures<- c("")
dist(train)
?complete.cases
str(train)
complete.cases(train)

mod1<-mod[,55:60]
ggpairs(mod1)
cor()
cor(matrix(mod),mod$classe)
str(matrix(mod))
str(train)
train$classe <- factor(train$classe)
train2<-(train[,-c(1:7)])
#train3<-train2[complete.cases(train2),]
cor1<-cor(train2[,1:100],unclass(train2$classe))
str(cor1)
class(cor1)
library(dplyr)
as.data.frame(cor1) %>% filter(is.na(cor1)==FALSE)
svd(train)
train
head(colSums(apply(train,2,is.na)))
library(YaleToolkit)
?whatis
noMissing<-whatis(train)$missing==0
train2<-train[,noMissing]

