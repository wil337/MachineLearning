library(YaleToolkit)
library(caret)
library(dplyr)

#get data
train<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",stringsAsFactors = FALSE,na.strings=c("NA","#DIV/0!"))
test<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
train$classe <- factor(train$classe)

#get rid of the first 7 columns
train2<-(train[,-c(1:7)])

#get rid of columns with NAs
noMissing<-whatis(train2)$missing==0
train3<-train2[,noMissing]

#partition into training, and cross validation dataset
set.seed(1)
trainIndex <- createDataPartition(train3$classe, p = .8, list = FALSE, times = 1)
train4<-train3[ trainIndex,]
crossval<-train3[-trainIndex,]

#get correlation between features. order by biggest to smallest.
cor1<-cor(select(train4,-classe),unclass(train4$classe))
variables <- rownames(cor1)
corr <- cor1[,1]
corrsort<- data.frame(variables=variables,corr=corr) %>%
  mutate(abscorr = abs(corr)) %>%
  top_n(10,abscorr)

fit1<-train(classe~magnet_belt_y+magnet_belt_z+pitch_arm+accel_arm_x+magnet_arm_x+magnet_arm_y+
             pitch_forearm+total_accel_forearm+accel_forearm_x+magnet_forearm_x,method="glmnet",data=train4)
fit2<-train(classe~magnet_belt_y+magnet_belt_z+pitch_arm+accel_arm_x+magnet_arm_x+magnet_arm_y+
              pitch_forearm+total_accel_forearm+accel_forearm_x+magnet_forearm_x,method="gbm",data=train4)
fit3<-train(classe~magnet_belt_y+magnet_belt_z+pitch_arm+accel_arm_x+magnet_arm_x+magnet_arm_y+
              pitch_forearm+total_accel_forearm+accel_forearm_x+magnet_forearm_x,method="rf",data=train4)
p1<-predict(fit1,newdata=train4)
p2<-predict(fit2,newdata=train4)
p3<-predict(fit3,newdata=train4)
cv1<-predict(fit1,newdata=crossval)
cv2<-predict(fit2,newdata=crossval)
cv3<-predict(fit3,newdata=crossval)
comb<- data.frame(p1,p2,classe=train4$classe)
fitcomb<-train(classe~.,method="rf",data=comb)

combcv<- predict(fitcomb,newdata=crossval)

confusionMatrix(fit1)
confusionMatrix(fit2)
confusionMatrix(fit3)
confusionMatrix(train4$classe,p1)
confusionMatrix(train4$classe,p2)
confusionMatrix(train4$classe,p3)
confusionMatrix(fitcomb)
t1<-predict(fit1,newdata=test)
t2<-predict(fit2,newdata=test)
combtest<- data.frame(p1=t1,p2=t2)
t3<-predict(fit3,newdata=test)

submission<-predict(fitcomb,newdata=combtest)

cbind(t1,t2,submission,t3)
cbind(t1,t2,submission,t3)

#fitall<-system.time(train(classe~.,method="glmnet",data=train4))


#Answers based on fit3 - random forest
#1 B
#2 A
#3 B
#4 A
#5 A
#6 E
#7 D
#8 B
#9 A
#10 A
#11 B
#12 C
#13 B
#14 A
#15 E
#16 E
#17 A
#18 B
#19 B
#20 B



