kyphosis_data<-read.csv("kyphosis.csv")
kyphosis_data$Kyphosis<-as.factor(kyphosis_data$Kyphosis)
colSums(is.na(kyphosis_data))
sum(is.na(kyphosis_data))


kyphosis_data$Age<-as.numeric(kyphosis_data$Age)

kyphosis_data$Age[is.na(kyphosis_data$Age)]<-mean(kyphosis_data$Age,na.rm = TRUE)

kyphosis_data$Number[is.na(kyphosis_data$Number)]<-mean(kyphosis_data$Number,na.rm = TRUE)

kyphosis_data$Start[is.na(kyphosis_data$Start)]<-mean(kyphosis_data$Start,na.rm = TRUE)

library(ggplot2)
ggplot(kyphosis_data,aes(x=Kyphosis,y=Age,fill=Kyphosis))+
  geom_boxplot()+labs(title = "Visualising age with kyphosis",x="age",y="kyphosis")+
  theme_minimal()

ggplot(kyphosis_data,aes(x=Age,fill =Kyphosis))+
  geom_histogram(bins = 30,alpha=0.5)+facet_wrap(~Kyphosis)+labs(title = "Visualising age with kyphosis",x="age",y="kyphosis")+
  theme_minimal()

ggplot(kyphosis_data,aes(x=Kyphosis,y=Number,fill=Kyphosis))+
  geom_boxplot()+labs(title = "number feature given the status of
Kyphosis feature ",x="Kyphosis",y="Number")+theme_minimal()


kyphosis_data$Start<-as.factor(kyphosis_data$Start)
levels(kyphosis_data$Start)

ggplot(kyphosis_data,aes(x=Number,y=Age,color=Kyphosis))+
  geom_point(alpha=0.5)+geom_smooth(method=loess)+facet_wrap(~Start)+labs(x="Number",y="Age")+
  theme_minimal()

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
tree_model<-rpart(Kyphosis~.,kyphosis_data,method = "class")
rpart.plot(tree_model,type=2,extra = 106,fallen.leaves = TRUE,main="Decision Tree for Kyphosis Prediction")
tree_model$variable.importance
barplot(tree_model$variable.importance)
set.seed(123)
training_index <- sample(seq_len(nrow(kyphosis_data)),size = 0.7*nrow(kyphosis_data))
train_data<-kyphosis_data[training_index,]
test_data<-kyphosis_data[-training_index,]
tree2_model<-rpart(Kyphosis~.,data = train_data,method = "class")
rpart.plot(tree2_model,type = 2,extra = 106,fallen.leaves = FALSE)
pred<-predict(tree2_model,test_data,type = "class")
conf_matrix<-table(Predicted = pred,Actual=test_data$Kyphosis)

TP<-3
TN<-16
FP<-3
FN<-3


accuracy<-(TP+TN)/(TP+TN+FP+FN)
precision<-(TP)/(TP+FP)
specificity<-(TN)/(TN+FN)
sensitivity<- TP /(TP+FN)





















































