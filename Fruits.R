rm(list = ls())
getwd()
list.files()
fruits_data<-read.csv("Date_Fruit_Datasets.csv")
levels(fruits_data$Class)
fruits_data$Class<-as.factor(fruits_data$Class)

colSums(is.na(fruits_data))

sum(is.na(fruits_data))

fruits_data$AREA[is.na(fruits_data$AREA)]<-mean(fruits_data$AREA,na.rm = TRUE)
fruits_data$ECCENTRICITY[is.na(fruits_data$ECCENTRICITY)]<-mean(fruits_data$ECCENTRICITY,na.rm = TRUE)
fruits_data$MINOR_AXIS[is.na(fruits_data$MINOR_AXIS)]<-mean(fruits_data$MINOR_AXIS,na.rm = TRUE)
fruits_data$CONVEX_AREA[is.na(fruits_data$CONVEX_AREA)]<-mean(fruits_data$CONVEX_AREA,na.rm = TRUE)

fruits_data$TYPE<- ifelse(fruits_data$Class=="BERHI","yes berhi","not berhi")
fruits_data$TYPE<-as.factor(fruits_data$TYPE)
table(fruits_data$TYPE)
fruits_data$TYPE<-NULL
library(ggplot2)
ggplot(fruits_data,aes(x=TYPE,y = AREA,color=TYPE))+
  geom_boxplot()+labs(title = "AREA feature given the type of fruit BERHI
versus NOT BERHI",x="Type",y="area")+theme_minimal()


ggplot(fruits_data,aes(x=MAJOR_AXIS,y=MINOR_AXIS,color=TYPE))+
  geom_point(size=2,alpha=0.5)+geom_smooth(method = "loess",se=FALSE)+facet_wrap(~TYPE)+labs(title = "MAJOR_AXIS versus MINOR_AXIS
making difference by the type of fruit BERHI versus NOT BERHI",x="MAJOR_AXIS",
                                            y="MINOR_AXIS")+theme_minimal()

fruits_data$comp_cat<-cut(fruits_data$COMPACTNESS,breaks = 4,
                          labels = c("low","medium low","medium high","high"))

ggplot(fruits_data, aes(x = SOLIDITY, y = ASPECT_RATIO, color = TYPE)) +
  geom_point(alpha = 0.6) +
  facet_grid(TYPE ~ comp_cat) +
  labs(
    title = "SOLIDITY vs ASPECT_RATIO by COMPACTNESS category and Fruit Type",
    x = "SOLIDITY",
    y = "ASPECT_RATIO"
  ) +
  theme_minimal()


fruits_data$Class<-as.factor(fruits_data$Class)
library(rpart)
library(rpart.plot)

my_tree<-rpart(Class~.,fruits_data,method = "class")

tree_model <- rpart(Class ~ ., data = fruits_data, method = "class")
rpart.plot(tree_model,type = 2,extra=2,fallen.leaves = TRUE,shadow.col = "green")
pred<-predict(tree_model,fruits_data,type="class")
conf_matrix<-table(Predict=pred,Actual=fruits_data$Class)
accuracy<-sum(diag(conf_matrix))/sum(conf_matrix)

































































