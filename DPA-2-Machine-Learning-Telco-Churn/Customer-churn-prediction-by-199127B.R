# Customer Churn - Assignment (Telco Dataset) | L. A. C. A. Sandaruwan (199127B)
library(car)
library(e1071)
library(caret)
library(caTools)
library(heatmaply)
library(naniar)
library(rpart)
library(ggplot2)

#Load the dataset
df_telco=read.csv("datasets_13996_18858_WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE)
str(df_telco)
nrow(df_telco)
df_telco%>% select(-1)->df_telco
str(df_telco)

#Check for missing values
vis_miss(df_telco)

# Remove columns with missing values (Since the missing values are less than the 0.01% of the dataset)
df_telco <- na.omit(df_telco)
vis_miss(df_telco)
nrow(df_telco)

#Understand about the Churn vs Customer Retention
options(repr.plot.width = 1, repr.plot.height = 4)
df_telco %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("GREEN", "RED"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = 1, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Customer Retention vs Churn")


#$Splitting the dataset as train (75%) and test (25%)
set.seed(123)
indices = sample.split(df_telco$Churn, SplitRatio = 0.75)
train_telco = df_telco[indices,]
test_telco = df_telco[!(indices),]
nrow(train_telco)
nrow(test_telco)
#test_telco$Churn = replace(test_telco$Churn,test_telco$Churn=="No"||test_telco$Churn== "Yes",NA)
#head(test_telco,5)

# By using Decesion Tree Algorithum, develop a model
dtree_train=rpart(Churn~.,data=train_telco)
summary(dtree_train)
dtree_predict=predict(dtree_train,newdata=test_telco,type = "class")
confusionMatrix(table(test_telco$Churn,dtree_predict))

