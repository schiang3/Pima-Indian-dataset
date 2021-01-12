#Setup Working Directory
setwd("C:/Users/Shuya C/Desktop/depaul/dsc510/project")

#Read in Data
library(readr)
diabetes <- read_csv("diabetes.csv", col_names = TRUE)
diabetes_1 <- read_csv("diabetes.csv", col_names = TRUE)
View(diabetes)
head(diabetes)
dim(diabetes)
names(diabetes)
str(diabetes)
#Check Missing Data
sum(is.na(diabetes)) 
#no missing value but need to remove Glucose BloodPressure SkinThickness Insulin  BMI  =0
summary(diabetes)
#Check Spearman Correlations
library(corrplot)
corrplot(cor(diabetes, method = "spearman"))
library(psych)
describe(diabetes$DiabetesPedigreeFunction)
describe(diabetes)

library(ggplot2)
hist(diabetes)
boxplot(diabetes)

for (i in 2:6) 
{diabetes <- diabetes[-which(diabetes[, i] == 0), ]}

diabetes_subset= diabetes%>% select(7,9)
hist(diabetes_subset)
boxplot(diabetes)
corrplot(cor(diabetes, method = "spearman"))

#research question: Is there a difference between diabetes test results for the pedigree function? 
hist(diabetes_subset$DiabetesPedigreeFunction)
library(RVAideMemoire)
shapiro.test(diabetes_subset$DiabetesPedigreeFunction) #P-value<0.05 Means DiabetesPedigreeFunction is not normal
byf.shapiro(as.matrix(diabetes_subset$DiabetesPedigreeFunction)~Outcome,data=diabetes_subset)
corrplot(cor(diabetes_subset, method = "spearman"))

#Mann-Whitney U (Wilcoxon) test - Nonparametric T-Test
wilcox.test(diabetes_subset$DiabetesPedigreeFunction~diabetes_subset$Outcome)

diabetes_subset$outcome_fact <- as.factor(diabetes_subset$Outcome) 
library(ggplot2)
ggplot(diabetes_subset, aes(x = DiabetesPedigreeFunction,group=outcome_fact,fill=outcome_fact)) +
  geom_histogram(position = "identity", bins = 10, alpha=0.5) +
  scale_color_manual(values = c("blue", "orange"))

#Kruskal-Wallis test - Nonparametric ANOVA
kruskal.test(DiabetesPedigreeFunction~outcome_fact, data = diabetes_subset)

library(Hmisc)
rcorr(as.matrix(diabetes))


diabetes$Outcome <- as.factor(diabetes$Outcome)
logistic2 <- glm(Outcome ~ DiabetesPedigreeFunction, data = diabetes, family = "binomial")
summary(logistic2)

#research question: What determines if someone will have diabetes?
logistic <- glm(Outcome ~ ., data = diabetes, family = "binomial")
summary(logistic)

#Mann-Whitney U (Wilcoxon) test - Nonparametric T-Test
wilcox.test(diabetes$DiabetesPedigreeFunction~diabetes$Outcome)
diabetes$outcome_fact <- as.factor(diabetes$Outcome) 
diabetes$outcome <- ifelse(diabetes$outcome_fact=="1","Diabetes", "Non-Diabetes")
library(ggplot2)
ggplot(diabetes, aes(x = DiabetesPedigreeFunction,group=outcome,fill=outcome_fact)) +
  geom_histogram(position = "identity", bins = 10, alpha=0.5) +
  scale_color_manual(values = c("blue", "orange"))
ggplot(diabetes, aes(x=outcome, y=DiabetesPedigreeFunction, fill=outcome)) + 
  geom_boxplot()+stat_summary(fun="mean",color='yellow', shape=18)+ labs(x = "Outcome", y="DiabetesPedigreeFunction", title="DiabetesPedigreeFunction by Diabetes Test result")

#Kruskal-Wallis test - Nonparametric ANOVA
kruskal.test(DiabetesPedigreeFunction~outcome_fact, data = diabetes)


library(broom.mixed)
library(dplyr)     # data wrangling
library(ggplot2)   # plotting
library(rsample)   # training and testing splitting
library(caret)     # for logistic regression modeling and prediction outputs
library(vip)       # variable importance
library(tidyverse)
library(gtsummary)
tidy(logistic)
#Coefficients in exponential form
library(dplyr)
logistic %>% 
  gtsummary::tbl_regression(exp = TRUE) 

library(ggplot2)
ggplot(diabetes, aes(x = DiabetesPedigreeFunction,group=Outcome,fill=Outcome)) +
  geom_histogram(position = "identity", bins = 10, alpha=0.5) +
  scale_color_manual(values = c("blue", "orange"))+ labs(x = "DiabetesPedigreeFunction", y="Count", title="DiabetesPedigreeFunction by Test result")

library(ggplot2)
ggplot(diabetes, aes(x = BMI,group=Outcome,fill=Outcome)) +
  geom_histogram(position = "identity", bins = 10, alpha=0.5) +
  scale_color_manual(values = c("blue", "orange"))

library(ggplot2)
ggplot(diabetes, aes(x = Glucose,group=Outcome,fill=Outcome)) +
  geom_histogram(position = "identity", bins = 10, alpha=0.5) +
  scale_color_manual(values = c("blue", "orange"))

diabetes$underweight<-ifelse(diabetes$BMI<18.5, 1, 0)
diabetes$healthyweight<-ifelse(diabetes$BMI>=18.5 & diabetes$BMI<25, 1, 0)
diabetes$overweight<-ifelse(diabetes$BMI>=25 & diabetes$BMI<30, 1, 0)
diabetes$obese<-ifelse(diabetes$BMI>=30, 1, 0)
diabetes$Glucose_normal<-ifelse(diabetes$Glucose<140, 1, 0)
diabetes$Glucose_diabetes<-ifelse(diabetes$Glucose>=200, 1, 0)
diabetes$Glucose_prediabetes<-ifelse(diabetes$Glucose>=140 & diabetes$Glucose<200, 1, 0)
u<-sum(diabetes$underweight==1)
h<-sum(diabetes$healthyweight==1)
over<-sum(diabetes$overweight==1)
ob<-sum(diabetes$obese==1)
write.csv(diabetes,"C:/Users/Shuya C/Desktop/depaul/dsc510/project/diabetes_1116.csv", row.names = FALSE)
# Pie Chart with Percentages
slices <- c(u, h, over, ob)
lbls <- c("underweight", "healthyweight", "overweight", "obese")
pct <- round(slices/sum(slices)*100, digits = 3)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls,col=rainbow(length(lbls)),border="brown")

library("ggplot2")


newdata_dia <- subset(diabetes, Outcome==1)
u2<-sum(newdata_dia$underweight==1)
u2
h2<-sum(newdata_dia$healthyweight==1)
h2
over2<-sum(newdata_dia$overweight==1)
over2
ob2<-sum(newdata_dia$obese==1)
ob2

data_weight_dia <- data.frame(weight =c("underweight", "healthyweight", "overweight", "obese"), 
                         
                         count =c(u2, h2, over2, ob2))

library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
barplot(height= c(u2, h2, over2, ob2), 
        names.arg=c("underweight", "healthyweight", "overweight", "obese")
        ,col=coul, xlab="categories OF weight", 
        ylab="COUNT", 
        main="BMI", 
        ylim=c(0,200))
newdata_nondia <- subset(diabetes, Outcome==0)
u3<-sum(newdata_nondia$underweight==1)
u3
h3<-sum(newdata_nondia$healthyweight==1)
h3
over3<-sum(newdata_nondia$overweight==1)
over3
ob3<-sum(newdata_nondia$obese==1)
ob3

data_weight<- data.frame(BMI_category =c("underweight", "healthyweight", "overweight", "obese")
                              ,diabetescount =c(u2, h2, over2, ob2),
                              non_diabetescount =c(u3, h3, over3, ob3))
write.csv(data_weight,"C:/Users/Shuya C/Desktop/depaul/dsc510/project/diabetes_11.csv", row.names = FALSE)
# Pie Chart with Percentages
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
library("ggplot2")
plot(data_weight, aes(x=BMI_category,y = diabetescount, fill=diabetescount)) +geom_bar(stat = "identity")
par(new = T)
plot(data_weight, aes(x=BMI_category, y=non_diabetescount, fill=non_diabetescount)) + 
  geom_bar(aes(fill=non_diabetescount),   # fill depends on cond2
           stat="identity",
           colour="black",    # Black outline for all
           position = 'dodge') 

library(ggplot2)
ggplot(diabetes, aes(x=Outcome, y=Glucose, fill=Outcome)) + 
  geom_boxplot()+stat_summary(fun="mean",color='yellow', shape=18)+ggtitle('Diabetes vs Glucose')
library(ggplot2)
ggplot(diabetes, aes(x=Outcome, y=DiabetesPedigreeFunction, fill=Outcome)) + 
  geom_boxplot()+stat_summary(fun="mean",color='yellow', shape=18)+ggtitle('Diabetes vs DiabetesPedigreeFunction')

gn1<-sum(newdata_nondia$Glucose_normal==1)
gn1
gn_d<-sum(newdata_nondia$Glucose_diabetes==1)
gn_d
gpre<-sum(newdata_nondia$Glucose_prediabetes==1)
gpre
gn2<-sum(newdata_dia$Glucose_normal==1)
gn2
gn_d2<-sum(newdata_dia$Glucose_diabetes==1)
gn_d2
gpre2<-sum(newdata_dia$Glucose_prediabetes==1)
gpre2

data_glucose<- data.frame(glucose =c("normal", "diabetes", "prediabetes")
                         ,diabetescount =c(gn2, gn_d2, gpre2),
                         non_diabetescount =c(gn1, gn_d, gpre))
write.csv(data_glucose,"C:/Users/Shuya C/Desktop/depaul/dsc510/project/diabetes_12.csv", row.names = FALSE)
#write.csv(diabetes,"C:/Users/Shuya C/Desktop/depaul/dsc510/project/diabetes_cleaned.csv", row.names = FALSE)

library(readr)
diabetes_clean <- read_csv("diabetes_cleaned.csv", col_names = TRUE)
diabetes_clean


library(xgboost)
library(caret)
set.seed(123)
split <- initial_split(diabetes_clean, prop = .8, strata = "Outcome")
train <- training(split)
test  <- testing(split)
library(tidyverse)
train.features<-train %>% select(1:8)
train.label<-train %>% select(9)
test.features<-test %>% select(1:8)
test.label<-test %>% select(9)

tnfeatures <- data.matrix(train.features)
tnlabels <- data.matrix(train.label)

testfeatures <- data.matrix(test.features)
testlabels <- data.matrix(test.label)
### XGBOOST
model<-xgboost(data = tnfeatures,label = tnlabels, nrounds = 3, nthread = 2,  prediction = TRUE,metrics = list("rmse","auc"),
        max_depth = 3, eta = 1, objective = "binary:logistic")
model

importance_matrix <- xgb.importance(model = model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

pred <- predict(model, testfeatures)
prediction<-as.numeric(pred > 0.5)
confusionMatrix(table(prediction, testlabels),positive='1')
xgb.plot.multi.trees(feature_names = names(tnfeatures), 
                     model = model)
### grid search
train.y <- as.factor(tnlabels)
xgb_trcontrol =trainControl(method = "cv",number = 5,   allowParallel = TRUE,verboseIter = FALSE, returnData = FALSE)

xgbGrid <- expand.grid(nrounds = c(100,200),  
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = c(0.01,0.05,0.1),
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1)

xgb_model = train( tnfeatures, train.y,   trControl = xgb_trcontrol, tuneGrid = xgbGrid, method = "xgbTree")
xgb_model
xgb_model$bestTune
predition_g <- predict(xgb_model, testfeatures)
predition_g
xgb.plot.tree(model = xgb_model)
confusionMatrix(table(predition_g, testlabels),positive='1')

#advanced functions train models.
dtrain <- xgb.DMatrix(data = tnfeatures, label=tnlabels)
dtest <- xgb.DMatrix(data = testfeatures, label=testlabels)

bst <- xgb.train(data=dtrain, max.depth=5, eta=0.01, nthread = 2, nrounds=20, watchlist=list(train=dtrain, test=dtest),  eval.metric = "error", eval.metric = "logloss",objective = "binary:logistic")
bst

label = getinfo(dtest, "label")
pred2 <- predict(bst, dtest)
importance_matrix2 <- xgb.importance(model = bst)
print(importance_matrix2)
xgb.plot.importance(importance_matrix = importance_matrix2, xlab = "Relative importance")
xgb.plot.multi.trees(feature_names = names(tnfeatures), 
                     model = bst)

prediction2<-as.numeric(pred2 > 0.5)
confusionMatrix(table(prediction2, testlabels),positive='1')


#### APPLY THE RESULT FROM BEST TUNING
dtrain <- xgb.DMatrix(data = tnfeatures, label=tnlabels)
dtest <- xgb.DMatrix(data = testfeatures, label=testlabels)

bst2 <- xgb.train(data=dtrain, max.depth=20, eta=0.01, nthread = 2, nrounds=100,gamma=0,colsample_bytree=0.5,
                  min_child_weight=1,subsample=1,early.stop.round = 20,watchlist=list(train=dtrain, test=dtest),  
                  eval.metric = "error", eval.metric = "logloss",objective = "binary:logistic")
bst2

label = getinfo(dtest, "label")
pred3 <- predict(bst2, dtest)
pred3
importance_3 <- xgb.importance(model = bst2)
print(importance_3)
library("ggplot2")

gg <- xgb.plot.importance(importance_matrix = importance_3, xlab = "Relative importance")+ ggtitle('Importance of variables')
gg 
prediction_3<-as.numeric(pred3> 0.5)
confusionMatrix(table(prediction_3, testlabels),positive='1')


### NBclassfier
library(e1071)
train$Outcome_2 <- as.factor(train$Outcome)
test$Outcome_2 <- as.factor(test$Outcome)
NBclassfier<-naiveBayes(Outcome_2~Glucose+Pregnancies+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, data=train)
print(NBclassfier)
NB_Predictions<-predict(NBclassfier,test)
confusionMatrix(table(NB_Predictions, test$Outcome_2),positive='1')

NBclassfier2<-naiveBayes(Outcome_2~Glucose+BMI+DiabetesPedigreeFunction+Insulin+Pregnancies, data=train)
print(NBclassfier2)
NB_Predictions2<-predict(NBclassfier2,test)
confusionMatrix(table(NB_Predictions2, test$Outcome_2),positive='1')

