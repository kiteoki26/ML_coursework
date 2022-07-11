library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(caret)


#R code to import and prepare the bank dataset
setwd("E:/Machine Learning Assignment/Part 3")
bank.data <- read.csv("bank.csv",sep=";",header=TRUE)

#Make copy of original table to keep integrity and remove certain variables
bank <- bank.data[c(-10:-12)]
nrow(bank)
summary(bank)

#Data analysis on categorical variables
p1 <- ggplot(bank,aes(x=age, fill=y))+
  geom_histogram(bins = 10)+
  facet_grid(~y)+
  theme_bw()+
  theme(legend.position = "none")

p2 <- ggplot(bank, aes(x=job, fill=y))+
  geom_bar(position="dodge")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p3 <- ggplot(bank, aes(x=marital, fill=y))+
  geom_bar(position="dodge")+
  theme_bw()+
  theme(legend.position = "none")

p4 <- ggplot(bank, aes(x=education, fill=y))+
  geom_bar(position="dodge")+
  theme_bw()+
  theme(legend.position = "none")

p5 <- ggplot(bank, aes(x=default, fill=y))+
  geom_bar(position="dodge")+
  theme_bw()+
  theme(legend.position = "top")

p6 <- ggplot(bank, aes(x=housing, fill=y))+
  geom_bar(position="dodge")+
  theme_bw()+
  theme(legend.position = "none")

p7 <- ggplot(bank, aes(x=loan, fill=y))+
  geom_bar(position="dodge")+
  theme_bw()+
  theme(legend.position = "none")

p8 <- ggplot(bank, aes(x=contact, fill=y))+
  geom_bar(position="dodge")+
  theme_bw()+
  theme(legend.position = "none")

p9 <- ggplot(bank, aes(x=poutcome, fill=y))+
  geom_bar(position="dodge")+
  theme_bw()+
  theme(legend.position = "none")

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol=3)

p10 <- ggplot(bank, aes(x=job, y=age, color=y))+
  geom_boxplot()+
  theme(legend.position = "none")

p11 <- ggplot(bank, aes(x=marital, y=age, color=y))+
  geom_boxplot()+
  theme(legend.position = "top")

grid.arrange(p11,p10, ncol=1)

#Check pdays for unusual value -1
sum(bank[11]==-1)
ggplot(bank,aes(x=pdays, fill=y))+
  geom_histogram(bins = 5)+
  facet_grid(~y)+
  theme_bw()+
  theme(legend.position = "none")


#Correlation matrix for numeric variables
bank.cor <- bank%>%
  select(age, balance, campaign, pdays, previous, y)
bank.cor$y <- ifelse(bank.cor$y == "yes", 1, 0)

model.matrix(~0+., data=bank.cor) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#Clean dataset
#Remove contact, pdays and poutcome 
bank <- bank[c(-9, -11,-13)]
#Remove rows with unknown values in job and education
bank[bank == "unknown"] <- NA
cc <- complete.cases(bank)
bank <- bank[cc,]
sum(bank=="unknown")
sum(is.na(bank))

###########LOGISTIC REGRESSION##############
#Let's check if the numeric variables are normally distributed
library(rcompanion)
library(MASS)
par(mfrow=c(2,2))
lapply(bank[c(1,6,9,10)], plotNormalHistogram)

#Transform all numeric variables with cube-root for normally distributed data
#Make copy of bank to protect dataset 
bank2 <- bank
bank2$y <- ifelse(bank2$y == "yes", 1, 0)
bank2[1] <- sign(bank$age) * abs(bank$age)^(1/3)
bank2[6] <- sign(bank$balance) * abs(bank$balance)^(1/3)
bank2[9] <- sign(bank$campaign) * abs(bank$campaign)^(1/3)
bank2[10] <- sign(bank$previous) * abs(bank$previous)^(1/3)

par(mfrow=c(2,2))
lapply(bank2[c(1,6,9,10)], plotNormalHistogram)

#Split data into training and testing sets
set.seed(6)
samp <- createDataPartition(bank2$y, p=0.8, list=F)
train.set <- bank2[samp,]
test.set <- bank2[-samp,]

#Train full model
glm.full <- glm(y~., data=train.set, family=binomial)
summary(glm.full)

#Train stepwise logistic model
step.glm <- stepAIC(glm.full, direction=c("backward"), 
                    trace=FALSE)
coef(step.glm)
summary(step.glm)

#Predict using model against test set
step.prob <- predict(step.glm, test.set, type="response")
head(step.prob)

predicted.classes <- ifelse(step.prob > 0.5, "yes", "no")
head(predicted.classes)

#Model accuracy
test.set$y <- ifelse(test.set$y == 1, "yes", "no")
mean(predicted.classes == test.set$y)
observed.class <- test.set$y
table(observed.class, predicted.classes)
################RANDOM FOREST CLASSIFIER TREE###################
library(randomForest)
train.set$y <- ifelse(train.set$y == 1, "yes", "no")
train.set$y <- as.factor(train.set$y)

set.seed(7)
rf.tree <- train(y~., data=train.set, method="rf",
                 trControl=trainControl("cv", number=10),
                 importance=TRUE)
#Best tuning parameter
rf.tree$bestTune
#Final model
rf.tree$finalModel
#Predict using model against test set
rf.prob <- predict(rf.tree, test.set)
mean(rf.prob == test.set$y)


importance(rf.tree$finalModel)
par(mfrow=c(1,1))
varImpPlot(rf.tree$finalModel, type = 1)
varImpPlot(rf.tree$finalModel, type = 2)



