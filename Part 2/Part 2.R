library(dplyr)
library(tidyverse)


#R code to import and prepare the student performance dataset
setwd("E:/Machine Learning Assignment/Part 2/datasets")
math=read.csv("student-mat.csv",sep=";",header=TRUE)
port=read.csv("student-por.csv",sep=";",header=TRUE)

#Remove G1 and G2 variables because we cannot use them
math <- math[c(-31:-32)]
port <- port[c(-31:-32)]

#Merge the two datasets where .x variables refer to math and .y variables refer to port
grades <-  merge(math, port, by=c("school", "sex", "age", "address", "famsize", "Pstatus",
                                  "Medu","Fedu","Mjob","Fjob","reason", "guardian",
                                  "traveltime","studytime", "schoolsup","famsup",
                                  "paid","activities","nursery", "higher", "internet","romantic", 
                                  "famrel","freetime","goout","Dalc","Walc","health"))

#Brief look at the data and check if there are missing values
nrow(grades)
sum(is.na(grades))

#Change variables with binary values 1 for "yes" and 0 for "no"
grades <- grades%>%
  mutate_at(c("schoolsup", "famsup","paid","activities","nursery","higher","internet","romantic"), 
            ~as.numeric(ifelse(.x=="yes",1,0)))



#Merge G3.x and G3.y into an average of G3 for that student
grades <- grades %>%
  mutate(G3avg=rowMeans(select(grades, c(31,34))))

#Drop G3.x and G3.y
grades <- grades[c(-31,-34)]


#Check the mean for both schools in both subjects
school_G3_mean <- grades%>%
  group_by(school)%>%
  summarize(G3Average=round(mean(G3avg), 2), G3Std=round(sd(G3avg)),
            StudentTotal=n())
            
gender_G3_mean <- grades%>%
  group_by(sex)%>%
  summarize(G3Average=round(mean(G3avg), 2), G3Std=round(sd(G3avg)),
            StudentTotal=n())

#Create separate table for correlation matrix
grades_cor <- grades[c(-1:-2, -4:-6, -9:-12)]

#Let's quickly check if there are any correlations between G3 and other variables
library(ggcorrplot)
model.matrix(~0+., data=grades_cor) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

p1 <- ggplot(data=grades, aes(x=address, y=G3avg, group=address, fill=address))+
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

p2 <- ggplot(data=grades, aes(x=famsize, y=G3avg, group=famsize, fill=famsize))+
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

p3 <- ggplot(data=grades, aes(x=Pstatus, y=G3avg, group=Pstatus, fill=Pstatus))+
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

p4 <- ggplot(data=grades, aes(x=Mjob, y=G3avg, group=Mjob, fill=Mjob))+
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

p5 <- ggplot(data=grades, aes(x=Fjob, y=G3avg, group=Fjob, fill=Fjob))+
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

p6 <- ggplot(data=grades, aes(x=reason, y=G3avg, group=reason, fill=reason))+
  geom_boxplot()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

p7 <- ggplot(data=grades, aes(x=guardian, y=G3avg, group=guardian, fill=guardian))+
  geom_boxplot()

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p7, ncol=3)


p6

#############STEPWISE REGRESSION##############
library(MASS)
library(caret)
library(leaps)
#Create a full model first
m1_full<- lm(G3avg~., grades)
#Splitting data into training and testings sets
set.seed(5)
samp <- createDataPartition(grades$G3avg, p=.7, list=F)
training <- grades[samp,]
testing <- grades[-samp,]

train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.m1 <- train(G3avg ~., data = training,
                    method = "leapBackward",
                 tuneGrid = data.frame(nvmax = 1:20),
                    trControl = train.control)
# Model accuracy
step.m1$results
# Best tune
step.m1$bestTune
# Summary of the model
summary(step.m1$finalModel)

#Predict using model against test set
step.m1.pred <- predict(step.m1, testing)
testing.list <- grades[-samp, "G3avg"]
par(mfrow=c(1,1))
plot(step.m1.pred,testing.list)
abline(0,1)
mean((step.m1.pred-testing.list)^2) #MSE
sqrt(mean((step.m1.pred-testing.list)^2)) #RMSE


##################INTERACTION EFFECTS####################

weekend_alc <- aov(G3avg ~ Walc*sex, data=grades)
summary(weekend_alc)

weekday_alc <- aov(G3avg ~ Dalc*sex, data=grades)
summary(weekday_alc)

par(mfrow=c(2,2))
interaction.plot(
  x.factor = grades$Walc,
  trace.factor = grades$sex,
  response = grades$G3avg,
  fun = median,
  ylab = "Final Grade Average",
  xlab = "Weekend alcohol consumption (very low to very high)",
  trace.label = "Gender",
  col = c("pink", "blue"),
  lty = 1,
  lwd = 3
)

interaction.plot(
  x.factor = grades$Dalc,
  trace.factor = grades$sex,
  response = grades$G3avg, 
  fun = median, 
  ylab = "Final Grade Average",
  xlab = "Weekday alcohol consumption (very low to very high)",
  col = c("pink", "blue"),
  lty = 1,
  lwd = 3, 
  trace.label = "Gender")


#Create new table with dummy variables
grades2 <- grades
grades2$guardianOther <- ifelse(grades2$guardian == "other", 1, 0)
grades2$Mjobhealth  <- ifelse(grades2$Mjob == "health", 1, 0)
grades2$Mjobother  <- ifelse(grades2$Mjob == "other", 1, 0)
grades2$Mjobservices  <- ifelse(grades2$Mjob == "services", 1, 0)
grades2$Mjobteacher  <- ifelse(grades2$Mjob == "teacher", 1, 0)
grades2$Fjobteacher  <- ifelse(grades2$Fjob == "teacher", 1, 0)
grades2$Fjobservices  <- ifelse(grades2$Fjob == "services", 1, 0)

#Splitting data into training and testings sets
set.seed(5)
samp <- createDataPartition(grades2$G3avg, p=0.7, list=F)
training2 <- grades2[samp,]
testing2 <- grades2[-samp,]

lm.m2 <- lm(formula = G3avg ~ sex*Walc+address+Mjobhealth+Mjobother+Mjobservices+Mjobteacher+Fjobteacher+Fjobservices+guardianOther+schoolsup+higher+romantic++goout+failures.x+absences.x+absences.y, data=training2)
summary(lm.m2)

#Predict using model against test set
lm.m2.pred <- predict(lm.m2, testing2)
testing2 <- grades2[-samp, "G3avg"]
plot(lm.m2.pred,testing2)
abline(0,1)
mean((lm.m2.pred-testing2)^2) #MSE
sqrt(mean((lm.m2.pred-testing2)^2)) #RMSE

#Check for heteroscedasticity 
par(mfrow=c(2,2))
plot(lm.m2)

###########CART Model Regression Tree############
library(rpart)
library(rpart.plot)

tree.m3=rpart(G3avg~., training)
par(mfrow=c(1,1))
prp(tree.m3, type=2, extra=1)

#Predict using model against test set
tree.m3.pred <- predict(tree.m3, testing)
plot(tree.m3.pred,testing.list)
abline(0,1)
mean((tree.m3.pred-testing.list)^2) #MSE
sqrt(mean((tree.m3.pred-testing.list)^2)) #RMSE


#########Random Forest Regression Tree##############
set.seed(8)
tree.m4 <- train(G3avg~., data = training, method = "rf",
  trControl = trainControl("cv", number = 10))

# Best tuning parameter mtry
tree.m4$bestTune
# Final model 
tree.m4$finalModel
#Predict using model against test set
tree.m4.pred <- tree.m4 %>% predict(testing)
head(tree.m4.pred )

plot(tree.m4.pred,testing.list)
abline(0,1)
mean((tree.m4.pred-testing.list)^2) #MSE
sqrt(mean((tree.m4.pred-testing.list)^2)) #RMSE
