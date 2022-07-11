################PART 1#####################
library(dplyr)
library(tidyverse)

#Read the data set
setwd("E:/Machine Learning Assignment/Part 1/")
dataset <- read.csv("EWCS_2016.csv", header=T)

#Rename certain columns
names(dataset)[1] <- "Gender"
names(dataset)[2] <- "Age"

#Check for null/NA values
summary(dataset)
str(dataset)

#There are some unusual minimum values (-999) for all columns. Let's count how many exist.
sum(dataset == -999)

#329 of these values exist. Let's convert them into NA values
a <- dataset
a[a == -999] <- NA

#Remove the missing values so we can perform PCA
cc <- complete.cases(a)
a <- a[cc,]
summary(a)

#Check Gender distribution
summary(as.factor(a$Gender))

#Make custom copy of original table
b <- a%>%
  mutate(Gender=case_when(
    .$Gender==1 ~ "Male",
    .$Gender==2 ~ "Female"))
b$Gender <- as.factor(b$Gender)


#Create count table for Gender
b1 <- b%>%
  group_by(Gender)%>%
  dplyr::summarize(Total=n())


#Check Age distribution
hist(a$Age, 
     main = "Age Distribution",
     xlab = "Age")

#Check distribution for each question
par(mfrow=c(3,3))
for (i in names(a[3:11])){
  a.plot <- hist(a[[i]], breaks=seq(0,6,1), main=i, xlab="Value", xaxt="n")
  axis(side=1, at=a.plot$mids, labels=seq(1,6), tick = FALSE, padj=-1)
}


#Compute PCA
a.pca <- prcomp(a, center=T, scale.=T)
summary(a.pca)
#Calculate the total variance explained by each PC
vars_ex <- a.pca$sdev^2 / sum(a.pca$sdev^2) 

#Make a scree plot
qplot(c(1:11), vars_ex)+
  geom_line(color="SteelBlue")+
  geom_point()+
  xlab("Princpal Component")+
  ylab("Variance Explained")+
  ggtitle("Scree Plot")+
  scale_x_continuous(breaks=seq(1,11,1))+
  ylim(0,0.5)

#Check loadings
a.pca
#Narrow it two first two PC for easy viewing
a.pca$rotation[,1:2]

#Make a PCA biplot to check score and loadings
par(mfrow=c(1,1))
biplot(a.pca, scale=0, cex=0.6)

#Make custom biplot with ggplot2+ggbiplot

library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
g <- ggbiplot(a.pca, obs.scale = 1, var.scale = 1, 
              groups = b$Gender, ellipse = TRUE, alpha=.2)+
  scale_color_manual(name="Gender", values=c("red", "dodgerblue"))+
  theme(legend.direction = 'horizontal', 
               legend.position = 'top')
g$layers <- c(g$layers, g$layers[[1]])
g


g2 <- ggbiplot(a.pca, obs.scale = 1, var.scale = 1, 
              groups = a$Q87a, ellipse = TRUE, alpha=.2)+
  labs(color="Q87a response")
g2$layers <- c(g2$layers, g2$layers[[1]])
g2


g3 <- ggbiplot(a.pca, obs.scale = 1, var.scale = 1, 
               groups = a$Q90f, ellipse = TRUE, alpha=.2)+
  labs(color="Q90f response")
g3$layers <- c(g3$layers, g3$layers[[1]])
g3


