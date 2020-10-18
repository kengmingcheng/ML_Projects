# dataset from https://www.kaggle.com/ronitf/heart-disease-uci
# install packages
library(lattice)
library(boot)
library(ggplot2)

RNGkind(sample.kind = "Rounding") # for 3.6.0
# import data
cl <- read.csv("C:/Users/qw223/Projects/R/Database/cleveland.csv", na = "?", stringsAsFactors = FALSE, header = TRUE)
hung <- read.csv("C:/Users/qw223/Projects/R/Database/hungarian.csv", na = "?", stringsAsFactors = FALSE, header = TRUE)
sw <- read.csv("C:/Users/qw223/Projects/R/Database/switzerland.csv", na = "?", stringsAsFactors = FALSE, header = TRUE)
va <- read.csv("C:/Users/qw223/Projects/R/Database/va.csv", na = "?", stringsAsFactors = FALSE, header = TRUE)
data <- rbind(cl,hung,sw,va)
names(data) <- c('Age', 'Gender', 'CP', 'Trestbps', 'Chol', 'FBS', 'RestECG',
                 'Thalach', 'Exang', 'Oldpeak', 'Slope', 'CA', 'Thal', 'Goal')

data <- data[!is.na(data$RestECG),]
data <- data[!is.na(data$Trestbps),]
data <- data[!is.na(data$Chol),]
data <- data[!is.na(data$Oldpeak),]
data <- data[!is.na(data$FBS),]

hist(data$Age)
ggplot(data,aes(x=Goal,group=Gender,fill=Gender,labels = c("Female,Male")))+
  geom_histogram(position="dodge",binwidth=1)+theme_bw()

plot(data$Age, data$Trestbps, col=ifelse(Gender==1,"blue", "red")
     , xlab="Age", ylab="Resting Blood Pressure")
plot(data$Age, data$Chol, col=ifelse(Gender==1,"blue", "red")
     , xlab="Age", ylab="Serum Cholesterol")
cylinder.Goal <- as.factor(data$Goal)
plot(cylinder.Goal, data$Age, xlab="Heart Disease", ylab="Age")
hist(data$Goal,data$Age,col=ifelse(Gender==1,"blue", "red")
     ,xlab="Heart Disease", ylab="Age")

data$Goal[data$Goal == 2] <- 1
data$Goal[data$Goal == 3] <- 1
data$Goal[data$Goal == 4] <- 1

summary(data)
#pairs(data)

# split to training and testing: 85 : 15
set.seed(10)
train = sample(740,629)
test.data = data[(!train),]

# Logistic Regression
data.glm.fit1 <- glm(Goal~Age+Gender+CP+Trestbps+Chol+FBS+RestECG+Thalach+Exang, data=data, family=binomial, subset = train)

data.glm.fit2 <- glm(Goal~Age, data=data, family=binomial, subset = train)
data.glm.fit3 <- glm(Goal~Gender, data=data, family=binomial, subset = train)
data.glm.fit4 <- glm(Goal~CP, data=data, family=binomial, subset = train)
data.glm.fit5 <- glm(Goal~Thalach, data=data, family=binomial, subset = train)
data.glm.fit6 <- glm(Goal~Exang, data=data, family=binomial, subset = train)

data.glm.fit7 <- glm(Goal~Age+Gender+CP+Thalach+Exang, data = data, family=binomial, subset = train)
data.glm.fit8 <- glm(Goal~Gender+CP+Thalach+Exang, data=data, family=binomial, subset = train)
data.glm.fit9 <- glm(Goal~Gender+CP+Exang, data=data, family=binomial, subset = train)


# Prediction
data.glm.prob1 <- predict(data.glm.fit1,data[train,],type="response")
data.glm.prob2 <- predict(data.glm.fit2,data[train,],type="response")
data.glm.prob3 <- predict(data.glm.fit3,data[train,],type="response")
data.glm.prob4 <- predict(data.glm.fit4,data[train,],type="response")
data.glm.prob5 <- predict(data.glm.fit5,data[train,],type="response")
data.glm.prob6 <- predict(data.glm.fit6,data[train,],type="response")
data.glm.prob7 <- predict(data.glm.fit7,data[train,],type="response")
data.glm.prob8 <- predict(data.glm.fit8,data[train,],type="response")
data.glm.prob9 <- predict(data.glm.fit9,data[train,],type="response")

# Prediction on test dataset
data.glm.prob.test1 <- predict(data.glm.fit1,data[-train,],type="response")
data.glm.prob.test2 <- predict(data.glm.fit2,data[-train,],type="response")
data.glm.prob.test3 <- predict(data.glm.fit3,data[-train,],type="response")
data.glm.prob.test4 <- predict(data.glm.fit4,data[-train,],type="response")
data.glm.prob.test5 <- predict(data.glm.fit5,data[-train,],type="response")
data.glm.prob.test6 <- predict(data.glm.fit6,data[-train,],type="response")
data.glm.prob.test7 <- predict(data.glm.fit7,data[-train,],type="response")
data.glm.prob.test8 <- predict(data.glm.fit8,data[-train,],type="response")
data.glm.prob.test9 <- predict(data.glm.fit9,data[-train,],type="response")

# train error rate
data.glm.pred1 = rep(0,629)
data.glm.pred1[data.glm.prob1 > 0.5] = 1

data.glm.pred2 = rep(0,629)
data.glm.pred2[data.glm.prob2 > 0.5] = 1

data.glm.pred3 = rep(0,629)
data.glm.pred3[data.glm.prob3 > 0.5] = 1

data.glm.pred4 = rep(0,629)
data.glm.pred4[data.glm.prob4 > 0.5] = 1

data.glm.pred5 = rep(0,629)
data.glm.pred5[data.glm.prob5 > 0.5] = 1

data.glm.pred6 = rep(0,629)
data.glm.pred6[data.glm.prob6 > 0.5] = 1

data.glm.pred7 = rep(0,629)
data.glm.pred7[data.glm.prob7 > 0.5] = 1

data.glm.pred8 = rep(0,629)
data.glm.pred8[data.glm.prob8 > 0.5] = 1

data.glm.pred9 = rep(0,629)
data.glm.pred9[data.glm.prob9 > 0.5] = 1

table(data.glm.pred1,data[train,]$Goal)
table(data.glm.pred2,data[train,]$Goal)
table(data.glm.pred3,data[train,]$Goal)
table(data.glm.pred4,data[train,]$Goal)
table(data.glm.pred5,data[train,]$Goal)
table(data.glm.pred6,data[train,]$Goal)
table(data.glm.pred7,data[train,]$Goal)
table(data.glm.pred8,data[train,]$Goal)
table(data.glm.pred9,data[train,]$Goal)

# test error rate
data.glm.test.pred1 = rep(0,111)
data.glm.test.pred1[data.glm.prob.test1 > 0.5] = 1

data.glm.test.pred2 = rep(0,111)
data.glm.test.pred2[data.glm.prob.test2 > 0.5] = 1

data.glm.test.pred3 = rep(0,111)
data.glm.test.pred3[data.glm.prob.test3 > 0.5] = 1

data.glm.test.pred4 = rep(0,111)
data.glm.test.pred4[data.glm.prob.test4 > 0.5] = 1

data.glm.test.pred5 = rep(0,111)
data.glm.test.pred5[data.glm.prob.test5 > 0.5] = 1

data.glm.test.pred6 = rep(0,111)
data.glm.test.pred6[data.glm.prob.test6 > 0.5] = 1

data.glm.test.pred7 = rep(0,111)
data.glm.test.pred7[data.glm.prob.test7 > 0.5] = 1

data.glm.test.pred8 = rep(0,111)
data.glm.test.pred8[data.glm.prob.test8 > 0.5] = 1

data.glm.test.pred9 = rep(0,111)
data.glm.test.pred9[data.glm.prob.test9 > 0.5] = 1

table(data.glm.test.pred1,data[-train,]$Goal)
table(data.glm.test.pred2,data[-train,]$Goal)
table(data.glm.test.pred3,data[-train,]$Goal)
table(data.glm.test.pred4,data[-train,]$Goal)
table(data.glm.test.pred5,data[-train,]$Goal)
table(data.glm.test.pred6,data[-train,]$Goal)
table(data.glm.test.pred7,data[-train,]$Goal)
table(data.glm.test.pred8,data[-train,]$Goal)
table(data.glm.test.pred9,data[-train,]$Goal)

# LOOCV
data.glm.cv.1 <- cv.glm(data[train,],data.glm.fit1)
data.glm.cv.2 <- cv.glm(data[train,],data.glm.fit2)
data.glm.cv.3 <- cv.glm(data[train,],data.glm.fit3)
data.glm.cv.4 <- cv.glm(data[train,],data.glm.fit4)
data.glm.cv.5 <- cv.glm(data[train,],data.glm.fit5)
data.glm.cv.6 <- cv.glm(data[train,],data.glm.fit6)
data.glm.cv.7 <- cv.glm(data[train,],data.glm.fit7)
data.glm.cv.8 <- cv.glm(data[train,],data.glm.fit8)
data.glm.cv.9 <- cv.glm(data[train,],data.glm.fit9)

data.glm.cv.1$delta
data.glm.cv.2$delta
data.glm.cv.3$delta
data.glm.cv.4$delta
data.glm.cv.5$delta
data.glm.cv.6$delta
data.glm.cv.7$delta
data.glm.cv.8$delta
data.glm.cv.9$delta

# K-fold Validation
set.seed(1)
data.glm.cv.5.1 <- cv.glm(data[train,],data.glm.fit1,K=5)
set.seed(1)
data.glm.cv.10.1 <- cv.glm(data[train,],data.glm.fit1,K=10)

set.seed(1)
data.glm.cv.5.2 <- cv.glm(data[train,],data.glm.fit2,K=5)
set.seed(1)
data.glm.cv.10.2 <- cv.glm(data[train,],data.glm.fit2,K=10)

set.seed(1)
data.glm.cv.5.3<- cv.glm(data[train,],data.glm.fit3,K=5)
set.seed(1)
data.glm.cv.10.3 <- cv.glm(data[train,],data.glm.fit3,K=10)

set.seed(1)
data.glm.cv.5.4 <- cv.glm(data[train,],data.glm.fit4,K=5)
set.seed(1)
data.glm.cv.10.4 <- cv.glm(data[train,],data.glm.fit4,K=10)

set.seed(1)
data.glm.cv.5.5 <- cv.glm(data[train,],data.glm.fit5,K=5)
set.seed(1)
data.glm.cv.10.5 <- cv.glm(data[train,],data.glm.fit5,K=10)

set.seed(1)
data.glm.cv.5.6 <- cv.glm(data[train,],data.glm.fit6,K=5)
set.seed(1)
data.glm.cv.10.6 <- cv.glm(data[train,],data.glm.fit6,K=10)

set.seed(1)
data.glm.cv.5.7 <- cv.glm(data[train,],data.glm.fit7,K=5)
set.seed(1)
data.glm.cv.10.7 <- cv.glm(data[train,],data.glm.fit7,K=10)

set.seed(1)
data.glm.cv.5.8 <- cv.glm(data[train,],data.glm.fit8,K=5)
set.seed(1)
data.glm.cv.10.8 <- cv.glm(data[train,],data.glm.fit8,K=10)

set.seed(1)
data.glm.cv.5.9 <- cv.glm(data[train,],data.glm.fit9,K=5)
set.seed(1)
data.glm.cv.10.9 <- cv.glm(data[train,],data.glm.fit9,K=10)

data.glm.cv.5.1$delta
data.glm.cv.10.1$delta
data.glm.cv.5.2$delta
data.glm.cv.10.2$delta
data.glm.cv.5.3$delta
data.glm.cv.10.3$delta
data.glm.cv.5.4$delta
data.glm.cv.10.4$delta
data.glm.cv.5.5$delta
data.glm.cv.10.5$delta
data.glm.cv.5.6$delta
data.glm.cv.10.6$delta
data.glm.cv.5.7$delta
data.glm.cv.10.7$delta
data.glm.cv.5.8$delta
data.glm.cv.10.8$delta
data.glm.cv.5.9$delta
data.glm.cv.10.9$delta

