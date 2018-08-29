library(readr)  
library(dplyr)

p1.main= read_csv("train.csv")
p1.main <- p1.main[-sample(1:nrow(p1.main), 1), ]
p1.predict <- read_csv("test.csv")

predID <- p1.predict$PassengerId

#dropping cabin due to amount of missing data, name ticket and passenger id due to lack of relevance
p1.main <- subset(p1.main, select = -c(Cabin, Name, PassengerId, Ticket))
p1.predict <- subset(p1.predict, select = -c(Cabin, Name, PassengerId, Ticket))

#finding mean age to fill in for missing values
meanAge <- ceiling(mean(p1.main$Age, na.rm = T))
p1.main[is.na(p1.main)] <- meanAge

meanAge <- ceiling(mean(p1.predict$Age, na.rm = T))
p1.predict[is.na(p1.predict)] <- meanAge

#setting variables as factors where needed
p1.main$Pclass <- factor(p1.main$Pclass)
p1.main$Embarked <- factor(p1.main$Embarked)

p1.predict$Pclass <- factor(p1.predict$Pclass)
p1.predict$Embarked <- factor(p1.predict$Embarked)

#establishing a training set
train.p1 <- sample(1:890, size=445) 
p1.train <- p1.main[train.p1,]
p1.valid <- p1.main[-train.p1,]

#develping logistic model
p1.lg <- glm(Survived~., data=p1.train, family = "binomial")
summary(p1.lg)

probs<-as.vector(predict(p1.lg, type="response"))
preds <- rep(0,445)
preds[probs>0.5] <- 1 
preds
table(preds,p1.valid$Survived)

#dropping SibSp, Parch, Fare, and Embarked due to lack of significance 
p1.lg2 <- glm(Survived~. -SibSp -Parch -Embarked -Fare, data=p1.train, family = "binomial")
summary(p1.lg2)

probs<-as.vector(predict(p1.lg2, type="response"))
preds <- rep(0,445) 
preds[probs>0.5] <- 1
preds
table(preds,p1.valid$Survived)

#applying model to test set
probs<-as.vector(predict(p1.lg2,newdata=p1.predict, type="response"))
preds <- rep(0,418)  
preds[probs>0.5] <- 1

#writting out
final <- data.frame(predID,preds)
names(final) <- c("PassengerId","Survived")
write.csv(final, file = "titanic-predictions.csv")