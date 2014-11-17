# import dataset, training and test sets
# strings are not imported as factors
train <- read.csv("train.csv", sep=",",stringsAsFactors=FALSE)
test <- read.csv("test.csv", sep=",",stringsAsFactors=FALSE)

# decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(Survived ~ Pclass + Sex + Age + 
                     SibSp + Parch + Fare + Embarked, data=train, method="class")

fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)


# Add features
train <- read.csv("train.csv", sep=",",stringsAsFactors=FALSE)
test <- read.csv("test.csv", sep=",",stringsAsFactors=FALSE)

test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, 
                      FUN=function(x) {strsplit(x, 
                                                split='[,.]')[[1]][2]})
# get rid of space in front
combi$Title <- sub(' ', '', combi$Title)
# group identical titles together
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady',
                               'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

# New feature, family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1
# Group people by family name
combi$Surname <- sapply(combi$Name, 
                        FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize),
                        combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

# quelques problemes avec les petites familles
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# split back
train <- combi[1:891,]
test <- combi[892:1309,]

fit2 <- rpart(Survived ~ Pclass + Sex + Age + 
                     SibSp + Parch + Fare + Embarked + 
                     Title + FamilySize + FamilyID, 
             data=train, method="class")

fancyRpartPlot(fit2)
Prediction <- predict(fit2, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myseconddtree.csv", row.names = FALSE)

## Random Forests
# Get rid of Age NA's
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare 
                + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
#fancyRpartPlot(Agefit)

# one blank in embarked
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# one na in fare
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)      

# number of levels for RF in R is 32
# 3 members families will also be small
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

library(randomForest)
set.seed(415)
# split back
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age 
                    + SibSp + Parch + Fare + Embarked +
                            Title + FamilySize +
                            FamilyID2, 
                    data=train, importance=TRUE, ntree=2000)
test$Sex <- as.factor(test$Sex)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId,
                     Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)


# another type of forests
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex +
                       Age + SibSp + Parch + Fare +
                       Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId,
                     Survived = Prediction)
write.csv(submit, file = "firstcforest.csv", row.names = FALSE)