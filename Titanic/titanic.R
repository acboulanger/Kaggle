# import dataset, training and test sets
# strings are not imported as factors
train <- read.csv("train.csv", sep=",",stringsAsFactors=FALSE)
test <- read.csv("test.csv", sep=",",stringsAsFactors=FALSE)

# Proportion of perished/survived
prop.table(table(train$Survived))
# Results are
# 0         1 
# 0.6161616 0.3838384

# Given proportion, decide all perished is a coherent prediction
test$Survived <- rep(0, 418)

# submit results
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# Have a look at other variables
# Sex
summary(train$Sex)
# female   male
# 314    577

prop.table(table(train$Sex, train$Survived),1)
#       0         1
# female 0.2579618 0.7420382
# male   0.8110919 0.188908

# A majority of female survived, most of the men died
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allmenperish.csv", row.names = FALSE)

# Age
train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#       Child    Sex    Survived
# 1     0       female  0.7528958
# 2     1       female  0.6909091
# 3     0       male    0.1657033
# 4     1       male    0.3965517

#Does not change the predictions...still men died, even when children

# Let us look at the fare variable
# First create categories
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# a new prediction based on aggregated results above
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allmenrichwomenperish.csv", row.names = FALSE)


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
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# split back
train <- combi[1:891,]
test <- combi[892:1309,]
