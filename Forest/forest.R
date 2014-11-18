# library(randomForest)
# print("Upload files")
# train <- read.csv("train.csv")
# test <- read.csv("test.csv")
# set.seed(415)
# 
# # feature engineering
# print("Feature engineering")
# test$Cover_Type <- NA
# combi <- rbind(train, test)
# 
# combi$dist2water <- sqrt(combi$Horizontal_Distance_To_Hydrology*combi$Horizontal_Distance_To_Hydrology +
#                              combi$Vertical_Distance_To_Hydrology*combi$Vertical_Distance_To_Hydrology)
# 
# combi$d11 <- abs(combi$Horizontal_Distance_To_Hydrology + combi$Horizontal_Distance_To_Roadways)
# combi$d12 <- abs(combi$Horizontal_Distance_To_Hydrology - combi$Horizontal_Distance_To_Roadways)
# 
# combi$d21 <- abs(combi$Horizontal_Distance_To_Hydrology + combi$Horizontal_Distance_To_Fire_Points)
# combi$d22 <- abs(combi$Horizontal_Distance_To_Hydrology - combi$Horizontal_Distance_To_Fire_Points)
# 
# combi$d31 <- abs(combi$Horizontal_Distance_To_Roadways + combi$Horizontal_Distance_To_Fire_Points)
# combi$d32 <- abs(combi$Horizontal_Distance_To_Roadways - combi$Horizontal_Distance_To_Fire_Points)
# 
# combi$sign1 <- combi$Vertical_Distance_To_Hydrology >= 0
# combi$sign2 <- combi$Vertical_Distance_To_Hydrology < 0
# 
# combi$ell1 <- combi$Hillshade_3pm^2 + combi$Hillshade_9am^2
# combi$ell2 <- combi$Hillshade_3pm^2 + combi$Hillshade_Noon^2
# combi$ell3 <- combi$Hillshade_Noon^2 + combi$Hillshade_9am^2
# 
# combi$meanShade <- 1/3*(combi$Hillshade_9am +
#                                 combi$Hillshade_Noon +
#                                 combi$Hillshade_3pm)
# combi$orientation <- NA
# combi$orientation[combi$Aspect <= 45 | combi$Aspect > 315] <- "North"
# combi$orientation[combi$Aspect > 45 & combi$Aspect <= 135] <- "West"
# combi$orientation[combi$Aspect > 135 & combi$Aspect <= 225] <- "South"
# combi$orientation[combi$Aspect > 225 & combi$Aspect <= 315] <- "East"
# combi$orientation <- as.factor(combi$orientation)
# 
# combi$wilderness <- NA
# combi$wilderness[combi$Wilderness_Area1==1] <- "1"
# combi$wilderness[combi$Wilderness_Area2==1] <- "2"
# combi$wilderness[combi$Wilderness_Area3==1] <- "3"
# combi$wilderness[combi$Wilderness_Area4==1] <- "4"
# combi$wilderness <- as.factor(combi$wilderness)
# 
# combi$soil <- NA
# featureName = paste("Soil_Type",1,sep="")
# for(i in 2:40)
# {
#         featureName = c(featureName,paste("Soil_Type",i,sep=""))
#         
# }
# i=0
# for(i in 1:40)
# {
#         name=featureName[i]
#         combi$soil[combi[,name]==1] <- i
# }
# 
# combi$soil <- as.factor(combi$soil)
# 
# train <- combi[1:15120,]
# test <- combi[15121:581012,]
# 
# train2 <- train[train$Cover_Type < 3,]

# learning phase
s = rep(0,7560)
s = c(s,rep(1,7560))
s = sample(s)

train1<-train[s==1,]
train2<-train[s==0,]

print("Learning 1...")
fit <- randomForest(as.factor(Cover_Type) ~ Elevation 
                    + Aspect 
                    + Slope 
                    + Horizontal_Distance_To_Hydrology
                    + dist2water
                    + meanShade
                    + orientation
                    + Vertical_Distance_To_Hydrology
                    + Horizontal_Distance_To_Roadways 
                    + Hillshade_9am  
                    + Hillshade_Noon 
                    + Hillshade_3pm 
                    + Horizontal_Distance_To_Fire_Points
                    + d11
                    + d12
                    + d21
                    + d22
                    + d31
                    + d32
                    + sign1
                    + sign2
                    + ell1
                    + ell2
                    + ell3
                    + Wilderness_Area1
                    + Wilderness_Area2
                    + Wilderness_Area3
                    + Wilderness_Area4
                    + wilderness
                    + Soil_Type1
                    + Soil_Type2
                    + Soil_Type3
                    + Soil_Type4
                    + Soil_Type5
                    + Soil_Type6
                    + Soil_Type7
                    + Soil_Type8
                    + Soil_Type9
                    + Soil_Type10
                    + Soil_Type11
                    + Soil_Type12
                    + Soil_Type13
                    + Soil_Type14
                    + Soil_Type15
                    + Soil_Type16
                    + Soil_Type17
                    + Soil_Type18
                    + Soil_Type19
                    + Soil_Type20
                    + Soil_Type21
                    + Soil_Type22
                    + Soil_Type23
                    + Soil_Type24
                    + Soil_Type25
                    + Soil_Type26
                    + Soil_Type27
                    + Soil_Type28
                    + Soil_Type29
                    + Soil_Type30
                    + Soil_Type31
                    + Soil_Type32
                    + Soil_Type33
                    + Soil_Type34
                    + Soil_Type35
                    + Soil_Type36
                    + Soil_Type37
                    + Soil_Type38
                    + Soil_Type39
                    + Soil_Type40,
 #                       +soil,
                    data=train1, importance=TRUE, ntree=200,mtry=40)
 print("..Done")
# #varImpPlot(fit)
 # Prediction
 print("Predicting...")
 PredictionTrain <- predict(fit, train2, predict.all=TRUE, nodes=TRUE,type="prob")
 PredictionTest <- predict(fit, test, predict.all=TRUE, nodes=TRUE,type="prob")
 print("..Done")

# Add new feature (proba)
print("New feature...")
train2$p1 <- PredictionTrain$aggregate[,1]
train2$p2 <- PredictionTrain$aggregate[,2]
train2$p3 <- PredictionTrain$aggregate[,3]
train2$p4 <- PredictionTrain$aggregate[,4]
train2$p5 <- PredictionTrain$aggregate[,5]
train2$p6 <- PredictionTrain$aggregate[,6]
train2$p7 <- PredictionTrain$aggregate[,7]

test$p1 <- PredictionTest$aggregate[,1]
test$p2 <- PredictionTest$aggregate[,2]
test$p3 <- PredictionTest$aggregate[,3]
test$p4 <- PredictionTest$aggregate[,4]
test$p5 <- PredictionTest$aggregate[,5]
test$p6 <- PredictionTest$aggregate[,6]
test$p7 <- PredictionTest$aggregate[,7]


# new forest
print("Learning 2...")
fit2 <- randomForest(as.factor(Cover_Type) ~ Elevation 
                    + Aspect 
                    + Slope 
                    + Horizontal_Distance_To_Hydrology
                    + dist2water
                    + meanShade
                    + orientation
                    + Vertical_Distance_To_Hydrology
                    + Horizontal_Distance_To_Roadways 
                    + Hillshade_9am  
                    + Hillshade_Noon 
                    + Hillshade_3pm 
                    + Horizontal_Distance_To_Fire_Points
                    + d11
                    + d12
                    + d21
                    + d22
                    + d31
                    + d32
                    + sign1
                    + sign2
                    + ell1
                    + ell2
                    + ell3
                    + Wilderness_Area1
                    + Wilderness_Area2
                    + Wilderness_Area3
                    + Wilderness_Area4
                    + wilderness
                    + Soil_Type1
                    + Soil_Type2
                    + Soil_Type3
                    + Soil_Type4
                    + Soil_Type5
                    + Soil_Type6
                    + Soil_Type7
                    + Soil_Type8
                    + Soil_Type9
                    + Soil_Type10
                    + Soil_Type11
                    + Soil_Type12
                    + Soil_Type13
                    + Soil_Type14
                    + Soil_Type15
                    + Soil_Type16
                    + Soil_Type17
                    + Soil_Type18
                    + Soil_Type19
                    + Soil_Type20
                    + Soil_Type21
                    + Soil_Type22
                    + Soil_Type23
                    + Soil_Type24
                    + Soil_Type25
                    + Soil_Type26
                    + Soil_Type27
                    + Soil_Type28
                    + Soil_Type29
                    + Soil_Type30
                    + Soil_Type31
                    + Soil_Type32
                    + Soil_Type33
                    + Soil_Type34
                    + Soil_Type35
                    + Soil_Type36
                    + Soil_Type37
                    + Soil_Type38
                    + Soil_Type39
                    + Soil_Type40
                    + p1
                    + p2
                    + p3
                    + p4
                    + p5
                    + p6
                    + p7,
                    #                       +soil,
                    data=train2, importance=TRUE, ntree=100,mtry=40)

# Prediction
print("Predicting 2...")
Prediction <- predict(fit2, test)
print("..Done")

# submit
 submit <- data.frame(Id = test$Id,
                      Cover_Type = Prediction)
 write.csv(submit, file = "TwoRF.csv", row.names = FALSE)