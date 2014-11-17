libray(randomForest)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
set.seed(415)

fit <- randomForest(as.factor(Cover_Type) ~ Elevation 
                    + Aspect 
                    + Slope 
                    + Horizontal_Distance_To_Hydrology
                    + Vertical_Distance_To_Hydrology
                    + Horizontal_Distance_To_Roadways 
                    + Hillshade_9am  
                    + Hillshade_Noon 
                    + Hillshade_3pm 
                    + Horizontal_Distance_To_Fire_Points
                    + Wilderness_Area1
                    + Wilderness_Area2
                    + Wilderness_Area3
                    + Wilderness_Area4,
                    data=train, importance=TRUE, ntree=2000)
Prediction <- predict(fit, test)
submit <- data.frame(Id = test$Id,
                     Cover_Type = Prediction)
write.csv(submit, file = "ForestType.csv", row.names = FALSE)