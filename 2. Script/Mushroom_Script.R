install.packages("party")
install.packages("varImp")
install.packages("rpart.plot")
library(tidyverse)
library(dplyr)
library(plyr)
library(cowplot)
library(expss)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(RColorBrewer)
library(randomForest)
library(C50)
library(doParallel)
library(partykit)
library(party)
library(varImp)
library(rpart)
library(rpart.plot)

# Javier Villasmil - 29/01/18
# Mushroom Script
# Model Selected: c.50

# Prepare Parallel Process
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#import the Mushrooms dataset
mushrooms <- read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Task 011 - Mushroom\\data_train13.csv")

#inspect Mushroom dataset
View(mushrooms)
head(mushrooms)
summary(mushrooms)

plot(mushrooms$class)
summary(mushrooms$class)


#Remove X and Stalk Root
mushrooms$X <- NULL
mushrooms$stalk.root <- NULL

#decision tree to check variable importance
rp <- rpart(class ~ ., mushrooms, cp =0.0005)
summary(rp)
rpart.plot(rp, type = 1)

as.data.frame(rp$variable.importance)

#Remove Odor <- is a good predictor but cannot be detected by a cellphone. We are going to use only visual attributes.
mushrooms$odor <- NULL

#we run the same decision tree
rp <- rpart(class ~ ., mushrooms, cp =0.0005)
summary(rp)
rpart.plot(rp, type = 1)

as.data.frame(rp$variable.importance)

#Random Forest - variable importance with a random forest
rforest_1 <- randomForest(class ~ .,data = mushrooms,  ntree = 500, do.trace = TRUE, importance = TRUE)
rforest_1$importance[order(rforest_1$importance[,3], decreasing = TRUE),]

#Checking variance in columns
mushroomsZeroVar <- nearZeroVar(mushrooms, saveMetrics = TRUE)
#order WAP's by decreasing FreqRatio
mushroomsZeroVar <- mushroomsZeroVar[order(mushroomsZeroVar$freqRatio, decreasing = TRUE ),]
#check variables
mushroomsZeroVar

plot(mushrooms$veil.color)
plot(mushrooms$gill.attachment)
plot(mushrooms$veil.type)

#we can also remove:
# -> veil type - zero variance
mushrooms$veil.type <- NULL

######## PLOTS ########
## some exploration ##
ggplot(mushrooms, aes(x = mushrooms$gill.color, fill = mushrooms$class)) + 
  geom_histogram(stat = "count")+
  scale_fill_brewer(type = "qual", palette = "Dark2", name="") + ylab("FREQUENCY")


#train and test sets
set.seed(123)
trainingindices <- createDataPartition(mushrooms$class, p = 0.80, list = FALSE)

training <- mushrooms[trainingindices,]
testing  <- mushrooms[-trainingindices,]

#checking sampling distribution
plot(training$class)
plot(testing$class)

#Modeling

#rf <- Random Forest generated two errors.
#set.seed(123)
#control <- trainControl(method="repeatedcv", number=10, repeats=3,verboseIter = TRUE)

#RF_caret <- train(class ~ ., data = training, method = "rf", trainControl = control, importance = TRUE, tuneLenght=5)
#prediction_RF_caret <- predict(RF_caret,testing)
#performance_RF_caret <- postResample(prediction_RF_caret,testing$class)

#confusionMatrix(prediction_RF_caret, testing$class)
# !@#$% I'M DEAD (TWICE)

#checking_set <- testing
#checking_set$pred_RF <- prediction_RF_caret

#c.50
c50_caret <- C5.0(class ~ .,data = training, ntree=500, importance=TRUE, do.trace = TRUE,trainControl = control)
prediction_c50_caret <- predict(c50_caret,testing)
performance_c50_caret <- postResample(prediction_c50_caret,testing$class)

confusionMatrix(prediction_c50_caret, testing$class)

checking_set$pred_c50 <- prediction_c50_caret
# PUUURFECT! I'M ALIVE

#BEST MODEL C.50 - 500 Trees
save(c50_caret, file = "c50_predict_MUSHROOM.rda")


############################################
checking_set %>%
  filter(class == "p" & pred_c50 == "e")

# PUUURFECT! I'M ALIVE
############################################

load("c50_predict_MUSHROOM.rda")

#import the Mushrooms dataset
mushrooms_validation <- read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Task 011 - Mushroom\\data_test13_validation.csv")

TEST_prediction_c50  <- predict(c50_caret,mushrooms_validation)
TEST_performance_c50 <- postResample(TEST_prediction_c50,mushrooms_validation$class)
confusionMatrix(TEST_prediction_c50, mushrooms_validation$class)

mushrooms_validation$class.PREDICTED <- TEST_prediction_c50

write.csv(mushrooms_validation, file = "Mushroom_prediction.csv")

