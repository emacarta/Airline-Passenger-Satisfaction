#-# LIBRERIE #-#----------------------------------------------------------------
library(readr)
library(magrittr)
library(dplyr)
library(fastDummies)
library(caret)
library(caTools)
library(ROCR)

#-# DATASET #-#-----------------------------------------------------------------
airline_train <- read_csv("Desktop/Machine Learning/ProgettoML/airline-train.csv", 
                          col_types = cols(Gender = col_factor(levels = c()), 
                                           `Customer Type` = col_factor(levels = c()), 
                                           Age = col_number(), `Type of Travel` = col_factor(levels = c()), 
                                           Class = col_factor(levels = c()), 
                                           `Flight Distance` = col_number(), 
                                           `Inflight wifi service` = col_factor(levels = c()), 
                                           `Departure/Arrival time convenient` = col_factor(levels = c()), 
                                           `Ease of Online booking` = col_factor(levels = c()), 
                                           `Gate location` = col_factor(levels = c()), 
                                           `Food and drink` = col_factor(levels = c()), 
                                           `Online boarding` = col_factor(levels = c()), 
                                           `Seat comfort` = col_factor(levels = c()), 
                                           `Inflight entertainment` = col_factor(levels = c()), 
                                           `On-board service` = col_factor(levels = c()), 
                                           `Leg room service` = col_factor(levels = c()), 
                                           `Baggage handling` = col_factor(levels = c()), 
                                           `Checkin service` = col_factor(levels = c()), 
                                           `Inflight service` = col_factor(levels = c()), 
                                           Cleanliness = col_factor(levels = c()), 
                                           `Departure Delay in Minutes` = col_number(), 
                                           `Arrival Delay in Minutes` = col_number(), 
                                           satisfaction = col_factor(levels = c())))

airline_test <- read_csv("Desktop/Machine Learning/ProgettoML/airline-test.csv",
                         col_types = cols(Gender = col_factor(levels = c()), 
                                          `Customer Type` = col_factor(levels = c()), 
                                          Age = col_number(), `Type of Travel` = col_factor(levels = c()), 
                                          Class = col_factor(levels = c()), 
                                          `Flight Distance` = col_number(), 
                                          `Inflight wifi service` = col_factor(levels = c()), 
                                          `Departure/Arrival time convenient` = col_factor(levels = c()), 
                                          `Ease of Online booking` = col_factor(levels = c()), 
                                          `Gate location` = col_factor(levels = c()), 
                                          `Food and drink` = col_factor(levels = c()), 
                                          `Online boarding` = col_factor(levels = c()), 
                                          `Seat comfort` = col_factor(levels = c()), 
                                          `Inflight entertainment` = col_factor(levels = c()), 
                                          `On-board service` = col_factor(levels = c()), 
                                          `Leg room service` = col_factor(levels = c()), 
                                          `Baggage handling` = col_factor(levels = c()), 
                                          `Checkin service` = col_factor(levels = c()), 
                                          `Inflight service` = col_factor(levels = c()), 
                                          Cleanliness = col_factor(levels = c()), 
                                          `Departure Delay in Minutes` = col_number(), 
                                          `Arrival Delay in Minutes` = col_number(), 
                                          satisfaction = col_factor(levels = c())))


#-# OPERAZIONI SUL DATASET DI TRAINING #-#--------------------------------------
data <- airline_train
data <- data[,-c(1,2)]
names(data) <- gsub(" ", "_", names(data))
names(data) <- gsub("/", "_", names(data))
names(data) <- gsub("-", "_", names(data))
#rimuovo le colonne che sono meno importanti
data <- data[,-c(1,3,10)]
#sostituisco gli na presenti con la media della coloanna 
data$Arrival_Delay_in_Minutes[is.na(data$Arrival_Delay_in_Minutes)]<-mean(data$Arrival_Delay_in_Minutes,na.rm=TRUE)

col_numeriche <- scale(data[,c(4,18,19)])
col_numeriche <- as.data.frame(col_numeriche)

data$Flight_Distance <- col_numeriche$Flight_Distance
data$Departure_Delay_in_Minutes <- col_numeriche$Departure_Delay_in_Minutes
data$Arrival_Delay_in_Minutes <- col_numeriche$Arrival_Delay_in_Minutes

levels(data$satisfaction)[levels(data$satisfaction) == "neutral or dissatisfied"] <- "0"
levels(data$satisfaction)[levels(data$satisfaction) == "satisfied"] <- "1"

df_train <- data

#-# OPERAZIONI SUL DATASET DI TESTING #-#---------------------------------------
data_test <- airline_test
data_test <- data_test[,-c(1,2)]
names(data_test) <- gsub(" ", "_", names(data_test))
names(data_test) <- gsub("/", "_", names(data_test))
names(data_test) <- gsub("-", "_", names(data_test))

data_test <- data_test[,-c(1,3,10)]

#sostituisco gli na presenti con la media della coloanna 
data_test$Arrival_Delay_in_Minutes[is.na(data_test$Arrival_Delay_in_Minutes)]<-mean(data_test$Arrival_Delay_in_Minutes,na.rm=TRUE)

col_numeriche_test <- scale(data_test[,c(4,18,19)])
col_numeriche_test <- as.data.frame(col_numeriche_test)

data_test$Flight_Distance <- col_numeriche_test$Flight_Distance
data_test$Departure_Delay_in_Minutes <- col_numeriche_test$Departure_Delay_in_Minutes
data_test$Arrival_Delay_in_Minutes <- col_numeriche_test$Arrival_Delay_in_Minutes

levels(data_test$satisfaction)[levels(data_test$satisfaction) == "neutral or dissatisfied"] <- "0"
levels(data_test$satisfaction)[levels(data_test$satisfaction) == "satisfied"] <- "1"

df_test <- data_test



#-# MODELLI #-#-----------------------------------------------------------------


LogiRegModel <- glm( satisfaction ~ ., data = df_train, family = "binomial")

prob <- predict(LogiRegModel,df_test, type = "response")
pred <- ifelse(prob > 0.5, 1, 0)

confusionMatrix(factor(pred), factor(df_test$satisfaction), positive = as.character(1))




ROCPred <- prediction(prob, df_test$satisfaction) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.8, .2, auc, title = "AUC", cex = 1)





 







