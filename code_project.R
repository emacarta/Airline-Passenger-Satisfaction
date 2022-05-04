#-# LIBRERIE #-#----------------------------------------------------------------
library(readr)
library(magrittr)
library(dplyr)
#-# DATASET #-#-----------------------------------------------------------------
airline_train <- read_csv("Desktop/Machine Learning/ProgettoML/airline-train.csv", 
                          col_types = cols(Age = col_number(), 
                                           `Flight Distance` = col_number(), 
                                           `Inflight wifi service` = col_character(), 
                                           `Departure/Arrival time convenient` = col_character(), 
                                           `Ease of Online booking` = col_character(), 
                                           `Gate location` = col_character(), 
                                           `Food and drink` = col_character(), 
                                           `Online boarding` = col_character(), 
                                           `Seat comfort` = col_character(), 
                                           `Inflight entertainment` = col_character(), 
                                           `On-board service` = col_character(), 
                                           `Leg room service` = col_character(), 
                                           `Baggage handling` = col_character(), 
                                           `Checkin service` = col_character(), 
                                           `Inflight service` = col_character(), 
                                           Cleanliness = col_character(), `Departure Delay in Minutes` = col_number(), 
                                           `Arrival Delay in Minutes` = col_number()))

airline_test <- read_csv("Desktop/Machine Learning/ProgettoML/airline-test.csv",
                         col_types = cols(Age = col_number(), 
                                          `Flight Distance` = col_number(), 
                                          `Inflight wifi service` = col_character(), 
                                          `Departure/Arrival time convenient` = col_character(), 
                                          `Ease of Online booking` = col_character(), 
                                          `Gate location` = col_character(), 
                                          `Food and drink` = col_character(), 
                                          `Online boarding` = col_character(), 
                                          `Seat comfort` = col_character(), 
                                          `Inflight entertainment` = col_character(), 
                                          `On-board service` = col_character(), 
                                          `Leg room service` = col_character(), 
                                          `Baggage handling` = col_character(), 
                                          `Checkin service` = col_character(), 
                                          `Inflight service` = col_character(), 
                                          Cleanliness = col_character(), `Departure Delay in Minutes` = col_number(), 
                                          `Arrival Delay in Minutes` = col_number()))

data <- airline_train

data <- data[,-c(1,2)]
names(data) <- gsub(" ", "_", names(data))

class(data$Gender) <- 'category'
class(data$Customer_Type) <- 'category'
class(data$Type_of_Travel) <- 'category'
class(data$Class) <- 'category'
class(data$Inflight_wifi_service) <- 'category'
class(data$'Departure/Arrival_time_convenient') <- 'category'
class(data$Ease_of_Online_booking) <- 'category'
class(data$Gate_location) <- 'category'
class(data$Food_and_drink) <- 'category'
class(data$Online_boarding) <- 'category'
class(data$Seat_comfort) <- 'category'
class(data$Inflight_entertainment) <- 'category'
class(data$"On-board_service") <- 'category'
class(data$Leg_room_service) <- 'category'
class(data$Baggage_handling) <- 'category'
class(data$Checkin_service) <- 'category'
class(data$Inflight_service) <- 'category'
class(data$Cleanliness) <- 'category'
class(data$Departure_Delay_in_Minutes) <- 'category'

#-# OPERAZIONI SUL DATASET DI TRAINING #-#--------------------------------------

data <- data[,c(1,3,10)]

data$Arrival_Delay_in_Minutes[is.na(data$Arrival_Delay_in_Minutes)]<-mean(data$Arrival_Delay_in_Minutes,na.rm=TRUE)

