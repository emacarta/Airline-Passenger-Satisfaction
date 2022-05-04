#-# LIBRERIE #-#----------------------------------------------------------------
library(readr)
library(magrittr)
library(dplyr)
library(fastDummies)
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

data <- airline_train

data <- data[,-c(1,2)]
names(data) <- gsub(" ", "_", names(data))


#-# OPERAZIONI SUL DATASET DI TRAINING #-#--------------------------------------

data <- data[,-c(1,3,10)]

#sostituisco gli na presenti con la media della coloanna 
data$Arrival_Delay_in_Minutes[is.na(data$Arrival_Delay_in_Minutes)]<-mean(data$Arrival_Delay_in_Minutes,na.rm=TRUE)

#trasformazione colonne non binarie 
df_train <- dummy_cols(data, select_columns = 'Class')
df_train <- dummy_cols(df_train, select_columns = 'Inflight_wifi_service')
df_train <- dummy_cols(df_train, select_columns = 'Departure/Arrival_time_convenient')
df_train <- dummy_cols(df_train, select_columns = 'Ease_of_Online_booking')
df_train <- dummy_cols(df_train, select_columns = 'Food_and_drink')
df_train <- dummy_cols(df_train, select_columns = 'Online_boarding')
df_train <- dummy_cols(df_train, select_columns = 'Seat_comfort')
df_train <- dummy_cols(df_train, select_columns = 'Inflight_entertainment')
df_train <- dummy_cols(df_train, select_columns = 'On-board_service')
df_train <- dummy_cols(df_train, select_columns = 'Leg_room_service')
df_train <- dummy_cols(df_train, select_columns = 'Baggage_handling')
df_train <- dummy_cols(df_train, select_columns = 'Checkin_service')
df_train <- dummy_cols(df_train, select_columns = 'Inflight_service')
df_train <- dummy_cols(df_train, select_columns = 'Cleanliness')

df_train <- df_train[,-c(3,5:17)]


df_train <- df_train[, c('Flight_Distance', 'Departure_Delay_in_Minutes',
                         'Arrival_Delay_in_Minutes', 'Class_Business', 'Class_Eco',
                         'Class_Eco Plus', 'Inflight_wifi_service_0', 'Inflight_wifi_service_1',
                         'Inflight_wifi_service_2', 'Inflight_wifi_service_3',
                         'Inflight_wifi_service_4', 'Inflight_wifi_service_5',
                         'Departure/Arrival_time_convenient_0',
                         'Departure/Arrival_time_convenient_1',
                         'Departure/Arrival_time_convenient_2',
                         'Departure/Arrival_time_convenient_3',
                         'Departure/Arrival_time_convenient_4',
                         'Departure/Arrival_time_convenient_5', 'Ease_of_Online_booking_0',
                         'Ease_of_Online_booking_1', 'Ease_of_Online_booking_2',
                         'Ease_of_Online_booking_3', 'Ease_of_Online_booking_4',
                         'Ease_of_Online_booking_5', 'Food_and_drink_0', 'Food_and_drink_1',
                         'Food_and_drink_2', 'Food_and_drink_3', 'Food_and_drink_4',
                         'Food_and_drink_5', 'Online_boarding_0', 'Online_boarding_1',
                         'Online_boarding_2', 'Online_boarding_3', 'Online_boarding_4',
                         'Online_boarding_5', 'Seat_comfort_0', 'Seat_comfort_1',
                         'Seat_comfort_2', 'Seat_comfort_3', 'Seat_comfort_4', 'Seat_comfort_5',
                         'Inflight_entertainment_0', 'Inflight_entertainment_1',
                         'Inflight_entertainment_2', 'Inflight_entertainment_3',
                         'Inflight_entertainment_4', 'Inflight_entertainment_5',
                         'On-board_service_0', 'On-board_service_1', 'On-board_service_2',
                         'On-board_service_3', 'On-board_service_4', 'On-board_service_5',
                         'Leg_room_service_0', 'Leg_room_service_1', 'Leg_room_service_2',
                         'Leg_room_service_3', 'Leg_room_service_4', 'Leg_room_service_5',
                         'Baggage_handling_1', 'Baggage_handling_2', 'Baggage_handling_3',
                         'Baggage_handling_4', 'Baggage_handling_5', 'Checkin_service_0',
                         'Checkin_service_1', 'Checkin_service_2', 'Checkin_service_3',
                         'Checkin_service_4', 'Checkin_service_5', 'Inflight_service_0',
                         'Inflight_service_1', 'Inflight_service_2', 'Inflight_service_3',
                         'Inflight_service_4', 'Inflight_service_5', 'Cleanliness_0',
                         'Cleanliness_1', 'Cleanliness_2', 'Cleanliness_3', 'Cleanliness_4',
                         'Cleanliness_5', 'Customer_Type', 'Type_of_Travel')]

col_numeriche <- scale(df_train[,c(1,2,3)])
col_numeriche <- as.data.frame(col_numeriche)

df_train$Flight_Distance <- col_numeriche$Flight_Distance
df_train$Departure_Delay_in_Minutes <- col_numeriche$Departure_Delay_in_Minutes
df_train$Arrival_Delay_in_Minutes <- col_numeriche$Departure_Delay_in_Minutes

#trasformazioni delle colonne binarie
levels(df_train$Customer_Type)[levels(df_train$Customer_Type) == "Loyal Customer"] <- "1"
levels(df_train$Customer_Type)[levels(df_train$Customer_Type) == "disloyal Customer"] <- "0"
levels(df_train$Type_of_Travel)[levels(df_train$Type_of_Travel) == "Personal Travel"] <- "0"
levels(df_train$Type_of_Travel)[levels(df_train$Type_of_Travel) == "Business travel"] <- "1"

#creazione del target
target_train <- data$satisfaction
target_train <- as.data.frame(target_train)
colnames(target_train) <- c('satisfaction')
levels(target_train$satisfaction)[levels(target_train$satisfaction) == "neutral or dissatisfied"] <- "0"
levels(target_train$satisfaction)[levels(target_train$satisfaction) == "satisfied"] <- "1"

