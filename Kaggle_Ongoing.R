library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(gbm)
library(MASS)
library(ISLR)

setwd("~/Desktop/5200/pricelala2")

# For the following code to work, ensure analysisData.csv and scoringData.csv are in your working directory.
data = read.csv('analysisData.csv')
scoringData = read.csv('scoringData.csv')

# data = data %>%
#   filter(!is.na(beds),!is.na(reviews_per_month))

data$reviews_per_month[is.na(data$reviews_per_month)] = mean(data$reviews_per_month, na.rm = T)
data$beds[is.na(data$beds)] = mean(data$beds, na.rm = T)
sum(is.na(data$reviews_per_month))

scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)] = mean(scoringData$reviews_per_month, na.rm = T)
scoringData$beds[is.na(scoringData$beds)] = mean(scoringData$beds, na.rm = T)
sum(is.na(scoringData$beds))

data$security_dep2 = ifelse(data$security_deposit > 0, 1, 0)
data$security_dep2[is.na(data$security_dep2)] <- 0
data$security_dep2 <- as.factor(data$security_dep2)

scoringData$security_dep2 = ifelse(scoringData$security_deposit > 0, 1, 0)
scoringData$security_dep2[is.na(scoringData$security_dep2)] <- 0
scoringData$security_dep2 <- as.factor(scoringData$security_dep2)

data$cleaning_fee2 = ifelse(data$cleaning_fee > 0, 1, 0)
data$cleaning_fee2[is.na(data$cleaning_fee2)] <- 0
data$cleaning_fee2 <- as.factor(data$cleaning_fee2)

scoringData$cleaning_fee2 = ifelse(scoringData$cleaning_fee > 0, 1, 0)
scoringData$cleaning_fee2[is.na(scoringData$cleaning_fee2)] <- 0
scoringData$cleaning_fee2 <- as.factor(scoringData$cleaning_fee2)

data <- data %>%
  mutate(host_since2 = as.Date(host_since)) %>%
  mutate(host_since2 = as.Date(host_since) - as.Date("2019-01-01")) %>%
  mutate(host_superhost = ifelse(host_is_superhost == "t", 1, 0),
         host_verified = ifelse(host_identity_verified == "t", 1, 0))

scoringData <- scoringData %>%
  mutate(host_since2 = as.Date(host_since)) %>%
  mutate(host_Since2 = as.Date(host_since) - as.Date("2019-01-01")) %>%
  mutate(host_superhost = ifelse(host_is_superhost == "t", 1, 0),
         host_verified = ifelse(host_identity_verified == "t", 1, 0))

set.seed(19950523)
split <- createDataPartition(data$price, p = 0.8, list = FALSE, groups = 100)
train = data[split,]
test = data[-split, ]

# Read data and construct 

# Linear models

model = train(price~accommodates+
                bathrooms+
                bedrooms+
                guests_included+
                extra_people+
                minimum_nights_avg_ntm+
                availability_365+
                number_of_reviews+
                review_scores_rating+
                calculated_host_listings_count+
                beds+
                reviews_per_month+
                review_scores_accuracy+
                review_scores_cleanliness+
                review_scores_checkin+
                review_scores_communication+
                review_scores_location+
                review_scores_value+
                neighbourhood_group_cleansed+
                host_is_superhost+
                host_has_profile_pic+
                host_identity_verified+
                is_location_exact+
                room_type+
                bed_type+
                instant_bookable+
                host_response_time+
                security_dep2+
                cleaning_fee2+
                host_since+
                host_response_rate+
                property_type+
                cancellation_policy+
                require_guest_profile_picture+
                require_guest_phone_verification,data=train, method="lm")
model
summary(model)

model_lm = lm(price~accommodates+
                bathrooms+
                bedrooms+
                guests_included+
                extra_people+
                minimum_nights_avg_ntm+
                availability_365+
                number_of_reviews+
                review_scores_rating+
                calculated_host_listings_count+
                beds+
                reviews_per_month+
                review_scores_accuracy+
                review_scores_cleanliness+
                review_scores_checkin+
                review_scores_communication+
                review_scores_location+
                review_scores_value+
                neighbourhood_group_cleansed+
                host_is_superhost+
                host_has_profile_pic+
                host_identity_verified+
                is_location_exact+
                room_type+
                bed_type+
                instant_bookable+
                host_response_time+
                security_dep2+
                cleaning_fee2+
                host_since+
                host_response_rate+
                property_type+
                cancellation_policy+
                require_guest_profile_picture+
                require_guest_phone_verification,train)
model_lm
summary(model_lm)


# Random forest

trControl=trainControl(method = "cv",number=10)
tuneGrid = expand.grid(mtry=20)
set.seed(19950523)
cvForest = train(price~accommodates+
                   bathrooms+
                   bedrooms+
                   guests_included+
                   extra_people+
                   minimum_nights_avg_ntm+
                   availability_365+
                   number_of_reviews+
                   review_scores_rating+
                   calculated_host_listings_count+
                   beds+
                   reviews_per_month+
                   review_scores_accuracy+
                   review_scores_cleanliness+
                   review_scores_checkin+
                   review_scores_communication+
                   review_scores_location+
                   review_scores_value+
                   neighbourhood_group_cleansed+
                   host_is_superhost+
                   host_has_profile_pic+
                   host_identity_verified+
                   is_location_exact+
                   room_type+
                   bed_type+
                   instant_bookable+
                   host_response_time+
                   security_dep2+
                   cleaning_fee2+
                   host_since+
                   host_response_rate+
                   property_type+
                   cancellation_policy+
                   require_guest_profile_picture+
                   require_guest_phone_verification,data=train,
                 method="rf",
                 ntree=100,
                 trControl=trControl,
                 tuneGrid=tuneGrid)
cvForest

# OOB

trControl2=trainControl(method = "oob")
tuneGrid2 = expand.grid(mtry=20)
set.seed(19950523)
oobForest = train(price~accommodates+
                    bathrooms+
                    bedrooms+
                    guests_included+
                    extra_people+
                    minimum_nights_avg_ntm+
                    availability_365+
                    number_of_reviews+
                    review_scores_rating+
                    calculated_host_listings_count+
                    beds+
                    reviews_per_month+
                    review_scores_accuracy+
                    review_scores_cleanliness+
                    review_scores_checkin+
                    review_scores_communication+
                    review_scores_location+
                    review_scores_value+
                    neighbourhood_group_cleansed+
                    host_is_superhost+
                    host_has_profile_pic+
                    host_identity_verified+
                    is_location_exact+
                    room_type+
                    bed_type+
                    instant_bookable+
                    host_response_time+
                    security_dep2+
                    cleaning_fee2+
                    host_since+
                    host_response_rate+
                    property_type+
                    cancellation_policy+
                    require_guest_profile_picture+
                    require_guest_phone_verification,data=train,
                  method="rf",
                  trControl=trControl2,
                  verbose=TRUE,
                  tuneGrid=tuneGrid2)
 oobForest

# Read scoring data and apply model to generate predictions

pred = predict(model,newdata=test)
test_rmse = sqrt(mean((pred - test$price)^2))
test_rmse

pred_lm = predict(model_lm,newdata=test)
test_lm_rmse = sqrt(mean((pred_lm - test$price)^2))
test_lm_rmse

predcvForest = predict(cvForest,newdata=test)
test_cvForest_rmse = sqrt(mean((predcvForest - test$price)^2))
test_cvForest_rmse

predoobForest = predict(oobForest,newdata=test)
test_oobForest_rmse = sqrt(mean((predoobForest - test$price)^2))
test_oobForest_rmse

# Construct submission from predictions

pred_scoring = predict(model,newdata=scoringData)

pred_lm_scoring = predict(model_lm,newdata=scoringData)

pred_cvForest_scoring = predict(cvForest, newdata=scoringData)

pred_oobForest_scoring = predict(oobForest, newdata=scoringData)

submissionFile = data.frame(id = scoringData$id, price = pred_oobForest_scoring)
write.csv(submissionFile, 'kaggle_submission6.csv',row.names = F)



str(data)
glimpse(data)
summary(data)


# 1
# accommodates
# bathrooms
# bedrooms
# guests_included
# extra_people
# minimum_nights_avg_ntm
# availability_365
# number_of_reviews
# review_scores_rating
# calculated_host_listings_count
# neighbourhood_group_cleansed

# 2
# review_scores_accuracy
# review_scores_cleanliness
# review_scores_checkin
# review_scores_communication
# review_scores_location
# review_scores_value
# *beds
# *reviews_per_month

# 3
# host_is_superhost
# host_has_profile_pic
# host_identity_verified
# is_location_exact
# room_type
# bed_type
# instant_bookable
# host_response_time

# 4
# *security_deposit - convert to Y/N
# *cleaning_fee - convert to Y/N
# host_since
# host_response_rate
# property_type
# cancellation_policy
# require_guest_profile_picture
# require_guest_phone_verification



sum(is.na(data$cancellation_policy))
data$cancelation

summary(data$host_response_rate)
glimpse(data$cancelation)
