#Yanxi Liu, Ria Pinjani, Kristen Wingrove

library(tidyverse)
library(gtools)
library(ROCR)

# Question B1

## B1.1 & B1.2
set.seed(2048)

data <- readr::read_csv('../data_hw4/sqf_08_16.csv')

sqf_data <- data %>%
        filter(year >= 2013 & year <= 2015,
               suspected.crime == "cpw") %>%
        mutate(precinct = as.factor(precinct),
               time.period = as.factor(time.period)) %>%
        select(id, year, found.weapon, precinct, location.housing, starts_with("additional."), starts_with("stopped.bc."), 
               suspect.age, suspect.build, suspect.sex, suspect.weight, suspect.height, inside, radio.run, officer.uniform, 
               observation.period, day, month, time.period) %>%
        filter(complete.cases(.))

## B1.3
row_shuffle <- sample(nrow(sqf_data))
sqf_data <- sqf_data[row_shuffle,]

train_sqf <- sqf_data %>% slice(1:(nrow(sqf_data)*.6))
validation_sqf <- sqf_data %>% slice((nrow(sqf_data)*.6+1):(nrow(sqf_data)*.8))
test_sqf <- sqf_data %>% slice((nrow(sqf_data)*.8+1):nrow(sqf_data))

# Question B.2

## B2.1 
names <- sqf_data %>% select(-id, -year, - found.weapon, -precinct)
names <- colnames(names)
names <- combinations(33,2,names)

model_performance <- tibble(feature_one = names[,1], feature_two = names[,2], validation_auc = rep(NA,528))

for(i in 1:nrow(model_performance)){
  
  glm_train <- train_sqf %>% select(found.weapon, precinct, as.character(model_performance[i,1]), as.character(model_performance[i,2]))
  glm_model <- glm(found.weapon ~., data = glm_train, family = 'binomial')
  glm_predict <- predict(glm_model, validation_sqf, type = 'response')
  
  valid_pred_auc <- prediction(glm_predict, validation_sqf$found.weapon)
  valid_auc <- performance(valid_pred_auc, "auc")
  
  model_performance[i,3] <- valid_auc@y.values[[1]]
  
}

## B2.2 - The combination of variables which maximizes the AUC is found.weapon ~ precinct + location.housing + stopped.bc.object.

## B2.3

b23_data <- rbind(train_sqf,validation_sqf)
b23_model <- glm(found.weapon ~ precinct + location.housing + stopped.bc.object, data = b23_data, family = 'binomial')
b23_predict <- predict(b23_model, test_sqf, type = 'response')
b23_pred_auc <- prediction(b23_predict, test_sqf$found.weapon)
b23_auc <- performance(b23_pred_auc, "auc")

b23_auc_value <- b23_auc@y.values[[1]]

## B2.4 

png("figures/question_b2.png")
hist(model_performance$validation_auc, xlab = "AUC", main = "Histogram of AUC Values", xlim = c(.62, .78), col = "grey")
abline(v = b23_auc_value, col = "red")
abline(v = model_performance$validation_auc[330],  col = "red", lty = 2)
dev.off()

## B2.5 - For your final fitted model, should you use the model from Question B2.3 or from Question B2.2? Which of the two values (corresponding to the red lines) from Question B2.4 gives the best estimate of the expected AUC of this final fitted model on out-of-sample data?

# Question B3

## B3.1 

sqf_pre_2015 <- sqf_data %>% filter(year <= 2014) %>% select(-id, - year)
sqf_2015 <- sqf_data %>% filter(year == 2015) %>% select(-id, -year)

## B3.2

shuffle_pre15 <- sample(nrow(sqf_pre_2015))
sqf_pre_2015 <- sqf_pre_2015[shuffle_pre15,]

sqf_pre_train <- sqf_pre_2015 %>% slice(1:(nrow(sqf_pre_2015)/2))
sqf_pre_test <- sqf_pre_2015 %>% slice((nrow(sqf_pre_2015)/2+1):nrow(sqf_pre_2015))

## B3.3

b33_model <- glm(found.weapon~., data = sqf_pre_train, family = 'binomial')

## B3.4

### AUC on Testing Data
b33_predict_pre <- predict(b33_model, sqf_pre_test, type = 'response')
b33_predict_pre_auc <- prediction(b33_predict_pre, sqf_pre_test$found.weapon)
b33_auc_pre <- performance(b33_predict_pre_auc, "auc")
(b33_auc_pre_value <- b33_auc_pre@y.values[[1]])
 
### AUC on sqf_2015
b33_predict_test <- predict(b33_model, sqf_2015, type = 'response')
b33_predict_test_auc <- prediction(b33_predict_test, sqf_2015$found.weapon)
b33_auc_test <- performance(b33_predict_test_auc, "auc")
(b33_auc_test_value <- b33_auc_test@y.values[[1]])

## B3.5 - Write a paragraph answering the following questions:
###i. Why do you think the AUC on sqf_pre_test is noticeably higher than the AUC on sqf_2015?
###ii. If you were planning to use this model to guide how officers make stops in the future (e.g., by having officers use the model to compute the probability that an individual suspected of criminal possession of a weapon will have a weapon, and then only making a stop if the model-estimated probability is sufficiently high), would the AUC on sqf_pre_test or sqf_2015 be a better estimate of performance on unseen data?
###iii. More generally, when evaluating a model using a simple training/validation split approach, should you always do the split by shuffling and splitting randomly?

# Question B4

## B4.1 

b4_data <- data %>%
    filter(suspected.crime == "cpw") %>%
    mutate(precinct = as.factor(precinct),
           time.period = as.factor(time.period)) %>%
    select(id, year, found.weapon, precinct, location.housing, starts_with("additional."), starts_with("stopped.bc."), 
           suspect.age, suspect.build, suspect.sex, suspect.weight, suspect.height, inside, radio.run, officer.uniform, 
           observation.period, day, month, time.period) %>%
    filter(complete.cases(.))

b4_data_08 <- b4_data %>% filter(year==2008) %>% select(-id, -year)

model_08 <- glm(found.weapon~., data = b4_data_08, family = 'binomial')

year <- c(2009,2010,2011,2012,2013,2014,2015,2016)
auc_vector <- rep(NA, 8)

for(k in 1:length(year)){
  loop_year <- year[k]
  loop_data <- b4_data %>% filter(year == loop_year) %>% select(-id, -year)
  predict_loop <- predict(model_08, subset(loop_data, precinct != 121), type = 'response')
  predict_loop_auc <- prediction(predict_loop, subset(loop_data$found.weapon, loop_data$precinct != 121))
  auc_loop <- performance(predict_loop_auc, "auc")
  auc_vector[k] <- auc_loop@y.values[[1]]
}

## B4.2

png("figures/question_b4.png")
plot(x = year, y = auc_vector, main = "AUC Scores", ylab = "AUC", xlab = "Year", pch = 15, col = "blue")
dev.off()

##In your writeup, report what you see in this plot, and why this might occur.
