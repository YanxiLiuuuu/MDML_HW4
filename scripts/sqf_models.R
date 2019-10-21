# Load packages
library(tidyverse)
library(lubridate)
library(class)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readr)
require(ROCR)
library(gtools)
set.seed(2048)

#B1.2

#Read data
#setwd("~/Desktop/2047 messy data/assignment/assignment4")

relevant_cols <- c('id', 'year', 
                   'found.weapon','suspect.age', 'suspect.build', 
                   'suspect.sex', 'suspect.height', 'suspect.weight',
                   'stopped.bc.desc', 'stopped.bc.violent', 'stopped.bc.other', 
                   'stopped.bc.object', 'stopped.bc.casing', 
                   'stopped.bc.lookout', 'stopped.bc.drugs', 
                   'stopped.bc.clothing', 'stopped.bc.furtive', 
                   'stopped.bc.bulge', 
                   'precinct', 'inside', 'location.housing', 
                   'observation.period', 'additional.report', 
                   'additional.investigation', 'additional.proximity', 
                   'additional.evasive', 'additional.associating', 
                   'additional.direction', 'additional.highcrime', 
                   'additional.time', 'additional.sights', 'additional.other', 
                   'radio.run', 'day', 'month', 'time.period','officer.uniform')

sqf_data<-read_csv("../data_hw4/sqf_08_16.csv") 
#test<-sqf_data
sqf_data<- sqf_data %>%
  filter(suspected.crime=="cpw")%>%
  filter(year>=2013 & year<=2015)

sqf_data<- sqf_data %>%
  dplyr::select(relevant_cols)%>%
  mutate(precinct = factor(precinct),
         time.period = factor(time.period))

sqf_data<- sqf_data %>%
  filter(complete.cases(sqf_data))

#B1.2 
#Randomly shuffle the rows of sqf_data
sqf_data<- sqf_data %>%slice(sample(1:n())) 

split_size = floor(nrow(sqf_data)*0.6)
split_size2 = floor(nrow(sqf_data)*0.2)

train_sqf<-sqf_data %>% slice(1:split_size)
validation_sqf<-sqf_data %>% slice(split_size+1:split_size+split_size2)
test_sqf<-sqf_data %>% slice(split_size+split_size2+1:n())

#B2.1

get_auc<-function(name1,name2){
 # mod<-glm(found.weapon~name1+name2+precinct,data=train_sqf, family = 'binomial')
  mod<-glm(as.formula(paste(colnames(train_sqf)[3], "~",
                            paste(name1,"+",name2),sep = "")),data=train_sqf,family="binomial")
  
  validation_sqf<-validation_sqf%>%
   mutate(predicted.probability =predict(mod, validation_sqf, type='response'))
 validation.pred <- prediction(validation_sqf$predicted.probability, validation_sqf$found.weapon)
  validation.perf <- performance(validation.pred, "auc")
  auc<-validation.perf@y.values[[1]]
  return(auc)
}


get_auc<-function(mod){
    validation_sqf<-validation_sqf%>%
    mutate(predicted.probability =predict(mod, validation_sqf, type='response'))
  validation.pred <- prediction(validation_sqf$predicted.probability, validation_sqf$found.weapon)
  validation.perf <- performance(validation.pred, "auc")
  auc<-validation.perf@y.values[[1]]
  return(auc)
}

train_sqf_comb<-train_sqf%>%select(-year,-id,-precinct,-found.weapon)
combination<-combinations(33,2,colnames(train_sqf_comb))
model_performance<-tibble(feature_one=combination[,1],feature_two=combination[,2],validation_auc=rep(NA,528))

auc<-vector()
for (i in 1:528){
  mod<-glm(as.formula(paste(colnames(train_sqf)[3], "~",
                            paste(combination[i,1],"+",combination[i,2]),sep = "")),data=train_sqf,family="binomial")
  auc[i]<-get_auc(mod =mod )
}
model_performance$validation_auc<-auc


for (i in 1:528){
  auc[i]<-get_auc(name1 = combination[i,1],name2 =combination[i,2]) 
}
model_performance$validation_auc<-auc

model_performance$feature_one[which(auc==max(auc))]
model_performance$feature_two[which(auc==max(auc))]


model_performance %>% 
  rowwise() %>% 
  do(data.frame(validation_auc=get_auc(name1 = .$feature_one,name2=.$feature_two)))

#feature one
model_performance$feature_one[which(model_performance$validation_auc==max(model_performance$validation_auc))]

#feature two
model_performance$feature_two[which(model_performance$validation_auc==max(model_performance$validation_auc))]

write.csv(model_performance,"data/model_performance.csv")

# SOMETHING RANDOM
