# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readr)
require(ROCR)
library(gtools)
set.seed(2048)

#B1.2

#Read data
sqf_data<-read_csv("../data_hw4/sqf_08_16.csv") 
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

#B1.3
#Randomly shuffle the rows of sqf_data
sqf_data<- sqf_data %>%slice(sample(1:n()))

#split sizes
split_size = floor(nrow(sqf_data)*0.6)
split_size2 = floor(nrow(sqf_data)*0.2)

# split datasets
train_sqf<-sqf_data %>% slice(1:split_size)
validation_sqf<-sqf_data %>% slice(split_size+1:split_size+split_size2)
test_sqf<-sqf_data %>% slice(split_size+split_size2+1:n())

#B2.1

#function to get auc
get_auc<-function(name1,name2){
  mod<-glm(as.formula(paste(colnames(train_sqf)[3], "~",
                            paste(name1,"+",name2, "+precinct"),sep = "")),data=train_sqf,family="binomial")
  
  validation_sqf<-validation_sqf%>%
   mutate(predicted.probability =predict(mod, validation_sqf, type='response'))
 validation.pred <- prediction(validation_sqf$predicted.probability, validation_sqf$found.weapon)
  validation.perf <- performance(validation.pred, "auc")
  auc<-validation.perf@y.values[[1]]
  return(auc)
}


#create 528 combinations
train_sqf_comb<-train_sqf%>%select(-year,-id,-precinct,-found.weapon)
combination<-combinations(33,2,colnames(train_sqf_comb))
model_performance<-tibble(feature_one=combination[,1],feature_two=combination[,2],validation_auc=rep(NA,528))



#get auc and model_performance
auc<-vector()
for (i in 1:528){
  auc[i]<-get_auc(name1 = combination[i,1],name2 =combination[i,2]) 
}
model_performance$validation_auc<-auc

#feature one:"stopped.bc.bulge"
feature1<-model_performance$feature_one[which(auc==max(auc))]
#feature two:"stopped.bc.object"
feature2<-model_performance$feature_two[which(auc==max(auc))]
feature<-data.frame(feature1=feature1,feature2=feature2);feature

#max auc
max_auc<-max(auc);max_auc


#B2.3

#combine 2 datasets
newdata<-rbind(train_sqf,validation_sqf)

#new model
#mod2<-glm(found.weapon~location.housing+stopped.bc.object+precinct,family = "binomial",data=newdata)
mod2<-glm(found.weapon~stopped.bc.bulge+stopped.bc.object+precinct,family = "binomial",data=newdata)

#calculate auc
test_sqf <- test_sqf  %>% mutate(predicted.probability =  
                          predict(mod2, test_sqf, type='response'))
test.pred <- prediction(test_sqf$predicted.probability, test_sqf$found.weapon)
test.perf <- performance(test.pred, "auc")
auc2<-test.perf@y.values[[1]]

#B2.4
p<-ggplot(model_performance, aes(x=validation_auc)) +
  geom_histogram()+
  geom_vline(aes(xintercept=auc2),
             color="red", linetype="solid", size=1)+
  geom_vline(aes(xintercept=max(validation_auc)),
             color="red", linetype="dashed", size=1)+
  ggtitle("Histogram of AUC")+
  xlab("Auc")


ggsave(plot=p, filename='question_b2.png', height=5, width=5,path  ="figures")

#B3.1
sqf_pre_2015<-sqf_data%>%filter(year<2015)%>%
  select(-id,-year)

sqf_2015<-sqf_data%>%filter(year>=2015)%>%
  select(-id,-year)

#B3.2
#split sizes
split_size2 = floor(nrow(sqf_pre_2015)*0.5)

#split datasets
sqf_pre_train<-sqf_pre_2015 %>% slice(1:split_size2)
sqf_pre_test<-sqf_pre_2015 %>%  slice(split_size2+1:n())

#B3.3
#fit the model
mod3<-glm(found.weapon~.,data =sqf_pre_train,family = "binomial" )

#B3.4
#calculate auc
sqf_pre_test <- sqf_pre_test %>% mutate(predicted.probability =  
                                   predict(mod3, sqf_pre_test, type='response'))
test.pred2 <- prediction(sqf_pre_test$predicted.probability, sqf_pre_test$found.weapon)
test.perf2 <- performance(test.pred2, "auc")
auc3<-test.perf2@y.values[[1]]# 0.8121758


#B4.1
#Read data
sqf_data2<-read_csv("../data_hw4/sqf_08_16.csv") 

sqf_data2<- sqf_data2 %>%
  filter(suspected.crime=="cpw")

sqf_data2<- sqf_data2 %>%
  dplyr::select(relevant_cols)%>%
  mutate(precinct = factor(precinct),
         time.period = factor(time.period))

sqf_data2<- sqf_data2 %>%
  filter(complete.cases(sqf_data2))

#create dataset to fit model
sqf_data2_08<-sqf_data2%>%filter(year==2008)%>%select(-id,-year)
#create dataset to calculate auc
sqf_data2_09_16<-sqf_data2%>%filter(year>=2009 & year<=2016)%>%select(-id,-year)

#fit a model
mod4<-glm(found.weapon~.,data =sqf_data2_08,family = "binomial" )

#calculate auc  ???
sqf_data2_09_16_new<-filter(sqf_data2_09_16,precinct!=121)

sqf_data2_09_16 <- sqf_data2_09_16 %>% mutate(predicted.probability =  
                                          predict(mod4, sqf_data2_09_16_new, type='response'))

test.pred3 <- prediction(sqf_data2_09_16$predicted.probability, sqf_data2_09_16$found.weapon)

test.perf3 <- performance(test.pred3, "auc")
auc4<-test.perf3@y.values[[1]]


auc4<-vector()
for (i in 2009:2016){
  sqf<-filter(sqf_data2_09_16,year==i)
  predicted.probability<- predict(mod4, sqf, type='response')
  test.pred <- prediction(predicted.probability, sqf$found.weapon)
  test.perf <- performance(test.pred, "auc")
  auc4[i]<-test.perf@y.values[[1]]
}
  
df<-data.frame(auc4=auc4,year=2009:2016)


#B4.2
p2<-ggplot(data=df,aes(x=year,y=auc4))+
  ggtitle("Year vs AUC")+
  ylab("Auc")+
  xlab("Years")

ggsave(plot=p2, filename='question_b4.png', height=5, width=5,path  ="figures")