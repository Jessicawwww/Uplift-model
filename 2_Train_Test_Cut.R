library(data.table)
library(fasttime)
library(corrplot)
library(moments)
library(glmnet)
library(randomForest)
library(caret)
library(ROSE)
library(gridExtra)
library(reshape2)
library(tidyverse)
library(DataExplorer)
library(dplyr)
library(plyr)
library(xgboost)
options(repr.matrix.max.rows=600, repr.matrix.max.cols=200)
setwd('/home/Uplift Model/AU/gmv_model/data')
library(readxl)
Campaign_calendar_Mega <- read_excel("Campaign_calendar_Mega.xlsx")
Megaweekend = Campaign_calendar_Mega[Campaign_calendar_Mega$campName!='SuperSunday',]
# View(Megaweekend)

##################################################################################
#########                     1) Train Test Split     end                  #######
##################################################################################

###using end week's promotion data to train model
# Loading C2C dataset
end_week = Megaweekend[Megaweekend$WeekType == 'END' & Megaweekend$measureStart >= '2019-02-01' & Megaweekend$measureStart <= '2019-04-30' , 7]


i = 1

measureStart = end_week[i,]

c2c = read.csv(file = paste("AU_C2C_ENGAGED_",as.Date(measureStart$measureStart),".csv",sep=""), header = T)
colnames(c2c) = tolower(colnames(c2c))
# names(c2c)
c2c$promo_gmv_ind_2d = 0
c2c$promo_gmv_ind_2d[c2c$promo_gmv_2d>0] = 1
# table(c2c$promo_lstg_cnt_ind_2d)

c2c_t =c2c%>% filter(tc_ind==1)
trainIndex_t = createDataPartition((c2c_t$promo_gmv_ind_2d), p = .7, list = FALSE)
c2c_t_train = c2c_t[trainIndex_t,]
c2c_t_test = c2c_t[-trainIndex_t,]
c2c_c =c2c%>% filter(tc_ind==0)
trainIndex_c = createDataPartition((c2c_c$promo_gmv_ind_2d), p = .7, list = FALSE)
c2c_c_train = c2c_c[trainIndex_c,]
c2c_c_test = c2c_c[-trainIndex_c,]


for (i in (2: lengths(end_week)))
{
  measureStart = end_week[i,]
  c2c = read.csv(file = paste("AU_C2C_ENGAGED_",as.Date(measureStart$measureStart),".csv",sep=""), header = T)
  colnames(c2c) = tolower(colnames(c2c))
  # names(c2c)
  c2c$promo_gmv_ind_2d = 0
  c2c$promo_gmv_ind_2d[c2c$promo_gmv_2d>0] = 1
  # table(c2c$promo_lstg_cnt_ind_2d)
  
  c2c_t =c2c%>% filter(tc_ind==1)
  trainIndex_t = createDataPartition((c2c_t$promo_gmv_ind_2d), p = .7, list = FALSE)
  c2c_t_train = rbind(c2c_t_train,c2c_t[trainIndex_t,])
  c2c_t_test = rbind(c2c_t_test,c2c_t[-trainIndex_t,])
  
  c2c_c =c2c%>% filter(tc_ind==0)
  trainIndex_c = createDataPartition((c2c_c$promo_gmv_ind_2d), p = .7, list = FALSE)
  c2c_c_train = rbind(c2c_c_train,c2c_c[trainIndex_c,])
  c2c_c_test = rbind(c2c_c_test,c2c_c[-trainIndex_c,])
}


# table(c2c_t_train$promo_dt)

save(c2c_t_train,file = "Week_End/c2c_t_train.RData")
save(c2c_t_test,file = "Week_End/c2c_t_test.RData")
save(c2c_c_train,file = "Week_End/c2c_c_train.RData")
save(c2c_c_test,file = "Week_End/c2c_c_test.RData")


##################################################################################
#########                     2) Train Test Split     body                #######
##################################################################################

###using end week's promotion data to train model
# Loading C2C dataset
body_week = Megaweekend[Megaweekend$WeekType == 'BODY' & Megaweekend$measureStart >= '2019-02-01' & Megaweekend$measureStart <= '2019-04-30' , 7]


i = 1

measureStart = body_week[i,]

c2c = read.csv(file = paste("AU_C2C_ENGAGED_",as.Date(measureStart$measureStart),".csv",sep=""), header = T)
colnames(c2c) = tolower(colnames(c2c))
# names(c2c)
c2c$promo_gmv_ind_2d = 0
c2c$promo_gmv_ind_2d[c2c$promo_gmv_2d>0] = 1
# table(c2c$promo_lstg_cnt_ind_2d)

c2c_t =c2c%>% filter(tc_ind==1)
trainIndex_t = createDataPartition((c2c_t$promo_gmv_ind_2d), p = .7, list = FALSE)
c2c_t_train = c2c_t[trainIndex_t,]
c2c_t_test = c2c_t[-trainIndex_t,]
c2c_c =c2c%>% filter(tc_ind==0)
trainIndex_c = createDataPartition((c2c_c$promo_gmv_ind_2d), p = .7, list = FALSE)
c2c_c_train = c2c_c[trainIndex_c,]
c2c_c_test = c2c_c[-trainIndex_c,]


for (i in (2: lengths(body_week)))
{
  measureStart = body_week[i,]
  c2c = read.csv(file = paste("AU_C2C_ENGAGED_",as.Date(measureStart$measureStart),".csv",sep=""), header = T)
  colnames(c2c) = tolower(colnames(c2c))
  # names(c2c)
  c2c$promo_gmv_ind_2d = 0
  c2c$promo_gmv_ind_2d[c2c$promo_gmv_2d>0] = 1
  # table(c2c$promo_lstg_cnt_ind_2d)
  
  c2c_t =c2c%>% filter(tc_ind==1)
  trainIndex_t = createDataPartition((c2c_t$promo_gmv_ind_2d), p = .7, list = FALSE)
  c2c_t_train = rbind(c2c_t_train,c2c_t[trainIndex_t,])
  c2c_t_test = rbind(c2c_t_test,c2c_t[-trainIndex_t,])
  
  c2c_c =c2c%>% filter(tc_ind==0)
  trainIndex_c = createDataPartition((c2c_c$promo_gmv_ind_2d), p = .7, list = FALSE)
  c2c_c_train = rbind(c2c_c_train,c2c_c[trainIndex_c,])
  c2c_c_test = rbind(c2c_c_test,c2c_c[-trainIndex_c,])
}


# table(c2c_t_train$promo_dt)

save(c2c_t_train,file = "Week_Body/c2c_t_train.RData")
save(c2c_t_test,file = "Week_Body/c2c_t_test.RData")
save(c2c_c_train,file = "Week_Body/c2c_c_train.RData")
save(c2c_c_test,file = "Week_Body/c2c_c_test.RData")