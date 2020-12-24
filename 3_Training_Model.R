library(data.table)
library(fasttime)
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

# _______________________________ ----
# 1.  End Week Model building  -------

load(file = "Week_End/c2c_t_train.RData")#c2c_t_train 203k
load(file = "Week_End/c2c_t_test.RData")#c2c_t_test 87k
load(file = "Week_End/c2c_c_train.RData")#c2c_t_train  17k
load(file = "Week_End/c2c_c_test.RData")#c2c_t_test 7257

##################################################################################
#                                                                                #
#                               For gmv ind                                  #
#               **********************************************                   #
#                       whether the seller sell or not                           #        
##################################################################################


# _______________________________ ----
######### 1.1 T_learner#######

select_data_col_Tlearner<-function(c2c, response)
{
  exc_col = c('promo_dt','user_id','segment','max_t365_shop_level',
              'promo_lstg_cnt_2d','promo_lstg_cnt_7d',
              'promo_gmv_2d','promo_gmv_ind_2d', 'promo_gmv_7d', 'promo_gmv_ind_7d', 'promo_si_2d','promo_si_7d',
              'promo_optin_ind','tc_ind')
  selected = c2c[,-which(colnames(c2c) %in% exc_col)]
  return(list(X = as.matrix(selected),Y = c2c[,which(colnames(c2c) %in% response)]))#return train data
}

# names(c2c_t_train)
# head(select_data_col_Tlearner(c2c_t_train, 'promo_gmv_ind_2d'))

xgbtrain<-function(train,test,response,selection_method,max_depth = 3, eta = 0.2, nrounds = 50)
{
  Trainlist_t = selection_method(train,response)
  Testlist_t = selection_method(test,response)
  
  dtrain_class <- xgb.DMatrix(Trainlist_t$X, label =Trainlist_t$Y)
  dtest_class <-xgb.DMatrix(Testlist_t$X, label = Testlist_t$Y)
  watchlist_class <- list(train = dtrain_class, eval = dtest_class)
  ## A simple xgb.train example:
  param_class <- list(max_depth = max_depth, eta = eta, nthread = 2,
                      objective = "binary:logistic", eval_metric = "auc")
  bst <- xgb.train(param_class, dtrain_class, nrounds =nrounds, watchlist_class)
  return(bst)
}

## 1.11 2d ##
bst_gmv2d_Tlearn_t = xgbtrain(c2c_t_train,c2c_t_test,'promo_gmv_ind_2d',select_data_col_Tlearner)
#auc: train-auc:0.970808	eval-auc:0.969249 
save(bst_gmv2d_Tlearn_t,file="Week_End/xgb_bst_gmv2d_Tlearn_t.RData")

bst_gmv2d_Tlearn_c = xgbtrain(c2c_c_train,c2c_c_test,'promo_gmv_ind_2d',select_data_col_Tlearner)
#auc: [50]	train-auc:0.968893	eval-auc:0.946993 
save(bst_gmv2d_Tlearn_c,file="Week_End/xgb_bst_gmv2d_Tlearn_c.RData")

## 1.12 7d ##
bst_gmv7d_Tlearn_t = xgbtrain(c2c_t_train,c2c_t_test,'promo_gmv_ind_7d',select_data_col_Tlearner)
#auc: train-auc:0.970808	eval-auc:0.969249 
save(bst_gmv7d_Tlearn_t,file="Week_End/xgb_bst_gmv7d_Tlearn_t.RData")

bst_gmv7d_Tlearn_c = xgbtrain(c2c_c_train,c2c_c_test,'promo_gmv_ind_7d',select_data_col_Tlearner)
#auc: [50]	train-auc:0.968893	eval-auc:0.946993 
save(bst_gmv7d_Tlearn_c,file="Week_End/xgb_bst_gmv7d_Tlearn_c.RData")

# _______________________________ ----
######### 1.2 S_learner#######

select_data_col_Slearner<-function(c2c, response)
{
  exc_col = c('promo_dt','user_id','segment','max_t365_shop_level',
              'promo_lstg_cnt_2d','promo_lstg_cnt_7d',
              'promo_gmv_2d','promo_gmv_ind_2d', 'promo_gmv_7d', 'promo_gmv_ind_7d', 'promo_si_2d','promo_si_7d',
              'promo_optin_ind')
  selected = c2c[,-which(colnames(c2c) %in% exc_col)]
  return(list(X = as.matrix(selected),Y = c2c[,which(colnames(c2c) %in% response)]))#return train data
}

## 1.21 2d ##
bst_gmv2d_Slearn = xgbtrain(rbind(c2c_t_train,c2c_c_train),
                      rbind(c2c_t_test,c2c_c_test),
                      'promo_gmv_ind_2d',
                      select_data_col_Slearner)
#auc: train-auc:0.970005	eval-auc:0.968325 

save(bst_gmv2d_Slearn,file="Week_End/xgb_bst_gmv2d_Slearn.RData")

## 1.22 7d ##
bst_gmv7d_Slearn = xgbtrain(rbind(c2c_t_train,c2c_c_train),
                            rbind(c2c_t_test,c2c_c_test),
                            'promo_gmv_ind_7d',
                            select_data_col_Slearner)
#auc: train-auc:0.970005	eval-auc:0.968325 

save(bst_gmv7d_Slearn,file="Week_End/xgb_bst_gmv7d_Slearn.RData")


# _______________________________ ----
######### 1.3 X_learner#######


load(file="Week_End/xgb_bst_gmv2d_Tlearn_c.RData") #bst_Tlearn_c
load(file="Week_End/xgb_bst_gmv2d_Tlearn_t.RData") #bst_Tlearn_t


select_data_col_Xlearner<-function(c2c,response,model_t,model_c,type = "t")
{
  #Slearner uses tc_ind indicator as a predictor
  exc_col = c('promo_dt','user_id','segment','max_t365_shop_level',
              'promo_lstg_cnt_2d','promo_lstg_cnt_7d',
              'promo_gmv_2d','promo_gmv_ind_2d', 'promo_gmv_7d', 'promo_gmv_ind_7d', 'promo_si_2d','promo_si_7d',
              'promo_optin_ind','tc_ind')
  selected = c2c[,-which(colnames(c2c) %in% exc_col)]
  selected =  as.matrix(selected)
  if(type == "t")
  {
    c2c$treatment = c2c[,which(colnames(c2c) %in% response)]  -  predict(model_c,selected)
  }
  if(type == "c")
  {
    c2c$treatment = predict(model_t,selected) - c2c[,which(colnames(c2c) %in% response)]
  }
  return(list(X = selected,Y = c2c$treatment))#return train data
}


xgb_xlearner<-function(train,test,response,selection_method,
                       model_t,model_c,type = "t",
                       max_depth = 3, eta = 0.2, nrounds = 50)
{
  Trainlist  = selection_method(train,response,model_t,model_c,type= type)
  Testlist  = selection_method(test,response,model_t,model_c,type= type)
  
  dtrain_class <- xgb.DMatrix(Trainlist$X, label =Trainlist$Y)
  dtest_class <-xgb.DMatrix(Testlist$X, label = Testlist$Y)
  watchlist_class <- list(train = dtrain_class, eval = dtest_class)
  ## A simple xgb.train example:
  param_class <- list(max_depth = max_depth, eta = eta, nthread = 2,
                      # objective = "reg:squarederror", 
                      eval_metric = "rmse")
  bst <- xgb.train(param_class, dtrain_class, nrounds =nrounds, watchlist_class)
  return(bst)
}

## 1.31 2d ##
bst_xgb_gmv2d_xlearner_t = xgb_xlearner(c2c_t_train,c2c_t_test,'promo_gmv_ind_2d', select_data_col_Xlearner,
                                        bst_gmv2d_Tlearn_t,bst_gmv2d_Tlearn_c, type="t",
                                        max_depth = 3, eta = 0.2, nrounds = 50)
# rmse train-rmse:0.214630	eval-rmse:0.216911 

bst_xgb_gmv2d_xlearner_c = xgb_xlearner(c2c_c_train,c2c_c_test,'promo_gmv_ind_2d', select_data_col_Xlearner,
                                        bst_gmv2d_Tlearn_t,bst_gmv2d_Tlearn_c, type="c",
                                        max_depth = 3, eta = 0.2, nrounds = 50)
# rmse train-rmse:0.193474	eval-rmse:0.214502 

save(bst_xgb_gmv2d_xlearner_t,file="Week_End/xgb_bst_gmv2d_Xlearner_t.RData")
save(bst_xgb_gmv2d_xlearner_c,file="Week_End/xgb_bst_gmv2d_Xlearner_c.RData")

## 1.32 7d ##
bst_xgb_gmv7d_xlearner_t = xgb_xlearner(c2c_t_train,c2c_t_test,'promo_gmv_ind_7d', select_data_col_Xlearner,
                                        bst_gmv7d_Tlearn_t,bst_gmv7d_Tlearn_c, type="t",
                                        max_depth = 3, eta = 0.2, nrounds = 50)
# rmse train-rmse:0.214630	eval-rmse:0.216911 

bst_xgb_gmv7d_xlearner_c = xgb_xlearner(c2c_c_train,c2c_c_test,'promo_gmv_ind_7d', select_data_col_Xlearner,
                                        bst_gmv7d_Tlearn_t,bst_gmv7d_Tlearn_c, type="c",
                                        max_depth = 3, eta = 0.2, nrounds = 50)
# rmse train-rmse:0.193474	eval-rmse:0.214502 

save(bst_xgb_gmv7d_xlearner_t,file="Week_End/xgb_bst_gmv7d_Xlearner_t.RData")
save(bst_xgb_gmv7d_xlearner_c,file="Week_End/xgb_bst_gmv7d_Xlearner_c.RData")


# _______________________________ ----
# 2.  Body Week Model building  -------

load(file = "Week_Body/c2c_t_train.RData")#c2c_t_train 216k
load(file = "Week_Body/c2c_t_test.RData")#c2c_t_test 93k
load(file = "Week_Body/c2c_c_train.RData")#c2c_t_train  18k
load(file = "Week_Body/c2c_c_test.RData")#c2c_t_test 7868

##################################################################################
#                                                                                #
#                               For gmv ind                                  #
#               **********************************************                   #
#                       whether the seller sell or not                           #        
##################################################################################


# _______________________________ ----
######### 2.1 T_learner#######

## 2.11 2d ##
bst_gmv2d_Tlearn_t = xgbtrain(c2c_t_train,c2c_t_test,'promo_gmv_ind_2d',select_data_col_Tlearner)
#auc: train-auc:0.974431	eval-auc:0.973892 
save(bst_gmv2d_Tlearn_t,file="Week_Body/xgb_bst_gmv2d_Tlearn_t.RData")

bst_gmv2d_Tlearn_c = xgbtrain(c2c_c_train,c2c_c_test,'promo_gmv_ind_2d',select_data_col_Tlearner)
#auc: [50]	train-auc:0.976087	eval-auc:0.956756  
save(bst_gmv2d_Tlearn_c,file="Week_Body/xgb_bst_gmv2d_Tlearn_c.RData")

## 2.11 7d ##
bst_gmv7d_Tlearn_t = xgbtrain(c2c_t_train,c2c_t_test,'promo_gmv_ind_7d',select_data_col_Tlearner)
#auc: train-auc:0.974431	eval-auc:0.973892 
save(bst_gmv7d_Tlearn_t,file="Week_Body/xgb_bst_gmv7d_Tlearn_t.RData")

bst_gmv7d_Tlearn_c = xgbtrain(c2c_c_train,c2c_c_test,'promo_gmv_ind_7d',select_data_col_Tlearner)
#auc: [50]	train-auc:0.976087	eval-auc:0.956756  
save(bst_gmv7d_Tlearn_c,file="Week_Body/xgb_bst_gmv7d_Tlearn_c.RData")


# _______________________________ ----
######### 2.2 S_learner#######


## 2.21 2d ##
bst_gmv2d_Slearn = xgbtrain(rbind(c2c_t_train,c2c_c_train),
                            rbind(c2c_t_test,c2c_c_test),
                            'promo_gmv_ind_2d',
                            select_data_col_Slearner)
#auc: train-auc:0.973766	eval-auc:0.973306 

save(bst_gmv2d_Slearn,file="Week_Body/xgb_bst_gmv2d_Slearn.RData")

## 2.21 7d ##
bst_gmv7d_Slearn = xgbtrain(rbind(c2c_t_train,c2c_c_train),
                            rbind(c2c_t_test,c2c_c_test),
                            'promo_gmv_ind_7d',
                            select_data_col_Slearner)
#auc: train-auc:0.973766	eval-auc:0.973306 

save(bst_gmv7d_Slearn,file="Week_Body/xgb_bst_gmv7d_Slearn.RData")

# _______________________________ ----
######### 2.3 X_learner#######


load(file="Week_Body/xgb_bst_gmv2d_Tlearn_c.RData") #bst_Tlearn_c
load(file="Week_Body/xgb_bst_gmv2d_Tlearn_t.RData") #bst_Tlearn_t

## 2.31 2d ##
bst_xgb_gmv2d_xlearner_t = xgb_xlearner(c2c_t_train,c2c_t_test,'promo_gmv_ind_2d', select_data_col_Xlearner,
                                        bst_gmv2d_Tlearn_t,bst_gmv2d_Tlearn_c, type="t",
                                        max_depth = 3, eta = 0.2, nrounds = 50)
# train-rmse:0.202878	eval-rmse:0.202632 

bst_xgb_gmv2d_xlearner_c = xgb_xlearner(c2c_c_train,c2c_c_test,'promo_gmv_ind_2d', select_data_col_Xlearner,
                                        bst_gmv2d_Tlearn_t,bst_gmv2d_Tlearn_c, type="c",
                                        max_depth = 3, eta = 0.2, nrounds = 50)
# rmse train-rmse:0.172930	eval-rmse:0.186951

save(bst_xgb_gmv2d_xlearner_t,file="Week_Body/xgb_bst_gmv2d_Xlearner_t.RData")
save(bst_xgb_gmv2d_xlearner_c,file="Week_Body/xgb_bst_gmv2d_Xlearner_c.RData")


## 2.32 7d ##
bst_xgb_gmv7d_xlearner_t = xgb_xlearner(c2c_t_train,c2c_t_test,'promo_gmv_ind_7d', select_data_col_Xlearner,
                                        bst_gmv7d_Tlearn_t,bst_gmv7d_Tlearn_c, type="t",
                                        max_depth = 3, eta = 0.2, nrounds = 50)
# train-rmse:0.202878	eval-rmse:0.202632 

bst_xgb_gmv7d_xlearner_c = xgb_xlearner(c2c_c_train,c2c_c_test,'promo_gmv_ind_7d', select_data_col_Xlearner,
                                        bst_gmv7d_Tlearn_t,bst_gmv7d_Tlearn_c, type="c",
                                        max_depth = 3, eta = 0.2, nrounds = 50)
# rmse train-rmse:0.172930	eval-rmse:0.186951

save(bst_xgb_gmv7d_xlearner_t,file="Week_Body/xgb_bst_gmv7d_Xlearner_t.RData")
save(bst_xgb_gmv7d_xlearner_c,file="Week_Body/xgb_bst_gmv7d_Xlearner_c.RData")
