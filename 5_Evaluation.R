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

# _______________________________ ----
############## 1. End Week ##############
validate_camp_End = c('2019-05-04', '2019-05-25')

###### Loading data ###########
for(j in 1:2){
  choose_date = validate_camp_End[j]
  
  c2c = read.csv(file = paste("AU_C2C_ENGAGED_",choose_date,".csv",sep=""), header = T)
  colnames(c2c) = tolower(colnames(c2c))
  
  test = c2c%>%filter(tc_ind == 1)
  control = c2c%>%filter(tc_ind == 0)
  
  # head(c2c)
  user_summary = read.csv(file = paste("VLDT_C2C_PERF/AU_C2C_ENGAGED_User_Summary_",choose_date,".csv",sep=""), header = T)
  colnames(user_summary) = tolower(colnames(user_summary))
  # head(user_summary)
  
  
  # Prepare X
  exc_col_S = c('promo_dt','user_id','segment','max_t365_shop_level',
                'promo_lstg_cnt_2d','promo_lstg_cnt_7d',
                'promo_gmv_2d','promo_gmv_ind_2d', 'promo_gmv_7d', 'promo_gmv_ind_7d', 'promo_si_2d','promo_si_7d',
                'promo_optin_ind')
  
  exc_col_T = c('promo_dt','user_id','segment','max_t365_shop_level',
                'promo_lstg_cnt_2d','promo_lstg_cnt_7d',
                'promo_gmv_2d','promo_gmv_ind_2d', 'promo_gmv_7d', 'promo_gmv_ind_7d', 'promo_si_2d','promo_si_7d',
                'promo_optin_ind','tc_ind')
  
  # S leaner
  test_s = test[, !colnames(test) %in% exc_col_S]
  # test_s = as.matrix(test_s)
  # colnames(test_s)
  # bst_gmv2d_Slearn$feature_names
  
  control_s = control[, !colnames(control) %in% exc_col_S]
  # control_s = as.matrix(control_s)
  
  # T & X leaner
  test_t = test[, !colnames(test) %in% exc_col_T]
  test_t = as.matrix(test_t)
  
  control_t = control[, !colnames(control) %in% exc_col_T]
  control_t = as.matrix(control_t)
  
  ###### Loading model ###########
  
  ##bst_gmv2d_Slearn
  load(file = "Week_End/xgb_bst_gmv2d_Slearn.RData")
  
  ##bst_gmv2d_Tlearn_t
  load(file = "Week_End/xgb_bst_gmv2d_Tlearn_t.RData")
  ##bst_gmv2d_Tlearn_c
  load(file = "Week_End/xgb_bst_gmv2d_Tlearn_c.RData")
  
  ##bst_xgb_gmv2d_xlearner_t
  load(file = "Week_End/xgb_bst_gmv2d_Xlearner_t.RData")
  ##bst_xgb_gmv2d_xlearner_c
  load(file = "Week_End/xgb_bst_gmv2d_Xlearner_c.RData")
  
  
  
  ###### predict score ###########
  
  # S leaner
  prediction_list_S<-function(model,data)
  {
    #model t(model with tc_ind=1) c(model with tc_ind=0)
    #data t c data.frame
    pred_lst = list()
    for(tc in c(1,0))
    {
      for(d in data)
      {
        data0 = d
        data0$tc_ind = tc
        data0 = as.matrix(data0)
        
        pred_lst = append(list(predict(model, newdata = data0)),pred_lst)
      }
    }
    
    names(pred_lst) = c("model_c_data_c","model_c_data_t","model_t_data_c","model_t_data_t")
    return(pred_lst)
  }
  
  Slearner_ind = prediction_list_S(model = bst_gmv2d_Slearn,
                                   data = list(data_t = test_s,
                                               data_c = control_s))
  
  # T & X leaner
  prediction_list_T<-function(model,data)
  {
    #model t c
    #data t c 
    pred_lst = list()
    for(m in model)
    {
      for(d in data)
      {
        pred_lst = append(list(predict(m, newdata = d)),pred_lst)
      }
    }
    names(pred_lst) = c("model_c_data_c","model_c_data_t","model_t_data_c","model_t_data_t")
    return(pred_lst)
  }
  
  Xlearner_ind = prediction_list_T(model = list(model_t = bst_xgb_gmv2d_xlearner_t,
                                              model_c = bst_xgb_gmv2d_xlearner_c),
                                 data = list(data_t = test_t,
                                             data_c = control_t))
  
  Tlearner_ind = prediction_list_T(model = list(model_t = bst_gmv2d_Tlearn_t,
                                              model_c = bst_gmv2d_Tlearn_c),
                                 data = list(data_t = test_t,
                                             data_c = control_t))
  
  ###### combine score and user info ###########
  
  # S Leaner
  treatment_effect_ind_Slearner_t = Slearner_ind$model_t_data_t-
    Slearner_ind$model_c_data_t
  treatment_effect_ind_Slearner_c =  Slearner_ind$model_t_data_c-
    Slearner_ind$model_c_data_c
  
  test$gmv2d_ind_SL = treatment_effect_ind_Slearner_t
  control$gmv2d_ind_SL = treatment_effect_ind_Slearner_c
  
  
  # T Leaner
  treatment_effect_ind_Tlearner_t =  Tlearner_ind$model_t_data_t-
    Tlearner_ind$model_c_data_t
  treatment_effect_ind_Tlearner_c = Tlearner_ind$model_t_data_c-
    Tlearner_ind$model_c_data_c
  
  test$gmv2d_ind_TL = treatment_effect_ind_Tlearner_t
  control$gmv2d_ind_TL = treatment_effect_ind_Tlearner_c
  
  
  # X Leaner
  treatment_effect_ind_Xlearner_t =
    0.5*Xlearner_ind$model_t_data_t+
    0.5*Xlearner_ind$model_c_data_t
  treatment_effect_ind_Xlearner_c = 
    0.5*Xlearner_ind$model_t_data_c+
    0.5*Xlearner_ind$model_c_data_c
  
  test$gmv2d_ind_XL = treatment_effect_ind_Xlearner_t
  control$gmv2d_ind_XL = treatment_effect_ind_Xlearner_c
  
  ###### save data ###########
  eng_score = rbind(test[, c(1,141,142,143)], control[, c(1,141,142,143)])
  
  final_data = merge(user_summary, eng_score, by.x = "user_id", by.y="user_id")

  write.csv(final_data, paste("VLDT_C2C_SCORE/AU_C2C_ENGAGED_gmv2d_Score_", choose_date,".csv",sep=""),row.names = FALSE)
}

# _______________________________ ----
############## 2. Body Week ##############
validate_camp_Body = c('2019-05-11', '2019-05-18')

###### Loading data ###########
for(j in 1:2){
  choose_date = validate_camp_Body[j]
  
  c2c = read.csv(file = paste("AU_C2C_ENGAGED_",choose_date,".csv",sep=""), header = T)
  colnames(c2c) = tolower(colnames(c2c))
  
  test = c2c%>%filter(tc_ind == 1)
  control = c2c%>%filter(tc_ind == 0)
  
  # head(c2c)
  user_summary = read.csv(file = paste("VLDT_C2C_PERF/AU_C2C_ENGAGED_User_Summary_",choose_date,".csv",sep=""), header = T)
  # head(user_summary)
  
  
  # Prepare X
  exc_col_S = c('promo_dt','user_id','segment','max_t365_shop_level',
                'promo_lstg_cnt_2d','promo_lstg_cnt_7d',
                'promo_gmv_2d','promo_gmv_ind_2d', 'promo_gmv_7d', 'promo_gmv_ind_7d', 'promo_si_2d','promo_si_7d',
                'promo_optin_ind')
  
  exc_col_T = c('promo_dt','user_id','segment','max_t365_shop_level',
                'promo_lstg_cnt_2d','promo_lstg_cnt_7d',
                'promo_gmv_2d','promo_gmv_ind_2d', 'promo_gmv_7d', 'promo_gmv_ind_7d', 'promo_si_2d','promo_si_7d',
                'promo_optin_ind','tc_ind')
  
  # S leaner
  test_s = test[, !colnames(test) %in% exc_col_S]
  # test_s = as.matrix(test_s)
  # colnames(test_s)
  # bst_gmv2d_Slearn$feature_names
  
  control_s = control[, !colnames(control) %in% exc_col_S]
  # control_s = as.matrix(control_s)
  
  # T & X leaner
  test_t = test[, !colnames(test) %in% exc_col_T]
  test_t = as.matrix(test_t)
  
  control_t = control[, !colnames(control) %in% exc_col_T]
  control_t = as.matrix(control_t)
  
  ###### Loading model ###########
  
  ##bst_gmv2d_Slearn
  load(file = "Week_Body/xgb_bst_gmv2d_Slearn.RData")
  
  ##bst_gmv2d_Tlearn_t
  load(file = "Week_Body/xgb_bst_gmv2d_Tlearn_t.RData")
  ##bst_gmv2d_Tlearn_c
  load(file = "Week_Body/xgb_bst_gmv2d_Tlearn_c.RData")
  
  ##bst_xgb_gmv2d_xlearner_t
  load(file = "Week_Body/xgb_bst_gmv2d_Xlearner_t.RData")
  ##bst_xgb_gmv2d_xlearner_c
  load(file = "Week_Body/xgb_bst_gmv2d_Xlearner_c.RData")
  
  
  
  ###### predict score ###########
  
  # S leaner
  prediction_list_S<-function(model,data)
  {
    #model t(model with tc_ind=1) c(model with tc_ind=0)
    #data t c data.frame
    pred_lst = list()
    for(tc in c(1,0))
    {
      for(d in data)
      {
        data0 = d
        data0$tc_ind = tc
        data0 = as.matrix(data0)
        
        pred_lst = append(list(predict(model, newdata = data0)),pred_lst)
      }
    }
    
    names(pred_lst) = c("model_c_data_c","model_c_data_t","model_t_data_c","model_t_data_t")
    return(pred_lst)
  }
  
  Slearner_ind = prediction_list_S(model = bst_gmv2d_Slearn,
                                   data = list(data_t = test_s,
                                               data_c = control_s))
  
  # T & X leaner
  prediction_list_T<-function(model,data)
  {
    #model t c
    #data t c 
    pred_lst = list()
    for(m in model)
    {
      for(d in data)
      {
        pred_lst = append(list(predict(m, newdata = d)),pred_lst)
      }
    }
    names(pred_lst) = c("model_c_data_c","model_c_data_t","model_t_data_c","model_t_data_t")
    return(pred_lst)
  }
  
  Xlearner_ind = prediction_list_T(model = list(model_t = bst_xgb_gmv2d_xlearner_t,
                                              model_c = bst_xgb_gmv2d_xlearner_c),
                                 data = list(data_t = test_t,
                                             data_c = control_t))
  
  Tlearner_ind = prediction_list_T(model = list(model_t = bst_gmv2d_Tlearn_t,
                                              model_c = bst_gmv2d_Tlearn_c),
                                 data = list(data_t = test_t,
                                             data_c = control_t))
  
  ###### combine score and user info ###########
  
  # S Leaner
  treatment_effect_ind_Slearner_t = Slearner_ind$model_t_data_t-
    Slearner_ind$model_c_data_t
  treatment_effect_ind_Slearner_c =  Slearner_ind$model_t_data_c-
    Slearner_ind$model_c_data_c
  
  test$gmv2d_ind_SL = treatment_effect_ind_Slearner_t
  control$gmv2d_ind_SL = treatment_effect_ind_Slearner_c
  
  
  # T Leaner
  treatment_effect_ind_Tlearner_t =  Tlearner_ind$model_t_data_t-
    Tlearner_ind$model_c_data_t
  treatment_effect_ind_Tlearner_c = Tlearner_ind$model_t_data_c-
    Tlearner_ind$model_c_data_c
  
  test$gmv2d_ind_TL = treatment_effect_ind_Tlearner_t
  control$gmv2d_ind_TL = treatment_effect_ind_Tlearner_c
  
  
  # X Leaner
  treatment_effect_ind_Xlearner_t =
    0.5*Xlearner_ind$model_t_data_t+
    0.5*Xlearner_ind$model_c_data_t
  treatment_effect_ind_Xlearner_c = 
    0.5*Xlearner_ind$model_t_data_c+
    0.5*Xlearner_ind$model_c_data_c
  
  test$gmv2d_ind_XL = treatment_effect_ind_Xlearner_t
  control$gmv2d_ind_XL = treatment_effect_ind_Xlearner_c
  
  ###### save data ###########
  eng_score = rbind(test[, c(1,141,142,143)], control[, c(1,141,142,143)])
  
  final_data = merge(c2c, eng_score, by.x = "user_id", by.y="user_id")
  
  write.csv(final_data, paste("VLDT_C2C_SCORE/AU_C2C_ENGAGED_gmv2d_Score_", choose_date,".csv",sep=""),row.names = FALSE)
}



