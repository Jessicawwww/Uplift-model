validate_camp = c('2019-05-04', '2019-05-11', '2019-05-18', '2019-05-25')

j = 1

choose_date = validate_camp[j]

user_score_summary = read.csv(file = paste("VLDT_C2C_SCORE/AU_C2C_ENGAGED_gmv2d_Score_", choose_date,".csv",sep=""), header = T)
user_score_summary$lister = 0
user_score_summary$lister[user_score_summary$listing > 0] = 1

user_score_summary$seller = 0
user_score_summary$seller[user_score_summary$si > 0] = 1

# _______________________________ ----
############## 1. get lift and cost ##############

# df = user_score_summary
# model_score = "gmv2d_ind_SL"
# num_q = 100

compute_impact <- function(df, model_score, num_q) 
{
  dataC = subset(df, user_group=="Control")
  dataT = subset(df, user_group=="Test")
  
  score_t = dataT[,which(colnames(dataT) %in% model_score)]
  score_c = dataC[,which(colnames(dataC) %in% model_score)]
  
  metricName = c("lister", "listing", "seller", "si", "gmv", "rev", "cost", "roi", "pop")
  metricValue = matrix(rep(0, (num_q+1)*length(metricName)) , ncol = length(metricName))
  colnames(metricValue) = metricName
  
  Sum_T = rep(0, length(metricName) - 3)
  Sum_C = rep(0, length(metricName) - 3)
  
  # result = list()
  
  for(q in (0:num_q))
  {
    index1_t = (score_t>= quantile(score_t , q/100))
    index1_c = (score_c>= quantile(score_c , q/100))
    
    popC = sum(index1_c)
    popT = sum(index1_t)
    
    factor_tc = popT / popC
    
    for(m in 1:(length(metricName)-3) )
    {
      Sum_T[m] = sum(dataT[index1_t, metricName[m]])
      Sum_C[m] = sum(dataC[index1_c, metricName[m]])
    }
    
    metricIncre = Sum_T - Sum_C*factor_tc
    TR_T = Sum_T[6] / Sum_T[5]
    TR_C = Sum_C[6] / Sum_C[5]
    cost = (Sum_T[5] - iGMV) * (TR_C - TR_T)
    roi = metricIncre[6] / cost 
    PopSize = popT
    
    metricValue[q+1, ] = c(metricIncre, cost , roi, PopSize)
  }
  
  return(metricValue)
}
# Incre = getIncre(user_score_summary, "gmv2d_ind_SL", 100)
# View(Incre)

Incre_SL = compute_impact(user_score_summary, "gmv2d_ind_SL", 100)
# View(Incre_SL)
Incre_TL = compute_impact(user_score_summary, "gmv2d_ind_TL", 100)
# View(Incre_TL)
Incre_XL = compute_impact(user_score_summary, "gmv2d_ind_XL", 100)
# View(Incre_XL)

# _______________________________ ----
############## 2. plot by different cutoff ##############
# lable_name = "gmv2d_ind_SL"
# df = user_score_summary
# num_q = 100

  
  impact_list = metricValue
  
  drop = c(0:num_q)
  
  iLister = impact_list[,1]
  iListing = impact_list[,2]
  iSeller = impact_list[,3]
  iSi = impact_list[,4]
  iGMV = impact_list[,5]
  iRev = impact_list[,6]
  cost = impact_list[,7]
  roi = impact_list[,8]*100
  Pop = impact_list[,9]
  
  p_list = list()
  
  p_list$p_iLister =  p_list$p_iLister + 
    geom_line(aes(x = drop,
                  y = iLister,col=label_name))+
    labs(ylab="iLister",xlab="drop tail %")
  
  p_list$p_iListing =  p_list$p_iListing + 
    geom_line(aes(x = drop,
                  y = iListing,col=label_name))+
    labs(ylab="iListing",xlab="drop tail %")
  
  p_list$p_iSeller =  p_list$p_iSeller + 
    geom_line(aes(x = drop,
                  y = iSeller,col=label_name))+
    labs(ylab="iSeller",xlab="drop tail %")
  
  p_list$p_iSi =  p_list$p_iSi + 
    geom_line(aes(x = drop,
                  y = iSi,col=label_name))+
    labs(ylab="iSi",xlab="drop tail %")
  
  p_list$p_iGMV =  p_list$p_iGMV + 
    geom_line(aes(x = drop,
                  y = iGMV,col=label_name))+
    labs(ylab="iGMV",xlab="drop tail %")
  
  p_list$p_iRev =  p_list$p_iRev + 
    geom_line(aes(x = drop,
                  y = iRev,col=label_name))+
    labs(ylab="iRev",xlab="drop tail %")
  
  p_list$p_cost =  p_list$p_cost +
    geom_line(aes(x = drop,
                  y = cost,col=label_name))+
    labs(ylab="cost",xlab="drop tail %")
  
  p_list$p_roi =  p_list$p_roi + 
    geom_line(aes(x = drop,
                  y = roi,col=label_name))+
    labs(ylab="roi",xlab="drop tail %")
  
  p_list$p_Pop =  p_list$p_Pop + 
    geom_line(aes(x = drop,
                  y = Pop,col=label_name))+
    labs(ylab="Pop",xlab="drop tail %")
  
  return(p_list)
}

# plot_impact()

save_plot<-function(filename)
{
  # fetch data element in the environment
  # save in the fold of filename
  p1<-ggplot()+theme_classic()+labs(ylab="iLister",xlab="drop tail %")
  p2<-ggplot()+theme_classic()+labs(ylab="iListing",xlab="drop tail %")
  p3<-ggplot()+theme_classic()+labs(ylab="iSeller",xlab="dtrop tail %")
  p4<-ggplot()+theme_classic()+labs(ylab="iSi",xlab="drop tail %")
  p5<-ggplot()+theme_classic()+labs(ylab="iGMV",xlab="drop tail %")
  p6<-ggplot()+theme_classic()+labs(ylab="iRev",xlab="drop tail %")
  p7<-ggplot()+theme_classic()+labs(ylab="cost",xlab="drop tail %")
  p8<-ggplot()+theme_classic()+labs(ylab="roi %",xlab="drop tail %")
  p9<-ggplot()+theme_classic()+labs(ylab="Pop_Size",xlab="drop tail %")
  
  
  pplot = list(
    p_iLister = p1,
    p_iListing = p2,
    p_iSeller = p3,
    p_iSi = p4,
    p_iGMV = p5,
    p_iRev = p6,
    p_cost=p7,
    p_roi=p8,
    p_incre=p9
  )
  
  
  pplot = pplot%>%plot_impact("gmv2d_ind_SL", 60)%>%
    plot_impact("gmv2d_ind_SL")%>%
    plot_impact("gmv2d_ind_SL")#%>%
  # plot_impact("treatment_effect_ind_Slearner")%>%
  # plot_impact("treatment_effect_cnt_Xlearner")%>%
  # plot_impact("treatment_effect_cnt_Tlearner")%>%
  # plot_impact("treatment_effect_cnt_Slearner")%>%
  # plot_impact("optin_treatment_effect_ind_Xlearner")%>%
  # plot_impact("control_treatment_effect_ind_Xlearner")
  
  
  save(pplot, file = paste(filename,".RData",sep=""))
  ###################################
  load( file = paste(filename,".RData",sep=""))
  tiff(paste(filename,"/roi.tiff",sep=""), units="in", width=8, height=4, res=500)
  # insert ggplot code
  pplot$p_roi
  dev.off()
  
  tiff(paste(filename,"/incre.tiff",sep=""), units="in", width=8, height=4, res=500)
  # insert ggplot code
  pplot$p_incre
  dev.off()
  
  tiff(paste(filename,"/cost.tiff",sep=""), units="in", width=8, height=4, res=500)
  # insert ggplot code
  pplot$p_cost
  dev.off()
  
  return(pplot)
}
