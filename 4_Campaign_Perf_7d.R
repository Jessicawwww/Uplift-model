## Script Parameters 
suppressMessages(library(RJDBC))
suppressMessages(library(plyr))
options(warn=-1)

setwd('/home/Uplift Model/AU/gmv_model/data')
validate_camp = c('2019-05-04', '2019-05-11', '2019-05-18', '2019-05-25')

miniWriteTable = function(dbCon, table, data, append=TRUE) {
  if (append == FALSE) 
    try(invisible(dbRemoveTable(dbCon,table)), silent=TRUE)
  tmp = !grepl(".",table,fixed=TRUE)
  if (tmp) {
    tmpTable = table
    table = "P_YUE_T.AU_12M_CHURN_Q1"
  }
  try(dbWriteTable(dbCon,table,data), silent=TRUE)
  query = rep(sprintf("INSERT INTO %s VALUES (",table), nrow(data))
  for (i in 1:ncol(data)) {
    value = data[[i]]
    if (!is.numeric(data[[i]])) {
      value = gsub("'", "''", value, fixed=TRUE)
      value = paste0("'", value, "'")
    }
    value = ifelse(is.na(data[[i]]), "NULL", value)
    query = paste0(query, value, ", ")
  }
  query = paste0(substr(query,1,nchar(query)-2), ");")
  for (i in 1:nrow(data))
    dbSendUpdate(dbCon,query[i])
  if (tmp) {
    query = sprintf("CREATE MULTISET VOLATILE TABLE %s AS (SELECT * FROM %s) WITH DATA ON COMMIT PRESERVE ROWS", tmpTable, table)
    result = try(dbSendUpdate(dbCon,query), silent=TRUE)
    if (class(result) == "try-error") 
      dbSendUpdate(dbCon,sprintf("INSERT INTO %s SELECT * FROM %s", tmpTable, table))
    invisible(dbRemoveTable(dbCon, table))
  }
}


# 1. Connection and  function  ----

for( j in  1: length(validate_camp))
{
  
  dw <- config::get(value = "datawarehouse", file = "~/credentials/config.yml")
  dbCon <- dbConnect(
    drv = JDBC(
      driverClass = dw$driverClass,
      classPath = dw$classPath
    ),
    url = dw$Mozart, user = dw$user, password = dw$password
  )
  rm(dw)
  
  # _______________________________ ----
  # 2. Parameter  ----
  
  # validate_c2c = read.csv(file = paste("AU_C2C_ENGAGED_",as.Date(validate_camp[j]),".csv",sep=""), header = T)
  # dbGetQuery(dbCon, "sel count(*) from P_AUC2C_T.AUC2C_UPLIFT_VLDT")
  
  args = commandArgs(trailingOnly=TRUE)
  args = vector(length=8)
  args[1] = "42054"
  args[2] = "MegaWeekend"
  args[3] = "RecurAdhoc"
  args[4] = validate_camp[j]
  
  print(paste("################### round",j ,validate_camp[j] , "start #####################"))
  
  # dbSendUpdate(dbCon, "del from P_AUC2C_T.AUC2C_UPLIFT_VLDT")
  
  # invisible(miniWriteTable(dbCon, "P_AUC2C_T.AUC2C_UPLIFT_VLDT", validate_c2c[,c(1,2,3)]))
  
  parRep = function(query) {
    query = gsub("@campID", args[1], query, fixed=TRUE)
    query = gsub("@campName", gsub("'","''",args[2],fixed=TRUE), query, fixed=TRUE)
    query = gsub("@campType", args[3], query, fixed=TRUE)
    query = gsub("@measureStart", args[4], query, fixed=TRUE)
    return(query)
  }
  
  
  # _______________________________ ----
  # 3. Prepare data  ----
  
  # 3.1 User part  ----
  
  query = "
  CREATE MULTISET VOLATILE TABLE AU_C2C_USR AS (
  SELECT
  USER_ID
  , CASE WHEN TRIM(CELLNAME) LIKE '%@_T' ESCAPE '@' THEN 'T' ELSE 'C' END AS TC_SEGMENT
  , CAST(PROCESS_DT AS DATE FORMAT'MM/DD/YYYY') AS PROMO_DT
  FROM P_WEIWEIWU_T.MEGAWEEK_BASE
  WHERE
  1 = 1
  AND PROMO_DT BETWEEN  CAST('@measureStart' AS DATE)-5 AND CAST('@measureStart' AS DATE) +4
  AND USER_ID < 1E15 
  AND CELLNAME NOT LIKE '%SEED%' 
  GROUP BY 1,2,3
  )
  WITH DATA
  PRIMARY INDEX (USER_ID, TC_SEGMENT, PROMO_DT)
  ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  # dbGetQuery(dbCon, "sel tc_segment, PROMO_DT, count(distinct user_id) from AU_C2C_USR group by 1,2")
  
  
  query = "
  CREATE MULTISET VOLATILE TABLE OPTIN90D_C2C AS (
  SELECT 
  U.*
  FROM AU_C2C_USR U 
  JOIN PRS_RESTRICTED_V.SLM_OPTIN_SEGM F12 ON U.USER_ID = F12.USER_ID 
  AND CAST('@measureStart' AS DATE)-1 BETWEEN START_DT AND END_DT
  )
  WITH DATA
  UNIQUE PRIMARY INDEX (USER_ID, TC_SEGMENT, PROMO_DT)
  ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  # dbGetQuery(dbCon, "sel tc_segment, count(distinct user_id) from OPTIN90D_C2C group by 1")
  # dbGetQuery(dbCon, "sel top 10 * from OPTIN90D_C2C ")
  
  query = "
  CREATE MULTISET VOLATILE TABLE AU_LISTINGS AS (
  SELECT
  USR.USER_ID
  ,USR.PROMO_DT
  ,SUM(CASE WHEN SD.CAL_DT >= USR.PROMO_DT - INTERVAL '365' DAY THEN NEW_LSTG_CNT ELSE 0 END) AS T365_LSTG_CNT
  FROM
  OPTIN90D_C2C USR
  LEFT JOIN
  PRS_RESTRICTED_V.SLR_INVNTRY_SD SD
  ON USR.USER_ID = SD.SLR_ID
  -- LISTINGS INSERTED BEFORE THE PROMO WINDOW, GOING BACK AT MOST TWO YEARS
  AND SD.CAL_DT BETWEEN USR.PROMO_DT - INTERVAL '730' DAY AND  USR.PROMO_DT - INTERVAL '1' DAY
  -- EARLIEST PROMO IS IN JAN 2019, SO TWO YEARS BEFORE THAT
  AND SD.CAL_DT >= DATE '2017-01-01'
  -- RESTRICT TO DE LISTINGS FOR EASIER JOINING TO THE CATEGORY TABLE
  AND SD.LSTG_SITE_ID = 15
  GROUP BY 1,2
  )
  WITH DATA
  UNIQUE PRIMARY INDEX (USER_ID, PROMO_DT)
  ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  # dbGetQuery(dbCon, "sel count(distinct user_id) from AU_LISTINGS")
  
  query = "
  CREATE MULTISET VOLATILE TABLE AU_LSTG_MNTHS AS (
  SELECT
  USR.USER_ID
  ,USR.PROMO_DT
  ,COUNT(DISTINCT CASE WHEN SD.CAL_DT >= USR.PROMO_DT - INTERVAL '365' DAY AND NEW_LSTG_CNT>0 THEN EXTRACT(MONTH FROM CAL_DT)  END) AS LSTG_MONTHS
  FROM
  OPTIN90D_C2C USR
  LEFT JOIN
  PRS_RESTRICTED_V.SLR_INVNTRY_SD SD
  ON USR.USER_ID = SD.SLR_ID
  AND SD.CAL_DT BETWEEN USR.PROMO_DT - INTERVAL '730' DAY AND  USR.PROMO_DT - INTERVAL '1' DAY
  AND SD.CAL_DT >= DATE '2017-01-01'
  AND SD.LSTG_SITE_ID = 15	
  GROUP BY 1,2
  )
  WITH DATA
  UNIQUE PRIMARY INDEX (USER_ID, PROMO_DT)
  ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  # dbGetQuery(dbCon, "sel count(distinct user_id) from AU_LSTG_MNTHS")
  
  query = "
  CREATE MULTISET VOLATILE TABLE AU_C2C_ENG AS (
  SELECT 
  USR.*
  ,LS.T365_LSTG_CNT
  ,LM.LSTG_MONTHS  AS lstg_months
  FROM 	OPTIN90D_C2C USR
  LEFT JOIN AU_LISTINGS LS ON USR.USER_ID = LS.USER_ID AND USR.PROMO_DT = LS.PROMO_DT
  LEFT JOIN	AU_LSTG_MNTHS LM	ON USR.USER_ID = LM.USER_ID	AND USR.PROMO_DT = LM.PROMO_DT
  WHERE 
  1 = 1
  AND T365_LSTG_CNT>5 
  AND LSTG_MONTHS>=4
  )
  WITH DATA
  UNIQUE PRIMARY INDEX (USER_ID, PROMO_DT)
  ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  # dbGetQuery(dbCon, "sel count(distinct user_id) from AU_C2C_ENG")
  # dbGetQuery(dbCon, "sel TOP 10 * from AU_C2C_ENG")
  
  # dbSendUpdate(dbCon,"Drop table USER_BASE_TMP")
  query = "
  CREATE MULTISET VOLATILE TABLE USER_BASE_TMP AS 
  (
  SEL A.USER_ID
  ,  'OVERALL' AS SEGM_NAME
  ,  'OVERALL' AS SEGM
  , CASE WHEN TC_SEGMENT  = 'T' THEN 'Test' ELSE 'Control' END AS USER_GROUP
  , PROMO_DT  AS CMPGN_SENT_DT
  , PROMO_DT  AS CMPGN_RUN_DT
  FROM AU_C2C_ENG A
  ) 
  WITH DATA PRIMARY INDEX (USER_ID,CMPGN_RUN_DT,CMPGN_SENT_DT) 
  ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  # dbGetQuery(dbCon, "sel top 10 * from USER_BASE_TMP")
  
  #UK_SEG_S14A15 <- dbGetQuery(dbCon,query)
  # userBaseT = dbReadTable(dbCon,"USER_BASE_TMP")
  # head(userBaseT)
  
  # Trim, update sent date, label FM, and dedup
  # dbGetQuery(dbCon, "sel USER_GROUP, count(*), count(distinct user_id) from user_base_tmp group by 1") 
  
  
  # dbSendUpdate(dbCon, "drop table USER_BASE")
  query = "
  CREATE MULTISET VOLATILE TABLE USER_BASE AS 
  (
  SELECT 
  A.USER_ID, 
  A.SEGM_NAME, 
  A.SEGM, 
  CASE 
  WHEN FM.SLR_FM_SEG_CD IN (0)  THEN 'B2C&Store'
  WHEN FM.SLR_FM_SEG_CD IN (1,2) THEN 'FTL_3M'
  WHEN FM.SLR_FM_SEG_CD IN (3) THEN '1_HVF'
  WHEN FM.SLR_FM_SEG_CD IN (4) THEN '2_HVI'
  WHEN FM.SLR_FM_SEG_CD IN (5) THEN '3_MVF'
  WHEN FM.SLR_FM_SEG_CD IN (6) THEN '4_MVI'
  WHEN FM.SLR_FM_SEG_CD IN (7) THEN '5_LVF'
  WHEN FM.SLR_FM_SEG_CD IN (8) THEN '6_LVI'
  WHEN FM.SLR_FM_SEG_CD IN (9) THEN '7_Unengage'
  WHEN FM.SLR_FM_SEG_CD IN (10) THEN '8_Lapsing seller'
  WHEN FM.SLR_FM_SEG_CD IN (11) THEN 'Lapsing lister'
  ELSE 'PURE BUYER' END AS SEGM_FM, 
  ----------B2C C2C Flag ------------
  CASE WHEN HIST.CUST_SGMNTN_CD  IN ('1','7','13','19','25','31',
  '2','8','14','20','26','32',
  '3','9','15','21','27','33')
  THEN 'B2C' 
  WHEN Y.PROD_ID IS NOT NULL THEN 'C2C_Store'
  ELSE 'C2C_NS' END AS B2C_C2C,
  -----------------------------------
  A.USER_GROUP, 
  A.CMPGN_SENT_DT 
  FROM 
  USER_BASE_TMP AS A 
  LEFT JOIN
  P_AUC2C_T.AU_C2C_FM_HST_W AS FM
  ON A.USER_ID=FM.USER_ID
  AND
  A.CMPGN_SENT_DT BETWEEN FM.UPDATE_BEG_DT AND FM.UPDATE_END_DT
  -------------------Add B2C C2C Flag------------------------
  LEFT JOIN PRS_RESTRICTED_V.DNA_CUST_SELLER_DSGNTN_SW  HIST                   
  ON A.USER_ID=HIST.SLR_ID AND A.CMPGN_SENT_DT BETWEEN HIST.CUST_SLR_SGMNTN_BEG_DT AND HIST.CUST_SLR_SGMNTN_END_DT
  LEFT JOIN DW_SUBSCRIPTION Y 
  ON PROD_ID IN (3,4,5) AND A.USER_ID = Y.ID 
  AND (CAST(Y.END_DATE AS DATE) >= A.CMPGN_SENT_DT OR Y.ACTIVE IN (0,1,2) OR CAST(END_DATE AS DATE) ='1969-12-31')      
  AND CAST(Y.SUB_DATE AS DATE) <= A.CMPGN_SENT_DT 
  ----------------------------------------------------------
  QUALIFY ROW_NUMBER() OVER (PARTITION BY A.USER_ID ORDER BY A.CMPGN_SENT_DT) = 1 
  ) 
  WITH DATA PRIMARY INDEX (USER_ID) ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  # dbGetQuery(dbCon, "sel top 10 * from USER_BASE")
  # dbGetQuery(dbCon, "sel USER_GROUP, count(*), count(distinct user_id) from USER_BASE group by 1") 
  
  print(paste("################### round",j ,validate_camp[j] , "user part #####################"))
  
  # 3.2 Listing part  ----
  
  query = "
  CREATE MULTISET VOLATILE TABLE LIST_DETAIL_POST AS
  (
  SELECT 
  ITEM.SLR_ID,
  ITEM.ITEM_ID,
  ITEM.AUCT_START_DT AS LSTG_DATE
  FROM   (
  SEL ITEM.SLR_ID, ITEM.ITEM_ID, ITEM.AUCT_START_DT, ITEM.ITEM_SITE_ID, ITEM.LEAF_CATEG_ID  
  
  FROM DW_LSTG_ITEM ITEM
  JOIN DW_LSTG_ITEM_COLD COLD ON ITEM.ITEM_ID = COLD.ITEM_ID AND ITEM.SLR_ID = COLD.SLR_ID AND ITEM.AUCT_END_DT = COLD.AUCT_END_DT
  
  WHERE  
  1= 1
  AND  ITEM.AUCT_END_DT>=CAST('@measureStart' AS DATE)
  AND  ITEM.SLR_ID IN (SEL USER_ID FROM USER_BASE) 
  AND CAST(COLD.AUCT_START_DATE+INTERVAL'17'HOUR AS DATE)  BETWEEN CAST('@measureStart' AS DATE) AND CAST('@measureStart' AS DATE)+1+5
  AND AUCT_TYPE_CODE NOT IN (12,15)
  --AND  ITEM.SLR_CNTRY_ID IN (15,149)
  AND APPLICATION_ID <>114100
  ) AS ITEM
  INNER JOIN DW_CATEGORY_GROUPINGS GROU 
  ON ITEM.ITEM_SITE_ID=GROU.SITE_ID  AND ITEM.LEAF_CATEG_ID=GROU.LEAF_CATEG_ID AND  GROU.LEAF_CATEG_ID = GROU.MOVE_TO
  WHERE  
  1 = 1                                  
  
  AND GROU.SAP_CATEGORY_ID NOT IN (5,7,23,41)
  
  GROUP BY 1,2,3
  )WITH DATA PRIMARY INDEX(ITEM_ID) ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  
  
  
  # 3.3 Transaction part  ----
  
  #GMV
  
  query = "
  CREATE MULTISET VOLATILE TABLE SELL_DETAIL_POST_V1 AS 
  (
  SELECT 
  TRANS.ITEM_ID,
  TRANS.TRANSACTION_ID,
  TRANS.SELLER_ID,
  TRANS.CREATED_DT AS TRANS_DT,   -- US TIME
  TRANS.ITEM_PRICE,
  TRANS.QUANTITY,
  TRANS.LSTG_CURNCY_EXCHNG_RATE
  FROM 
  LIST_DETAIL_POST LIST
  INNER JOIN ACCESS_VIEWS.DW_CHECKOUT_TRANS AS TRANS ON LIST.ITEM_ID = TRANS.ITEM_ID AND LIST.SLR_ID = TRANS.SELLER_ID
  WHERE 
  AUCT_END_DT>=DATE'2019-01-01'
  AND 
  CAST(CREATED_TIME+INTERVAL'17'HOUR AS DATE) between CAST('@measureStart' AS DATE) AND CAST('@measureStart' AS DATE) +1 +5 +30 
  AND 
  CK_WACKO_YN='N'
  AND 
  SALE_TYPE NOT IN (12,15)        
  )
  WITH DATA PRIMARY INDEX (TRANS_DT,ITEM_ID,TRANSACTION_ID) ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  
  query = "
  CREATE MULTISET VOLATILE TABLE SELL_DETAIL_POST_TXN AS 
  (
  SELECT
  Z.SELLER_ID AS USER_ID,
  SUM(CAST(((QUANTITY*ITEM_PRICE*LSTG_CURNCY_EXCHNG_RATE)/EXCH.EXCHNG_RATE) AS DECIMAL(18,2))) GMV,
  ZEROIFNULL(SUM(QUANTITY)) AS SI
  FROM     
  SELL_DETAIL_POST_V1 Z 
  left JOIN 
  DW_DAILY_EXCHANGE_RATES EXCH
  ON EXCH.DAY_OF_RATE_DT = Z.TRANS_DT
  and  EXCH.CURNCY_ID = 5
  GROUP BY 1
  )
  WITH DATA PRIMARY INDEX (USER_ID) ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  
  query = "
  CREATE MULTISET VOLATILE TABLE RVN_DETAIL_POST AS 
  (
  SELECT  
  ACCT_TRANS_DT
  ,LSTG_ID
  ,SLR_ID
  ,LKP.REV_BKT_ID
  ,SUM( CAST( -1 * AMT_USD AS FLOAT ) ) AMT_USD
  
  FROM     DW_GEM2_CMN_RVNU_I R
  JOIN DW_ACCT_ACTN_CODE_LKP LKP ON R.ACTN_CODE = LKP.ACTN_CODE    
  WHERE    ACCT_TRANS_DT  between CAST('@measureStart' AS DATE)-1 AND CAST('@measureStart' AS DATE) + 1 +5 +30-1
  AND      LSTG_TYPE_CODE NOT IN( 10,15 )
  AND      ADJ_TYPE_ID NOT    IN( -1,-7,5 )
  AND      LSTG_SITE_ID <> 223
  AND SLR_CNTRY_ID IN (15,149)
  AND LSTG_ID IN (SEL DISTINCT ITEM_ID FROM LIST_DETAIL_POST)
  GROUP BY 1,2,3,4
  
  UNION ALL
  
  /* Wash data of bad transactions */
  SELECT   ACCT_TRANS_DT
  ,LSTG_ID
  ,SLR_ID
  ,LKP.REV_BKT_ID
  ,SUM( CAST( AMT_USD AS FLOAT ) ) AMT_USD
  
  FROM     DW_GEM2_CMN_ADJ_RVNU_I R
  JOIN DW_ACCT_ACTN_CODE_LKP LKP ON R.ACTN_CODE = LKP.ACTN_CODE    
  WHERE    ACCT_TRANS_DT  between CAST('@measureStart' AS DATE)-1 AND CAST('@measureStart' AS DATE) + 1 +5 +30-1
  AND      LSTG_TYPE_CODE NOT IN( 10,15 )
  AND      LSTG_SITE_ID <> 223
  AND      ISWACKO_YN_ID = 1
  AND SLR_CNTRY_ID IN (15,149)
  AND LSTG_ID IN (SEL DISTINCT ITEM_ID FROM LIST_DETAIL_POST)
  GROUP BY 1,2,3,4
  
  )
  WITH DATA PRIMARY INDEX (ACCT_TRANS_DT,LSTG_ID, REV_BKT_ID) ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  
  query = "
  CREATE MULTISET VOLATILE TABLE SELL_DETAIL_POST_REV AS 
  (
  SELECT
  Z.SLR_ID AS USER_ID,
  SUM(CASE WHEN REV_BKT_ID IN (29,30,31,32,33,34,35,36) THEN AMT_USD/EXCH.EXCHNG_RATE ELSE 0 END) AS TXN_REV
  
  FROM     
  RVN_DETAIL_POST Z 
  left JOIN 
  DW_DAILY_EXCHANGE_RATES EXCH
  ON EXCH.DAY_OF_RATE_DT = Z.ACCT_TRANS_DT
  and  EXCH.CURNCY_ID = 5
  GROUP BY 1
  
  )
  WITH DATA PRIMARY INDEX (USER_ID) ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  
  print(paste("################### round",j ,validate_camp[j] , "listing and txn part #####################"))
  
  # Summarize 
  query = "
  CREATE MULTISET VOLATILE TABLE USER_SUMMARY_TMP AS 
  ( 
  SELECT 
  A.USER_ID, 
  A.SEGM, 
  A.SEGM_FM, 
  ------ Add B2C C2C ---
  A.B2C_C2C,
  ----------------------
  A.USER_GROUP, 
  COALESCE(D.LISTING,0) AS LISTING, 
  COALESCE(G.SI,0) AS SI, 
  COALESCE(G.GMV,0) AS GMV, 
  COALESCE(H.REV,0) AS REV
  FROM 
  USER_BASE AS A 
  LEFT OUTER JOIN 
  (
  SELECT SLR_ID, COUNT(*) AS LISTING
  FROM LIST_DETAIL_POST 
  GROUP BY 1 
  ) AS D ON A.USER_ID = D.SLR_ID 
  LEFT OUTER JOIN 
  (
  SELECT USER_ID, SI, GMV 
  FROM SELL_DETAIL_POST_TXN 
  ) AS G ON A.USER_ID = G.USER_ID 
  LEFT OUTER JOIN 
  (
  SELECT USER_ID, TXN_REV AS REV 
  FROM SELL_DETAIL_POST_REV 
  ) AS H ON A.USER_ID = H.USER_ID 
  ) 
  WITH DATA PRIMARY INDEX (USER_ID) ON COMMIT PRESERVE ROWS
  ;
  "
  dbSendUpdate(dbCon,parRep(query))
  
  # _______________________________ ----
  # 4. Save summary data  ----
  
  user_summary = dbGetQuery(dbCon, "SEL * FROM USER_SUMMARY_TMP;")
  # head(user_summary)
  
  write.csv(user_summary,paste("VLDT_C2C_PERF/AU_C2C_ENGAGED_User_Summary_7d_", validate_camp[j],".csv",sep=""),row.names = FALSE)
  
  invisible(dbDisconnect(dbCon))  
  
  print(paste("################### round",j ,validate_camp[j] , "End #####################"))
  
}
