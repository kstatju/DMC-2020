

dt_prep_2nd_ensemble <- function(path){
    library(dplyr)
    se_var = c("itemID"   ,              "week"      ,             "order"  ,               
               "mean_order" ,            "new_price_nonstd"  ,     "new_price" ,           
               "tr_ts"     ,             "brand"    ,              "manufacturer"  ,        
               "category1"   ,           "category2" ,             "category3"    ,         
               "customerRating" ,        "recommendedRetailPrice")
    
    dt = read.csv(paste(path, "all_pred_combine_w13.csv", sep = ""), header = T) %>% 
                select(se_var)
    
    rf = read.csv(paste(path, "rf_ensemble_preds.csv", sep = ""), header = T)[,-1]
    xgb = read.csv(paste(path, "xgb_ensemble_pred.csv", sep = ""), header = T) %>% 
                select(c("itemID", "week", "pred_xgb"))
    lm = read.csv(paste(path, "pred_lm_train.csv", sep = ""), header = T)%>% 
      select(c("itemID", "week", "pred_lm_train")) %>% 
      rename(pred_lm = pred_lm_train) %>% 
      bind_rows(lm1 = read.csv(paste(path, "pred_lm_test.csv", sep = ""), header = T)%>% 
      select(c("itemID", "week", "pred_lm_test")) %>% 
      rename(pred_lm = pred_lm_test)) %>% as.data.frame()
    
    df = left_join(left_join(left_join(dt, rf, by = c("itemID", "week")),
                   xgb, by = c("itemID", "week")),
                   lm, by = c("itemID", "week"))
    
    return(df)
}

path = "D:/ISU/DMC/DMC 2020/Code/ensemble/2n_layer_ensemble/"
dt = dt_prep_2nd_ensemble(path)

write.csv(dt, paste(path, "ensemble_data_2nd_layer.csv", sep = ""), row.names = F)


aa = as.data.frame(colSums(is.na(dt)))
library(ggplot2)
library(reshape2)

# df1 = df %>% select(c("order", "tr_ts", "rf1_preds")) %>% melt(id = "tr_ts")
ggplot(data = dt, aes(pred_xgb, group=tr_ts,
                       fill=as.factor(tr_ts))) + geom_density(alpha=0.5, adjust=2) +
          xlim(0,30)

ggplot(data = dt[dt$week>10,], aes(pred_xgb, group=tr_ts,
                                   fill=as.factor(tr_ts))) + geom_density(alpha=0.5, adjust=2) +
  xlim(0,30)


ggplot(data = dt[dt$week<14, ], aes(x = order, y = pred_xgb)) + geom_point()+
  geom_abline(slope=1, intercept=0, color="red")
