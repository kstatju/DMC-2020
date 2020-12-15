combin_pred <- function(path){
  library(dplyr)
    knn = read.csv(paste(path, "knn_pred_top_5_final.csv", sep = ""), header = T)
    xgb = read.csv(paste(path, "xgb_best_preds_w13train.csv", sep = ""), header = T)[,-1]
    names(xgb) = c(paste("xgb", 1:10, sep = ""), "week", "itemID")
    nn1 = read.csv(paste(path, "NN_pred_top_3_final.csv", sep = ""), header = T) %>% 
              select(c("itemID", "week", "pred_NN1_w14", "pred_NN2_w14", "pred_NN3_w14")) %>%
              dplyr::rename(pred_NN1_nonlog = pred_NN1_w14, pred_NN2_nonlog = pred_NN2_w14, 
                            pred_NN3_nonlog = pred_NN3_w14)
    # rf1 = knn = read.csv(paste(path, "RF_top5_data _without_std.csv", sep = ""), header = T)
    rf2 = read.csv(paste(path, "rf_pred_top_10_final.csv", sep = ""), header = T) %>% 
              select(c("itemID", "week", "pred_rf1", "pred_rf2", "pred_rf3", "pred_rf4",
                       "pred_rf5", "pred_rf6", "pred_rf7", "pred_rf8", "pred_rf9", "pred_rf10"))
    
    var_name = c("itemID",                 "week",                   "order",
                 "mean_order",
                 "new_price_nonstd",       "new_price",              "tr_ts",                 
                 "brand",                  "manufacturer",           "category1",             
                 "category2",              "category3",              "customerRating",        
                 "recommendedRetailPrice", "pred_knn20",            
                 "pred_knn21",             "pred_knn22",             "pred_knn23",            
                 "pred_knn28",             "xgb1",                   "xgb2",                  
                 "xgb3",                   "xgb4",                   "xgb5",                  
                 "xgb6",                   "xgb7",                   "xgb8",                  
                 "xgb9",                   "xgb10",                  "pred_NN1_nonlog",       
                 "pred_NN2_nonlog",        "pred_NN3_nonlog",        "pred_rf1",              
                 "pred_rf2",               "pred_rf3",               "pred_rf4",              
                 "pred_rf5",               "pred_rf6",               "pred_rf7",              
                 "pred_rf8",               "pred_rf9",               "pred_rf10")
    
    pred_name = c("pred_knn20",            
                 "pred_knn21",             "pred_knn22",             "pred_knn23",            
                 "pred_knn28",             "xgb1",                   "xgb2",                  
                 "xgb3",                   "xgb4",                   "xgb5",                  
                 "xgb6",                   "xgb7",                   "xgb8",                  
                 "xgb9",                   "xgb10",                  "pred_NN1_nonlog",       
                 "pred_NN2_nonlog",        "pred_NN3_nonlog",        "pred_rf1",              
                 "pred_rf2",               "pred_rf3",               "pred_rf4",              
                 "pred_rf5",               "pred_rf6",               "pred_rf7",              
                 "pred_rf8",               "pred_rf9",               "pred_rf10")
    
    dt = left_join(left_join(left_join(knn, 
                    xgb, by = c("itemID", "week")),
                    nn1, by = c("itemID", "week")),
                    rf2, by = c("itemID", "week")) %>% select(var_name) %>% as.data.frame()
    dt$mean_all_pred = round(rowMeans(dt[,pred_name]))
    dt = as.data.frame(dt)
    
    return(dt)
}


path = "D:/ISU/DMC/DMC 2020/Code/all_models/w13/"
dt = combin_pred(path)

write.csv(dt, paste(path, "all_pred_combine_w13.csv", sep = ""), row.names = F)


var_name = c("itemID",                 "week",                   "order",
             "mean_order",
             "new_price_nonstd",       "new_price",              "tr_ts",                 
             "brand",                  "manufacturer",           "category1",             
             "category2",              "category3",              "customerRating",        
             "recommendedRetailPrice", "pred_knn20",            
             "pred_knn21",             "pred_knn22",             "pred_knn23",            
             "pred_knn28",             "xgb1",                   "xgb2",                  
             "xgb3",                   "xgb4",                   "xgb5",                  
             "xgb6",                   "xgb7",                   "xgb8",                  
             "xgb9",                   "xgb10",                  "pred_NN1_nonlog",       
             "pred_NN2_nonlog",        "pred_NN3_nonlog",        "pred_rf1",              
             "pred_rf2",               "pred_rf3",               "pred_rf4",              
             "pred_rf5",               "pred_rf6",               "pred_rf7",              
             "pred_rf8",               "pred_rf9",               "pred_rf10")

pred_var = c("pred_knn20",            
             "pred_knn21",             "pred_knn22",             "pred_knn23",            
             "pred_knn28",             "xgb1",                   "xgb2",                  
             "xgb3",                   "xgb4",                   "xgb5",                  
             "xgb6",                   "xgb7",                   "xgb8",                  
             "xgb9",                   "xgb10",                  "pred_NN1_nonlog",       
             "pred_NN2_nonlog",        "pred_NN3_nonlog",        "pred_rf1",              
             "pred_rf2",               "pred_rf3",               "pred_rf4",              
             "pred_rf5",               "pred_rf6",               "pred_rf7",              
             "pred_rf8",               "pred_rf9",               "pred_rf10")