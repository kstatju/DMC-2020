knn_prediction_final <- function(train_dt, test_dt, path, pred_name, param, x_feature,
                                 respons = "order"){
  library(caret)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(zoo)
  
  b_feature = c("itemID",               "week",                   "order",                 
                "new_price",             "tr_ts",                  "brand" ,                
                "manufacturer",           "category1",              "category2",
                "category3",              "customerRating",         "recommendedRetailPrice")
  
  
  
  
  
  fl_dt_tr = train_dt  %>% select(b_feature)
  fl_dt_ts = test_dt %>% select(b_feature)
  
  x_train = train_dt %>% 
    select_at(.vars = x_feature) %>% 
    mutate(new_pr_npromoIW = new_price*(npromoIW+1)) %>% as.matrix()
  
  x_test = test_dt %>% select_at(.vars = x_feature) %>% 
    mutate(new_pr_npromoIW = new_price*(npromoIW+1)) %>% as.matrix()
  
  y_train = train_dt %>% 
    select_at(.vars = respons) %>% as.matrix()
  
  y_test = test_dt %>% select_at(.vars = respons) %>% as.matrix()
  
  
  fit <- caret::knnreg(x = x_train, y = y_train, k = param[[1]], use.all = TRUE)
  
  yhat_train = predict(fit, x_train) 
  yhat_train = round(ifelse(yhat_train<0, 0, yhat_train))
  
  yhat_test = predict(fit, x_test)
  yhat_test = round(ifelse(yhat_test<0, 0, yhat_test))
  
  yhat_train = as.data.frame(yhat_train)
  names(yhat_train) = pred_name
  
  yhat_test = as.data.frame(yhat_test)
  names(yhat_test) = pred_name
  
  
  fl_dt_tr = fl_dt_tr %>% bind_cols(yhat_train)
  fl_dt_ts = fl_dt_ts %>% bind_cols(yhat_test)
  
  pred_dt = fl_dt_tr %>% bind_rows(fl_dt_ts)
  
  write.csv(pred_dt, file = paste(path, "knn_", pred_name, "_final.csv", sep = ""), row.names = F)
  return(pred_dt)
}


library(parallel)

path1 = "/work/STAT/kanak/knn_final/"

tr_dt = read.csv(paste(path1, "final_tr_std_promo3_alldt.csv", sep = ""), header = T)
ts_dt = read.csv(paste(path1, "final_ts_std_promo3_alldt.csv", sep = ""), header = T)

respons1   = "order"
param1 = c(21,           22,           20,           23,           28)

x_feature1    = c(   "week",                     "manufacturer_std",      "brand_std",
                     "category1_std",            "category2_std",         "category3_std",
                     "customerRating",           "new_price",             "recommendedRetailPrice",
                     "lag1_order",               "lag2_order",            "lag3_order",
                     "comp1_order",              "comp2_order",           "comp3_order",
                     "comp1_npromoIW",           "comp2_npromoIW",        "comp3_npromoIW",
                     "comp4_npromoIW",           "comp5_npromoIW",        "comp6_npromoIW",
                     "comp7_npromoIW",           "comp8_npromoIW",        "comp9_npromoIW",
                     "comp10_npromoIW",          "supp1_npromoIW",        "supp2_npromoIW",
                     "morderI",                  "mncusI",                "mncusIW",
                     "mtmncusBMC123",            "npromoBMC123W",         "npromoBMW",
                     "npromoI",                  "npromoIW",              "npromoIW_npromoIW")

ncombin = length(param1)

no_cores <- detectCores() - 1
message(paste("***** Using ", no_cores, "Cores *****"))
message("Running models ...", appendLF = FALSE)
cl <- makeCluster(no_cores)

clexp = list("knn_prediction_final", "tr_dt", "ts_dt", "respons1", "x_feature1", 
             "param1", "path1")
clusterExport(cl, clexp)

estimates <-  parLapplyLB(cl = cl, X = 1:ncombin, 
                          fun = function(i) {
                            knn_prediction_final(train_dt = tr_dt, test_dt = ts_dt, 
                                                 path = path1, pred_name = paste("pred_knn", param1[i], sep = ""), 
                                                 param = param1[i], 
                                                 x_feature = x_feature1, respons = respons1)
                          })

stopCluster(cl)
message("Done")
message(paste("***** Stop Cores *****"))
