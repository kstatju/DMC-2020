# loss_func  = function(pred , orders, price){
# 
#   if (length(pred)==length(orders) & length(pred)==length(price)){
#     err = price*(orders - pred)
#     err[err>0] = err[err>0]*0.6
#     return(mean(abs(err)))
#   }else {
#     print("Dimenstion of ordrs, price and predicted are not same")
#     return()
#   }
# 
# }


knn_prediction_final <- function(train_dt, test_dt, path, pred_name, param, x_feature,
                                 respons = "order", 
                                 price = "new_price", test_week = 13){
    library(caret)
    library(lubridate)
    library(dplyr)
    library(tidyr)
    library(zoo)
    
    b_feature = c("itemID",               "week",                   "order",                 
                  "new_price",             "tr_ts",                  "brand" ,                
                  "manufacturer",           "category1",              "category2",
                  "category3",              "customerRating",         "recommendedRetailPrice")
    
    
    
    
    
    fl_dt_tr = train_dt %>% filter(week < test_week) %>% select(b_feature)
    fl_dt_vl = train_dt %>% filter(week == test_week) %>% mutate(tr_ts = 4) %>% select(b_feature) 
    test_dt = left_join(test_dt %>% select(-respons), train_dt %>% group_by(itemID) %>%
                          summarise(order = mean(order)) %>% as.data.frame(), 
                        by = "itemID") %>% replace_na(list(order = 0))
    fl_dt_ts = test_dt %>% select(b_feature)
    
    x_train = train_dt %>% filter(week <test_week) %>%
                  select_at(.vars = x_feature) %>% 
                  mutate(new_pr_npromoIW = new_price*(npromoIW+1)) %>% as.matrix()
    x_val = train_dt %>% filter(week == test_week) %>%
                  select_at(.vars = x_feature) %>% 
                  mutate(new_pr_npromoIW = new_price*(npromoIW+1)) %>% as.matrix()
    x_test = test_dt %>% select_at(.vars = x_feature) %>% 
                  mutate(new_pr_npromoIW = new_price*(npromoIW+1)) %>% as.matrix()
    
    x_train_price = train_dt %>% filter(week < test_week) %>%
                          select_at(.vars = price) %>% as.matrix()
    x_val_price = train_dt %>% filter(week == test_week) %>%
                          select_at(.vars = price) %>% as.matrix()
    x_test_price = test_dt %>% select_at(.vars = price) %>% as.matrix()
    
    y_train = train_dt %>% filter(week < test_week) %>%
                          select_at(.vars = respons) %>% as.matrix()
    y_val = train_dt %>% filter(week == test_week) %>%
                          select_at(.vars = respons) %>% as.matrix()
    
    y_test = test_dt %>% select_at(.vars = respons) %>% as.matrix()
    
    
    fit <- caret::knnreg(x = x_train, y = y_train, k = param[[1]], use.all = TRUE)
    
    yhat_train = predict(fit, x_train) 
    yhat_train = round(ifelse(yhat_train<0, 0, yhat_train))
    yhat_val = predict(fit, x_val)
    yhat_val = round(ifelse(yhat_val<0, 0, yhat_val))
    yhat_test = predict(fit, x_test)
    yhat_test = round(ifelse(yhat_test<0, 0, yhat_test))
    
    yhat_train = as.data.frame(yhat_train)
    names(yhat_train) = pred_name
    yhat_val = as.data.frame(yhat_val)
    names(yhat_val) = pred_name
    yhat_test = as.data.frame(yhat_test)
    names(yhat_test) = pred_name
    
    
    fl_dt_tr = fl_dt_tr %>% bind_cols(yhat_train)
    fl_dt_vl = fl_dt_vl %>% bind_cols(yhat_val)
    fl_dt_ts = fl_dt_ts %>% bind_cols(yhat_test)
    
    pred_dt = fl_dt_tr %>% bind_rows(fl_dt_vl) %>% bind_rows(fl_dt_ts)
    
    write.csv(pred_dt, file = paste(path, "knn_", pred_name, ".csv", sep = ""), row.names = F)
    return(pred_dt)
}


library(parallel)

path1 = "/work/STAT/kanak/knn_final/"

tr_dt = read.csv(paste(path1, "final_tr_std_promo3_alldt.csv", sep = ""), header = T)
ts_dt = read.csv(paste(path1, "final_ts_std_promo3_alldt.csv", sep = ""), header = T)

respons1   = "order"
price1     = "new_price"
test_week1 = 13
pred_name1 = "pred_knn1"
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
              "price1", "param1", "path1", "test_week1")
clusterExport(cl, clexp)

estimates <-  parLapplyLB(cl = cl, X = 1:ncombin, 
                          fun = function(i) {
                            knn_prediction_final(train_dt = tr_dt, test_dt = ts_dt, 
                                                path = path1, pred_name = paste("pred_knn", param1[i], sep = ""), 
                                                param = param1[i], 
                                                x_feature = x_feature1, respons = respons1, 
                                                price = price1, 
                                                test_week = test_week1)
                            })

stopCluster(cl)
message("Done")
message(paste("***** Stop Cores *****"))


### Add and summary of all knn prediction
############################################
get_simulationPrice<-function(path){
    
    # library(plyr)
    library(dplyr)
    library(tidyr)
    library(zoo)
    
    df = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
    df$time = as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
    df$date <- as.Date(df$time , format = "%Y-%m-%d", tz = "GMT")
    
    df = df %>% group_by(itemID, date) %>% mutate(order = sum(order),
                                                  salesPrice = mean(salesPrice))
    
    min_day = as.Date(min(df$date), format="%Y-%m-%d", tz = "GMT")
    mmax_day = as.Date(max(df$date), format="%Y-%m-%d", tz = "GMT")
    max_day = min_day
    
    date_var = data.frame(date = seq.Date(as.Date("2018-01-01"), as.Date("2018-06-29"), by = 'days'))
    date_var$date = as.Date(date_var$date, format="%Y-%m-%d", tz = "GMT")
    date_var$week = NA
    i=1
    while (max_day < mmax_day){
        if (i==1){
            min_day = min_day - 1
            max_day = max_day + 11
            date_var[(date_var$date > min_day) & (date_var$date <= max_day),"week"] = i
        } else{
            min_day = max_day
            max_day = max_day + 14
            date_var[(date_var$date > min_day) & (date_var$date <= max_day),"week"] = i
        }
        i = i+1
    }
    
    df = left_join(df, date_var, by = c("date"))
    date_var = data.frame(df[,c("itemID", "date", "salesPrice", "week")])
    date_var = date_var %>%  distinct() %>% mutate(new_price = NA)
    
    sel_var <- function(dt){
        dt = as.data.frame(dt)
        uni_week = sort(unique(dt[,"week"]))
        # print(uni_week)
        for (j in 1:length(uni_week)){
            if (j==1){
                subdt = dt[(dt$week == uni_week[j]) & (dt[,"salesPrice"]>0),]
                dt[(dt$week == uni_week[j]),
                   "new_price"] = subdt[which.min(subdt$date), "salesPrice"]
            } else{
                subdt = dt[(dt$week == uni_week[j-1]) & (dt[,"salesPrice"]>0),]
                dt[(dt$week == uni_week[j]),
                   "new_price"] = subdt[which.max(subdt$date), "salesPrice"]
            }
        }
        return(dt)
    }
    
    date_var = as.data.frame(date_var %>% group_by(itemID) %>% do(data.frame(sel_var(.)))) %>%
        select((c("itemID", "week", "new_price"))) %>% distinct()
    # sum(is.na(date_var$new_price))
    Experiment = expand.grid(list(itemID = sort(unique(date_var$itemID)),
                                  week = sort(unique(date_var$week))))
    date_var = left_join(Experiment, date_var, by = c("itemID", "week")) %>%
        replace_na(list(order = 0)) %>% arrange(itemID, week) %>% group_by(itemID) %>%
        mutate(new_price = na.locf(new_price, na.rm = F)) %>%
        mutate(new_price = na.locf(new_price, na.rm = F, fromLast = T)) %>% as.data.frame()
    
    infos = read.csv(paste(path, "infos.csv", sep = ""), header = TRUE, sep = "|") %>%
        mutate(week = max(date_var$week)+1) %>%
        dplyr::rename(new_price = simulationPrice) %>%
        select(c("itemID", "week", "new_price"))
    date_var = date_var %>% bind_rows(infos) %>% as.data.frame()
    
    rm(df, infos, Experiment)
    return(date_var)
}

dt_path =  "D:/ISU/DMC/DMC 2020/Data/DMC-2020-Task/DMC20_Data/"
sim_price = get_simulationPrice(path = dt_path)
names(sim_price) = c("itemID", "week", "new_price_nonstd")

path = "D:/ISU/DMC/DMC 2020/Code/knn/pred_week1_13/"
knn20 = read.csv(paste(path, "knn_pred_knn20_final.csv", sep = ""), header = T) %>% mutate(pred_knn20 = round(pred_knn20))
knn21 = read.csv(paste(path, "knn_pred_knn21_final.csv", sep = ""), header = T)%>% mutate(pred_knn21 = round(pred_knn21))
knn22 = read.csv(paste(path, "knn_pred_knn22_final.csv", sep = ""), header = T)%>% mutate(pred_knn22 = round(pred_knn22))
knn23 = read.csv(paste(path, "knn_pred_knn23_final.csv", sep = ""), header = T)%>% mutate(pred_knn23 = round(pred_knn23))
knn28 = read.csv(paste(path, "knn_pred_knn28_final.csv", sep = ""), header = T)%>% mutate(pred_knn28 = round(pred_knn28))

knn_pred = left_join(left_join(left_join(left_join(knn20, 
                                                   knn21 %>% select(c("itemID", "week", "pred_knn21")), by = c("itemID", "week")),
                                         knn22 %>% select(c("itemID", "week", "pred_knn22")), by = c("itemID", "week")),
                               knn23 %>% select(c("itemID", "week", "pred_knn23")), by = c("itemID", "week")),
                     knn28 %>% select(c("itemID", "week", "pred_knn28")), by = c("itemID", "week")) %>%
    as.data.frame()
knn_pred$knn_pred_mean = rowMeans(knn_pred[,13:17])

varnn = c("itemID",                 "week",                   "order",
          "new_price_nonstd",
          "new_price",              "tr_ts",                  "brand",                 
          "manufacturer",           "category1",              "category2",             
          "category3",              "customerRating",         "recommendedRetailPrice",
          "mean_order",
          "pred_knn20",             "pred_knn21",             "pred_knn22",            
          "pred_knn23",             "pred_knn28")

knn_pred = left_join(knn_pred, sim_price, by = c("itemID", "week")) %>% as.data.frame()

knn_pred = left_join(knn_pred, knn_pred %>% filter(tr_ts == 1) %>% 
              group_by(itemID) %>% summarise(mean_order = mean(order)) %>%
              as.data.frame(), by = "itemID") %>% replace_na(list(mean_order = 0)) %>% 
    select(varnn)
knn_pred$order[is.na(knn_pred$order)] = knn_pred$mean_order[is.na(knn_pred$order)]

rm(knn20, knn21, knn22, knn23, knn28)
write.csv(knn_pred, paste(path, "knn_pred_top_5_final.csv", sep = ""), row.names = F)

sumrytr = knn_pred %>% filter(week<13) %>% 
                summarise(loss_knn20 = loss_func(pred = pred_knn20, orders = order, price = new_price),
                          r2_knn20 = cor(order, pred_knn20, use = "pairwise.complete.obs")^2,
                          loss_knn21 = loss_func(pred = pred_knn21, orders = order, price = new_price),
                          r2_knn21 = cor(order, pred_knn21, use = "pairwise.complete.obs")^2,
                          loss_knn22 = loss_func(pred = pred_knn22, orders = order, price = new_price),
                          r2_knn22 = cor(order, pred_knn22, use = "pairwise.complete.obs")^2,
                          loss_knn23 = loss_func(pred = pred_knn23, orders = order, price = new_price),
                          r2_knn23 = cor(order, pred_knn23, use = "pairwise.complete.obs")^2,
                          loss_knn28 = loss_func(pred = pred_knn28, orders = order, price = new_price),
                          r2_knn28 = cor(order, pred_knn28, use = "pairwise.complete.obs")^2)

sumryvl = knn_pred %>% filter(week==13) %>% 
    summarise(loss_knn20 = loss_func(pred = pred_knn20, orders = order, price = new_price),
              r2_knn20 = cor(order, pred_knn20, use = "pairwise.complete.obs")^2,
              loss_knn21 = loss_func(pred = pred_knn21, orders = order, price = new_price),
              r2_knn21 = cor(order, pred_knn21, use = "pairwise.complete.obs")^2,
              loss_knn22 = loss_func(pred = pred_knn22, orders = order, price = new_price),
              r2_knn22 = cor(order, pred_knn22, use = "pairwise.complete.obs")^2,
              loss_knn23 = loss_func(pred = pred_knn23, orders = order, price = new_price),
              r2_knn23 = cor(order, pred_knn23, use = "pairwise.complete.obs")^2,
              loss_knn28 = loss_func(pred = pred_knn28, orders = order, price = new_price),
              r2_knn28 = cor(order, pred_knn28, use = "pairwise.complete.obs")^2)

sumryts = knn_pred %>% filter(week==14) %>% 
    summarise(loss_knn20 = loss_func(pred = pred_knn20, orders = order, price = new_price),
              r2_knn20 = cor(order, pred_knn20, use = "pairwise.complete.obs")^2,
              loss_knn21 = loss_func(pred = pred_knn21, orders = order, price = new_price),
              r2_knn21 = cor(order, pred_knn21, use = "pairwise.complete.obs")^2,
              loss_knn22 = loss_func(pred = pred_knn22, orders = order, price = new_price),
              r2_knn22 = cor(order, pred_knn22, use = "pairwise.complete.obs")^2,
              loss_knn23 = loss_func(pred = pred_knn23, orders = order, price = new_price),
              r2_knn23 = cor(order, pred_knn23, use = "pairwise.complete.obs")^2,
              loss_knn28 = loss_func(pred = pred_knn28, orders = order, price = new_price),
              r2_knn28 = cor(order, pred_knn28, use = "pairwise.complete.obs")^2)

sumrytr$data = "Week_1-12"
sumryvl$data = "Week_13"
sumryts$data = "Week_14"
sumry = rbind(sumrytr, sumryvl, sumryts)
