data_prep <- function(path, test_week, logtr = TRUE){
  
  # ordersdt2w <- function(path){
  #   library(plyr)
  #   library(dplyr)
  #   library(tidyr)
  #   library(zoo)
  #   orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
  #   orders$time = as.POSIXct(orders$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
  #   orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
  #   week = gen_week(path)
  #   orders = as.data.frame(orders)
  #   orders22 = orders %>% dplyr::group_by(itemID, date) %>% summarise(order = sum(order),
  #                                                                     salesPrice = mean(salesPrice))  %>% as.data.frame()
  #   orders1 = left_join(orders22, week, by = "date") %>%
  #     group_by(itemID, week) %>% summarise(order = sum(order),
  #                                          salesPrice = mean(salesPrice)) %>%
  #     ungroup() %>% as.data.frame()
  # 
  #   Experiment = expand.grid(list(itemID = sort(unique(orders1$itemID)),
  #                                 week = 1:13))
  #   orders1 = left_join(Experiment, orders1, by = c("itemID", "week")) %>%
  #     replace_na(list(order = 0))%>% arrange(itemID, week) %>% group_by(itemID) %>%
  #     mutate(salesPrice = na.locf(salesPrice, na.rm = F)) %>%
  #     mutate(salesPrice = na.locf(salesPrice, na.rm = F, fromLast = T))
  #   return(orders1)
  # }
  # 
  # promotion <- function(path){
  #   library(plyr)
  #   library(dplyr)
  #   library(tidyr)
  #   library(zoo)
  #   orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
  #   orders$time = as.POSIXct(orders$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
  #   orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
  # 
  #   orders = orders %>% group_by(itemID, date) %>% summarise(order = sum(order),
  #                                                            meanSalesPrice = mean(salesPrice)) %>% ungroup()
  #   orders1 = orders
  #   promo1 = orders %>%  mutate(promo1 = ifelse(meanSalesPrice == 0,1,0)) %>%
  #     select(c("itemID", "date", "promo1")) %>% as.data.frame()
  # 
  # 
  #   promo2 = orders %>% arrange(itemID, date) %>% group_by(itemID) %>%
  #     mutate(lag1 = na.locf(lag(meanSalesPrice, n=1L), na.rm = F, fromLast = T),
  #            lead1 = na.locf(lead(meanSalesPrice, n = 1L), na.rm = F),
  #            lead2 = na.locf(lead(meanSalesPrice, n = 2L), na.rm = F),
  #            lead3 = na.locf(lead(meanSalesPrice, n = 3L), na.rm = F),
  #            lead4 = na.locf(lead(meanSalesPrice, n = 4L), na.rm = F),
  #            lead5 = na.locf(lead(meanSalesPrice, n = 5L), na.rm = F),
  #            promo2 = ifelse(((meanSalesPrice < lag1) & (lead1 > meanSalesPrice))|
  #                              ((meanSalesPrice < lag1) & (lead2 > lead1) & (lead1 <= meanSalesPrice))|
  #                              ((meanSalesPrice < lag1) & (lead3 > lead2) & (lead1 <= meanSalesPrice) & (lead2 <= lead1))|
  #                              ((meanSalesPrice < lag1) & (lead4 > lead3) & (lead1 <= meanSalesPrice) & (lead2 <= lead1) & (lead3 <= lead2)) |
  #                              ((meanSalesPrice < lag1) & (lead5 > lead4) & (lead1 <= meanSalesPrice) & (lead2 <= lead1) & (lead3 <= lead2) & (lead4 <= lead3)), 1,0)
  #     ) %>% replace_na(list(promo2 = 0)) %>%
  #     select(c("itemID", "date", "promo2")) %>% ungroup()
  # 
  # 
  #   Experiment = expand.grid(list(itemID = sort(unique(orders$itemID)),
  #                                 date = seq(min(orders$date), max(orders$date), "days")))
  #   orders = left_join(x = Experiment, y = orders, by = c("itemID", "date")) %>%
  #     replace_na(list(order = 0))%>% arrange(itemID, date) %>% group_by(itemID) %>%
  #     mutate(meanSalesPrice = na.locf(meanSalesPrice, na.rm = F)) %>%
  #     mutate(meanSalesPrice = na.locf(meanSalesPrice, na.rm = F, fromLast = T)) %>%
  #     as.data.frame()
  # 
  # 
  #   promo3 = orders %>%
  #     group_by(itemID) %>% mutate(medianorder = median(order),
  #                                 promo3 = ifelse((order %in%
  #                                                    boxplot(order, plot=FALSE)$out[boxplot(order, plot=FALSE)$out>medianorder]) &
  #                                                   (order>30),1,0),
  #                                 zeroprice = ifelse(meanSalesPrice == 0,1,0)
  #     ) %>% ungroup() %>%
  #     select(c("itemID", "date", "promo3")) %>% as.data.frame()
  # 
  #   promo4 = read.csv(paste(path, "promotions.csv", sep = ""), header = TRUE, sep = ",")[,-1]
  #   promo4$date = as.Date(promo4$date, format="%Y-%m-%d", tz = "GMT")
  #   promo = join_all(list(orders1, promo1, promo2, promo3,promo4), by = c("itemID", "date"), type='left')
  #   promo$promo = rowSums(promo[,5:12])
  #   promo = promo %>% mutate(promo = ifelse(promo>=2,1,0),
  #                            tr_ts = 1) %>%
  #     select(c("itemID", "date", "promo", "tr_ts")) %>% as.data.frame()
  #   rm(promo1, promo2, promo3, promo4)
  # 
  # 
  #   infos = read.csv(paste(path, "infos.csv", sep = ""), header = TRUE, sep = "|")
  #   a1 <- gsub(pattern=",", replacement="_", infos$promotion, fixed = TRUE)
  #   a1[a1==""] = NA
  #   a2 = strsplit(a1, split = "_")
  #   a3 = data.frame(t(data.frame(lapply(a2, "length<-", max(lengths(a2))))))
  #   row.names(a3) = NULL
  #   names(a3) = paste('promotion', c(1:ncol(a3)), sep = "")
  #   infos = cbind(infos[,1:2], a3)
  #   rm(a1, a2, a3)
  #   infos$promotion1 = as.Date(infos$promotion1,format="%Y-%m-%d", tz = 'GMT')
  #   infos$promotion2 = as.Date(infos$promotion2,format="%Y-%m-%d", tz = 'GMT')
  #   infos$promotion3 = as.Date(infos$promotion3,format="%Y-%m-%d", tz = 'GMT')
  # 
  #   Experiment = expand.grid(list(itemID = sort(unique(infos$itemID)),
  #                                 date = seq(as.Date("2018-06-30"), as.Date("2018-07-13"), "days")))
  # 
  #   aa = unique(orders$itemID)
  # 
  #   infos1 = left_join(Experiment, infos, by = c("itemID")) %>%
  #     mutate(promo = ifelse((date == promotion1) | (date == promotion2) | (date == promotion3),1,0))%>%
  #     replace_na(list(promo = 0)) %>% mutate(tr_ts = ifelse(itemID %in% aa,2,3)) %>%
  #     select(c("itemID", "date", "promo", "tr_ts")) %>%
  #     as.data.frame()
  #   # infos1 = infos1 %>% mutate(promo = ifelse((promo == 1) & (itemID %in% aa), 1,0),
  #   #                            tr_ts = 2)  %>%
  #   #   select(c("itemID", "date", "promo", "tr_ts")) %>%
  #   #   as.data.frame()
  # 
  #   # Experiment = expand.grid(list(itemID = infos$itemID[!(infos$itemID %in% sort(unique(orders$itemID)))],
  #   #                               date = seq(as.Date("2018-06-30"), as.Date("2018-07-13"), "days")))
  #   # notintrain = Experiment %>% mutate(promo = 0, tr_ts = 3) %>%
  #   #   select(c("itemID", "date", "promo", "tr_ts")) %>%
  #   #   bind_rows(infos1) %>% as.data.frame()
  # 
  #   promo = promo %>% bind_rows(infos1) %>%
  #     filter(promo == 1) %>%
  #     select(c("itemID", "date", "promo")) %>% as.data.frame()
  #   rm(infos, infos1, Experiment, orders)
  # 
  #   return(promo)
  # }
  # 
  # 
  # get_simulationPrice<-function(path){
  # 
  #   library(plyr)
  #   library(dplyr)
  #   library(tidyr)
  #   library(zoo)
  # 
  #   df = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
  #   df$time = as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
  #   df$date <- as.Date(df$time , format = "%Y-%m-%d", tz = "GMT")
  # 
  #   df = df %>% group_by(itemID, date) %>% mutate(order = sum(order),
  #                                                 salesPrice = mean(salesPrice))
  # 
  #   min_day = as.Date(min(df$date), format="%Y-%m-%d", tz = "GMT")
  #   mmax_day = as.Date(max(df$date), format="%Y-%m-%d", tz = "GMT")
  #   max_day = min_day
  # 
  #   date_var = data.frame(date = seq.Date(as.Date("2018-01-01"), as.Date("2018-06-29"), by = 'days'))
  #   date_var$date = as.Date(date_var$date, format="%Y-%m-%d", tz = "GMT")
  #   date_var$week = NA
  #   i=1
  #   while (max_day < mmax_day){
  #     if (i==1){
  #       min_day = min_day - 1
  #       max_day = max_day + 11
  #       date_var[(date_var$date > min_day) & (date_var$date <= max_day),"week"] = i
  #     } else{
  #       min_day = max_day
  #       max_day = max_day + 14
  #       date_var[(date_var$date > min_day) & (date_var$date <= max_day),"week"] = i
  #     }
  #     i = i+1
  #   }
  # 
  #   df = left_join(df, date_var, by = c("date"))
  #   date_var = data.frame(df[,c("itemID", "date", "salesPrice", "week")])
  #   date_var = date_var %>%  distinct() %>% mutate(new_price = NA)
  # 
  #   sel_var <- function(dt){
  #     dt = as.data.frame(dt)
  #     uni_week = sort(unique(dt[,"week"]))
  #     # print(uni_week)
  #     for (j in 1:length(uni_week)){
  #       if (j==1){
  #         subdt = dt[(dt$week == uni_week[j]) & (dt[,"salesPrice"]>0),]
  #         dt[(dt$week == uni_week[j]),
  #            "new_price"] = subdt[which.min(subdt$date), "salesPrice"]
  #       } else{
  #         subdt = dt[(dt$week == uni_week[j-1]) & (dt[,"salesPrice"]>0),]
  #         dt[(dt$week == uni_week[j]),
  #            "new_price"] = subdt[which.max(subdt$date), "salesPrice"]
  #       }
  #     }
  #     return(dt)
  #   }
  # 
  #   date_var = as.data.frame(date_var %>% group_by(itemID) %>% do(data.frame(sel_var(.)))) %>%
  #     select((c("itemID", "week", "new_price"))) %>% distinct()
  #   # sum(is.na(date_var$new_price))
  #   Experiment = expand.grid(list(itemID = sort(unique(date_var$itemID)),
  #                                 week = sort(unique(date_var$week))))
  #   date_var = left_join(Experiment, date_var, by = c("itemID", "week")) %>%
  #     replace_na(list(order = 0)) %>% arrange(itemID, week) %>% group_by(itemID) %>%
  #     mutate(new_price = na.locf(new_price, na.rm = F)) %>%
  #     mutate(new_price = na.locf(new_price, na.rm = F, fromLast = T)) %>% as.data.frame()
  # 
  #   infos = read.csv(paste(path, "infos.csv", sep = ""), header = TRUE, sep = "|") %>%
  #     mutate(week = max(date_var$week)+1) %>%
  #     rename(new_price = simulationPrice) %>%
  #     select(c("itemID", "week", "new_price"))
  #   date_var = date_var %>% bind_rows(infos) %>% as.data.frame()
  # 
  #   rm(df, infos, Experiment)
  #   return(date_var)
  # }
  # 
  # 
  # gen_week <- function(path){
  #   library(plyr)
  #   library(dplyr)
  #   library(tidyr)
  #   library(zoo)
  #   orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
  #   orders$time = as.POSIXct(orders$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
  #   orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
  # 
  #   date_var = data.frame(date = seq.Date(as.Date("2018-01-01"), as.Date("2018-07-13"), by = 'days'))
  #   date_var$date = as.Date(date_var$date, format="%Y-%m-%d", tz = "GMT")
  #   min_day = as.Date(min(date_var$date), format="%Y-%m-%d", tz = "GMT")
  #   mmax_day = as.Date(max(date_var$date), format="%Y-%m-%d", tz = "GMT")
  #   max_day = min_day
  #   date_var$week = NA
  #   i=1
  #   while (max_day < mmax_day){
  #     if (i==1){
  #       min_day = min_day - 1
  #       max_day = max_day + 11
  #       date_var[(date_var$date > min_day) & (date_var$date <= max_day),"week"] = i
  #     } else{
  #       min_day = max_day
  #       max_day = max_day + 14
  #       date_var[(date_var$date > min_day) & (date_var$date <= max_day),"week"] = i
  #     }
  #     i = i+1
  #   }
  #   return(date_var)
  # }
  # 
  # 
  # gen_time_diff_ncust <- function(path){
  #   library(plyr)
  #   library(dplyr)
  #   library(tidyr)
  #   library(zoo)
  #   orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
  #   orders$time = as.POSIXct(orders$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
  #   orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
  # 
  #   date_diff = orders %>% select(c("itemID", "date")) %>%
  #     distinct() %>% arrange(itemID, (date)) %>% group_by(itemID) %>%
  #     mutate(time_diff = c(0, diff(date)))
  # 
  #   ncustomer = orders %>% group_by(itemID, date) %>%
  #     summarise(ncustomer = length(unique(transactID)))
  # 
  #   df = left_join(date_diff, ncustomer, by = c("itemID", "date"))
  # 
  #   # Experiment = expand.grid(list(itemID = sort(unique(orders$itemID)),
  #   #                               date = seq(as.Date("2018-01-01"), as.Date("2018-06-29"), "days")))
  #   #
  #   # df = left_join(Experiment, df, by = c("itemID", "date")) %>%
  #   #   replace_na(list(ncustomer = 0, time_diff = 0))
  #   rm(orders, date_diff, ncustomer)
  #   return(df)
  # }
  
  
  comp_supp_func <- function(dt, var_name){
    marge_to = dt %>% select(var_name, "week") %>% 'colnames<-'(c("itemID", "week")) %>% as.data.frame()
    df = left_join(marge_to, dt %>% select("itemID", "week", "order") %>% 
                     arrange(itemID, week) %>% group_by(itemID) %>%
                     mutate(order = lag(order, n = 1L)) %>% ungroup() %>%
                     replace_na(list(order = 0)), by = c("itemID", "week")) %>% 
      replace_na(list(order = 0))
    return(unlist(df[,3]))
  }
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(zoo)
  
  test_week = 13
  
  # week = gen_week(path)
  # write.csv(week, file = paste(path, "fl_week.csv", sep = ""), row.names = F)
  # week = read.csv(file = paste(path, "fl_week.csv", sep = ""), header = T)
  # week$date = as.Date(week$date, format = "%Y-%m-%d", tz = "GMT")

  # items = read.csv(paste(path, "items.csv", sep = ""), header = TRUE, sep = "|")
  # write.csv(items, file = paste(path, "fl_items.csv", sep = ""), row.names = F)
  # items = read.csv(file = paste(path, "fl_items.csv", sep = ""), header = T)
  
  
  
  # orders2w = ordersdt2w(path) %>% as.data.frame()
  # orders = left_join(orders2w, items, by = "itemID") %>%
  #   select(c("itemID",                 "week",                   "order",
  #            "salesPrice",             "brand" ,
  #            "manufacturer",           "category1",              "category2",
  #            "category3",              "customerRating",         "recommendedRetailPrice")) %>% as.data.frame()
  # 
  # simulation_price = get_simulationPrice(path)
  # # write.csv(simulation_price, file = paste(path, "fl_simulation_price.csv", sep = ""), row.names = F)
  # 
  # orders = left_join(orders, simulation_price, by = c("itemID", "week"))
  # write.csv(orders, file = paste(path, "fl_orders.csv", sep = ""), row.names = F)
  orders = read.csv(file = paste(path, "fl_orders.csv", sep = ""), header = T)
  
  orders = orders %>% mutate(tr_ts = ifelse(week == test_week,2,1)) %>% 
    select(c("itemID",               "week",                   "order",                 
             "salesPrice",             "tr_ts",                  "brand" ,                
             "manufacturer",           "category1",              "category2",
             "category3",              "customerRating",         "recommendedRetailPrice",
             "new_price")) %>% as.data.frame()
  
  # orders2w = left_join(orders2w, items, by = "itemID")
  # write.csv(orders2w, file = paste(path, "fl_orders2w.csv", sep = ""), row.names = F)
  orders2w = read.csv(file = paste(path, "fl_orders2w.csv", sep = ""), header = T)
  
  orders2w = orders2w %>% filter(week != test_week)
  
  
  
  
  
  # Experiment = expand.grid(list(itemID = sort(unique(orders2w$itemID)),
  #                               date = seq(as.Date("2018-01-01"), as.Date("2018-06-29"), "days")))
  # 
  # time_dff_ncust = gen_time_diff_ncust(path)
  # ncust = left_join(left_join(left_join(Experiment, time_dff_ncust, by = c("itemID","date")) %>%
  #                               replace_na(list(ncustomer = 0)) %>%
  #                               mutate(time_diff = na.locf(time_diff, na.rm = F, fromLast = T),
  #                                      time_diff = na.locf(time_diff, na.rm = F, fromLast = F),
  #                                      time_ncust = ncustomer*time_diff), week, by = "date"),
  #                   items, by = "itemID")
  # rm(Experiment)
  # ncust = ncust %>% select(c("itemID",               "time_diff",              "ncustomer",
  #                            "time_ncust",           "week",                   "brand",
  #                            "manufacturer",         "category1",              "category2",
  #                            "category3"))
  # write.csv(ncust, file = paste(path, "fl_ncust.csv", sep = ""), row.names = F)
  
  ncust = read.csv(file = paste(path, "fl_ncust.csv", sep = ""), header = T)
  
  # promo = promotion(path)
  # promo = left_join(promo, week, by ='date')
  # promo = left_join(promo, items, by ='itemID') %>% select(-"date")
  # 
  # write.csv(promo, file = paste(path, "fl_promo.csv", sep = ""), row.names = F)
  # promo = read.csv(file = paste(path, "fl_promo.csv", sep = ""), header = T)

  
    
  mean_op_by_itm = orders2w %>% group_by(itemID) %>%
    summarise(morderI = mean(order))  %>% as.data.frame()
  
  # promo_by_week_muf_brn_cat123 = promo %>% group_by(manufacturer, brand, category1, category2, category3, week) %>%
  #   summarise(npromoBMC123W = sum(promo)) %>% as.data.frame()
  # write.csv(promo_by_week_muf_brn_cat123, file = paste(path, "fl_promo_by_week_muf_brn_cat123.csv", sep = ""), row.names = F)
  promo_by_week_muf_brn_cat123 = read.csv(file = paste(path, "fl_promo_by_week_muf_brn_cat123.csv", sep = ""), header = T)
  
  # promo_by_week_bnd_mnf = promo %>% group_by(brand, manufacturer, week) %>%
  #   summarise(npromoBMW = sum(promo)) %>% as.data.frame()
  # write.csv(promo_by_week_bnd_mnf, file = paste(path, "fl_promo_by_week_bnd_mnf.csv", sep = ""), row.names = F)
  promo_by_week_bnd_mnf = read.csv(file = paste(path, "fl_promo_by_week_bnd_mnf.csv", sep = ""), header = T)
  
  # promo_by_item = promo %>% group_by(itemID) %>%
  #   summarise(npromoI = sum(promo)) %>% as.data.frame()
  # write.csv(promo_by_item, file = paste(path, "fl_promo_by_item.csv", sep = ""), row.names = F)
  promo_by_item = read.csv(file = paste(path, "fl_promo_by_item.csv", sep = ""), header = T)
  
  # promo_by_week_item = promo %>% group_by(itemID, week) %>%
  #   summarise(npromoIW = sum(promo)) %>% as.data.frame()
  # write.csv(promo_by_week_item, file = paste(path, "fl_promo_by_week_item.csv", sep = ""), row.names = F)
  promo_by_week_item = read.csv(file = paste(path, "fl_promo_by_week_item.csv", sep = ""), header = T)
  
  orders = left_join(left_join(left_join(left_join(left_join(orders, 
                                                             mean_op_by_itm, by = c("itemID")), 
                                                   promo_by_week_muf_brn_cat123, by = c("manufacturer", "brand", "category1", "category2", "category3", "week")), 
                                         promo_by_week_bnd_mnf, by = c("brand", "manufacturer", "week")), 
                               promo_by_item, by = c("itemID")), 
                     promo_by_week_item, by = c("itemID", "week")) %>%
    replace_na(list(npromoBMC123W = 0, npromoBMW = 0, npromoI = 0, npromoIW = 0, morderI = 0)) %>% 
    mutate(npromoIW_npromoIW = npromoIW^2)
  
  rm(promo_by_week_muf_brn_cat123, promo_by_week_bnd_mnf, promo_by_item, mean_op_by_itm)
  
  
  tmncus_by_itm = ncust %>% filter(week != test_week) %>% group_by(itemID) %>%
    summarise(mncusI = mean(ncustomer)) %>% as.data.frame()
  tmncus_by_itmwk = ncust  %>% filter(week != test_week) %>% group_by(itemID, week) %>%
    summarise(mncusIW = mean(ncustomer)) %>% as.data.frame()
  tmncus_by_banmnfc123 = ncust  %>% filter(week != test_week) %>% group_by(brand, manufacturer, category1, category2, category3) %>%
    summarise(mtmncusBMC123=mean(time_ncust)) %>% as.data.frame()
  
  
  orders = left_join(left_join(left_join(orders, 
                                                             tmncus_by_itm, by = c("itemID")), 
                                                   tmncus_by_itmwk, by = c("itemID", "week")), 
                                         tmncus_by_banmnfc123, by = c("brand", "manufacturer", "category1", "category2", "category3")) %>%
    arrange(itemID, week) %>% 
    mutate(mncusIW = lag(mncusIW, n = 1L)) %>% 
    replace_na(list(mncusI =  0,   mncusIW = 0,    mtmncusBMC123 = 0))

  rm(tmncus_by_itm, tmncus_by_itmwk, tmncus_by_banmnfc123)
  
  orders = orders %>% arrange(itemID, week) %>% group_by(itemID) %>%
    mutate(lag1_order = lag(order, n = 1L),
           lag2_order = lag(order, n = 2L),
           lag3_order = lag(order, n = 3L)) %>%
    replace_na(list(lag1_order = 0, lag2_order = 0, lag3_order = 0)) %>% ungroup()

  
  comp_supp_features = read.csv(paste(path, "fl_comp_supp_features.csv", sep = ""), header = TRUE, sep = ",")[,-1]
  orders = left_join(orders, comp_supp_features, by = "itemID") %>% 
    replace_na(list(comp10 = 0,   comp9 = 0,    
                    comp8 = 0,    comp7 = 0,    comp6 = 0,                 
                    comp5 = 0,    comp4 = 0,    comp3 = 0,                 
                    comp2 = 0,    comp1 = 0,    supp10 = 0,               
                    supp9 = 0,    supp8 = 0,    supp7 = 0,                 
                    supp6 = 0,    supp5 = 0,    supp4 = 0,                 
                    supp3 = 0,    supp2 = 0,    supp1 = 0))
  
  orders = orders %>% mutate(comp1_order= comp_supp_func(dt = orders, var_name = "comp1"),
                             comp2_order= comp_supp_func(dt = orders, var_name = "comp2"),
                             comp3_order= comp_supp_func(dt = orders, var_name = "comp3"))
  
  nname = c("comp1",                 "comp2",                 
            "comp3",                  "comp4",                  "comp5",                 
            "comp6",                  "comp7",                  "comp8",
            "comp9",                  "comp10",                  "supp1",
            "supp2")

  
  
  orders = left_join(left_join(left_join(left_join(left_join(
                  left_join(left_join(left_join(left_join(left_join(left_join(left_join(orders, 
                  promo_by_week_item %>% 'colnames<-'(c("comp1", "week", "comp1_npromoIW")), by = c("comp1", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp2", "week", "comp2_npromoIW")), by = c("comp2", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp3", "week", "comp3_npromoIW")), by = c("comp3", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp4", "week", "comp4_npromoIW")), by = c("comp4", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp5", "week", "comp5_npromoIW")), by = c("comp5", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp6", "week", "comp6_npromoIW")), by = c("comp6", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp7", "week", "comp7_npromoIW")), by = c("comp7", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp8", "week", "comp8_npromoIW")), by = c("comp8", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp9", "week", "comp9_npromoIW")), by = c("comp9", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("comp10", "week", "comp10_npromoIW")), by = c("comp10", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("supp1", "week", "supp1_npromoIW")), by = c("supp1", "week")), 
                  promo_by_week_item %>% 'colnames<-'(c("supp2", "week", "supp2_npromoIW")), by = c("supp2", "week")) %>%
    replace_na(list(comp1_npromoIW = 0, comp2_npromoIW = 0, comp3_npromoIW = 0, comp4_npromoIW = 0,
                    comp5_npromoIW = 0, comp6_npromoIW = 0, comp7_npromoIW = 0, comp8_npromoIW = 0,
                    comp9_npromoIW = 0, comp10_npromoIW = 0, supp1_npromoIW = 0, supp2_npromoIW = 0))


  rm(promo_by_week_item, ncust, orders2w, comp_supp_features)
  
  final_var = c("itemID",                   "week",                  "order",
                "logorder",                 "tr_ts",                  "manufacturer",
                "category1",                "category2",             "category3",
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
  
  orders = orders %>% mutate(logorder = log(order+1),
                             npromoIW_npromoIW = npromoIW^2) %>% select(final_var)
  
  imp_var = c(  "customerRating",           "new_price",             "recommendedRetailPrice",
                "lag1_order",               "lag2_order",            "lag3_order",
                "comp1_order",              "comp2_order",           "comp3_order",
                "comp1_npromoIW",           "comp2_npromoIW",        "comp3_npromoIW",
                "comp4_npromoIW",           "comp5_npromoIW",        "comp6_npromoIW",
                "comp7_npromoIW",           "comp8_npromoIW",        "comp9_npromoIW",
                "comp10_npromoIW",          "supp1_npromoIW",        "supp2_npromoIW",
                "morderI",                  "mncusI",                "mncusIW",
                "mtmncusBMC123",            "npromoBMC123W",         "npromoBMW",
                "npromoI",                  "npromoIW",              "npromoIW_npromoIW")
  
  if(logtr){
    orders12 = orders %>% mutate(lag1_order = log(lag1_order+1),
                                 lag2_order = log(lag2_order+1),
                                 lag3_order = log(lag3_order+1),
                                 comp1_order = log(comp1_order+1),
                                 comp2_order +log(comp2_order+1),
                                 comp3_order +log(comp3_order+1),
                                 comp1_npromoIW = log(comp1_npromoIW+1),
                                 comp2_npromoIW = log(comp2_npromoIW+1),
                                 comp3_npromoIW = log(comp3_npromoIW+1),
                                 comp4_npromoIW = log(comp4_npromoIW+1),
                                 comp5_npromoIW = log(comp5_npromoIW+1),
                                 comp6_npromoIW = log(comp6_npromoIW+1),
                                 comp7_npromoIW = log(comp7_npromoIW+1),
                                 comp8_npromoIW = log(comp8_npromoIW+1),
                                 comp9_npromoIW = log(comp9_npromoIW+1),
                                 comp10_npromoIW = log(comp10_npromoIW+1),
                                 supp1_npromoIW = log(supp1_npromoIW+1),
                                 supp2_npromoIW = log(supp1_npromoIW+1),
                                 mncusI = log(mncusI +1),
                                 mncusIW = log(mncusIW +1),
                                 morderI = log(morderI+1),
                                 mtmncusBMC123 = log(mtmncusBMC123 + 1),
                                 npromoBMC123W = log(npromoBMC123W +1),
                                 npromoBMW = log(npromoBMW +1),
                                 npromoI = log(npromoI +1),
                                 npromoIW = log(npromoIW +1),
                                 npromoIW_npromoIW = log(npromoIW_npromoIW + 1)) %>% select(final_var)  
  }else{
    orders12 = orders %>% select(final_var)
  }
  
  mean_tr_dt = orders12 %>% filter(tr_ts == 1) %>% select(imp_var) %>% 
    summarise_all(mean)
  sd_tr_dt = orders12 %>% filter(tr_ts == 1)  %>% select(imp_var) %>% 
    summarise_all(sd)
  tr_dt = orders12 %>% filter(tr_ts == 1) %>% mutate_at(.vars = imp_var,
                                                        ~(scale(.) %>% as.vector))%>%
    as.data.frame()
  
  ts_dt =  orders12 %>% filter(tr_ts >1)
  ts_dt[,imp_var] = scale(ts_dt[,imp_var], mean_tr_dt, sd_tr_dt)
  # orders12_std = tr_dt %>% bind_rows(ts_dt) %>% as.data.frame()
  
  if(logtr){
    write.csv(x = tr_dt, file = paste(path, "final_tr_std_log_tsw", test_week, ".csv", sep = ""), row.names = F)
    write.csv(x = ts_dt , file = paste(path, "final_ts_std_log_tsw", test_week, ".csv", sep = ""), row.names = F)
  }else{
    write.csv(x = tr_dt, file = paste(path, "final_tr_std_tsw", test_week, ".csv", sep = ""), row.names = F)
    write.csv(x = ts_dt , file = paste(path, "final_ts_std_tsw", test_week, ".csv", sep = ""), row.names = F)
  }
  
  
  
  rm(sd_tr_dt, mean_tr_dt, orders, orders12)
  
  
  if(logtr){
    return(list(train_dt_std_log = tr_dt, test_dt_std_log = ts_dt, 
                tr_ts_log = orders12, tr_ts_all_std_log = orders12_std))
  }else {
    return(list(train_dt_std = tr_dt, test_dt_std = ts_dt, 
                tr_ts = orders12, tr_ts_all_std = orders12_std))
  }
}

path = "D:/ISU/DMC/DMC 2020/Data/DMC-2020-Task/DMC20_Data/"

df = data_prep(path, test_week = 11, logtr = T)
