data_prep <- function(path){

    ordersdt2w <- function(path){
      library(plyr)
      library(dplyr)
      library(tidyr)
      library(zoo)
      orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
      orders$time = as.POSIXct(orders$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
      orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
      week = gen_week(path)
      orders1 = left_join(orders %>% group_by(itemID, date) %>% summarise(order = sum(order), 
                                                                          salesPrice = mean(salesPrice)) %>% 
                            ungroup() %>% as.data.frame(), week, by = "date") %>% 
        group_by(itemID, week) %>% summarise(order = sum(order), 
                                             salesPrice = mean(salesPrice)) %>% 
        ungroup() %>% as.data.frame()
      
      Experiment = expand.grid(list(itemID = sort(unique(orders1$itemID)), 
                                    week = 1:13))
      orders1 = left_join(Experiment, orders1, by = c("itemID", "week")) %>% 
        replace_na(list(order = 0))%>% arrange(itemID, week) %>% group_by(itemID) %>%
        mutate(salesPrice = na.locf(salesPrice, na.rm = F)) %>% 
        mutate(salesPrice = na.locf(salesPrice, na.rm = F, fromLast = T))
      return(orders1)
    }
    
    promotion <- function(path){
      library(plyr)
      library(dplyr)
      library(tidyr)
      library(zoo)
      orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
      orders$time = as.POSIXct(orders$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
      orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
      
      orders = orders %>% group_by(itemID, date) %>% summarise(order = sum(order), 
                                                               meanSalesPrice = mean(salesPrice)) %>% ungroup()
      orders1 = orders
      promo1 = orders %>%  mutate(promo1 = ifelse(meanSalesPrice == 0,1,0)) %>% 
        select(c("itemID", "date", "promo1")) %>% as.data.frame()
      
      
      promo2 = orders %>% arrange(itemID, date) %>% group_by(itemID) %>% 
        mutate(lag1 = na.locf(lag(meanSalesPrice, n=1L), na.rm = F, fromLast = T),
               lead1 = na.locf(lead(meanSalesPrice, n = 1L), na.rm = F),
               lead2 = na.locf(lead(meanSalesPrice, n = 2L), na.rm = F),
               lead3 = na.locf(lead(meanSalesPrice, n = 3L), na.rm = F),
               lead4 = na.locf(lead(meanSalesPrice, n = 4L), na.rm = F),
               lead5 = na.locf(lead(meanSalesPrice, n = 5L), na.rm = F),
               promo2 = ifelse(((meanSalesPrice < lag1) & (lead1 > meanSalesPrice))|
                                 ((meanSalesPrice < lag1) & (lead2 > lead1) & (lead1 <= meanSalesPrice))|
                                 ((meanSalesPrice < lag1) & (lead3 > lead2) & (lead1 <= meanSalesPrice) & (lead2 <= lead1))|
                                 ((meanSalesPrice < lag1) & (lead4 > lead3) & (lead1 <= meanSalesPrice) & (lead2 <= lead1) & (lead3 <= lead2)) |
                                 ((meanSalesPrice < lag1) & (lead5 > lead4) & (lead1 <= meanSalesPrice) & (lead2 <= lead1) & (lead3 <= lead2) & (lead4 <= lead3)), 1,0)
        ) %>% replace_na(list(promo2 = 0)) %>% 
        select(c("itemID", "date", "promo2")) %>% ungroup()
      
      
      Experiment = expand.grid(list(itemID = sort(unique(orders$itemID)), 
                                    date = seq(min(orders$date), max(orders$date), "days")))
      orders = left_join(x = Experiment, y = orders, by = c("itemID", "date")) %>%
        replace_na(list(order = 0))%>% arrange(itemID, date) %>% group_by(itemID) %>%
        mutate(meanSalesPrice = na.locf(meanSalesPrice, na.rm = F)) %>% 
        mutate(meanSalesPrice = na.locf(meanSalesPrice, na.rm = F, fromLast = T)) %>% 
        as.data.frame()
      
      
      promo3 = orders %>%  
        group_by(itemID) %>% mutate(medianorder = median(order),
                                    promo3 = ifelse((order %in% 
                                                       boxplot(order, plot=FALSE)$out[boxplot(order, plot=FALSE)$out>medianorder]) & 
                                                      (order>30),1,0),
                                    zeroprice = ifelse(meanSalesPrice == 0,1,0)
        ) %>% ungroup() %>%
        select(c("itemID", "date", "promo3")) %>% as.data.frame()
      
      promo4 = read.csv(paste(path, "promotions.csv", sep = ""), header = TRUE, sep = ",")[,-1]
      promo4$date = as.Date(promo4$date, format="%Y-%m-%d", tz = "GMT")
      promo = join_all(list(orders1, promo1, promo2, promo3,promo4), by = c("itemID", "date"), type='left')
      promo$promo = rowSums(promo[,5:12]) 
      promo = promo %>% mutate(promo = ifelse(promo>=2,1,0),
                               tr_ts = 1) %>% 
        select(c("itemID", "date", "promo", "tr_ts")) %>% as.data.frame()
      rm(promo1, promo2, promo3, promo4)
      
      
      infos = read.csv(paste(path, "infos.csv", sep = ""), header = TRUE, sep = "|")
      a1 <- gsub(pattern=",", replacement="_", infos$promotion, fixed = TRUE)
      a1[a1==""] = NA
      a2 = strsplit(a1, split = "_")
      a3 = data.frame(t(data.frame(lapply(a2, "length<-", max(lengths(a2))))))
      row.names(a3) = NULL
      names(a3) = paste('promotion', c(1:ncol(a3)), sep = "")
      infos = cbind(infos[,1:2], a3)
      rm(a1, a2, a3)
      infos$promotion1 = as.Date(infos$promotion1,format="%Y-%m-%d", tz = 'GMT')
      infos$promotion2 = as.Date(infos$promotion2,format="%Y-%m-%d", tz = 'GMT')
      infos$promotion3 = as.Date(infos$promotion3,format="%Y-%m-%d", tz = 'GMT')
      
      Experiment = expand.grid(list(itemID = sort(unique(orders$itemID)), 
                                    date = seq(as.Date("2018-06-30"), as.Date("2018-07-13"), "days")))
      
      aa = unique(promo$itemID[promo$promo==1])
      
      infos1 = left_join(Experiment, infos, by = c("itemID")) %>% 
        mutate(promo = ifelse((date == promotion1) | (date == promotion2) | (date == promotion3),1,0))%>%
        replace_na(list(promo = 0)) 
      infos1 = infos1 %>% mutate(promo = ifelse((promo == 1) & (itemID %in% aa), 1,0),
                                 tr_ts = 2)  %>% 
        select(c("itemID", "date", "promo", "tr_ts")) %>%
        as.data.frame()
      
      # Experiment = expand.grid(list(itemID = infos$itemID[!(infos$itemID %in% sort(unique(orders$itemID)))], 
      #                               date = seq(as.Date("2018-06-30"), as.Date("2018-07-13"), "days")))
      # notintrain = Experiment %>% mutate(promo = 0, tr_ts = 3) %>% 
      #   select(c("itemID", "date", "promo", "tr_ts")) %>% 
      #   bind_rows(infos1) %>% as.data.frame()
      
      promo = promo %>% bind_rows(infos1) %>% 
                  filter(promo == 1) %>% 
                  select(c("itemID", "date", "promo")) %>% as.data.frame()
      rm(infos, infos1, Experiment, orders)
      
      return(promo)
    }
    
    
    get_simulationPrice<-function(path){
    
      library(plyr)
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
        rename(new_price = simulationPrice) %>%
        select(c("itemID", "week", "new_price"))
      date_var = date_var %>% bind_rows(infos) %>% as.data.frame()
      
      rm(df, infos, Experiment)
      return(date_var)
    }
    
    
    gen_week <- function(path){
      library(plyr)
      library(dplyr)
      library(tidyr)
      library(zoo)
      orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
      orders$time = as.POSIXct(orders$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
      orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
      
      date_var = data.frame(date = seq.Date(as.Date("2018-01-01"), as.Date("2018-07-13"), by = 'days')) 
      date_var$date = as.Date(date_var$date, format="%Y-%m-%d", tz = "GMT")
      min_day = as.Date(min(date_var$date), format="%Y-%m-%d", tz = "GMT")
      mmax_day = as.Date(max(date_var$date), format="%Y-%m-%d", tz = "GMT")
      max_day = min_day
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
      return(date_var)
    }
    
    
    gen_time_diff_ncust <- function(path){
      library(plyr)
      library(dplyr)
      library(tidyr)
      library(zoo)
      orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
      orders$time = as.POSIXct(orders$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
      orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
      
      date_diff = orders %>% select(c("itemID", "date")) %>% 
        distinct() %>% arrange(itemID, (date)) %>% group_by(itemID) %>%
        mutate(time_diff = c(0, diff(date)))
      
      ncustomer = orders %>% group_by(itemID, date) %>%
        summarise(ncustomer = length(unique(transactID)))
      
      df = left_join(date_diff, ncustomer, by = c("itemID", "date"))
      
      # Experiment = expand.grid(list(itemID = sort(unique(orders$itemID)), 
      #                               date = seq(as.Date("2018-01-01"), as.Date("2018-06-29"), "days")))
      # 
      # df = left_join(Experiment, df, by = c("itemID", "date")) %>% 
      #   replace_na(list(ncustomer = 0, time_diff = 0))
      rm(orders, date_diff, ncustomer)
      return(df)
    }
    
    
    comp_supp_func <- function(dt, var_name){
      marge_to = dt %>% select(var_name, "week") %>% 'colnames<-'(c("itemID", "week")) %>% as.data.frame()
      df = left_join(marge_to, dt %>% select("itemID", "week", "order") %>% 
                      arrange(itemID, week) %>% group_by(itemID) %>%
                      mutate(order = lag(order, n = 1L)) %>% ungroup() %>%
                      replace_na(list(order = 0)), by = c("itemID", "week"))
      return(unlist(df[,3]))
    }
    
    library(plyr)
    library(dplyr)
    library(tidyr)
    library(zoo)
    
    week = gen_week(path)
    
    items = read.csv(paste(path, "items.csv", sep = ""), header = TRUE, sep = "|")
    
    orders2w = ordersdt2w(path) %>% as.data.frame()
    orders = left_join(orders2w %>% mutate(tr_ts = 1) %>% 
               bind_rows(data.frame(itemID = read.csv(paste(path, "infos.csv", sep = ""), header = TRUE, sep = "|")[,1]) %>%
                  mutate(week = 14, order = NA, salesPrice = NA,
                         tr_ts = ifelse(itemID %in% unique(orders2w$itemID),2,3))),
               items, by = "itemID") %>% 
                select(c("itemID",               "week",                   "order",                 
                       "salesPrice",             "tr_ts",                  "brand" ,                
                       "manufacturer",           "category1",              "category2",
                       "category3",              "customerRating",         "recommendedRetailPrice")) %>% as.data.frame()
    # colSums(is.na(orders))
    orders2w = left_join(orders2w, items, by = "itemID")
    
    orders = left_join(orders, get_simulationPrice(path), by = c("itemID", "week"))
    
    
    ################################
    # Items
    ################################
    
    rat_reprice_by_item = items %>% group_by(itemID) %>% 
              summarise(ratingI = mean(customerRating), repriceI = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_brnd = items %>% group_by(brand) %>% 
              summarise(ratingB = mean(customerRating), repriceB = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_manuf = items %>% group_by(manufacturer) %>% 
              summarise(ratingM = mean(customerRating), repriceM = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_cat1 = items %>% group_by(category1) %>% 
              summarise(ratingC1 = mean(customerRating), repriceC1 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_cat2 = items %>% group_by(category2) %>% 
              summarise(ratingC2 = mean(customerRating), repriceC2 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_cat3 = items %>% group_by(category3) %>% 
              summarise(ratingC3 = mean(customerRating), repricec3 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_cat123 = items %>% group_by(category1, category2, category3) %>% 
              summarise(ratingC123 = mean(customerRating), repriceC123 = mean(recommendedRetailPrice)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(left_join(left_join(
                  orders, rat_reprice_by_item, by = "itemID"), 
                      rat_reprice_by_brnd, by = "brand"), 
                      rat_reprice_by_manuf, by = "manufacturer"), 
                      rat_reprice_by_cat1, by = "category1"), 
                      rat_reprice_by_cat2, by = "category2"), 
                      rat_reprice_by_cat3, by = "category3"), 
                      rat_reprice_by_cat123, by = c("category1", "category2", "category3"))
    
    rm(rat_reprice_by_item, rat_reprice_by_brnd,rat_reprice_by_manuf,rat_reprice_by_cat1,
       rat_reprice_by_cat2,rat_reprice_by_cat3,rat_reprice_by_cat123)
    # colSums(is.na(orders))
    
    
    rat_reprice_by_brndmnf = items %>% group_by(brand, manufacturer) %>% 
              summarise(ratingBM = mean(customerRating), repriceBM = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_bcat1 = items %>% group_by(brand, category1) %>% 
              summarise(ratingBC1 = mean(customerRating), repriceBC1 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_bcat2 = items %>% group_by(brand, category2) %>% 
              summarise(ratingBC2 = mean(customerRating), repriceBC2 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_bcat3 = items %>% group_by(brand, category3) %>% 
              summarise(ratingBC3 = mean(customerRating), repriceBC3 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_bcat123 = items %>% group_by(brand, category1, category2, category3) %>% 
              summarise(ratingBC123 = mean(customerRating), repriceBC123 = mean(recommendedRetailPrice)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
                rat_reprice_by_brndmnf, by = c("brand", "manufacturer")), 
                rat_reprice_by_bcat1, by = c("brand", "category1")), 
                rat_reprice_by_bcat2, by = c("brand", "category2")), 
                rat_reprice_by_bcat3, by = c("brand", "category3")), 
                rat_reprice_by_bcat123, by = c("brand", "category1", "category2", "category3"))
    
    rm(rat_reprice_by_brndmnf, rat_reprice_by_bcat1,rat_reprice_by_bcat2,
       rat_reprice_by_bcat3,rat_reprice_by_bcat123)
    # colSums(is.na(orders))
    
    rat_reprice_by_mcat1 = items %>% group_by(manufacturer, category1) %>% 
              summarise(ratingMC1 = mean(customerRating), repriceMC1 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_mcat2 = items %>% group_by(manufacturer, category2) %>% 
              summarise(ratingMC2 = mean(customerRating), repriceMC2 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_mcat3 = items %>% group_by(manufacturer, category3) %>% 
              summarise(ratingMC3 = mean(customerRating), repriceMC3 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_mcat123 = items %>% group_by(manufacturer, category1, category2, category3) %>% 
              summarise(ratingMC123 = mean(customerRating), repriceMC123 = mean(recommendedRetailPrice)) %>% as.data.frame()
    rat_reprice_by_bmcat123 = items %>% group_by(manufacturer, brand, category1, category2, category3) %>% 
              summarise(ratingBMC123 = mean(customerRating), repriceBMC123 = mean(recommendedRetailPrice)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
                    rat_reprice_by_mcat1, by = c("manufacturer", "category1")), 
                    rat_reprice_by_mcat2, by = c("manufacturer", "category2")), 
                    rat_reprice_by_mcat3, by = c("manufacturer", "category3")), 
                    rat_reprice_by_mcat123, by = c("manufacturer", "category1", "category2", "category3")), 
                    rat_reprice_by_bmcat123, by = c("manufacturer", "brand", "category1", "category2", "category3"))
    
    rm(rat_reprice_by_mcat1, rat_reprice_by_mcat2,rat_reprice_by_mcat3,rat_reprice_by_mcat123,
       rat_reprice_by_bmcat123)
    # colSums(is.na(orders))
    
    #####################################################
    # Promotion
    #####################################################
    promo = promotion(path)
    promo = left_join(promo, week, by ='date')
    promo = left_join(promo, items, by ='itemID')
    
    promo_by_item = promo %>% group_by(itemID) %>%
            summarise(npromoI = sum(promo)) %>% as.data.frame()
    promo_by_brand = promo %>% group_by(brand) %>%
            summarise(npromoB = sum(promo)) %>% as.data.frame()
    promo_by_manuf = promo %>% group_by(manufacturer) %>%
            summarise(npromoM = sum(promo)) %>% as.data.frame()
    promo_by_cat1 = promo %>% group_by(category1) %>%
            summarise(npromoC1 = sum(promo)) %>% as.data.frame()
    promo_by_cat2 = promo %>% group_by(category2) %>%
            summarise(npromoC2 = sum(promo)) %>% as.data.frame()
    promo_by_cat3 = promo %>% group_by(category3) %>%
            summarise(npromoC3 = sum(promo)) %>% as.data.frame()
    promo_by_cat1_cat3 = promo %>% group_by(category1, category2, category3) %>%
            summarise(npromoC123 = sum(promo)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(left_join(left_join(orders, 
              promo_by_item, by = c("itemID")), 
              promo_by_brand, by = c("brand")), 
              promo_by_manuf, by = c("manufacturer")), 
              promo_by_cat1, by = c("category1")),
              promo_by_cat2, by = c("category2")),
              promo_by_cat3, by = c("category3")),
              promo_by_cat1_cat3, by = c("category1", "category2", "category3")) %>%
              replace_na(list(npromoI = 0, npromoB = 0, npromoM = 0, npromoC2 = 0, npromoC123 = 0))
    
    rm(promo_by_item, promo_by_brand, promo_by_manuf, promo_by_cat1,
       promo_by_cat2, promo_by_cat3, promo_by_cat1_cat3)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
    
    
    promo_by_bnd_mnf = promo %>% group_by(brand, manufacturer) %>%
              summarise(npromoBM = sum(promo)) %>% as.data.frame()
    promo_by_bndc1 = promo %>% group_by(brand, category1) %>%
              summarise(npromoBC1 = sum(promo)) %>% as.data.frame()
    promo_by_bndc2 = promo %>% group_by(brand, category2) %>%
              summarise(npromoBC2 = sum(promo)) %>% as.data.frame()
    promo_by_bndc3 = promo %>% group_by(brand, category3) %>%
              summarise(npromoBC3 = sum(promo)) %>% as.data.frame()
    promo_by_bndc123 = promo %>% group_by(brand, category1, category2, category3) %>%
              summarise(npromoBC123 = sum(promo)) %>% as.data.frame()
    promo_by_mnfc1 = promo %>% group_by(manufacturer, category1) %>%
              summarise(npromoMC1 = sum(promo)) %>% as.data.frame()
    promo_by_mnfc2 = promo %>% group_by(manufacturer, category2) %>%
              summarise(npromoMC2 = sum(promo)) %>% as.data.frame()
    promo_by_mnfc3 = promo %>% group_by(manufacturer, category3) %>%
              summarise(npromoMC3 = sum(promo)) %>% as.data.frame()
    promo_by_mnfc123 = promo %>% group_by(manufacturer, category1, category2, category3) %>%
              summarise(npromoMC123 = sum(promo)) %>% as.data.frame()
    promo_by_brnmnfc123 = promo %>% group_by(manufacturer, brand, category1, category2, category3) %>%
      summarise(npromoBMC123 = sum(promo)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(orders, 
                promo_by_bnd_mnf, by = c("brand", "manufacturer")), 
                promo_by_bndc1, by = c("brand", "category1")), 
                promo_by_bndc2, by = c("brand", "category2")), 
                promo_by_bndc3, by = c("brand", "category3")),
                promo_by_bndc123, by = c("brand", "category1", "category2", "category3")),
                promo_by_mnfc1, by = c("manufacturer", "category1")),
                promo_by_mnfc2, by = c("manufacturer", "category2")),
                promo_by_mnfc3, by = c("manufacturer", "category3")),
                promo_by_mnfc123, by = c("manufacturer", "category1", "category2", "category3")),
                promo_by_brnmnfc123, by = c("manufacturer", "brand", "category1", "category2", "category3")) %>%
      replace_na(list(npromoBM = 0, npromoBC1 = 0, npromoBC2 = 0, npromoBC3 = 0, 
                      npromoBC123 = 0, npromoMC1= 0, npromoMC2 = 0,
                      npromoMC3 = 0, npromoMC123 = 0, npromoBMC123 = 0))
    
    rm(promo_by_bnd_mnf, promo_by_bndc1, promo_by_bndc2, promo_by_bndc3,
       promo_by_bndc123, promo_by_mnfc1, promo_by_mnfc2, promo_by_mnfc3, promo_by_mnfc123,
       promo_by_brnmnfc123)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
    
    
    promo_by_week = promo %>% group_by(week) %>%
              summarise(npromoW = sum(promo)) %>% as.data.frame()
    promo_by_week_item = promo %>% group_by(itemID, week) %>%
              summarise(npromoIW = sum(promo)) %>% as.data.frame()
    promo_by_week_brand = promo %>% group_by(brand, week) %>%
             summarise(npromoBW = sum(promo)) %>% as.data.frame()
    promo_by_week_manuf = promo %>% group_by(manufacturer, week) %>%
              summarise(npromoMW = sum(promo)) %>% as.data.frame()
    promo_by_week_cat1 = promo %>% group_by(category1, week) %>%
              summarise(npromoC1W = sum(promo)) %>% as.data.frame()
    promo_by_week_cat2 = promo %>% group_by(category2, week) %>%
              summarise(npromoC2W = sum(promo)) %>% as.data.frame()
    promo_by_week_cat3 = promo %>% group_by(category3, week) %>%
              summarise(npromoC3W = sum(promo)) %>% as.data.frame()
    promo_by_week_cat1_cat3 = promo %>% group_by(category1, category2, category3, week) %>%
              summarise(npromoC123W = sum(promo)) %>% as.data.frame()
    
    
    orders = left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(orders, 
              promo_by_week, by = c("week")), 
              promo_by_week_item, by = c("itemID", "week")), 
              promo_by_week_brand, by = c("brand", "week")), 
              promo_by_week_manuf, by = c("manufacturer", "week")),
              promo_by_week_cat1, by = c("category1", "week")),
              promo_by_week_cat2, by = c("category2", "week")),
              promo_by_week_cat3, by = c("category3", "week")),
              promo_by_week_cat1_cat3, by = c("category1", "category2", "category3", "week")) %>%
      replace_na(list(npromoIW = 0, npromoBW = 0, npromoMW = 0, npromoBC3 = 0, 
                      npromoC1W = 0, npromoC2W= 0, npromoC3W = 0,
                      npromoC123W = 0))
    
    rm(promo_by_week, promo_by_week_item, promo_by_week_brand, promo_by_week_manuf,
       promo_by_week_cat1, promo_by_week_cat2, promo_by_week_cat3, promo_by_week_cat1_cat3)
    
    # colSums(is.na(orders1[orders1$tr_ts<3,]))
    
    
    
    promo_by_week_bnd_mnf = promo %>% group_by(brand, manufacturer, week) %>%
              summarise(npromoBMW = sum(promo)) %>% as.data.frame()
    promo_by_week_bnd_cat1 = promo %>% group_by(brand, category1, week) %>%
              summarise(npromoBC1W = sum(promo)) %>% as.data.frame()
    promo_by_week_bnd_cat2 = promo %>% group_by(brand, category2, week) %>%
              summarise(npromoBC2W = sum(promo)) %>% as.data.frame()
    promo_by_week_bnd_cat3 = promo %>% group_by(brand, category3, week) %>%
              summarise(npromoBC3W = sum(promo)) %>% as.data.frame()
    promo_by_week_bnd_cat123 = promo %>% group_by(brand, category1, category2, category3, week) %>%
              summarise(npromoBC123W = sum(promo)) %>% as.data.frame()
    promo_by_week_muf_cat1 = promo %>% group_by(manufacturer, category1, week) %>%
              summarise(npromoMC1W = sum(promo)) %>% as.data.frame()
    promo_by_week_muf_cat2 = promo %>% group_by(manufacturer, category2, week) %>%
              summarise(npromoMC2W = sum(promo)) %>% as.data.frame()
    promo_by_week_muf_cat3 = promo %>% group_by(manufacturer, category3, week) %>%
              summarise(npromoMC3W = sum(promo)) %>% as.data.frame()
    promo_by_week_muf_cat123 = promo %>% group_by(manufacturer, category1, category2, category3, week) %>%
              summarise(npromoMC123W = sum(promo)) %>% as.data.frame()
    promo_by_week_muf_brn_cat123 = promo %>% group_by(manufacturer, brand, category1, category2, category3, week) %>%
      summarise(npromoBMC123W = sum(promo)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(orders, 
                  promo_by_week_bnd_mnf, by = c("brand", "manufacturer", "week")), 
                  promo_by_week_bnd_cat1, by = c("brand", "category1", "week")), 
                  promo_by_week_bnd_cat2, by = c("brand", "category2", "week")), 
                  promo_by_week_bnd_cat3, by = c("brand", "category3", "week")),
                  promo_by_week_bnd_cat123, by = c("brand", "category1", "category2", "category3", "week")),
                  promo_by_week_muf_cat1, by = c("manufacturer", "category1", "week")),
                  promo_by_week_muf_cat2, by = c("manufacturer", "category2", "week")),
                  promo_by_week_muf_cat3, by = c("manufacturer", "category3", "week")),
                  promo_by_week_muf_cat123, by = c("manufacturer", "category1", "category2", "category3", "week")),
                  promo_by_week_muf_brn_cat123, by = c("manufacturer", "brand", "category1", "category2", "category3", "week")) %>%
      replace_na(list(npromoBMW = 0, npromoBC1W = 0, npromoBC2W = 0, npromoBC3W = 0, 
                      npromoBC123W = 0, npromoMC1W= 0, npromoMC2W = 0,
                      npromoMC3W = 0, npromoMC123W = 0, npromoBMC123W = 0))
    
    rm(promo_by_week_bnd_mnf, promo_by_week_bnd_cat1, promo_by_week_bnd_cat2, 
       promo_by_week_bnd_cat3, promo_by_week_bnd_cat123, promo_by_week_muf_cat1, 
       promo_by_week_muf_cat2, promo_by_week_muf_cat3, promo_by_week_muf_cat123,
       promo_by_week_muf_brn_cat123)
    
    # colSums(is.na(orders))
    
    
    
    #########################################
    ## Orders Data 2 weeks
    #########################################
    
    mean_op_by_itm = orders2w %>% group_by(itemID) %>%
              summarise(morderI = mean(order), mpriceI = mean(salesPrice))  %>% as.data.frame()
    mean_op_by_bnd = orders2w %>% group_by(brand) %>%
              summarise(morderB = mean(order), mpriceB = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_mnf = orders2w %>% group_by(manufacturer) %>%
              summarise(morderM = mean(order), mpriceM = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_cat1 = orders2w %>% group_by(category1) %>%
              summarise(morderC1 = mean(order), mpriceC1 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_cat2 = orders2w %>% group_by(category2) %>%
              summarise(morderC2 = mean(order), mpriceC2 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_cat3 = orders2w %>% group_by(category3) %>%
              summarise(morderC3 = mean(order), mpriceC3 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_cat123 = orders2w %>% group_by(category1, category2, category3) %>%
              summarise(morderC123 = mean(order), mpriceC123 = mean(salesPrice)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(left_join(left_join(orders, 
                mean_op_by_itm, by = c("itemID")), 
                mean_op_by_bnd, by = c("brand")), 
                mean_op_by_mnf, by = c("manufacturer")), 
                mean_op_by_cat1, by = c("category1")),
                mean_op_by_cat2, by = c("category2")),
                mean_op_by_cat3, by = c("category3")),
                mean_op_by_cat123, by = c("category1", "category2", "category3"))
    
    rm(mean_op_by_itm, mean_op_by_bnd, mean_op_by_mnf, mean_op_by_cat1,
       mean_op_by_cat2, mean_op_by_cat3, mean_op_by_cat123)
    
     # colSums(is.na(orders))
    
    
    
    mean_op_by_bndw = orders2w %>% group_by(brand, week) %>%
              summarise(morderBW = mean(order), mpriceBW = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_mnfw = orders2w %>% group_by(manufacturer, week) %>%
              summarise(morderMW = mean(order), mpriceMW = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_cat1w = orders2w %>% group_by(category1, week) %>%
              summarise(morderC1W = mean(order), mpriceC1W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_cat2w = orders2w %>% group_by(category2, week) %>%
              summarise(morderC2W = mean(order), mpriceC2W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_cat3w = orders2w %>% group_by(category3, week) %>%
              summarise(morderC3W = mean(order), mpriceC3W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_cat123w = orders2w %>% group_by(category1, category2, category3, week) %>%
              summarise(morderC123W = mean(order), mpriceC123W = mean(salesPrice)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(left_join(orders, 
              mean_op_by_bndw, by = c("brand", "week")), 
              mean_op_by_mnfw, by = c("manufacturer", "week")), 
              mean_op_by_cat1w, by = c("category1", "week")), 
              mean_op_by_cat2w, by = c("category2", "week")),
              mean_op_by_cat3w, by = c("category3", "week")),
              mean_op_by_cat123w, by = c("category1", "category2", "category3", "week")) %>%
              arrange(itemID, week) %>%
              mutate(morderBW = lag(morderBW, n = 1L),
                     mpriceBW = lag(mpriceBW, n = 1L),
                     morderMW = lag(morderMW, n = 1L),
                     mpriceMW = lag(mpriceMW, n = 1L),
                     morderC1W = lag(morderC1W, n = 1L),
                     mpriceC1W = lag(mpriceC1W, n = 1L),
                     morderC2W = lag(morderC2W, n = 1L),
                     mpriceC2W = lag(mpriceC2W, n = 1L),
                     morderC3W = lag(morderC3W, n = 1L),
                     mpriceC3W = lag(mpriceC3W, n = 1L),
                     morderC123W = lag(morderC123W, n = 1L),
                     mpriceC123W = lag(mpriceC123W, n = 1L)) %>% 
      replace_na(list(morderBW = 0,
                       mpriceBW = 0,
                       morderMW = 0,
                       mpriceMW = 0,
                       morderC1W = 0,
                       mpriceC1W = 0,
                       morderC2W = 0,
                       mpriceC2W = 0,
                       morderC3W = 0,
                       mpriceC3W = 0,
                       morderC123W = 0,
                       mpriceC123W = 0))
    
    rm(mean_op_by_bndw, mean_op_by_mnfw, mean_op_by_cat1w, mean_op_by_cat2w,
       mean_op_by_cat3w, mean_op_by_cat123w)
    
    # colSums(is.na(orders1[orders1$tr_ts<3,]))
    
    
    mean_op_by_bndmnf = orders2w %>% group_by(brand, manufacturer) %>%
              summarise(morderBM = mean(order), mpriceBM = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_bndc1 = orders2w %>% group_by(brand, category1) %>%
              summarise(morderBC1 = mean(order), mpriceBC1 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_bndc2 = orders2w %>% group_by(brand, category2) %>%
              summarise(morderBC2 = mean(order), mpriceBC2 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_bndc3 = orders2w %>% group_by(brand, category3) %>%
              summarise(morderBC3 = mean(order), mpriceBC3 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_bndc123 = orders2w %>% group_by(brand, category1, category2, category3) %>%
              summarise(morderBC123 = mean(order), mpriceBC123 = mean(salesPrice)) %>% as.data.frame()
    
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
              mean_op_by_bndmnf, by = c("brand", "manufacturer")), 
              mean_op_by_bndc1, by = c("brand", "category1")), 
              mean_op_by_bndc2, by = c("brand", "category2")), 
              mean_op_by_bndc3, by = c("brand", "category3")),
              mean_op_by_bndc123, by = c("brand", "category1", "category2", "category3"))
    
    rm(mean_op_by_bndmnf, mean_op_by_bndc1, mean_op_by_bndc2, mean_op_by_bndc3,
       mean_op_by_bndc123)
    
    # colSums(is.na(orders1[orders1$tr_ts<3,]))
    
    
    
    mean_op_by_bndmnfw = orders2w %>% group_by(brand, manufacturer, week) %>%
      summarise(morderBMW = mean(order), mpriceBMW = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_bndc1w = orders2w %>% group_by(brand, category1, week) %>%
      summarise(morderBC1W = mean(order), mpriceBC1W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_bndc2w = orders2w %>% group_by(brand, category2, week) %>%
      summarise(morderBC2W = mean(order), mpriceBC2W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_bndc3w = orders2w %>% group_by(brand, category3, week) %>%
      summarise(morderBC3W = mean(order), mpriceBC3W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_bndc123w = orders2w %>% group_by(brand, category1, category2, category3, week) %>%
      summarise(morderBC123W = mean(order), mpriceBC123W = mean(salesPrice)) %>% as.data.frame()
    
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
                mean_op_by_bndmnfw, by = c("brand", "manufacturer", "week")), 
                mean_op_by_bndc1w, by = c("brand", "category1", "week")), 
                mean_op_by_bndc2w, by = c("brand", "category2", "week")), 
                mean_op_by_bndc3w, by = c("brand", "category3", "week")),
                mean_op_by_bndc123w, by = c("brand", "category1", "category2", "category3", "week")) %>%
      arrange(itemID, week) %>%
      mutate(morderBMW = lag(morderBMW, n = 1L),
             mpriceBMW = lag(mpriceBMW, n = 1L),
             morderBC1W = lag(morderBC1W, n = 1L),
             mpriceBC1W = lag(mpriceBC1W, n = 1L),
             morderBC2W = lag(morderBC2W, n = 1L),
             mpriceBC2W = lag(mpriceBC2W, n = 1L),
             morderBC3W = lag(morderBC3W, n = 1L),
             mpriceBC3W = lag(mpriceBC3W, n = 1L),
             morderBC123W = lag(morderBC123W, n = 1L),
             mpriceBC123W = lag(mpriceBC123W, n = 1L)) %>% 
      replace_na(list(morderBMW = 0,
                      mpriceBMW = 0,
                      morderBC1W = 0,
                      mpriceBC1W = 0,
                      morderBC2W = 0,
                      mpriceBC2W = 0,
                      morderBC3W = 0,
                      mpriceBC3W = 0,
                      morderBC123W = 0,
                      mpriceBC123W = 0))
    
    rm(mean_op_by_bndmnfw, mean_op_by_bndc1w, mean_op_by_bndc2w, mean_op_by_bndc3w,
       mean_op_by_bndc123w)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
    
    
    mean_op_by_mnfc1 = orders2w %>% group_by(manufacturer, category1) %>%
              summarise(morderMC1 = mean(order), mpriceMC1 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_mnfc2 = orders2w %>% group_by(manufacturer, category2) %>%
              summarise(morderMC2 = mean(order), mpriceMC2 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_mnfc3 = orders2w %>% group_by(manufacturer, category3) %>%
              summarise(morderMC3 = mean(order), mpriceMC3 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_mnfc123 = orders2w %>% group_by(manufacturer, category1, category2, category3) %>%
              summarise(morderMC123 = mean(order), mpriceMC123 = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_brnmnfc123 = orders2w %>% group_by(manufacturer, brand, category1, category2, category3) %>%
      summarise(morderBMC123 = mean(order), mpriceBMC123 = mean(salesPrice)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
                mean_op_by_mnfc1, by = c("manufacturer", "category1")), 
                mean_op_by_mnfc2, by = c("manufacturer", "category2")), 
                mean_op_by_mnfc3, by = c("manufacturer", "category3")), 
                mean_op_by_mnfc123, by = c("manufacturer", "category1", "category2", "category3")), 
                mean_op_by_brnmnfc123, by = c("manufacturer", "brand", "category1", "category2", "category3"))
    
    rm(mean_op_by_mnfc1, mean_op_by_mnfc2, mean_op_by_mnfc3, mean_op_by_mnfc123,
       mean_op_by_brnmnfc123)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
    
    
    mean_op_by_mnfc1w = orders2w %>% group_by(manufacturer, category1, week) %>%
              summarise(morderMC1W = mean(order), mpriceMC1W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_mnfc2w = orders2w %>% group_by(manufacturer, category2, week) %>%
              summarise(morderMC2W = mean(order), mpriceMC2W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_mnfc3w = orders2w %>% group_by(manufacturer, category3, week) %>%
              summarise(morderMC3W = mean(order), mpriceMC3W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_mnfc123w = orders2w %>% group_by(manufacturer, category1, category2, category3, week) %>%
              summarise(morderMC123W = mean(order), mpriceMC123W = mean(salesPrice)) %>% as.data.frame()
    mean_op_by_brnmnfc123w = orders2w %>% group_by(manufacturer, brand, category1, category2, category3, week) %>%
              summarise(morderBMC123W = mean(order), mpriceBMC123W = mean(salesPrice)) %>% as.data.frame()
    
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
                mean_op_by_mnfc1w, by = c("manufacturer", "category1", "week")), 
                mean_op_by_mnfc2w, by = c("manufacturer", "category2", "week")), 
                mean_op_by_mnfc3w, by = c("manufacturer", "category3", "week")), 
                mean_op_by_mnfc123w, by = c("manufacturer", "category1", "category2", "category3", "week")), 
                mean_op_by_brnmnfc123w, by = c("manufacturer", "brand", "category1", "category2", "category3", "week")) %>%
      arrange(itemID, week) %>%
      mutate(morderBMC123W = lag(morderBMC123W, n = 1L),
             mpriceBMC123W = lag(mpriceBMC123W, n = 1L),
             morderMC1W = lag(morderMC1W, n = 1L),
             mpriceMC1W = lag(mpriceMC1W, n = 1L),
             morderMC2W = lag(morderMC2W, n = 1L),
             mpriceMC2W = lag(mpriceMC2W, n = 1L),
             morderMC3W = lag(morderMC3W, n = 1L),
             mpriceMC3W = lag(mpriceMC3W, n = 1L),
             morderMC123W = lag(morderMC123W, n = 1L),
             mpriceMC123W = lag(mpriceMC123W, n = 1L)) %>% 
      replace_na(list(mpriceBMC123W = 0,
                      morderBMC123W = 0,
                      morderMC1W = 0,
                      mpriceMC1W = 0,
                      morderMC2W = 0,
                      mpriceMC2W = 0,
                      morderMC3W = 0,
                      mpriceMC3W = 0,
                      morderMC123W = 0,
                      mpriceMC123W = 0))
    
    rm(mean_op_by_mnfc1w, mean_op_by_mnfc2w, mean_op_by_mnfc3w, mean_op_by_mnfc123w,
       mean_op_by_brnmnfc123w)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
    
    ###########################################
    
    
    
    ############################################
    # time difference and Number of Customer
    ############################################
    Experiment = expand.grid(list(itemID = sort(unique(orders2w$itemID)),
                                  date = seq(as.Date("2018-01-01"), as.Date("2018-06-29"), "days")))
    
    ncust = left_join(left_join(left_join(Experiment, gen_time_diff_ncust(path), by = c("itemID","date")) %>% 
              replace_na(list(ncustomer = 0)) %>% 
              mutate(time_diff = na.locf(time_diff, na.rm = F, fromLast = T),
                     time_diff = na.locf(time_diff, na.rm = F, fromLast = F),
                     time_ncust = ncustomer*time_diff), week, by = "date"),
              items, by = "itemID")
    rm(Experiment)
    
    tmncus_by_itm = ncust %>% group_by(itemID) %>%
            summarise(mtmmedfI = mean(time_diff), mncusI = mean(ncustomer), mtmncusI=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_week = ncust %>% group_by(week) %>%
            summarise(mtmmedfW = mean(time_diff), mncusW = mean(ncustomer), mtmncusW=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_bnd = ncust %>% group_by(brand) %>%
            summarise(mtmmedfB = mean(time_diff), mncusB = mean(ncustomer), mtmncusB=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_mnf = ncust %>% group_by(manufacturer) %>%
            summarise(mtmmedfM = mean(time_diff), mncusM = mean(ncustomer), mtmncusM=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_cat1 = ncust %>% group_by(category1) %>%
            summarise(mtmmedfC1 = mean(time_diff), mncusC1 = mean(ncustomer), mtmncusC1=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_cat2 = ncust %>% group_by(category2) %>%
            summarise(mtmmedfC2 = mean(time_diff), mncusC2 = mean(ncustomer), mtmncusC2=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_cat3 = ncust %>% group_by(category3) %>%
            summarise(mtmmedfC3 = mean(time_diff), mncusC3 = mean(ncustomer), mtmncusC3=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_cat123 = ncust %>% group_by(category1, category2, category3) %>%
            summarise(mtmmedfC123 = mean(time_diff), mncusC123 = mean(ncustomer), mtmncusC123=mean(time_ncust)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(left_join(left_join(left_join(orders, 
                tmncus_by_itm, by = c("itemID")), 
                tmncus_by_week, by = c("week")), 
                tmncus_by_bnd, by = c("brand")), 
                tmncus_by_mnf, by = c("manufacturer")), 
                tmncus_by_cat1, by = c("category1")), 
                tmncus_by_cat2, by = c("category2")), 
                tmncus_by_cat3, by = c("category3")), 
                tmncus_by_cat123, by = c("category1", "category2", "category3")) %>%
      arrange(itemID, week) %>%
      mutate(mtmmedfW =  lag(mtmmedfW, n = 1L),
             mncusW = lag(mncusW, n = 1L),
             mtmncusW = lag(mtmncusW, n = 1L)) %>% 
      replace_na(list(mtmmedfW = 0,
                      mncusW = 0,
                      mtmncusW = 0))
    
    rm(tmncus_by_itm, tmncus_by_week, tmncus_by_bnd, tmncus_by_mnf,
       tmncus_by_cat1, tmncus_by_cat2, tmncus_by_cat3, tmncus_by_cat123)
    
    # colSums(is.na(orders1[orders1$tr_ts<3,]))
    
    
    
    tmncus_by_itmwk = ncust %>% group_by(itemID, week) %>%
            summarise(mtmmedfIW = mean(time_diff), mncusIW = mean(ncustomer), mtmncusIW=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_bndwk = ncust %>% group_by(brand, week) %>%
            summarise(mtmmedfBW = mean(time_diff), mncusBW = mean(ncustomer), mtmncusBW=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_bndmnf = ncust %>% group_by(brand, manufacturer) %>%
            summarise(mtmmedfBM = mean(time_diff), mncusBM = mean(ncustomer), mtmncusBM=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_bndmnfwk = ncust %>% group_by(brand, manufacturer, week) %>%
            summarise(mtmmedfBMW = mean(time_diff), mncusBMW = mean(ncustomer), mtmncusBMW=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_mnfwk = ncust %>% group_by(manufacturer, week) %>%
            summarise(mtmmedfMW = mean(time_diff), mncusMW = mean(ncustomer), mtmncusMW=mean(time_ncust)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
                tmncus_by_itmwk, by = c("itemID", "week")), 
                tmncus_by_bndwk, by = c("brand", "week")), 
                tmncus_by_bndmnf, by = c("brand", "manufacturer")), 
                tmncus_by_bndmnfwk, by = c("brand", "manufacturer", "week")), 
                tmncus_by_mnfwk, by = c("manufacturer", "week")) %>%
      arrange(itemID, week) %>%
      mutate(mtmmedfIW =  lag(mtmmedfIW, n = 1L),
             mncusIW = lag(mncusIW, n = 1L),
             mtmncusIW = lag(mtmncusIW, n = 1L),
             mtmmedfBW = lag(mtmmedfBW, n = 1L),
             mncusBW = lag(mncusBW, n = 1L),
             mtmncusBW = lag(mtmncusBW, n = 1L),
             mtmmedfBMW = lag(mtmmedfBMW, n = 1L),
             mncusBMW = lag(mncusBMW, n = 1L),
             mtmncusBMW = lag(mtmncusBMW, n = 1L),
             mtmmedfMW = lag(mtmmedfMW, n = 1L),
             mncusMW  = lag(mncusMW , n = 1L),
             mtmncusMW = lag(mtmncusMW, n = 1L)) %>% 
      replace_na(list(mtmmedfIW =  0,
                      mncusIW = 0,
                      mtmncusIW = 0,
                      mtmmedfBW = 0,
                      mncusBW = 0,
                      mtmncusBW = 0,
                      mtmmedfBMW = 0,
                      mncusBMW = 0,
                      mtmncusBMW = 0,
                      mtmmedfMW = 0,
                      mncusMW  = 0,
                      mtmncusMW = 0))
    
    rm(tmncus_by_itmwk, tmncus_by_bndwk, tmncus_by_bndmnf, tmncus_by_bndmnfwk,
       tmncus_by_mnfwk)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
     
    
    tmncus_by_banc1 = ncust %>% group_by(brand, category1) %>%
            summarise(mtmmedfBC1 = mean(time_diff), mncusBC1 = mean(ncustomer), mtmncusBC1=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_banc2 = ncust %>% group_by(brand, category2) %>%
            summarise(mtmmedfBC2 = mean(time_diff), mncusBC2 = mean(ncustomer), mtmncusBC2=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_banc3 = ncust %>% group_by(brand, category3) %>%
            summarise(mtmmedfBC3 = mean(time_diff), mncusBC3 = mean(ncustomer), mtmncusBC3=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_banc123 = ncust %>% group_by(brand, category1, category2, category3) %>%
            summarise(mtmmedfBC123 = mean(time_diff), mncusBC123 = mean(ncustomer), mtmncusBC123=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_banmnfc123 = ncust %>% group_by(brand, manufacturer, category1, category2, category3) %>%
            summarise(mtmmedfBMC123 = mean(time_diff), mncusBMC123 = mean(ncustomer), mtmncusBMC123=mean(time_ncust)) %>% as.data.frame()
    
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
                tmncus_by_banc1, by = c("brand", "category1")), 
                tmncus_by_banc2, by = c("brand", "category2")), 
                tmncus_by_banc3, by = c("brand", "category3")), 
                tmncus_by_banc123, by = c("brand", "category1", "category2", "category3")), 
                tmncus_by_banmnfc123, by = c("brand", "manufacturer", "category1", "category2", "category3")) 
    
    rm(tmncus_by_banc1, tmncus_by_banc2, tmncus_by_banc3, tmncus_by_banc123,
       tmncus_by_banmnfc123)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
    
    
    tmncus_by_banc1wk = ncust %>% group_by(brand, category1, week) %>%
            summarise(mtmmedfBC1W = mean(time_diff), mncusBC1W = mean(ncustomer), mtmncusBC1W=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_banc2wk = ncust %>% group_by(brand, category2, week) %>%
            summarise(mtmmedfBC2W = mean(time_diff), mncusBC2W = mean(ncustomer), mtmncusBC2W=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_banc3wk = ncust %>% group_by(brand, category3, week) %>%
            summarise(mtmmedfBC3W = mean(time_diff), mncusBC3W = mean(ncustomer), mtmncusBC3W=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_banc123wk = ncust %>% group_by(brand, category1, category2, category3, week) %>%
            summarise(mtmmedfBC123W = mean(time_diff), mncusBC123W = mean(ncustomer), mtmncusBC123W=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_banmnfc123wk = ncust %>% group_by(brand, manufacturer, category1, category2, category3, week) %>%
            summarise(mtmmedfBMC123W = mean(time_diff), mncusBMC123W = mean(ncustomer), mtmncusBMC123W=mean(time_ncust)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(left_join(orders, 
                tmncus_by_banc1wk, by = c("brand", "category1", "week")), 
                tmncus_by_banc2wk, by = c("brand", "category2", "week")), 
                tmncus_by_banc3wk, by = c("brand", "category3", "week")), 
                tmncus_by_banc123wk, by = c("brand", "category1", "category2", "category3", "week")), 
                tmncus_by_banmnfc123wk, by = c("brand", "manufacturer", "category1", "category2", "category3", "week")) %>%
      arrange(itemID, week) %>%
      mutate(mtmmedfBMC123W =  lag(mtmmedfBMC123W, n = 1L),
             mncusBMC123W = lag(mncusBMC123W, n = 1L),
             mtmncusBMC123W = lag(mtmncusBMC123W, n = 1L),
             mtmmedfBC1W =  lag(mtmmedfBC1W, n = 1L),
             mncusBC1W = lag(mncusBC1W, n = 1L),
             mtmncusBC1W = lag(mtmncusBC1W, n = 1L),
             mtmmedfBC2W = lag(mtmmedfBC2W, n = 1L),
             mncusBC2W = lag(mncusBC2W, n = 1L),
             mtmncusBC2W = lag(mtmncusBC2W, n = 1L),
             mtmmedfBC3W = lag(mtmmedfBC3W, n = 1L),
             mncusBC3W = lag(mncusBC3W, n = 1L),
             mtmncusBC3W = lag(mtmncusBC3W, n = 1L),
             mtmmedfBC123W = lag(mtmmedfBC123W, n = 1L),
             mncusBC123W  = lag(mncusBC123W , n = 1L),
             mtmncusBC123W = lag(mtmncusBC123W, n = 1L)) %>% 
      replace_na(list(mtmmedfBMC123W =0,
                      mncusBMC123W = 0,
                      mtmncusBMC123W = 0,
                      mtmmedfBC1W =  0,
                      mncusBC1W = 0,
                      mtmncusBC1W = 0,
                      mtmmedfBC2W = 0,
                      mncusBC2W = 0,
                      mtmncusBC2W = 0,
                      mtmmedfBC3W = 0,
                      mncusBC3W = 0,
                      mtmncusBC3W = 0,
                      mtmmedfBC123W = 0,
                      mncusBC123W  = 0,
                      mtmncusBC123W = 0))
    
    rm(tmncus_by_banc1wk, tmncus_by_banc2wk, tmncus_by_banc3wk, tmncus_by_banc123wk,
       tmncus_by_banmnfc123wk)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
    
    
    tmncus_by_mnfc1 = ncust %>% group_by(manufacturer, category1) %>%
            summarise(mtmmedfMC1 = mean(time_diff), mncusMC1 = mean(ncustomer), mtmncusMC1=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_mnfc2 = ncust %>% group_by(manufacturer, category2) %>%
            summarise(mtmmedfMC2 = mean(time_diff), mncusMC2 = mean(ncustomer), mtmncusMC2=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_mnfc3 = ncust %>% group_by(manufacturer, category3) %>%
            summarise(mtmmedfMC3 = mean(time_diff), mncusMC3 = mean(ncustomer), mtmncusMC3=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_mnfc123 = ncust %>% group_by(manufacturer, category1, category2, category3) %>%
            summarise(mtmmedfMC123 = mean(time_diff), mncusMC123 = mean(ncustomer), mtmncusMC123=mean(time_ncust)) %>% as.data.frame()
    
    
    orders = left_join(left_join(left_join(left_join(orders, 
                tmncus_by_mnfc1, by = c("manufacturer", "category1")), 
                tmncus_by_mnfc2, by = c("manufacturer", "category2")), 
                tmncus_by_mnfc3, by = c("manufacturer", "category3")), 
                tmncus_by_mnfc123, by = c("manufacturer", "category1", "category2", "category3")) 
    
    rm(tmncus_by_mnfc1, tmncus_by_mnfc2, tmncus_by_mnfc3, tmncus_by_mnfc123)
    
    # colSums(is.na(orders[orders$tr_ts<3,]))
    
    
    
    tmncus_by_mnfc1wk = ncust %>% group_by(manufacturer, category1, week) %>%
            summarise(mtmmedfMC1W = mean(time_diff), mncusMC1W = mean(ncustomer), mtmncusMC1W=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_mnfc2wk = ncust %>% group_by(manufacturer, category2, week) %>%
            summarise(mtmmedfMC2W = mean(time_diff), mncusMC2W = mean(ncustomer), mtmncusMC2W=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_mnfc3wk = ncust %>% group_by(manufacturer, category3, week) %>%
            summarise(mtmmedfMC3W = mean(time_diff), mncusMC3W = mean(ncustomer), mtmncusMC3W=mean(time_ncust)) %>% as.data.frame()
    tmncus_by_mnfc123wk = ncust %>% group_by(manufacturer, category1, category2, category3, week) %>%
            summarise(mtmmedfMC123W = mean(time_diff), mncusMC123W = mean(ncustomer), mtmncusMC123W=mean(time_ncust)) %>% as.data.frame()
    
    orders = left_join(left_join(left_join(left_join(orders, 
                tmncus_by_mnfc1wk, by = c("manufacturer", "category1", "week")), 
                tmncus_by_mnfc2wk, by = c("manufacturer", "category2", "week")), 
                tmncus_by_mnfc3wk, by = c("manufacturer", "category3", "week")), 
                tmncus_by_mnfc123wk, by = c("manufacturer", "category1", "category2", "category3", "week")) %>%
      arrange(itemID, week) %>%
      mutate(mtmmedfMC1W =  lag(mtmmedfMC1W, n = 1L),
             mncusMC1W = lag(mncusMC1W, n = 1L),
             mtmncusMC1W = lag(mtmncusMC1W, n = 1L),
             mtmmedfMC2W = lag(mtmmedfMC2W, n = 1L),
             mncusMC2W = lag(mncusMC2W, n = 1L),
             mtmncusMC2W = lag(mtmncusMC2W, n = 1L),
             mtmmedfMC3W = lag(mtmmedfMC3W, n = 1L),
             mncusMC3W = lag(mncusMC3W, n = 1L),
             mtmncusMC3W = lag(mtmncusMC3W, n = 1L),
             mtmmedfMC123W = lag(mtmmedfMC123W, n = 1L),
             mncusMC123W  = lag(mncusMC123W , n = 1L),
             mtmncusMC123W = lag(mtmncusMC123W, n = 1L)) %>% 
      replace_na(list(mtmmedfMC1W =  0,
                      mncusMC1W = 0,
                      mtmncusMC1W = 0,
                      mtmmedfMC2W = 0,
                      mncusMC2W = 0,
                      mtmncusMC2W = 0,
                      mtmmedfMC3W = 0,
                      mncusMC3W = 0,
                      mtmncusMC3W = 0,
                      mtmmedfMC123W = 0,
                      mncusMC123W  = 0,
                      mtmncusMC123W = 0))
    
    rm(tmncus_by_mnfc1wk, tmncus_by_mnfc2wk, tmncus_by_mnfc3wk, tmncus_by_mnfc123wk)
    
    # colSums(is.na(orders1[orders1$tr_ts<3,]))
    
    ################################################
    # Lag Order
    ################################################
    
    orders = orders %>% arrange(itemID, week) %>% group_by(itemID) %>%
      mutate(lag1_order = lag(order, n = 1L),
             lag2_order = lag(order, n = 2L),
             lag3_order = lag(order, n = 3L),
             lag4_order = lag(order, n = 4L),
             lag5_order = lag(order, n = 5L),
             lag6_order = lag(order, n = 6L)) %>%
      replace_na(list(lag1_order = 0, lag2_order = 0, lag3_order = 0, 
                      lag4_order = 0, lag5_order = 0, lag6_order = 0)) %>% ungroup()
    
    ###################################################
    
    ########################################
    ## comp_supp_features
    #########################################
    comp_supp_features = read.csv(paste(path, "comp_supp_features.csv", sep = ""), header = TRUE, sep = ",")[,-1]
    orders = left_join(orders, comp_supp_features, by = "itemID")
    
    nname = c("comp10",                 "comp9",                 
              "comp8",                  "comp7",                  "comp6",                 
              "comp5",                  "comp4",                  "comp3",                 
              "comp2",                  "comp1",                  "supp10",               
              "supp9",                  "supp8",                  "supp7",                 
              "supp6",                  "supp5",                  "supp4",                 
              "supp3",                  "supp2",                  "supp1")
    
    for (i in c(1:length(nname))){
      orders[,paste(nname[i],"_order", sep = "")]= comp_supp_func(dt = orders,
                                                                  var_name = nname[i])
    }
    
    promo_by_item = promo %>% group_by(itemID) %>%
              summarise(npromoI = sum(promo)) %>% as.data.frame()
    tmncus_by_itm = ncust %>% group_by(itemID) %>%
              summarise(mncusI = mean(ncustomer)) %>% as.data.frame()
    mean_op_by_itm = orders2w %>% group_by(itemID) %>%
              summarise(morderI = mean(order))  %>% as.data.frame()
    promo_by_week_item = promo %>% group_by(itemID, week) %>%
              summarise(npromoIW = sum(promo)) %>% as.data.frame()
    
    for (i in c(1:length(nname))){
      orders = left_join(left_join(left_join(left_join(orders, 
                          promo_by_item %>% 'colnames<-'(c(nname[i], paste(nname[i],"_npromoI", sep = ""))), by = c(nname[i])),
                          tmncus_by_itm %>% 'colnames<-'(c(nname[i], paste(nname[i],"_mncusI", sep = ""))), by = c(nname[i])),
                          mean_op_by_itm %>% 'colnames<-'(c(nname[i], paste(nname[i],"_morderI", sep = ""))), by = c(nname[i])),
                          promo_by_week_item %>% 'colnames<-'(c(nname[i], "week", paste(nname[i],"_npromoIW", sep = ""))), by = c(nname[i], "week")) 
      orders[is.na(orders[,paste(nname[i],"_npromoI", sep = "")]), paste(nname[i],"_npromoI", sep = "")] = 0
      orders[is.na(orders[,paste(nname[i],"_npromoIW", sep = "")]), paste(nname[i],"_npromoIW", sep = "")] = 0
                        
    }
    
    orders = orders %>% select(-c(nname))
    rm(promo_by_item, tmncus_by_itm, mean_op_by_itm, promo_by_week_item)
    rm(items, ncust, comp_supp_features, promo, orders2w, week)
    
    #colSums(is.na(orders[orders$tr_ts<3,]))
    ###########################################
    
    aa = data.frame(order = cor(orders[orders$tr_ts==1,])[,3])
    bb = rownames(aa)[which((abs(aa$order)>=.1) & (abs(aa$order)<1))]
    orders1 = orders %>% select(c(names(orders)[1:13], bb))
    tr_dt = orders1 %>% filter(tr_ts ==1) %>% mutate(logorder = log(order+1),
                                                     invorder = 1/(order+1))
    imp_var = c("customerRating", "recommendedRetailPrice", "new_price" ,bb)
    mean_tr_dt = tr_dt %>% select(imp_var) %>% 
                          summarise_all(mean) %>% as.data.frame()
    sd_tr_dt = tr_dt %>% select(imp_var) %>% 
                          summarise_all(sd) %>% as.data.frame()
    tr_dt = tr_dt %>% mutate_at(.vars = imp_var,
                                ~(scale(.) %>% as.vector))
    
    ts_dt =  orders1 %>% filter(tr_ts >1)
    ts_dt[,imp_var] = scale(ts_dt[,imp_var], mean_tr_dt, sd_tr_dt)
    rm(orders, orders1, mean_tr_dt, sd_tr_dt)
    
    return(list(train_dt = tr_dt, test_dt = ts_dt, imp_variable = imp_var))
}

path = "D:/ISU/DMC/DMC 2020/Data/DMC-2020-Task/DMC20_Data/"

df = data_prep(path)
train_dt = df$train_dt %>% as.data.frame()
test_dt = df$test_dt %>% as.data.frame()
imp_variable = df$imp_variable

write.csv(x = train_dt, file = paste(path, "train_dt.csv", sep = ""), row.names = F)
write.csv(x = test_dt, file = paste(path, "test_dt.csv", sep = ""), row.names = F)
write.csv(x = imp_variable, file = paste(path, "imp_variable.csv", sep = ""), row.names = F)

train_dt = read.csv(file = paste(path, "train_dt.csv", sep = ""), header = T)
test_dt = read.csv(file = paste(path, "test.csv", sep = ""), header = T)
imp_variable = read.csv(file = paste(path, "imp_variable.csv", sep = ""), header = T)


library(corrplot)
aa = data.frame(order = cor(train_dt)[,3])
bb = rownames(aa)[which((abs(aa$order)>=.2) & (abs(aa$order)<1))]
cr = cor(train_dt[,c("order", bb)])
corrplot(cr)

