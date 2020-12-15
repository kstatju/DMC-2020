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
  promo = promo %>% mutate(promo = ifelse(promo>=2,1,0)) %>% 
    select(c("itemID", "date", "promo")) %>% as.data.frame()
  
  
  
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
  infos1 = infos1 %>% mutate(order = NA, promo = ifelse((promo == 1) & (itemID %in% aa), 1,0),
                             tr_ts = 2) %>%
    rename(new_price = simulationPrice) %>% 
    select(c("itemID", "date", "order", "new_price", "promo", "tr_ts")) %>%
    as.data.frame()
  
  Experiment = expand.grid(list(itemID = infos$itemID[!(infos$itemID %in% sort(unique(orders$itemID)))], 
                                date = seq(as.Date("2018-06-30"), as.Date("2018-07-13"), "days")))
  notintrain = Experiment %>% mutate(order = 0, promo = 0, new_price = NA, tr_ts = 3) %>% 
    select(c("itemID", "date", "order", "new_price", "promo", "tr_ts")) %>% 
    bind_rows(infos1) %>% as.data.frame()
  
  rm(infos, infos1)
  
  return(list(tr_promo = promo, ts_promo = notintrain))
}


library(dplyr)
library(tidyr)
library(zoo)


promo = promotion(path)
tr_promo = promo$tr_promo
ts_promo = promo$ts_promo
rm(promo)

get_simulationPrice<-function(path, df = NULL){
  
  df = df %>% group_by(itemID, date) %>% mutate(order = sum(order),
                                                salesPrice = mean(salesPrice))
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
    mutate(new_price = na.locf(new_price, na.rm = F, fromLast = T))  
  
  return(date_var)
}


## Items Data
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
date_diff = orders %>% select(c("itemID", "date")) %>% 
                distinct() %>% arrange(itemID, (date)) %>% group_by(itemID) %>%
          mutate(time_diff = c(0, diff(date)))

ncustomer = orders %>% group_by(itemID, date) %>%
  summarise(ncustomer = length(unique(transactID)))
  

orders1 = orders %>% group_by(itemID, date) %>% summarise(order = sum(order), 
                                                          salesPrice = mean(salesPrice)) %>% 
            ungroup() 
orders1 = left_join(orders1, date_diff, by = c("itemID", "date"))
orders1 = left_join(orders1, ncustomer, by = c("itemID", "date"))

Experiment = expand.grid(list(itemID = sort(unique(orders$itemID)), 
                              date = seq(as.Date("2018-01-01"), as.Date("2018-06-29"), "days")))

orders1 = left_join(Experiment, orders1, by = c("itemID", "date")) %>% 
  replace_na(list(order = 0, ncustomer = 0, time_diff = 0)) %>% 
  arrange(itemID, date) %>% group_by(itemID) %>%
  mutate(salesPrice = na.locf(salesPrice, na.rm = F)) %>% 
  mutate(salesPrice = na.locf(salesPrice, na.rm = F, fromLast = T),
         tr_ts = 1) %>% ungroup()
orders1 = left_join(orders1, tr_promo, by = c("itemID", "date")) %>% 
                replace_na(list(promo = 0)) %>% 
          select(c("itemID", "date", "order", "salesPrice", "time_diff",
                   "ncustomer", "promo",  "tr_ts"))
orders1 = left_join(orders1, date_var, by = c("date"))
orders1 = left_join(orders1, get_simulationPrice(path, orders), by = c("itemID", "week")) %>% 
              select(c("itemID", "date", "order", "new_price", "time_diff",
                       "ncustomer", "promo",  "tr_ts", "week"))

rm(date_diff, Experiment, ncustomer)
ts_promo = ts_promo %>% mutate(time_diff = NA, ncustomer = NA) %>%
                select(c("itemID", "date", "order", "new_price", "time_diff",
                         "ncustomer", "promo",  "tr_ts"))
ts_promo = left_join(ts_promo, date_var, by = c("date"))

orders1 = orders1 %>% bind_rows(ts_promo)

## Items Data
items = read.csv(paste(path, "items.csv", sep = ""), header = TRUE, sep = "|")
orders1 = left_join(orders1, items, by = c("itemID"))

orders1 = left_join(orders1, orders1 %>% filter(tr_ts == 1) %>% group_by(brand) %>% 
            summarise(brand_mean = mean(order)), by = "brand")

orders1 = left_join(orders1, orders1 %>% filter(tr_ts == 1) %>% group_by(manufacturer) %>% 
            summarise(manufacturer_mean = mean(order)), by = "manufacturer")

orders1 = left_join(orders1, orders1 %>% filter(tr_ts == 1) %>% group_by(itemID) %>% 
            summarise(item_mean = mean(order)), by = "itemID")

orders1 = left_join(orders1, orders1 %>% filter(tr_ts == 1) %>% group_by(category1) %>% 
            summarise(category1_mean = mean(order)), by = "category1")

orders1 = left_join(orders1, orders1 %>% filter(tr_ts == 1) %>% group_by(category2) %>% 
            summarise(category2_mean = mean(order)), by = "category2")

orders1 = left_join(orders1, orders1 %>% filter(tr_ts == 1) %>% group_by(category3) %>% 
            summarise(category3_mean = mean(order)), by = "category3")

orders1 = orders1 %>% arrange(itemID, date) %>% group_by(itemID) %>%
            mutate(lag1_order = lag(order, n = 1L),
                   lag2_order = lag(order, n = 2L),
                   lag3_order = lag(order, n = 3L),
                   lag4_order = lag(order, n = 4L),
                   lag5_order = lag(order, n = 5L),
                   lag6_order = lag(order, n = 6L)) %>%
            replace_na(list(lag1_order = 0, lag2_order = 0, lag3_order = 0, 
                            lag4_order = 0, lag5_order = 0, lag6_order = 0)) %>% ungroup()


item_clusters = read.csv(paste(path, "item_clusters.csv", sep = ""), header = TRUE, sep = ",")[,-1]
manuf_clusters = read.csv(paste(path, "manuf_clusters.csv", sep = ""), header = TRUE, sep = ",")[,-1]
brand_clusters = read.csv(paste(path, "brand_clusters.csv", sep = ""), header = TRUE, sep = ",")[,-1]

category1 = read.csv(paste(path, "categories_culsters_FA.csv", sep = ""), header = TRUE, sep = ",") %>%
                select(c("category1", "new_category1")) %>% unique()
category2 = read.csv(paste(path, "categories_culsters_FA.csv", sep = ""), header = TRUE, sep = ",") %>%
  select(c("category2", "new_category2")) %>% unique()
category3 = read.csv(paste(path, "categories_culsters_FA.csv", sep = ""), header = TRUE, sep = ",") %>%
  select(c("category3", "new_category3")) %>% unique()



## comp_supp_features

comp_supp_func <- function(dt, var_name){
  df = left_join(dt, orders1 %>% select(c("itemID", "date", "order")), 
                 by = c(setNames("itemID", var_name), "date"))
  return(unlist(df[,3]))
}

nname = c("comp10",                 "comp9",                 
          "comp8",                  "comp7",                  "comp6",                 
          "comp5",                  "comp4",                  "comp3",                 
          "comp2",                  "comp1",                  "supp10",               
          "supp9",                  "supp8",                  "supp7",                 
          "supp6",                  "supp5",                  "supp4",                 
          "supp3",                  "supp2",                  "supp1")


for (i in c(1:length(nname))){
  orders1[,nname[i]]= comp_supp_func(orders1%>% select(nname[i], "date"), nname[i])
}

comp_supp_features = read.csv(paste(path, "comp_supp_features.csv", sep = ""), header = TRUE, sep = ",")[,-1]
orders1 = left_join(orders1, comp_supp_features, by = "itemID")


            mutate(comp101 = comp_supp_func(as.data.frame(bind_rows(comp10, date)), var_name))
