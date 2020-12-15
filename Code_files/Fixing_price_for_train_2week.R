get_simulationPrice<-function(path, df = NULL){
  library(dplyr)
  # library(tidyr)
  # library(tidyverse)
  library(zoo)

  if(is.null(df)){
    df = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
    df$time = as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
    df$date <- as.Date(df$time , format = "%Y-%m-%d", tz = "GMT") 
  }
  
  # df = df %>% select(c("itemID", "date", "salesPrice"))
  n_obs = nrow(df)
  min_day = as.Date(min(df[,"date"]), format="%Y-%m-%d", tz = "GMT")
  mmax_day = as.Date(max(df[,"date"]), format="%Y-%m-%d", tz = "GMT")
  max_day = min_day
  
  date_var = data.frame(df[,c("itemID", "date", "salesPrice")])
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
  df$week = date_var$week
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

path = "D:/ISU/DMC/DMC 2020/Data/DMC-2020-Task/DMC20_Data/"

new_price = get_simulationPrice(path)
