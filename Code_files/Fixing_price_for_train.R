get_simulationPrice<-function(df, date_time_var, price_var){
  # The test period is from 2018-06-30 till 2018-07-13
  # Breaking dates into 2 week periods
  n_obs = nrow(df)
  df[,date_time_var] = as.POSIXct(df[,date_time_var], format="%Y-%m-%d")
  min_day = as.POSIXlt(min(df[,date_time_var]), format="%Y-%m-%d")
  mmax_day = as.POSIXlt(max(df[,date_time_var]), format="%Y-%m-%d")
  max_day = min_day
  
  date_var = data.frame(df[,c("itemID", date_time_var, price_var)])
  # date_var[,date_time_var] = as.POSIXct(date_var[,date_time_var], format="%Y-%m-%d")
  date_var$week = NA
  i=1
  while (max_day < mmax_day){
    if (i==1){
      min_day$mday = min_day$mday - 1
      max_day$mday = max_day$mday + 4 
      date_var[(date_var$date > min_day) & (date_var$date <= max_day),"week"] = i
    }
    else{
      min_day = max_day
      max_day$mday = max_day$mday + 7
      date_var[(date_var$date > min_day) & (date_var$date <= max_day),"week"] = i
    }
    i = i+1
  }
  date_var = date_var %>%  distinct() %>% mutate(new_price = NA, 
                                                 two_week = week%/%2+week%%2)
  
  # uni_itm = sort(unique(date_var$itemID)) 
  # 
  # for (i in uni_itm){
  #   uni_week = sort(unique(date_var[date_var$itemID == i, "two_week"]))
  #   for (j in 1:length(uni_week)){
  #       if (j==1){
  #         subdt = date_var[(date_var$itemID == i) & (date_var$two_week == uni_week[j]) & (date_var[,price_var]>0),]
  #         date_var[(date_var$itemID == i) & (date_var$two_week == uni_week[j]), 
  #                  "new_price"] = subdt[which.min(subdt$date), price_var]
  #       }
  #       else{
  #         subdt = date_var[(date_var$itemID == i) & (date_var$two_week == uni_week[j-1]) & (date_var[,price_var]>0),]
  #         date_var[(date_var$itemID == i) & (date_var$two_week == uni_week[j]), 
  #                  "new_price"] = subdt[which.max(subdt$date), price_var]
  #       }
  #   }
  # }
  # 
  # ##########
  
  sel_var <- function(dt){
    dt = as.data.frame(dt)
    uni_week = sort(unique(dt[,"two_week"]))
    # print(uni_week)
    for (j in 1:length(uni_week)){
      if (j==1){
        subdt = dt[(dt$two_week == uni_week[j]) & (dt[,price_var]>0),]
        dt[(dt$two_week == uni_week[j]), 
           "new_price"] = subdt[which.min(subdt$date), price_var]
      }
      else{
        subdt = dt[(dt$two_week == uni_week[j-1]) & (dt[,price_var]>0),]
        dt[(dt$two_week == uni_week[j]), 
           "new_price"] = subdt[which.max(subdt$date), price_var]
      }
    }
    return(dt)
  }
  
  date_var = as.data.frame(date_var %>% group_by(itemID) %>% do(data.frame(sel_var(.)))) %>% 
    select((c("itemID", date_time_var, "new_price"))) %>% distinct()
  df = left_join(df, date_var, by = c("itemID", date_time_var))
  return(df)
}

date_time_var = "date"                  # Date variable name
price_var     = "salesPrice"            # price variable name

Train = get_simulationPrice(df = Train, date_time_var = date_time_var, price_var = price_var)