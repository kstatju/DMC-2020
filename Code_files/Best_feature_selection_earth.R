library(earth)
library(dplyr)
path = "/work/STAT/kanak/"

############# 1st set ###########################
Train = df$tr_ts_all_std %>% mutate(logorder = log(order+1))
Train = read.csv(file = paste(path, "train_dt.csv", sep = ""), header = T)
tr_dt = Train %>% filter(tr_ts ==1) %>% 
                select(-c("tr_ts", "order", "salesPrice")) %>% as.data.frame()
rm(Train)
cf1 <- earth(logorder ~ ., data= tr_dt) # build model
ev <- evimp (cf1)

print(ev)
write.csv(x = ev, file = paste(path, "imp_var_earth.csv", sep = ""), row.names = F)

rm(cf1, ev, tr_dt)

############ 4rd set ##################################
Train = read.csv(file = paste(path, "orders_all_features.csv", sep = ""), header = T)
tr_dt = Train %>% filter(tr_ts ==1) %>% mutate(logorder = log(order+1)) %>% 
            select(-c("tr_ts", "order", "salesPrice"))%>% as.data.frame()
rm(Train)
cf1 <- earth(logorder ~ ., data= tr_dt)
ev <- evimp (cf1)

print(ev)
write.csv(x = ev, file = paste(path, "imp_var_earth_all350.csv", sep = ""), row.names = F)

rm(cf1, ev, tr_dt)

