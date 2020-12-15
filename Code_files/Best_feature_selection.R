library(party)
library(dplyr)
path = "/work/STAT/kanak/"

############# 1st set ###########################
Train = read.csv(file = paste(path, "train_dt.csv", sep = ""), header = T)
tr_dt = Train %>% filter(tr_ts ==1) %>% as.data.frame()
cf1 <- cforest(logorder ~ . , data= tr_dt %>% select(-c("tr_ts", "order", "salesPrice", "invorder")), 
               control=cforest_unbiased(mtry=0,ntree=50)) 
imp_var = sort(varimp(cf1), decreasing = T)
imp_var_con = sort(varimp(cf1, conditional=TRUE), decreasing = T)

print(imp_var)
print(imp_var_con)

write.csv(x = imp_var, file = paste(path, "imp_var.csv", sep = ""), row.names = F)
write.csv(x = imp_var_con, file = paste(path, "imp_var_conditional.csv", sep = ""), row.names = F)

nname = names(Train)[names(Train) != "logorder"]


##################### 2nd set ######################
Train = read.csv(file = paste(path, "orders_all_features.csv", sep = ""), header = T)
tr_dt = Train %>% filter(tr_ts ==1) %>% select(-nname)%>% as.data.frame()
nname1 = names(tr_dt)[names(tr_dt) != "logorder"]
tr_dt = tr_dt %>% select(c("logorder", nname1[1:100])) %>% as.data.frame()
cf1 <- cforest(logorder ~ . , data= tr_dt, 
               control=cforest_unbiased(mtry=0,ntree=50)) 
imp_var = sort(varimp(cf1), decreasing = T)
imp_var_con = sort(varimp(cf1, conditional=TRUE), decreasing = T)

print(imp_var)
print(imp_var_con)

write.csv(x = imp_var, file = paste(path, "imp_var_1.csv", sep = ""), row.names = F)
write.csv(x = imp_var_con, file = paste(path, "imp_var_conditional_1.csv", sep = ""), row.names = F)


############ 3rd set ##################################
tr_dt = Train %>% filter(tr_ts ==1) %>% select(-nname)%>% as.data.frame()
nname1 = names(tr_dt)[names(tr_dt) != "logorder"]
tr_dt = tr_dt %>% select(c("logorder", nname1[101:length(names(Train))])) %>% as.data.frame()
cf1 <- cforest(logorder ~ . , data= tr_dt, 
               control=cforest_unbiased(mtry=0,ntree=50)) 
imp_var = sort(varimp(cf1), decreasing = T)
imp_var_con = sort(varimp(cf1, conditional=TRUE), decreasing = T)

print(imp_var)
print(imp_var_con)

write.csv(x = imp_var, file = paste(path, "imp_var_2.csv", sep = ""), row.names = F)
write.csv(x = imp_var_con, file = paste(path, "imp_var_conditional_2.csv", sep = ""), row.names = F)
