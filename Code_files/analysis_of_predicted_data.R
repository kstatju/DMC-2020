loss_func  = function(pred , orders, price){

  if (length(pred)==length(orders) & length(pred)==length(price)){
    err = price*(orders - pred)
    err[err>0] = err[err>0]*0.6
    return(mean(abs(err)))
  }else {
    print("Dimenstion of ordrs, price and predicted are not same")
    return()
  }

}

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)


path = "D:/ISU/DMC/DMC 2020/Code/knn/final_model/"
knn20 = read.csv(paste(path, "knn_pred_knn20.csv", sep = ""), header = T) %>% mutate(pred_knn20 = round(pred_knn20))
knn21 = read.csv(paste(path, "knn_pred_knn21.csv", sep = ""), header = T)%>% mutate(pred_knn21 = round(pred_knn21))
knn22 = read.csv(paste(path, "knn_pred_knn22.csv", sep = ""), header = T)%>% mutate(pred_knn22 = round(pred_knn22))
knn23 = read.csv(paste(path, "knn_pred_knn23.csv", sep = ""), header = T)%>% mutate(pred_knn23 = round(pred_knn23))
knn28 = read.csv(paste(path, "knn_pred_knn28.csv", sep = ""), header = T)%>% mutate(pred_knn28 = round(pred_knn28))

knn_pred = left_join(left_join(left_join(left_join(knn20, 
                  knn21 %>% select(c("itemID", "week", "pred_knn21")), by = c("itemID", "week")),
                  knn22 %>% select(c("itemID", "week", "pred_knn22")), by = c("itemID", "week")),
                  knn23 %>% select(c("itemID", "week", "pred_knn23")), by = c("itemID", "week")),
                  knn28 %>% select(c("itemID", "week", "pred_knn28")), by = c("itemID", "week")) 
knn_pred$knn_pred_mean = rowMeans(knn_pred[,13:17])

rm(knn20, knn21, knn22, knn23, knn28)

# write.csv(knn_pred, paste(path, "knn_pred_top_5.csv", sep = ""), row.names = F)

knn_pred = left_join(knn_pred, knn_pred %>% filter(tr_ts %in% c(1,4)) %>% 
                    group_by(itemID) %>% summarise(mean_order = mean(order)) %>%
                    as.data.frame(), by = "itemID") %>% replace_na(list(mean_order = 0))

################# tr_ts
loss_trts = knn_pred %>% group_by(tr_ts) %>% summarise(loss_model20 = loss_func(pred = pred_knn20, orders = order, 
                                                                price = new_price),
                                                       loss_model21 = loss_func(pred = pred_knn21, orders = order, 
                                                                                price = new_price),
                                                       loss_model22 = loss_func(pred = pred_knn22, orders = order, 
                                                                                price = new_price),
                                                       loss_model23 = loss_func(pred = pred_knn23, orders = order, 
                                                                                price = new_price),
                                                       loss_model28 = loss_func(pred = pred_knn28, orders = order, 
                                                                                price = new_price),
                                                       loss_model_mean = loss_func(pred = knn_pred_mean, orders = order, 
                                                                                price = new_price),
                                                    loss_mean = loss_func(pred = mean_order, orders = order, 
                                                                          price = new_price)) %>% as.data.frame() %>%
                              melt(id.vars = "tr_ts", value.name = "loss")

ggplot(data = loss_trts )+aes(x = tr_ts, y = loss, color = variable) + 
  geom_line(aes(linetype = variable), size = 1)

ggplot(data = loss_trts %>% filter(variable != "loss_mean"))+aes(x = tr_ts, y = loss, color = variable) + 
  geom_point(aes(linetype = variable), size = 1)

############## item #################
loss_trts_item = knn20 %>% group_by(tr_ts, itemID) %>% summarise(loss = loss_func(pred = pred_knn20, orders = order, 
                                                                price = new_price)) %>% as.data.frame()

ggplot(data = loss_trts_item %>% filter(tr_ts %in% c(1,4)))+aes(x = itemID, y = loss, color = as.factor(tr_ts)) + 
  geom_point()

ggplot(data = loss_trts_item )+aes(x = itemID, y = loss, color = as.factor(tr_ts)) + 
  geom_point()


############ Brand ################

loss_trts_bnd = dt %>% group_by(tr_ts, brand) %>% summarise(loss_model = loss_func(pred = pred_knn20, orders = order, 
                                                                                      price = new_price),
                                                               loss_mean = loss_func(pred = mean_order, orders = order, 
                                                                                     price = new_price)) %>% as.data.frame() %>%
  melt(id.vars = c("tr_ts", "brand"), value.name = "loss")

ggplot(data = loss_trts_bnd %>% filter(tr_ts %in% c(1)) )+aes(x = brand, y = loss, color = as.factor(tr_ts), 
                                                                lintype = variable) + 
  geom_line(aes(linetype = variable), size = 1)

ggplot(data = loss_trts_bnd %>% filter(tr_ts %in% c(4)) )+aes(x = brand, y = loss, color = as.factor(tr_ts), 
                                                              lintype = variable) + 
  geom_line(aes(linetype = variable), size = 1)

ggplot(data = loss_trts_bnd %>% filter((tr_ts %in% c(1,4)) & (variable =="loss_model")) )+aes(x = brand, y = loss, color = as.factor(tr_ts), 
                                                              lintype = variable) + 
  geom_line(aes(linetype = variable), size = 1)

ggplot(data = loss_trts_bnd)+
  geom_density(data=ddf, aes(x=MEI, group=Region, fill=Region),alpha=0.5, adjust=2) 

############

ggplot(data = knn20)+aes(x = log(order+1), y = log(pred_knn20+1), color = as.factor(tr_ts)) + 
  geom_point() + geom_abline(slope=1, intercept=0)

ggplot(data = knn20)+aes(x = log(order+1), y = log(pred_knn20+1), color = as.factor(tr_ts)) + 
  geom_point() + geom_abline(slope=1, intercept=0)

df <- tidyr::unite(knn20,"id_loc",tr_ts,brand,remove = F)
ggplot(data = knn20)+aes(x = log(order+1), y = log(pred_knn20+1), color = as.factor(tr_ts), size = brand) + 
  geom_point() + geom_abline(slope=1, intercept=0)








ggplot(data = dt, aes(x = order, y = mean_all_pred, color = as.factor(tr_ts))) + 
  geom_point() + geom_abline(slope=1, intercept=0)
