library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)


path = "D:/ISU/DMC/DMC 2020/Code/ensemble/2n_layer_ensemble/"

dt = read.csv(paste(path, "all_pred_combine_w13.csv", sep = ""), header = T)
dt$subset_mean = round(rowMeans(dt[,c("xgb6", "pred_NN1_nonlog", "pred_rf1", 
                                        "pred_rf5","pred_rf6" ,  "pred_rf7")])) 
dt$subset_mean1 = round(rowMeans(dt[,c("pred_NN1_nonlog", "pred_rf1", 
                                      "pred_rf5","pred_rf6" ,  "pred_rf7")]))

dt_final = dt %>%
              dplyr::filter(week == 14) %>% select(c("itemID", "subset_mean1")) %>% 
              rename( demandPrediction = subset_mean1) %>%
              mutate(demandPrediction = round(demandPrediction)) %>% as.data.frame()

write.table(dt_final, paste(path, "Uni_State_Iowa_1.csv", sep = ""), row.names = F,
          sep = "|", quote = F)

aa = read.csv(paste(path, "Uni_State_Iowa_1.csv", sep = ""), header = T, sep = "|")








# aa = cbind(dt %>% group_by(week) %>% summarise(a = sum(subset_mean)),
#            (dt %>% group_by(week) %>% summarise(b = round(sum(order))))[,2],
#            (dt %>% group_by(week) %>% summarise(b = round(sum(subset_mean1))))[,2]) %>% as.data.frame()
# 
# ggplot(data = dt[dt$week>10,])+aes(x = log(order+1), y = log(subset_mean+1), 
#                                    color = as.factor(tr_ts)) +
#   geom_point(aes(shape = factor(week))) + geom_abline(slope=1, intercept=0)
# 
# ############ Analysis of prediction
# #
# #
# # a3 = read.csv(paste(path, "rf_ensemble_preds_subset.csv", sep = ""), header = T)[,-1]
# #
# # va1 = c("pred_knn21",             "pred_knn22"   ,          "pred_knn23"       ,
# #         "pred_knn28" ,            "xgb1"          ,         "xgb2"              ,
# #         "xgb3"        ,           "xgb4"           ,        "xgb5"               ,
# #         "xgb6"         ,          "xgb7"            ,       "xgb8"                ,
# #         "xgb9"          ,         "xgb10"            ,      "pred_NN1_nonlog"      ,
# #         "pred_NN2_nonlog",        "pred_NN3_nonlog"   ,     "pred_rf1"              ,
# #         "pred_rf2"        ,       "pred_rf3"           ,    "pred_rf4"              ,
# #         "pred_rf5"         ,      "pred_rf6"            ,   "pred_rf7"              ,
# #         "pred_rf8"          ,     "pred_rf9"             ,  "pred_rf10"             ,
# #         "mean_all_pred")
# #
# #
# # dt1 = read.csv(paste(path, "all_pred_combine_w13.csv", sep = ""), header = T)
# # dt1$sumset_mean = round(rowMeans(dt1[,c("xgb6", "pred_NN1_nonlog", "pred_rf1",
# #                                         "pred_rf5","pred_rf6" ,  "pred_rf7")]))
# #
# # dt1 = left_join(dt1, a3, by = c("itemID", "week"))
# # dt2 = dt1 %>%filter(week<14) %>% group_by(itemID) %>% mutate(count0 = sum(order==0),
# #                                                              countgt0 = sum(order != 0))
# #
# # df = dt1
# # df$order[df$week==14] = df$sumset_mean[df$week==14]
# # ggplot(data = dt[dt$week>8,])+
# #   geom_density(data=df, aes(x=pred_NN2_nonlog, group=as.factor(tr_ts), fill=as.factor(tr_ts)),
# #                alpha=0.5, adjust=2) + xlim(0,25)
# # ggplot(data = df[dt$week>8,], aes(x=order, y = pred_rf7, color = as.factor(tr_ts)))+
# #                 geom_point()
# #
# # dtlat4w = dt1 %>% filter(week %in% c(6:13)) %>% group_by(itemID) %>%
# #       summarise(mean_order6w = round(mean(order)))
# # dt1 = left_join(dt1, dtlat4w, by = "itemID") %>% replace_na(mean_order6w = 0)
# #
# #
# # a2 = dt1 %>% group_by(week) %>% summarise_all(sum) %>% data.frame()
# # a2 = a2[,-c(2,5:14)]
# # df = dt1
# # a1 = df %>% group_by(week) %>% summarise(order = sum(order),
# #                                          mean_order = sum(round(mean_order)),
# #                                          mean_all = sum(mean_all_pred)) %>%
# #   melt(id = "week")
# #
# # ggplot(data = a2, aes(x = week, y = log(value), color = variable)) + geom_line()
# #
# #
# #
# # dt = read.csv(paste(path, "layer_2_ensemble_dat.csv", sep = ""), header = T)[,-1]
# # dt$mean_2l_en = round(rowMeans(dt[,c("rf1_preds", "rf2_preds", "pred_xgb", "pred_lm")]))
# # dt = left_join(dt, dt1 %>% select(c("itemID", "week", "mean_all_pred")),
# #                by = c("itemID", "week"))
# #
# # df = dt
# # df$order[df$week==14] = df$preds[df$week==14]
# # ggplot(data = dt)+
# #   geom_density(data=df, aes(x=order, group=as.factor(tr_ts), fill=tr_ts),
# #                alpha=0.5, adjust=2) + xlim(0,25)
# #
# #
# #
# # a1 = df %>% group_by(week) %>% summarise(order = sum(order),
# #                                                 en2_rf = sum(preds),
# #                                          mean_en1 = sum(mean_all_pred),
# #                                                 mean_en2 = sum(mean_2l_en),
# #                                          mean_order = sum(round(mean_order))) %>%
# #   melt(id = "week")
# #
# # ggplot(data = a1, aes(x = week, y = log(value), color = variable)) + geom_line()
# #
# #
# #
# #
# #
