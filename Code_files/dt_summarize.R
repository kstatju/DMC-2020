########################################################
## Library

library(dplyr)
library(tidyr)
library(ggplot2)


##################################################



data_merge = function(path){
  
  library(dplyr)
  library(tidyr)
  
  ## Items Data
  items = read.csv(paste(path, "items.csv", sep = ""), header = TRUE, sep = "|")
  
  ##########################################################
  ## Infos Data
  infos = read.csv(paste(path, "infos.csv", sep = ""), header = TRUE, sep = "|")
  a1 <- gsub(pattern=",", replacement="_", infos$promotion, fixed = TRUE)
  a1[a1==""] = NA
  a2 = strsplit(a1, split = "_")
  a3 = data.frame(t(data.frame(lapply(a2, "length<-", max(lengths(a2))))))
  row.names(a3) = NULL
  names(a3) = paste('promotion', c(1:ncol(a3)), sep = "")
  infos = cbind(infos[,1:2], a3)
  rm(a1, a2, a3)
  infos$promotion1 = as.POSIXct(infos$promotion1,format="%Y-%m-%d", tz = 'GMT')
  infos$promotion2 = as.POSIXct(infos$promotion2,format="%Y-%m-%d", tz = 'GMT')
  infos$promotion3 = as.POSIXct(infos$promotion3,format="%Y-%m-%d", tz = 'GMT')
  
  
  ################################################
  ## Orders Data
  
  orders = read.csv(paste(path, "orders.csv", sep = ""), header = TRUE, sep = "|")
  orders$time = as.POSIXct(orders$time,format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
  orders$date <- as.Date(orders$time , format = "%Y-%m-%d", tz = "GMT")
  orders$wday = strftime(orders$date, "%A")
  
  orders = merge(orders, items, by = "itemID")
  orders = merge(orders, infos, by = "itemID")
  
  return(orders)
}


path = "D:/ISU/DMC/DMC 2020/Data/DMC-2020-Task/DMC20_Data/"
Train = data_merge(path = path)
# write.csv(df, paste(path, "merge_data.csv"), sep = "|", row.names = FALSE)

colSums(is.na(df))


orderbyday = orders %>% group_by(itemID, date) %>% summarise_at(.vars = "order", sum)
orderbyday$wday = strftime(orderbyday$date, "%A")


aa = lapply(unique(orders$itemID), FUN = function(x) ifelse(x %in% unique(infos$itemID), 1,0))




nofibbyday  = orders %>% group_by(transactID, date) %>% summarise_at(.vars = "order", sum)

orderbyday %>% group_by(itemID, wday) %>% summarise_at("order", sum) %>% 
  ggplot(aes(x=itemID, y=wday, size = order)) +
  geom_point(alpha=0.5)

orderbyday %>% ggplot(aes(x=date, y=itemID, size = order, color = wday)) +
  geom_point(alpha=0.5)

orderbyday[orderbyday$itemID<50,] %>% ggplot(aes(x=wday, y=itemID, size = order)) +
  geom_point(alpha=0.5)




colSums(is.na(orders))


