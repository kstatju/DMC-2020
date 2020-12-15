## Function to merge all data files

data_merge = function(path){
  
  library(dplyr)
  
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
  orders$date <- format(orders$time ,format = "%Y-%m-%d")
  orders$wday = strftime(orders$date, "%A")
  
  orders = merge(orders, items, by = "itemID")
  orders = merge(orders, infos, by = "itemID")
  
  return(orders)
}


path = "D:/ISU/DMC/DMC 2020/Data/DMC-2020-Task/DMC20_Data/"

df = data_merge(path = path)

# write.csv(df, paste(path, "merge_data.csv"), sep = "|", row.names = FALSE)