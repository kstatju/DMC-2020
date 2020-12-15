wt_ensemble <- function(dt, pred_var, ntimes){
  library(dplyr)
  
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
    
    weight_men <- function(weight, pred){
      return(as.matrix(pred) %*% weight)
    }
  
    wt_en <- function(dt, pred_var){
        # set.seed(123)
        wt = runif(length(pred_var))
        wt = round(wt/sum(wt), digits = 5)
        
        dt$ensempble = weight_men(weight = wt, dt[,pred_var])
        
        
        pred_var = c(pred_var,   "ensempble")
        
        a1 = which(dt$tr_ts==1)
        a2 = which(dt$tr_ts==2)
        a3 = which(dt$tr_ts==3)
        a4 = which(dt$tr_ts==4)
        
        summy = c(wt, loss_func(pred = dt$ensempble[a1], 
                                orders = dt$order[a1], price = dt$new_price_nonstd[a1]),
                  loss_func(pred = dt$ensempble[a4], 
                            orders = dt$order[a4], price = dt$new_price_nonstd[a4]),
                  loss_func(pred = dt$ensempble[c(a2, a3)], 
                            orders = dt$order[c(a2, a3)], price = dt$new_price_nonstd[c(a2, a3)]),
                  loss_func(pred = dt$ensempble[a1], 
                            orders = dt$order[a1], price = dt$new_price[a1]),
                  loss_func(pred = dt$ensempble[a4], 
                            orders = dt$order[a4], price = dt$new_price[a4]),
                  loss_func(pred = dt$ensempble[c(a2, a3)], 
                            orders = dt$order[c(a2, a3)], price = dt$new_price[c(a2, a3)]),
                  cor(dt$order[a1], dt$ensempble[a1] , use = "pairwise.complete.obs")^2,
                  cor(dt$order[a4], dt$ensempble[a4] , use = "pairwise.complete.obs")^2,
                  cor(dt$order[c(a2, a3)], dt$ensempble[c(a2, a3)] , use = "pairwise.complete.obs")^2
                  )
        
        return(summy)
    }
    
    
    aa = t(replicate(n = ntimes, wt_en(dt = dt, pred_var = prd_var))) %>% as.data.frame()
    names(aa) = c(paste("wt", 1:length(prd_var), sep = ""), "loss1_12", "loss13", "loss14_ts",
                  "loss1_12_old", "loss13_old", "loss14_ts_old",  "r2_12", "r2_13", "r2_14_ts")
    
    
    return(aa)
}

library(parallel)
prd_var = c("pred_knn20",            
              "pred_knn21",             "pred_knn22",             "pred_knn23",            
              "pred_knn28",             "xgb1",                   "xgb2",                  
              "xgb3",                   "xgb4",                   "xgb5",                  
              "xgb6",                   "xgb7",                   "xgb8",                  
              "xgb9",                   "xgb10",                  "pred_NN1_nonlog",       
              "pred_NN2_nonlog",        "pred_NN3_nonlog",        "pred_rf1",              
              "pred_rf2",               "pred_rf3",               "pred_rf4",              
              "pred_rf5",               "pred_rf6",               "pred_rf7",              
              "pred_rf8",               "pred_rf9",               "pred_rf10")


################ Change ###############
path = "/work/STAT/kanak/ensemble/"
ntime = 317
nrep = 317
###############################

dt = read.csv(paste(path, "all_pred_combine.csv", sep = ""), header = T)

# res = wt_ensemble(dt = knn_pred, pred_var = prd_var, ntimes = ntime)

no_cores <- detectCores() - 1
message(paste("***** Using ", no_cores, "Cores *****"))
message("Running models ...", appendLF = FALSE)
cl <- makeCluster(no_cores)

clexp = list("wt_ensemble", "ntime", "prd_var", "dt")
clusterExport(cl, clexp)

estimates <-  parLapplyLB(cl = cl, X = 1:nrep, 
                          fun = function(i) {
                            wt_ensemble(dt = dt, pred_var = prd_var, ntimes = ntime)
                          }) 

stopCluster(cl)
message("Done")
message(paste("***** Stop Cores *****"))


estimates = do.call("rbind", estimates)

write.csv(estimates, paste(path, "ensemble_summary.csv", sep = ""), row.names = F)



## Compare with these results
#################################
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


summ <- function(df, pred_var){
     df = as.data.frame(df)
     aa = lapply(pred_var, FUN = function(jj) loss_func(pred = df[,jj], orders = df[,"order"], price = df[,"new_price_nonstd"]))
     bb = lapply(pred_var, FUN = function(jj) loss_func(pred = df[,jj], orders = df[,"order"], price = df[,"new_price"]))
     corr = lapply(pred_var, FUN = function(jj) cor(df[,"order"], df[,jj] , use = "pairwise.complete.obs")^2)
     aa = data.frame(varaible = pred_var, loss = unlist(aa), loss_old = unlist(bb), corr = unlist(corr))
     return(aa)
  }


summy = dt %>% group_by(tr_ts) %>% group_map(~summ(df = .x, pred_var = prd_var))

write.csv(summy, paste(path, "all_pred_summary.csv", sep = ""), row.names = F)
