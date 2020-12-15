Parallel_function <- function(tr_dt, ts_dt, params, tune_names){
  library(caret)
  library(lubridate)
  library(glmnet)
  library(dplyr)
  library(kernlab)
  library(liquidSVM)
  
  
  #------------------------------------------------------------------------------------
  # Loss Function
  #------------------------------------------------------------------------------------
  
  
  loss_func  = function(pred , orders, price){
    
    if (length(pred)==length(orders) & length(pred)==length(price)){
      err = price*(orders - pred) 
      err[err>0] = err[err>0]*0.6
      return(sum(abs(err)))
    }else {
      print("Dimenstion of ordrs, price and predicted are not same")
      return()
    }
    
  }
  
  
  
  #------------------------------------------------------------------------------------
  # Fit a model on data
  #------------------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------------
  # **************           Change in the following part       ********************
  #-----------------------------------------------------------------------------------
  
  
  Fit_model   = function(train_dt, test_dt, x_feature, val_dt = NULL, respons = "order", 
                         price = price_var, param = NULL){
    
    if(!neural){
      x_train = as.matrix(train_dt %>% select_at(.vars = x_feature))
      x_test = as.matrix(test_dt %>% select_at(.vars = x_feature) )
      
      x_train_price = as.matrix(train_dt %>% select_at(.vars = price))
      x_test_price = as.matrix(test_dt %>% select_at(.vars = price) )
      
      y_train = as.matrix(train_dt %>% select_at(.vars = respons) )
      y_test = as.matrix(test_dt %>% select_at(.vars = respons) )
      
      rm(train_dt, test_dt)
    }else{
      
      
      x_val = as.matrix(val_dt  %>% select_at(.vars = x_feature))
      x_val_price = as.matrix(val_dt %>% select_at(.vars = price))
      y_val = as.matrix(val_dt  %>% select_at(.vars = respons) )
      
      rm(val_dt)
    }
    
    err1 = 0
    a1 = Sys.time()
    result <- tryCatch({
      
      ######################### Change in this part only ############################        
      fit = liquidSVM::exSVM(x = x_train, y = y_train, scale = T, display = 0,
                           weights = param[[6]],
                           partition_choice = 5,
                           adaptivity_control = 2,
                           grid_choice = 2,
                           random_seed = 123,
                           clipping = -1.0,
                           min_gamma = param[[1]],
                           max_gamma = param[[2]],
                           min_lambda = param[[3]],
                           max_lambda = param[[4]],
                           KERNEL = as.character(param[[5]]),
                           gamma_steps = 100,
                           lambda_steps = 100,
                           threads = -1)
      
      yhat_train = round(predict(fit, x_train))
      yhat_train = ifelse(yhat_train<0, 0, yhat_train)
      yhat_test  = round(predict(fit, x_test))
      yhat_test = ifelse(yhat_test<0, 0, yhat_test)
      tr_loss =  loss_func(pred = yhat_train, orders = y_train, price = x_train_price)
      ts_loss = loss_func(pred = yhat_test, orders = y_test, price = x_test_price)
      
      if(neural){
        yhat_val = predict(fit, x_val)
        vl_loss = loss_func(pred = yhat_val, orders = y_val, price = x_val_price)  
      }
      
      
      ###############################################################################
      
      if(!neural){
        tr_loss = tr_loss/nrow(x_train)
        ts_loss = ts_loss/nrow(x_test)
        tr_r2 = cor(yhat_train, y_train, use = "pairwise.complete.obs")^2
        ts_r2 = cor(yhat_test, y_test, use = "pairwise.complete.obs")^2
        # print(c(yhat_train, yhat_test))
        nna_tr = sum(is.na(yhat_train))
        nna_ts = sum(is.na(yhat_test))
        res = c(unlist(param), tr_loss, ts_loss, tr_r2, ts_r2,  nna_tr, nna_ts, err1)
        rm(fit, x_train, x_test, y_train, y_test, x_train_price,
           x_test_price, yhat_test, yhat_train)
      }else{
        tr_loss = tr_loss/nrow(x_train)
        vl_loss = vl_loss/nrow(x_val)
        ts_loss = ts_loss/nrow(x_test)
        tr_r2 = cor(yhat_train, y_train, use = "pairwise.complete.obs")^2
        vl_r2 = cor(yhat_val, y_val, use = "pairwise.complete.obs")^2
        ts_r2 = cor(yhat_test, y_test, use = "pairwise.complete.obs")^2
        nna_tr = sum(is.na(yhat_train))
        nna_vl = sum(is.na(yhat_val))
        nna_ts = sum(is.na(yhat_test))
        res = c(unlist(param), tr_loss, vl_loss, ts_loss, tr_r2, vl_r2, ts_r2, nna_tr, nna_vl, nna_ts, err1)
        rm(fit, x_train, x_test, y_train, y_test, x_train_price,
           x_test_price, x_val_price, x_val,
           yhat_val, yhat_test, yhat_train)
      }
      res = res
    },  error = function(err){
      err1 = 1
      tr_loss = -9E9
      ts_loss = -9E9
      write(toString("******* ERROR *******"), paste(path,"out.txt", sep = ""), 
            append = TRUE)
      write.table(unlist(param), paste(path,"out.txt", sep = ""), 
                  append = TRUE, sep = ",", dec = ".",
                  row.names = FALSE, col.names = FALSE)
      write(toString(err), paste(path,"out.txt", sep = ""), 
            append = TRUE)
      # print(err)
      write(toString("*******************"), paste(path,"out.txt", sep = ""), 
            append = TRUE)
      
      if(!neural){
        return(c(unlist(param), tr_loss, ts_loss, 0, 0, 0,0, err1))
      } else{
        vl_loss = -9E9
        return(c(unlist(param), tr_loss, vl_loss, ts_loss, 0, 0, 0, 0,0,0,err1))
      }
    }, finally =  NULL)
    
    
    # print(c(unlist(param), tr_loss, ts_loss, war1, err1))
    a2 = Sys.time()    
    tm = as.numeric(difftime(a2, a1, units = "secs"))
    # write.table(c(result, tm), paste(path,"out.txt", sep = ""), 
    #             append = TRUE, sep = ",", dec = ".",
    #             row.names = FALSE, col.names = FALSE)
    return(c(result, tm))
  }
  
  #-----------------------------------------------------------------------------------
  #**************           No Change in the following part       ********************
  #-----------------------------------------------------------------------------------
  
  nindex = 13
  
  if (!neural){
    trial_model = lapply(1:nindex, FUN = function(j){
      Fit_model(train_dt = tr_dt[[j]], test_dt = ts_dt[[j]],
                x_feature = x_features, 
                respons = respons_var, param = params, price = price_var)
    }) 
    
    trial_model = do.call(rbind.data.frame, trial_model) 
    names(trial_model) = c(tune_names, "Tr_loss", "Ts_loss", "tr_r2", "ts_r2", "na_tr", "na_ts",
                           "Error", "Fold_Time")
    trial_model = trial_model %>% group_by_at(tune_names) %>% 
      summarise(Tr_loss_mean = mean(Tr_loss),
                Tr_loss_sd = sd(Tr_loss),
                Tr_loss_min = min(Tr_loss),
                Tr_loss_max = max(Tr_loss),
                Tr_r2_mean = mean(tr_r2),
                Tr_na_mean = mean(na_tr),
                Ts_loss_mean = mean(Ts_loss),
                Ts_loss_sd = sd(Ts_loss),
                Ts_loss_min = min(Ts_loss),
                Ts_loss_max = max(Ts_loss),
                Ts_r2_mean = mean(ts_r2),
                Ts_na_mean = mean(na_ts),
                Error = ifelse(any(Error==1),1,0),
                Fold_time = mean(Fold_Time))  %>% 
      mutate(model = Model, logtr = logtr,
             fit_parallel  = Fit_Parallel)
    names(trial_model) = c(tune_names, "Tr_loss", "Tr_loss_sd", "Tr_loss_max", "Tr_loss_min",
                           "Tr_r2", "Tr_na", "Ts_loss", "Ts_loss_sd" ,  "Ts_loss_max", 
                           "Ts_loss_min",  "Ts_r2", "Ts_na", "Error", "Fold_time",
                           "model_name", "logtr", "fit_parallel")
  } else{
    trial_model = lapply(1:nindex, FUN = function(j){
      val_week = ifelse(j == 1, 2, j -1)
      Fit_model(train_dt = tr_dt[[j]] %>% filter(tr_ts != val_week), 
                test_dt = ts_dt[[j]], 
                val_dt = tr_dt[[j]] %>% filter(tr_ts == val_week),
                x_feature = x_features, 
                respons = respons_var, param = params, price = price_var)
    })
    
    trial_model = do.call(rbind.data.frame, trial_model) 
    names(trial_model) = c(tune_names, "Tr_loss", "Vl_loss", "Ts_loss", "tr_r2","vl_r2",
                           "ts_r2",  "na_tr", "na_vl", "na_ts", "Error", "Fold_Time")
    trial_model = trial_model %>% group_by_at(tune_names) %>% 
      summarise(Tr_loss_mean = mean(Tr_loss),
                Vl_loss_mean = mean(Vl_loss),
                Ts_loss_mean = mean(Ts_loss),
                Tr_loss_sd = sd(Tr_loss),
                Vl_loss_mean = sd(Vl_loss),
                Ts_loss_sd = sd(Ts_loss),
                Tr_loss_max = max(Tr_loss),
                Vl_loss_max = max(Vl_loss),
                Ts_loss_max = max(Ts_loss),
                Tr_loss_min = min(Tr_loss),
                Vl_loss_min = min(Vl_loss),
                Ts_loss_min = min(Ts_loss),
                Tr_r2_mean = mean(tr_r2),
                Vl_r2_mean = mean(vl_r2),
                Ts_r2_mean = mean(ts_r2),
                Tr_na_mean = mean(na_tr),
                Vl_na_mean = mean(na_vl),
                Ts_na_mean = mean(na_ts),
                Error = ifelse(any(Error==1),1,0),
                Fold_time = mean(Fold_Time))  %>% 
      mutate(model = Model, logtr = logtr,
             fit_parallel  = Fit_Parallel)
    names(trial_model) = c(tune_names, "Tr_loss", "Vl_loss", "Ts_loss", "Tr_loss_sd",
                           "Vl_loss_sd" , "Ts_loss_sd" , "Tr_loss_max", "Vl_loss_max", 
                           "Ts_loss_max", "Tr_loss_min", "Vl_loss_min",
                           "Ts_loss_min", "Tr_r2", "Vl_r2", "Ts_r2", "Tr_na", "Vl_na", "Ts_na", "Error", "Fold_time",
                           "model_name", "logtr", "fit_parallel")
  }
  
  return(trial_model)
  
}    

parallel_fit <- function(file_name_tr, file_name_ts){
  library(caret)
  library(lubridate)
  library(stats)
  library(glmnet)
  library(dplyr)
  library(parallel)
  
  if (!(Exp_Params[1] == "None")){
    Config_param   = function(Exp_Params = Exp_Params){
      # Exp_Params = list(alpha = seq(0, 1, length.out = 5), 
      #                   nlambda = 10)
      
      Experiment = expand.grid(Exp_Params)
      # Experiment = data.frame(id = 1:nrow(Experiment), Experiment)
      # colnames(Experiment) = c('id', names(Exp_Params))
      
      return(list(Exp = Experiment , names = names(Exp_Params)))
      
    }
    
    params1 = Config_param(Exp_Params = Exp_Params)
    params_df    = params1$Exp
    tune_names  = params1$names
    ncombin = nrow(params_df)
    rm(params1)
  }else {
    params_df    = as.data.frame(list(None = "None"))
    tune_names  = "None"
    ncombin = 1
  }
  
  
  tr_data = lapply(1:13, function(i) read.csv(file = paste(path, file_name_tr, i, ".csv", sep = ""), header = T))
  ts_data = lapply(1:13, function(i) read.csv(file = paste(path, file_name_ts, i, ".csv", sep = ""), header = T))
  
  
  file.create(paste(path,"out.txt", sep = ""))
  
  if (Fit_Parallel){
    no_cores <- detectCores() - 1
    message(paste("***** Using ", no_cores, "Cores *****"))
    message("Running models ...", appendLF = FALSE)
    cl <- makeCluster(no_cores, outfile=paste(path,"out.txt", sep = ""))
    
    clexp = list("Parallel_function", "Model", "logtr", "price_var", 
                 "x_features", "respons_var",  "path", 
                 "Fit_Parallel", "neural")
    clusterExport(cl, clexp)
    # Compare time to run function using parApply and apply
    
    estimates <-  parLapplyLB(cl = cl, X = 1:ncombin, 
                              fun = function(i){tryCatch({
                                a1 = Sys.time()
                                res = Parallel_function(tr_dt = tr_data,
                                                        ts_dt = ts_data,
                                                        params = params_df[i,], 
                                                        tune_names = tune_names)
                                a2 = Sys.time()    
                                tm = as.numeric(difftime(a2, a1, units = "secs"))
                                return(c(res, tm))  
                              }, finally = NULL)
                              })
    
    stopCluster(cl)
    message("Done")
    message(paste("***** Stop Cores *****"))
  } else {
    message("Running models ...", appendLF = FALSE)
    estimates <-  lapply(X = 1:ncombin, FUN = function(i){
      a1 = Sys.time()
      res = Parallel_function(tr_dt = tr_data,
                              ts_dt = ts_data,
                              params = params_df[i,], 
                              tune_names = tune_names)
      a2 = Sys.time()    
      tm = as.numeric(difftime(a2, a1, units = "secs"))
      return(c(res, tm))
    })
    
    message("Done")
  }
  
  
  estimates = do.call(rbind.data.frame, estimates)
  names(estimates) = c(names(estimates)[-ncol(estimates)], "Time")
  # message("Done
  # close("out.txt", type = "rw")
  rm(params_df, tr_data, ts_data)
  return(estimates)
}



#------------------------------------------------------------------------------------
#                                Experiment configuration 
#       **************           Change the following part       ********************
#-----------------------------------------------------------------------------------

path = "/work/STAT/kanak/"

Fit_Parallel  = FALSE                   # if want to fit model parallel 
Model         = "SVM(lssvm)"          # Model used to train
price_var     = "new_price"            # price variable name
logtr         = TRUE
neural        = FALSE
x_features    = c(  "week",                     "manufacturer_std",      "brand_std",
                    "category1_std",            "category2_std",         "category3_std",
                    "customerRating",           "new_price",             "recommendedRetailPrice",
                    "lag1_order",               "lag2_order",            "lag3_order",
                    "comp1_order",              "comp2_order",           "comp3_order",
                    "comp1_npromoIW",           "comp2_npromoIW",        "comp3_npromoIW",
                    "comp4_npromoIW",           "comp5_npromoIW",        "comp6_npromoIW",
                    "comp7_npromoIW",           "comp8_npromoIW",        "comp9_npromoIW",
                    "comp10_npromoIW",          "supp1_npromoIW",        "supp2_npromoIW",
                    "morderI",                  "mncusI",                "mncusIW",
                    "mtmncusBMC123",            "npromoBMC123W",         "npromoBMW",
                    "npromoI",                  "npromoIW",              "npromoIW_npromoIW")       # X Features name list
respons_var   = "order"                 # Response variable name

# Exp_Params = list(min_gamma = c(.005, 25, 50, 75, 100),
#                   max_gamma = c(25, 50, 75, 100, 125),
#                   min_lambda = c(1E-4, 25, 50, 75, 100),
#                   max_lambda = c(25, 50, 75, 100, 125),
#                   KERNEL = c("GAUSS_RBF", "POISSON"))         # list of hyper-parameters 


Exp_Params = list(min_gamma = c(.005),
                  max_gamma = c(5000),
                  min_lambda = c(1E-4),
                  max_lambda = c(5000),
                  KERNEL = c("POISSON"),
                  weights = seq(.4, .6, length.out = 15))         # list of hyper-parameters 

# Read data file and 
# Define the data file as "Train"
result = parallel_fit(file_name_tr = "final_tr_std_log_promo3_tsw", file_name_ts = "final_ts_std_log_promo3_tsw")

path = "/work/STAT/kanak/exsvm/"

write.csv(x = result, file = paste(path, "exsvm_promo3_res1.csv", sep = ""), row.names = F)
