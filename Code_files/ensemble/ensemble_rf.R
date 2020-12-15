Parallel_function <- function(Train, Test, params, tune_names){
    # library(caret)
    library(dplyr)
  
  ########### Change ############## 
  ##### Please include package for your model here
    library(randomForest)
    
  
  
    loss_func  = function(pred , orders, price){
      
      if (length(pred)==length(orders) & length(pred)==length(price)){
        err = price*(orders - pred) 
        err[err>0] = err[err>0]*0.6
        return(mean(abs(err)))
      } 
      else {
        print("Dimenstion of ordrs, price and predicted are not same")
        return()
      }
      
    }
    
    
    build_datasets = function(df , n_folds=5, seed = NULL){
      
      if(!is.null(seed)) set.seed(seed)
      
      n_obs = nrow(df)
      sz = round(n_obs/n_folds)
      
      folds = replicate(n_folds, (sample(1:n_obs, size = sz, replace = FALSE)))
      
      train_folds = list()
      test_folds = list()
      for(i in 1:ncol(folds)){
        train_folds[[i]] = c(1:n_obs)[-folds[i]]
        test_folds[[i]]  = c(1:n_obs)[folds[,i]]
      }
      rm(folds)
      return(list(train_index = train_folds , test_index = test_folds))  
      
    }
    
    
    Fit_model   = function(df_tr, df_ts, df_train, df_test, x_feature, respons = "order", 
                           price = price_var, param = NULL){
      
      x_train = as.matrix(df_tr %>% select_at(.vars = x_feature) %>% slice(df_train))
      x_val = as.matrix(df_tr %>% select_at(.vars = x_feature) %>% slice(df_test))
      x_test = as.matrix(df_ts %>% filter(week == 13) %>% select_at(.vars = x_feature))
      
      x_train_price = as.matrix(df_tr %>% select_at(.vars = price) %>% slice(df_train))
      x_val_price = as.matrix(df_tr %>% select_at(.vars = price) %>% slice(df_test))
      x_test_price = as.matrix(df_ts %>% filter(week == 13) %>% select_at(.vars = price))
      
      x_train_price1 = as.matrix(df_tr %>% select_at(.vars = "new_price") %>% slice(df_train))
      x_val_price1 = as.matrix(df_tr %>% select_at(.vars = "new_price") %>% slice(df_test))
      x_test_price1 = as.matrix(df_ts %>% filter(week == 13) %>% select_at(.vars = "new_price"))
      
      y_train = as.matrix(df_tr %>% select_at(.vars = respons) %>% slice(df_train))
      y_val = as.matrix(df_tr %>% select_at(.vars = respons) %>% slice(df_test))
      y_test = as.matrix(df_ts %>% filter(week == 13) %>% select_at(.vars = respons))
      
      #################### Change ##################
      fit = randomForest::randomForest(x = x_train, y = y_train, 
                             mtry = param[1])
        
      ##############################
      yhat_train = predict(fit, x_train)
      yhat_train = round(ifelse(yhat_train<0, 0, yhat_train))
      yhat_val = predict(fit, x_val)
      yhat_val = round(ifelse(yhat_val<0, 0, yhat_val))
      yhat_test  = predict(fit, x_test)
      yhat_test = round(ifelse(yhat_test<0, 0, yhat_test))
      
      tr_loss = loss_func(pred = yhat_train, orders = y_train, price = x_train_price)
      vl_loss = loss_func(pred = yhat_val, orders = y_val, price = x_val_price)
      ts_loss = loss_func(pred = yhat_test, orders = y_test, price = x_test_price)
      
      tr_loss1 = loss_func(pred = yhat_train, orders = y_train, price = x_train_price1)
      vl_loss1 = loss_func(pred = yhat_val, orders = y_val, price = x_val_price1)
      ts_loss1 = loss_func(pred = yhat_test, orders = y_test, price = x_test_price1)
      
      tr_r2 = cor(yhat_train, y_train, use = "pairwise.complete.obs")^2
      vl_r2 = cor(yhat_val, y_val, use = "pairwise.complete.obs")^2
      ts_r2 = cor(yhat_test, y_test, use = "pairwise.complete.obs")^2
    
      return(c(unlist(param), tr_loss, vl_loss, ts_loss, tr_loss1, vl_loss1, ts_loss1, tr_r2, vl_r2, ts_r2))  
      
    }
    
    
    df_folds = build_datasets(df = Train, n_folds = n_folds, seed = rseed)
    
    nindex = length(df_folds$train_index)
    
    trial_model = lapply(1:nindex, FUN = function(j){
      Fit_model(df_tr =  Train, df_ts = Test, df_train = df_folds$train_index[[j]] , 
                df_test = df_folds$test_index[[j]],  x_feature = x_features, 
                respons = respons_var, param = params, price = price_var)
    })
    
    trial_model = do.call(rbind.data.frame, trial_model) %>% summarise_all(mean)
    names(trial_model) = c(tune_names, "Tr_loss", "Vl_loss", "Ts_loss", "Tr_loss_old", 
                           "Vl_loss_old", "Ts_loss_old", "Tr_r2", "Vl_r2", "Ts_r2")
    
    return(trial_model)
}

parallel_fit <- function(){
  # library(caret)
  library(stats)
  library(dplyr)
  library(parallel)
  
  Config_param   = function(Exp_Params = Exp_Params){
    
    Experiment = expand.grid(Exp_Params)

    return(list(Exp = Experiment , names = names(Exp_Params)))
    
  }
  
  params1 = Config_param(Exp_Params = Exp_Params)
  params_df    = params1$Exp
  tune_names  = params1$names
  ncombin = nrow(params_df)
  rm(params1)
  
  no_cores <- detectCores() - 1
  message(paste("***** Using ", no_cores, "Cores *****"))
  message("Running models ...", appendLF = FALSE)
  cl <- makeCluster(no_cores)
  
  clexp = list("Parallel_function", "Train", "Test", "n_folds",
               "rseed", "price_var", "x_features", "respons_var")
  clusterExport(cl, clexp)
  # Compare time to run function using parApply and apply
  
  estimates <-  parLapplyLB(cl = cl, X = 1:ncombin, 
                            fun = function(i){tryCatch({
                              a1 = Sys.time()
                              res = Parallel_function(Train = Train, Test = Test,
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

  estimates = do.call(rbind.data.frame, estimates)
  names(estimates) = c(names(estimates)[-ncol(estimates)], "Time")
  # message("Done
  # close("out.txt", type = "rw")
  rm(params_df)
  return(estimates)
}

library(dplyr)


path = "D:/ISU/DMC/DMC 2020/Code/ensemble/"

n_folds       = 5                       # number of folds for cross validation
rseed          = 123                    # Random seed
price_var     = "new_price_nonstd"            # price variable name
x_features    = c("pred_knn20",            
                  "pred_knn21",             "pred_knn22",             "pred_knn23",            
                  "pred_knn28",             "xgb1",                   "xgb2",                  
                  "xgb3",                   "xgb4",                   "xgb5",                  
                  "xgb6",                   "xgb7",                   "xgb8",                  
                  "xgb9",                   "xgb10",                  "pred_NN1_nonlog",       
                  "pred_NN2_nonlog",        "pred_NN3_nonlog",        "pred_rf1",              
                  "pred_rf2",               "pred_rf3",               "pred_rf4",              
                  "pred_rf5",               "pred_rf6",               "pred_rf7",              
                  "pred_rf8",               "pred_rf9",               "pred_rf10")       # X Features name list
respons_var   = "order"                 # Response variable name

Exp_Params = list(mtry = c(2, 3, 5))         # list of hyper-parameters 

dt = read.csv(paste(path, "all_pred_combine.csv", sep = ""), header = T) 
Train = dt %>% filter(week < 13) %>% as.data.frame()
Test = dt %>% filter(week == 13) %>% as.data.frame()
Train = Train[1:100,]
Test = Test[100:200,]

aa = parallel_fit()
  