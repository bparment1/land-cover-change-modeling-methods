
# clear all existing objects  

#rm(list = ls()) #loosing all the input parameters if left 

# Function to Install and Load R Packages

# Specify the list of required packages to be installed and load    

p = c('pdist', 'fields', 'TOC', 'R.matlab', 'nnet') 
# , "ggplot2", "maps", "reshape", "gridExtra", "Rcpp", "sp", "raster", "caret");

install_package<-function(pack)
{if(!(pack %in% row.names(installed.packages())))
{
  update.packages(ask=F)
  install.packages(pack,dependencies=F) # T
}
 require(pack,character.only=TRUE)
}


# Call the Function install_package

for(pack in p) {install_package(pack)}


# Call all needed functions used to perform land use analysis, data normalisation, data split, calibration, validation, mapping ... 

#source("needed_funtions1.R")

# read_data is a function to read a dataset in MATLAB format (mat file)

read_data <- function(matfile, xvr, yvr, IndLU_t1, IndLU_t2, CodeNU, CodeU,in_dir=NULL){
  #d

  if(is.null(in_dir)){
    #inputFile1 = paste("data/", matfile, sep="")
    inputFile1 <- file.path("./data",matilfile)
  }else{
    inputFile1 <- file.path(in_dir,matfile)
  }
         
  data1 = loadMat(inputFile1)
	   
  #View(data1)
  data1 = data1[complete.cases(data1), ]

  ########################################
  # initialisation 
  # xvr = 6:11; yvr = 14; ratio = 0.7

  # m = length(xvr)

  change1    =  data1[ ((data1[,IndLU_t1]==CodeNU) & (data1[,IndLU_t2]==CodeU)) ,] # 4 and 5
  no_change1 =  data1[ (data1[,IndLU_t1]==CodeNU & data1[,IndLU_t2]==CodeNU) ,]
  # exc_layer1 =  data1[ ( (data1[,IndLU_t1]==CodeU)  & (data1[,IndLU_t2]==CodeNU) ) | ( (data1[,IndLU_t1]==CodeU)  & (data1[,IndLU_t2]==CodeU) ),] 

  # rrr = balanced_dataset(change1, no_change1, xvr, yvr) ####
  # ch = rrr$change 						  ###
  # no_ch = rrr$no_change			                    ### 

  r = list(ch = change1, no_ch = no_change1)

  return(r)

}

run_ltm <- function(change1, no_change1, xvr, m, yvr, ratio, K, sampling_name) { # IndLU_t1, IndLU_t2, CodeNU, CodeU
  # 
  # This functions runs a neural network model with binary input data.
  # INPUTS:
  # 1) change1
  # 2) no_change2
  # 3) xvr: index range for the covariates used in the model
  # 4) m: number of covariates
  # 5) yvr: index for the y variable
  
  ################# Start script ####################
  
  #########################
  #### Part 1: generate training and testing and run LTM
  
  ### Careful sampling name is a function!!!!!
  
  dataset1 = rbind( cbind(change1, rep(1, nrow(change1)) ), cbind(no_change1, rep(0, nrow(no_change1) ))) ### 
  
  ### This is a user defined function, rescaling
  dataset1N <- fn.normalize(dataset1, xvr, yvr) # dataset1, scaling by min and max: x- min(x)/(max (x) -min(x))
  # data split  according to the sampling strategy 
  
  # LT  = eval(as.call(list(as.name(sampling_name), dataset1N, yvr, ratio))) 
  # FUN <- match.fun(sampling_name) 
  
  #### ?
  #debug(sampling_name)!!! This is a function
  LT = sampling_name(dataset1N, yvr, ratio) # output is alist
  T = LT$T # matrix of data for testing
  L = LT$T # matrix of data for training
  
  ########################################
  # LTM, 		without clustering prior learning 
  ########################################
  # install.packages('TOC') # library(TOC)
  
  ##############################################################################################################################
  ###############################################################
  # run the LTM model, without clustering prior learning 
  ###############################################################
  ##############################################################################################################################
  
  #what is the use of the variable threshold?
  #############
  threshold = round(nrow(T[T[,yvr]==1, ]) / nrow(T), 2)  # T ; dataset1N
  print("th = ") 
  print(threshold) 
  #############
  
  #### THis function run LTM (nnet) for training and testing
  newT = fnLUCAnalysis_maxP(L, T, xvr, yvr, m, threshold) # dataset1N; dataset1N, T, rbind(L,T)
  #newT is a matrix with same size but two more columns, one for the activation and one for the hardening into one and zero
  ###
  
  ###########################
  ###### Part 2: generate accuracy metrics #####
  
  ### Crosstabutlation of observed versus predicted
  cm = table(newT[,yvr], newT[,yvr+2]) 
  
  print("cross-tab from the LTM model: ")
  print(cm)
  
  errors = apply(newT, 1, metrics, yvr, yvr+2)
  
  # Total Operating Characteristic - TOC and ROC curves
  
  tocd <- TOC(newT[,yvr], newT[,yvr+2], mask=NULL, nthres = 100)
  
  pcm = pcm_evaluation(cm)
  # mean(pcm)
  
  
  
  ##### This is the return object with all the results
  res= list(pcm_ltm = pcm, 
            cm_ltm = cm, 
            errors_ltm = errors, 
            toc_ltm =tocd,  
            change_ltm = change1, no_change_ltm = no_change1, newT = newT)
  
  return(res)
}

#### This is the function that generates predictions using different methods: 
#-neural network
#-logistic
#-randomForest
#
run_land_change_models <- function(change1, no_change1, xvr, m, yvr, ratio, K, 
                                   sampling_name,model_opt="ltm",data_df=NULL,
                                   names_col=NULL,out_dir=NULL,out_suffix=NULL) { # IndLU_t1, IndLU_t2, CodeNU, CodeU
  # 
  # This functions runs a neural network model with binary input data.
  # INPUTS:
  # 1) change1: change part of the dataset
  # 2) no_change2: no change part of the dataset
  # 3) xvr: index range for the covariates used in the model
  # 4) m: number of covariates
  # 5) yvr: index for the y variable
  
  ################# Start script ####################
  
  if(is.null(out_dir)){
    out_dir <- getwd()
  }
  if(is.null(out_suffix)){
    out_suffix <-""
  }
  
  #########################
  #### Part 1: generate training and testing and run LTM
  
  ### Careful sampling name is a function!!!!!
  if(is.null(data_df)){
    data_df = rbind( cbind(change1, rep(1, nrow(change1)) ), #adding a variable, change: 1 if changed
                     cbind(no_change1, rep(0, nrow(no_change1) ))) ###  #populating with 0 for no change
    names_col <- c(names_col,"change") #adding one name
    
  }
  
  # LT  = eval(as.call(list(as.name(sampling_name), dataset1N, yvr, ratio))) 
  # FUN <- match.fun(sampling_name) 
  
  #### ?
  #debug(sampling_name)!!! This is a function
  LT = sampling_name(data_df, yvr, ratio) # output is alist
  T = LT$T # matrix of data for testing
  L = LT$T # matrix of data for training
  
  ########################################
  
  #run_model_fun()
  
  #### THis function run LTM (nnet) for training and testing
  if(model_opt=="ltm"){
    ### This is a user defined function, rescaling
    dataset1N <- fn.normalize(dataset1, xvr, yvr) # dataset1, scaling by min and max: x- min(x)/(max (x) -min(x))
    # data split  according to the sampling strategy 
    
    newT = fnLUCAnalysis_maxP(L, T, xvr, yvr, m, threshold) # dataset1N; dataset1N, T, rbind(L,T)
    #newT is a matrix with same size but two more columns, one for the activation and one for the hardening into one and zero
    ###
    yvr_predicted <- yvr +2
    
  }
  
  if(model_opt!="ltm"){
    L_df <- as.data.frame(L)
    #dim(L)
    #dim(data_df)
    
    names(L_df)<- names_col
    T_df <- as.data.frame(T)
    names(T_df)<- names_col
    
    #### generate formula for modeling
    y_var <- names_col[yvr]
    L_df[[y_var]] <- as.factor(L_df[[y_var]]) #this is needed for randomForest to get a classification
    T_df[[y_var]] <- as.factor(T_df[[y_var]])
    explanatory_variables <- names_col[xvr] #covariate
    
    right_side_formula <- paste(explanatory_variables,collapse = " + ")
    model_formula_str <- paste0(y_var," ~ ",right_side_formula)
    
    num_cores <- 1
    #out_dir <-
    
    #debug(test_glm)
    test_glm <- run_model_fun(data_df=L_df, #note this can be a list
                              model_formula_str = model_formula_str,
                              model_opt=model_opt, #"logistic",
                              data_testing=T_df, #note this can be a list
                              num_cores=num_cores,
                              out_dir=out_dir,
                              out_suffix=out_suffix)
    #names(test_glm)
    #str(test_glm$mod[[1]])
    
    browser() #this is a break point
    
    if(model_opt=="logistic"){
      y_fitted <- test_glm$mod[[1]]$fitted.values #predicted on training
      y_predicted <- (test_glm$predicted_val[[1]]) #predicted on testing
      ### hardening the soft prediction:
      ### find the threshold based on quantity!!!
      quantity_change <- sum(as.numeric(L_df[,yvr])) #sum of ones
      
      y_predicted_ranked <- sort(y_predicted)
      y_fitted_ranked <- sort(y_fitted)

      y_predicted_hard <- y_predicted_ranked[1:quantity_change] 
      y_fitted_hard <- y_fitted_ranked[1:quantity_change] 
      #also keep the probability!!
    }
    
    if(model_opt=="randomForest"){
      #add here, not running right now on the laptop
    }

    ### store the values
    L_df$y_fitted <- y_fitted
    T_df$y_pred <- y_predicted

    L_df$y_fitted_hard <- y_fitted_hard
    T_df$y_pred_hard <- y_predicted_hard
    
    ### Now transform out put into newT
    #newT is a matrix with same size but two more columns, one for the activation 
    #and one for the hardening into one and zero
    newT <- T_df
    ###
    
    #yvr_predicted <- yvr + 1
  }
  
  ###########################
  ###### Part 2: generate accuracy metrics #####
  
  ### Crosstabutlation of observed versus predicted
  cm = table(newT[,yvr], newT[,yvr+2]) 
  
  print("cross-tab from the LTM model: ")
  print(cm)
  
  errors = apply(newT, 1, metrics, yvr, yvr_predicted)
  
  # Total Operating Characteristic - TOC and ROC curves
  # Note that it should be yvr +1 for TOC? since this is the activation
  
  tocd <- TOC(newT[,yvr], newT[,yvr+2], mask=NULL, nthres = 100)
  
  pcm = pcm_evaluation(cm)
  # mean(pcm)
  
  ##### This is the return object with all the results
  res= list(pcm_ltm = pcm, 
            cm_ltm = cm, 
            errors_ltm = errors, 
            toc_ltm =tocd,  
            change_ltm = change1, no_change_ltm = no_change1, newT = newT)
  
  return(res)
}

fct_result <-  function (res_case, IT) { # IT is the number of replications 
  
  auc_ltm = c()
  pcm_ltm = c()
  ER_ltm = c()
  
  for (i in 1:IT) {
    
    auc_ltm = c( auc_ltm, res_case[,i]$toc_ltm@AUC )
    
    pcm_ltm = c( pcm_ltm, mean(res_case[,i]$pcm_ltm))
    
    ER_ltm = c(ER_ltm, round((res_case[,i]$cm_ltm[1,2] + res_case[,i]$cm_ltm[2,1]) * 100 / sum(res_case[,i]$cm_ltm),3) )
    
  }
  
  print(auc_ltm)
  
  print(ER_ltm)
  
  print(pcm_ltm)
  
  auc_ltmMSD = list(mean = mean(auc_ltm) , sd = sd(auc_ltm))
  pcm_ltmMSD = list(mean = mean(pcm_ltm), sd = sd(pcm_ltm))
  ER_ltmMSD = list(mean = mean(ER_ltm), sd = sd(ER_ltm))
  
  result_mean = c(AUC = auc_ltmMSD$mean, PCM = pcm_ltmMSD$mean, ER = ER_ltmMSD$mean) 
  
  result_sd =   c(AUC = auc_ltmMSD$sd, PCM = pcm_ltmMSD$sd, ER = ER_ltmMSD$sd) 
  
  # row.names(result_mean) <- c("res_ltm_mean")
  # row.names(result_sd) <- c("res_ltm_sd")
  
  result = list(mean = result_mean, sd = result_sd)
  
  return(result)
}


