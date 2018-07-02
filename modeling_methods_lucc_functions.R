####################################   Land cover change methods  #######################################
############################  Predicting and evaluating models for LUCC and other applications  #######################################
#This script contains functionto model data using the random forest and logistic models.
#
#
#AUTHORS: Benoit Parmentier,Hichem Omrani                                            
#DATE CREATED: 05/09/2018 
#DATE MODIFIED: 06/25/2018
#Version: 1
#PROJECT: LUCC LISER modeling
#TO DO: provide options and functions to take in raster inputs
#
#COMMIT: reorganization of code
#
#################################################################################################

###### Library used

library(gtools)                              # loading some useful tools 
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gdata)                               # various tools with xls reading, cbindX
library(rasterVis)                           # Raster plotting functions
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(plyr)                                # Various tools including rbind.fill
library(spgwr)                               # GWR method
library(rgeos)                               # Geometric, topologic library of functions
library(gridExtra)                           # Combining lattice plots
library(colorRamps)                          # Palette/color ramps for symbology
library(ggplot2)                             # Plot package 
library(lubridate)                           # Date utility fuctions
library(dplyr)                               # data manipulation and wrangling
library(ROCR)                                # ROC curve package
library(pROC)                                # prob ROC curve
library(TOC)                                 # TOC and ROC curve package
library(randomForest)                        # random forests
library(lattice)                             # Plot package
library(caret)                               # Modeling with assessment hold outs, CV folds and data splitting
library(gplots)                              # Plot package
library(sf)
library(ranger)                              # efficient random forest with parallelization   
#
run_logistic_fun <- function(i,model_formula_str,data_df){
  #Function to run logistic model
  #We use glm in this implementation.
  
  ### Begin script
  
  data_input<-data_df[[i]]; 
  mod_glm <-  glm(model_formula_str, data = data_input,family=binomial());
  
  return(mod_glm)
}

run_random_forest_fun <- function(i, model_formula_str, data_df,
                                  ntree=100,nodesize=5,num_cores=1){
  #Function to run random forest model
  #We use randomForest in this implementation.
  
  ### Begin script
  
  data_input<-data_df[[i]] 
  
  #memory.limit(10 * 10^10) #windows specific, think about that option later
  
  #mod_rf <- randomForest(as.formula(model_formula_str),
  #                       type="classification",
  #                       data=data_input,
  #                       importance = TRUE, 
  #                       ntree = ntree,
  #                       nodesize= nodesize,
  #                       proximity=TRUE) 
  
  mod_rf <- ranger(formula=as.formula(model_formula_str), 
                      data = data_input, 
                      num.trees = ntree, 
                      write.forest = TRUE, # save ranger.forest object, this is required for prediction 
                      probability = FALSE, 
                      num.threads = num_cores, 
                      importance="none", 
                      save.memory = FALSE)
  
  #rf.ranger <- ranger(dependent.variable.name="class", 
  #                    data = data1, 
  #                    num.trees = 1001, mtry = 236, 
  #                    write.forest = FALSE, 
  #                    probability = FALSE, 
  #                    num.threads = 2, 
  #                    importance="none", 
  #                    save.memory = TRUE)
  
  #Predict with new or similar data, need to ask for probability otherwise the output is {0,1}
  #predicted_rf_mat <- predict(mod_rf, data=data, type="prob")
  
  return(mod_rf)
}

predict_logistic_val <- function(i,list_mod,data_testing){
  #Predictions using random forest model
  #
  data_v<-data_testing[[i]] 
  mod_glm <- list_mod[[i]]
  predicted_val <- predict(mod_glm,newdata=data_v,type='response')
  return(predicted_val)
}

predict_random_forest_val <- function(i,list_mod,data_testing,model_param){
  #
  #Goal: Use fitted model from "ranger" package to predict using random forest method
  #AUTHORS: Benoit Parmentier
  #CREATED: 05/09/2018
  #MODIFIED: 07/01/2018
  
  ###### Start script #######
  
  if(!is.null(model_param)){
    predict_all <- model_param$predict_all #Keep prediction for every trees 
  }else{
    predict_all=F
  }
  
  data_v<-data_testing[[i]]; 
  mod_rf <- list_mod[[i]];
  #predicted_rf_mat <- predict(mod_rf, data=data_v, type="prob");
  predicted_rf_obj <- predict(mod_rf,
                              data=data_v,
                              predict.all= predict_all,
                              type='response') #for ranger package random forest
  
  if(predict_all==TRUE){
    predicted_df <- (predicted_rf_obj$predictions) - 1 #reclassifify in 0-1
    predicted_val <- rowMeans(predicted_df)
  }else{
    predicted_val <- (predicted_rf_obj$predictions)
    predicted_df <- NULL
  }
  #dim(predict_all)
  #predict_all=T
  histogram(predicted_val)

  predicted_obj <- list(predicted_val,predicted_df,predicted_rf_obj)
  names(predicted_obj) <- c("predicted_val","predicted_df","predicted_rf_obj")
  
  return(predicted_obj)
}

run_model_fun <- function(data_df,model_formula_str,model_opt,model_param=NULL,data_testing=NULL,num_cores=1,out_dir=".",out_suffix=""){
  #
  # Goal: Function to run models using logistic and randomForest methods.
  #
  # CREATED: 05/09/2018
  # MODIFIED: 06/28/2018
  # AUTHORS: Benoit Parmentier
  # INPUTS:
  # 1) data_df: input data.frame with data used in modeling
  # 2) model_formula_str: modele formula 
  # 3) model_opt: "logistic","randomForest"
  # 4) model_param: parameters for the models (e.g. nodesize etc.)
  # 5) out_dir: output directory
  # 6) out_suffix: output suffix
  # OUTPUTS
  # 1)
  
  ###################### Start script #################
  
  ### Make a list if data is not a list:
  if(class(data_df)!="list"){
    data_df <- list(data_df)
  }
  
  if(!is.null(data_testing)){
    if(class(data_testing)!="list"){
      data_testing <- list(data_testing)
    }
  }
  
  if(model_opt=="logistic"){
    
    #browser()
    #debug(run_logistic_fun)
    #list_mod <- run_logistic_fun(1,model_formula_str,data_df)
    
    list_mod <- mclapply(1:length(data_df),
                         FUN=run_logistic_fun,
                         model_formula_str=model_formula_str,
                         data_df=data_df,
                         mc.preschedule = FALSE,
                         mc.cores =num_cores)
    #mclapply used to return a list that is the same length as the x component (in this case x=1:length)
    #mc.preschedule=False because there aren't large numbers of x values 
    ### Apprend prediction to training data.frame!!!
    
    if(!is.null(data_testing)){
      #
      
      list_predicted_val <- mclapply(1:length(data_df),
                                     FUN=predict_logistic_val,
                                     list_mod=list_mod,
                                     data_testing=data_testing,
                                     mc.preschedule = FALSE,
                                     mc.cores =num_cores)
      
    }else{
      list_predicted_val <- NULL
    }
    
  }
  
  #browser()
  if(model_opt=="randomForest"){
    if(!is.null(model_param)){
      ntree<- model_param$ntree
      nodesize <- model_param$nodsize
    }else{
      ntree <- 100
      node_size<-1
    }
    #ntree <- 30
    #nodesize <- 200
    #nodesize <- 500
    #nodesize <- 1000
    
    #undebug(run_random_forest_fun)
    
    #list_mod <- run_random_forest_fun(1,model_formula_str,data_df,ntree=ntree,nodesize = nodesize)
     
    #list_mod <- mclapply(1:length(data_df),
    #                     FUN= run_random_forest_fun,
    #                     model_formula_str=model_formula_str,
    #                     data_df=data_df,
    #                     ntree=ntree,
    #                     nodesize=nodesize,
    #                     mc.preschedule = FALSE,
    #                     mc.cores =num_cores)
    
    #Error in mcfork(detached) : 
    #  unable to fork, possible reason: Cannot allocate memory
    list_mod <- lapply(1:length(data_df),
                         FUN= run_random_forest_fun,
                         model_formula_str=model_formula_str,
                         data_df=data_df,
                         ntree=ntree,
                         nodesize=nodesize,
                         num_cores=num_cores)
    browser()
    model_param_predict <- list(predict_all=TRUE) #if you want to retain perdiction by trees
    list_fitted_val <- mclapply(1:length(data_df),
                                 FUN=predict_random_forest_val,
                                 list_mod=list_mod,
                                 data_testing=data_df,
                                 model_param=model_param_predict,
                                 mc.preschedule = FALSE,
                                 mc.cores =num_cores)
    
    y_fitted_soft <- lapply(list_fitted_val,function(x){x$predicted_val})
    #mc.preschedule = FALSE,
    #mc.cores =num_cores)
    if(!is.null(data_testing)){
      #
      browser()
      #debug(predict_random_forest_val)
      #test <- predict_random_forest_val(1,list_mod=list_mod,data_testing=data_testing,model_param)
      
      #list_predicted_val <- mclapply(1:length(data_df),
      #                               FUN=predict_random_forest_val,
      #                               list_mod=list_mod,
      #                               data_testing=data_testing,
      #                               model_param=model_param,
      #                               mc.preschedule = FALSE,
      #                               mc.cores =num_cores)
      
      model_param_predict <- list(predict_all=TRUE) #if you want to retain perdiction by trees
      list_predicted_val <- lapply(1:length(data_df),
                                     FUN=predict_random_forest_val,
                                     list_mod=list_mod,
                                     data_testing=data_testing,
                                     model_param=model_param_predict)
                                     #mc.preschedule = FALSE,
                                     #mc.cores =num_cores)
      
    } else{
      list_predicted_val <- NULL
    }
  } 
  
  ####### Prepare return object 
  
  model_obj <- list(list_mod,list_predicted_val,y_fitted_soft,data_df,data_testing)
  names(model_obj) <- c("mod","predicted_val","y_fitted_soft","data_training","data_testing")
  return(model_obj)
} 


ROC_evaluation_fun <- function(i,list_data,y_var,predicted_val,save_fig=T,out_suffix="",out_dir="."){
  
  ### Now do accuracy assessment:
  
  data_df <- list_data[[i]]
  dataset_name <- names(list_data)[i]
  #mod$fitted.values #these are the probability values from ROC
  #data_df[,y_var]
  #table(data_df[,y_var])
  
  mask_val <- 1:length(predicted_val[[i]])
  
  ##convert factor to numeric vector for reference values
  y_ref <- as.numeric(as.character(data_df[[y_var]])) #boolean reference values
  index_val <- predicted_val[[i]] #probabilities
  
  rocd2 <- ROC(index=index_val, 
               boolean=y_ref, 
               mask=mask_val, 
               nthres = 100)
  
  AUC_val <- slot(rocd2,"AUC") #this is your AUC from the logistic modeling
  
  ROC_table <- slot(rocd2,"table")
  
  
  #Plot ROC curve:
  
  if(save_fig==TRUE){
    
    out_suffix_str <- paste0(dataset_name,"_",out_suffix)
    
    res_pix<-480 #set as function argument...
    col_mfrow<-1
    #row_mfrow<-2
    row_mfrow<-1
    
    png_filename<- paste("Figure_","ROC_plot_",out_suffix_str,".png", sep="")
    
    png(filename=file.path(out_dir,png_filename),
        width=col_mfrow*res_pix,height=row_mfrow*res_pix)
    par(mfrow=c(row_mfrow,col_mfrow))
    
    plot(rocd2) #add option later...
    dev.off()
  }
  
  ###### prepare object ####
  roc_obj <- list(AUC_val,ROC_table,png_filename)
  names(roc_obj) <-c("AUC_val","ROC_table","png_filename")
  return(roc_obj)
  
}

generate_change_from_quantity <- function(y_predicted,data_df,y_obs){
  #
  #
  
  ###############
  
  df_summary <- as.data.frame(table(as.numeric(data_df[,y_obs])))
  quantity_change <- df_summary[2,2]
  
  #quantity_change <- sum(as.numeric(L_df[,yvr])) #sum of ones
  ##Not efficient with large dataset    
  df_val <- as.data.frame(y_predicted)
  df_val$ID <- 1:length(y_predicted)
  
  df_val <- arrange(df_val,desc(y_predicted))
  #y_predicted_ranked <- sort(y_predicted)
  #y_fitted_ranked <- sort(y_fitted)
  
  #y_predicted_hard <- y_predicted_ranked[1:quantity_change] 
  #y_fitted_hard <- y_fitted_ranked[1:quantity_change] 
  
  df_val$y_predicted_hard <- 0
  df_val$y_predicted_ranked <- 1:length(y_predicted)
  
  df_val$y_predicted_hard <- df_val$y_predicted_ranked > quantity_change
  df_val$y_predicted_hard <- as.numeric(df_val$y_predicted_hard)
  
  return(df_val)
}

############################### END OF SCRIPT ################################