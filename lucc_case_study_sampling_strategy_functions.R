
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

read_data <- function(matfile, xvr, yvr, IndLU_t1, IndLU_t2, CodeNU, CodeU){

	   inputFile1 = paste("data/", matfile, sep="") 
         data1 = loadMat(inputFile1)
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
# r$change, r$n_ch

dataset1 = rbind( cbind(change1, rep(1, nrow(change1)) ), cbind(no_change1, rep(0, nrow(no_change1) ))) ### 

dataset1N <- fn.normalize(dataset1, xvr, yvr) # dataset1 

	# data split  according to the sampling strategy 

# LT  = eval(as.call(list(as.name(sampling_name), dataset1N, yvr, ratio))) 
# FUN <- match.fun(sampling_name) 

LT = sampling_name(dataset1N, yvr, ratio)
T = LT$T 
L = LT$T
 
########################################
# LTM, 		without clustering prior learning 
########################################
# install.packages('TOC') # library(TOC)

##############################################################################################################################
###############################################################
# run the LTM model, without clustering prior learning 
###############################################################
##############################################################################################################################

#############
threshold = round(nrow(T[T[,yvr]==1, ]) / nrow(T), 2)  # T ; dataset1N
print("th = ") 
print(threshold) 
#############

newT = fnLUCAnalysis_maxP(L, T, xvr, yvr, m, threshold) # dataset1N; dataset1N, T, rbind(L,T)
###

cm = table(newT[,yvr], newT[,yvr+2]) 
print("cross-tab from the LTM model: ")
print(cm)

errors = apply(newT, 1, metrics, yvr, yvr+2)

# Total Operating Characteristic - TOC and ROC curves

tocd <- TOC(newT[,yvr], newT[,yvr+2], mask=NULL, nthres = 100)

pcm = pcm_evaluation(cm)
# mean(pcm)

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


