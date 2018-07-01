####################################   Land cover change methods sampling study   #######################################
############################  Comparing methods and sampling strategy for LUCC  #######################################
#This script performs analyzes to examine the effect of sampling strategies on land cover change modeling performance.
#
#
#AUTHORS: Hichem Omrani, Benoit Parmentier                                             
#DATE CREATED: 05/09/2018 
#DATE MODIFIED: 07/01/2018
#Version: 1
#PROJECT: LUCC LISER modeling
#TO DO:
#
#COMMIT: reorganization of code
#
#################################################################################################

###Loading R library and packages                                                      

library(sp) # spatial/geographfic objects and functions
library(rgdal) #GDAL/OGR binding for R with functionalities
#library(gstat) #spatial interpolation and kriging methods
library(spdep) #spatial analyses operations, functions etc.
library(gtools) # contains mixsort and other useful functions
library(maptools) # tools to manipulate spatial data
library(parallel) # parallel computation, part of base package no
library(rasterVis) # raster visualization operations
library(raster) # raster functionalities
library(forecast) #ARIMA forecasting
library(xts) #extension for time series object and analyses
library(zoo) # time series object and analysis
library(lubridate) # dates functionality
library(colorRamps) #contains matlab.like color palette
library(rgeos) #contains topological operations
library(sphet) #contains spreg, spatial regression modeling
library(BMS) #contains hex2bin and bin2hex, Bayesian methods
library(bitops) # function for bitwise operations
library(foreign) # import datasets from SAS, spss, stata and other sources
library(gdata) #read xls, dbf etc., not recently updated but useful
library(classInt) #methods to generate class limits
library(plyr) #data wrangling: various operations for splitting, combining data
library(readxl) #functionalities to read in excel type data
library(psych) #pca/eigenvector decomposition functionalities
library(sf)

###### Functions used in this script

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#function_preprocessing_and_analyses <- "fire_alaska_analyses_preprocessing_functions_03102017.R" #PARAM 1
#function_analyses <- "exercise2_fire_alaska_analyses_functions_03232017.R" #PARAM 1
script_path <- "/media/dan/Data/land-cover-change-modeling-methods/scripts"
#source(file.path(script_path,function_preprocessing_and_analyses)) #source all functions used in this script 1.
#source(file.path(script_path,function_analyses)) #source all functions used in this script 1.

# Call all needed functions used to perform land use analysis, data normalisation, data split, calibration, validation, mapping ... 

source(file.path(script_path,"needed_functions1_06292018.R"))
source(file.path(script_path,"main_for_3studies_07012018.R")) 
source(file.path(script_path,"mapping_05092018.R"))
source(file.path(script_path,"modeling_methods_lucc_functions_07012018.R"))

#####  Parameters and argument set up ###########

# setwd("//crc/Mildred/Mobility/Hichem/EMS-big data paper-March2018/R codes-EMS paper/")

in_dir <- "/media/dan/Data/land-cover-change-modeling-methods/data"
out_dir <- "/media/dan/Data/land-cover-change-modeling-methods/outputs"

#setwd("//crc/profiles/RedirectFolders/hichem/Desktop/EMS-big data paper-March2018/R codes-EMS paper/")

Muskegon_data_file_name<- "MuskegonData.mat"
Boston_data_file_name <- "Boston_dataset123.mat"
Boston_data_file_name <- "change_and_no_change_sewi.mat" 

#region coordinate reference system: we need to find out this information for each region/site
CRS_reg <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

file_format <- ".tif" #PARAM5
NA_flag_val <-  -9999 
out_suffix <-"ltm_and_sampling_06252018" #output suffix for the files and ouptu folder #PARAM 8
create_out_dir_param=TRUE #PARAM9

################# START SCRIPT ###############################

### PART 1 : READ AND PREPARE DATA FOR ANALYSES #######

## First create an output directory

if(is.null(out_dir)){
  out_dir <- dirname(in_dir) #output will be created in the input dir
}

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix_s)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

##############
## PART 2: process and analyse Muskegon 
##############

### We need to change this input and use a shapefile or series of tiff with the geographic coordinates
#r = read_data("MuskegonData.mat", 6:11, 14, 4, 5, 0, 1) 
#undebug(read_data)

#r <- read_data(file.path(in_dir,Muskegon_data_file_name),6:11, 14, 4, 5, 0, 1)

#this is in main_for_3studies_05092018.R

r <- read_data(matfile=Muskegon_data_file_name, #matlabfilename
          xvr=6:11, # indices for columns containing covariates
          yvr=14, # indices for outcome variable
          IndLU_t1=4, # land cover time 1
          IndLU_t2=5, # land cover time 2
          CodeNU=0, # code pour categorie non urbain
          CodeU=1, # code pour categorie urbain
          in_dir=in_dir) #input dir
  
class(r)  
#View(r[[1]])
N = 1  # number of  replications 

class(r$ch)

muskegon_data_df <- as.data.frame(r$ch)
dim(r$ch) #what is the last column/

names(muskegon_data_df) <- c("id","x_coord","y_coord","LU78","LU98","x1","x2","x3","x4","x5","x6","x7","y_var")
head(muskegon_data_df)

#debug(run_ltm)

names(r)
head(r$ch) #change data
#xvr : covariates?
#  yvr: response?
#  indLU_t1: land cover type at time T1
#IndLU_t2: land cover type at time T2
#codeNU: ?
#  codeU: ?
#run_ltm <- function(change1, no_change1, xvr, m, yvr, ratio, K, sampling_name) { # IndLU_t1, IndLU_t2, CodeNU, CodeU
  
test <- run_ltm(change1=r$ch, 
                no_change1=r$no_ch, 
                xvr=6:11,  #covariates
                m=length(6:11), #?
                yvr=14, #label?
                ratio=0.7, #training testing ratio?
                K=3, #?
               sampling_name=splitdt)#sampling method/strategy? # Note that a function is passed here!!!

results_mus_RS = replicate(N, run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3, splitdt))
results_mus_SR_eqP = replicate(N, run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3, stratified_eqP))
results_mus_SR_invP = replicate(N, run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3, stratified_invP))

####

#debug(run_land_change_models)
names_col <- c("id","x_coord","y_coord","LU78","LU98","x1","x2","x3","x4","x5","x6","x7","y_var")
model_opt <- "logistic"
#model_opt <- "randomForest"

results_logistic_obj <- run_land_change_models(change1=r$ch, 
                       no_change1=r$no_ch, 
                       xvr=6:11,  #covariates
                       m=length(6:11), #?
                       yvr=14, #label?
                       ratio=0.7, #training testing ratio?
                       K=3, #?
                       sampling_name=splitdt, #sampling method/strategy? # Note that a function is passed here!!!
                       model_opt=model_opt,
                       data_df=NULL,
                       names_col=names_col,
                       out_dir=NULL,
                       out_suffix=NULL) 
  

#undebug(run_land_change_models)
names_col <- c("id","x_coord","y_coord","LU78","LU98","x1","x2","x3","x4","x5","x6","x7","y_var")
#model_opt <- "logistic"
model_opt <- "randomForest"

model_param <- list(ntree=100,node_size=1)

results_randomeForest_obj <- run_land_change_models(change1=r$ch, 
                                               no_change1=r$no_ch, 
                                               xvr=6:11,  #covariates
                                               m=length(6:11), #?
                                               yvr=14, #label?
                                               ratio=0.7, #training testing ratio?
                                               K=3, #?
                                               sampling_name=splitdt, #sampling method/strategy? # Note that a function is passed here!!!
                                               model_opt=model_opt,
                                               model_param = model_param,
                                               data_df=NULL,
                                               names_col=names_col,
                                               out_dir=NULL,
                                               out_suffix=NULL) 

##### Error here:
#> results_mus_SR_invP = replicate(N, run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3, stratified_invP))
#Show Traceback

#Rerun with Debug
#Error in runif(nrow(dt)) : invalid arguments 

result_mus_RS      = fct_result(results_mus_RS, N)
result_mus_SR_eqP  = fct_result(results_mus_SR_eqP, N)
result_mus_SR_invP = fct_result(results_mus_SR_invP, N)

result_1 <- rbind(SC1=result_mus_RS$mean, SC2=result_mus_SR_eqP$mean, SC3=result_mus_SR_invP$mean) # result[[i]]

# }

####


# color_cl = c("black", "darkgray", "lightgray") # c(heat.colors(3))

## mapping all needed maps for the Muskegon case study 
# mapping_luc(results_mus[,1]$change_ltm, results_mus[,1]$no_change_ltm, 2, 3, c("red", "grey50"), "Muskegon", "FigLucMus4", "(A)", c("Urban-gain", "Non-urban")) 
# mapping_cl(results_mus[,1]$newT_cl, 2, 3, 14, color_cl, "Muskegon", "FigLucMus4_cl", "(A)", c("Cluster 1", "Cluster 2", "Cluster 3")) 
# mapping_error_map(results_mus[,1]$newT, results_mus[,1]$errors_ltm, 2, 3, c("red", "darkorchid4", "green", "darkgoldenrod1"), "Muskegon", "FigErrorMapMus4", "(A)", c("H", "CR", "FA", "M"))
# mapping_error_map(results_mus[,1]$newT_cl, results_mus[,1]$errors_ltm_cluster, 2, 3, c("red", "darkorchid4", "green", "darkgoldenrod1"), "Muskegon", "FigErrorMapMus_cl4", "(B)", c("H", "CR", "FA", "M"))
# mapping_toc(results_mus[,1]$toc_ltm, "Muskegon", "FigTocMus4", "(A)")
# mapping_toc(results_mus[,1]$toc_ltm_cluster, "Muskegon", "FigTocMus_cl4", "(B)")
# mapping_2toc(results_mus[,1]$toc_ltm, results_mus[,1]$toc_ltm_cluster, "Muskegon", "FigTocMus_cl_2toc", "")


##############
### Boston 	       : to check from here 
##############

#source("main_for_3studies.R")

#source("mapping.R")
#r2 = read_data("Boston_dataset123.mat", 1:6, 11, 7, 8, 1, 2) 

Boston_data_file_name <- "Boston_dataset123.mat"

r2 <- read_data(matfile=Boston_data_file_name,
              xvr=1:6, 
              yvr=11, 
              IndLU_t1=7, 
              IndLU_t2=8, 
              CodeNU=1, 
              CodeU=2,
              in_dir=in_dir)

boston_data_df <- as.data.frame(r2$ch)
names(boston_data_df) <- c("id","x","y","LU78","LU98","x1","x2","x3","x4","x5","x6","x7")

head(boston_data_df)

#--- 
#  1. Boston dataset (cells with 2m resolution)
#--- 
#  x: xcoordiante
#y: ycoordinate
#values 1971: land use class in 1971 (1 for non-urban)
#values 1999: land use class in 1999 (1 for non-urban and 2 for urban)
#set of input drivers
#1. xden71
#2. xdetr71
#3. xdeu71
#4. xdewa71
#5. xdu71
#6. xdwa71

N = 1  # number of  replications 

# results_bos = replicate(N, run_ltm(r2$ch, r2$no_ch, 1:6, length(1:6), 11, 0.7, 3))
# IndLU_t1: 7; IndLU_t2=8; CodeNU= 1; CodeU= 2

results_mus_RS = replicate(N, run_ltm(r2$ch, r2$no_ch, 1:6, length(1:6), 11, 0.7, 3, splitdt))
results_mus_SR_eqP = replicate(N, run_ltm(r2$ch, r2$no_ch, 1:6, length(1:6), 11, 0.7, 3, stratified_eqP))
results_mus_SR_invP = replicate(N, run_ltm(r2$ch, r2$no_ch, 1:6, length(1:6), 11, 0.7, 3, stratified_invP))

result_mus_RS      = fct_result(results_mus_RS, N)
result_mus_SR_eqP  = fct_result(results_mus_SR_eqP, N)
result_mus_SR_invP = fct_result(results_mus_SR_invP, N)

result_2 <- rbind(SC1=result_mus_RS$mean, SC2=result_mus_SR_eqP$mean, SC3=result_mus_SR_invP$mean) # result[[i]]

## mapping all needed maps for the Boston case study 

# mapping_luc(results_bos[,1]$change_ltm, results_bos[,1]$no_change_ltm, 9, 10, c("red", "grey50"), "Boston", "FigLucBos4", "(B)", c("Urban-gain", "Non-urban")) 
# mapping_cl(results_bos[,1]$newT_cl, 9, 10, 11, c(heat.colors(3)), "Boston", "FigLucBos4_cl", "(B)", c("Cluster 1", "Cluster 2", "Cluster 3")) 
# mapping_error_map(results_bos[,1]$newT, results_bos[,1]$errors_ltm, 9, 10, c("red", "darkorchid4", "green", "darkgoldenrod1"), "Boston", "FigErrorMapBos4", "(A)", c("H", "CR", "FA", "M"))
# mapping_error_map(results_bos[,1]$newT_cl, results_bos[,1]$errors_ltm_cluster, 9, 10, c("red", "darkorchid4", "green", "darkgoldenrod1"), "Boston", "FigErrorMapBos_cl4", "(B)", c("H", "CR", "FA", "M"))
# mapping_toc(results_bos[,1]$toc_ltm, "Boston", "FigTocBos4", "(A)")
# mapping_toc(results_bos[,1]$toc_ltm_cluster, "Boston", "FigTocBos_cl4", "(B)")


##############
### Sewi 
##############

#--- 
#  3. South-Eastern-Wisconsin dataset (cells with 30m resolution)
#--- 
#  column1: xcoordinate
#column2: ycoordinate
#column3: LU in 1990
#column4: LU in 2000
#column5: LU in 2006

#column6: x1 (X=(x1, ..., x17) are the set of drivers in 1990)
#column7: x2
#column8: x3
#column9: x4
#column10: x5
#column11: x6
#column12: x7
#column13: x8
#column14: x9
#column15: x10
#column16: x11
#column17: x12
#column18: x13
#column19: x14
#column20: x15
#column21: x16
#column22: x17

#column23: Land use_t1 in 7 classs
#column24: LU in 1990 (0 for non-change and 1 for change) 
#column25: LU in 2000 (0 for non-change and 1 for change)
#column26: cluster ID 

r3 = read_data("change_and_no_change_sewi.mat", 6:22, 23, 4, 5, 0, 1) 

# results_sewi = replicate(N, run_ltm_and_ltmCluster(r3$ch, r3$no_ch, 6:22, length(6:22), 23, 0.7, 4, 5, 0, 1, 3))
# IndLU_t1: 4; IndLU_t2=5; CodeNU= 0; CodeU= 1

results_mus_RS = replicate(N, run_ltm(r3$ch, r3$no_ch, 6:22, length(6:22), 23, 0.7, 3, splitdt))
results_mus_SR_eqP = replicate(N, run_ltm(r3$ch, r3$no_ch, 6:22, length(6:22), 23, 0.7, 3, stratified_eqP))
results_mus_SR_invP = replicate(N, run_ltm(r3$ch, r3$no_ch, 6:22, length(6:22), 23, 0.7, 3, stratified_invP))

result_mus_RS      = fct_result(results_mus_RS, N)
result_mus_SR_eqP  = fct_result(results_mus_SR_eqP, N)
result_mus_SR_invP = fct_result(results_mus_SR_invP, N)

result_3 <- rbind(SC1=result_mus_RS$mean, SC2=result_mus_SR_eqP$mean, SC3=result_mus_SR_invP$mean) # result[[i]]


# result_bos = fct_result(results_sewi, N)
# print(result_sewi$mean)
# print(result_sewi$sd)

## mapping all needed maps for the SEWI case study 

# mapping_luc(results_sewi[,1]$change_ltm, results_mus[,1]$no_change_ltm, 9, 10, c("red", "grey50"), "SEWI", "FigLucSewi4", "(B)", c("Urban-gain", "Non-urban")) 
# mapping_cl(results_sewi[,1]$newT_cl, 9, 10, 11, c(heat.colors(3)), "SEWI", "FigLucSewi4_cl", "(B)", c("Cluster 1", "Cluster 2", "Cluster 3")) 
# mapping_error_map(results_sewi[,1]$newT, results_sewi[,1]$errors_ltm, 9, 10, c("red", "darkorchid4", "green", "darkgoldenrod1"), "SEWI", "FigErrorMapSewi4", "(A)", c("H", "CR", "FA", "M"))
# mapping_error_map(results_sewi[,1]$newT_cl, results_sewi[,1]$errors_ltm_cluster, 9, 10, c("red", "darkorchid4", "green", "darkgoldenrod1"), "SEWI", "FigErrorMapSewi_cl4", "(B)", c("H", "CR", "FA", "M"))
# mapping_toc(results_sewi[,1]$toc_ltm, "SEWI", "FigTocSewi4", "(A)")
# mapping_toc(results_sewi[,1]$toc_ltm_cluster, "SEWI", "FigTocSewi_cl4", "(B)")


###############################################  End of script #########################################



#--- 
#  2. Muskegon-County dataset (cells with 100m resolution)
#--- 
#  column1: id of cell
#column2: xcoordinate
#column3: ycoordinate
#column4: LU in 1978 (0 for non-urban and 1 for urban; LU in time 1978 could be added as input in the model)
#column5: LU in 1898 (0 for non-urban and 1 for urban)

#column6: x1 (X={x1, ..., x7} are the set of drivers in 1978)
#column7: x2
#column8: x3
#column9: x4
#column10: x5
#column11: x6
#column12: x7

#-------------------------------
#  # result <- list()
#  
#  # spl = seq(0.5, 0.8, 0.1)
#  
#  # for (i in 1:length(spl)) {
  