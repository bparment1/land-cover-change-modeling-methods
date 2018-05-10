####################################   Land cover change methods sampling study   #######################################
############################  Analyse using time series and fire locations  #######################################
#This script performs analyzes to examine the effect of sampling strategies on land cover change modeling performance.
#
#
#AUTHORS: Hichem Omrani, Benoit Parmentier                                             
#DATE CREATED: 05/09/2018 
#DATE MODIFIED: 05/09/2018
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
library(gstat) #spatial interpolation and kriging methods
library(readxl) #functionalities to read in excel type data
library(psych) #pca/eigenvector decomposition functionalities
library(sf)

###### Functions used in this script

#function_preprocessing_and_analyses <- "fire_alaska_analyses_preprocessing_functions_03102017.R" #PARAM 1
#function_analyses <- "exercise2_fire_alaska_analyses_functions_03232017.R" #PARAM 1
script_path <- "/home/bparmentier/c_drive/Users/bparmentier/Data/LISER/land-cover-change-modeling-methods/scripts"
#source(file.path(script_path,function_preprocessing_and_analyses)) #source all functions used in this script 1.
#source(file.path(script_path,function_analyses)) #source all functions used in this script 1.

# Call all needed functions used to perform land use analysis, data normalisation, data split, calibration, validation, mapping ... 

#source("needed_funtions1.R")
source(file.path(script_path,"needed_functions1_05092018.R"))

source(file.path(script_path,"main_for_3studies_05092018b.R"))
source(file.path(script_path,"mapping_05092018.R"))

#####  Parameters and argument set up ###########


##############
## Muskegon 
##############

Muskegon_data_file_name<- "MuskegonData.mat"

# setwd("//crc/Mildred/Mobility/Hichem/EMS-big data paper-March2018/R codes-EMS paper/")

in_dir <- "/home/bparmentier/c_drive/Users/bparmentier/Data/LISER/land-cover-change-modeling-methods/data"

#setwd("//crc/profiles/RedirectFolders/hichem/Desktop/EMS-big data paper-March2018/R codes-EMS paper/")
setwd(in_dir)

### We need to change this input and use a shapefile or series of tiff with the geographic coordinates
#r = read_data("MuskegonData.mat", 6:11, 14, 4, 5, 0, 1) 
undebug(read_data)

#r <- read_data(file.path(in_dir,Muskegon_data_file_name),6:11, 14, 4, 5, 0, 1)

#this is in main_for_3studies_05092018.R

r<- read_data(matfile=Muskegon_data_file_name,
          xvr=6:11, 
          yvr=14, 
          IndLU_t1=4, 
          IndLU_t2=5, 
          CodeNU=0, 
          CodeU=1,
          in_dir=in_dir)
  
class(r)  
View(r[[1]])
N = 1  # number of  replications 

# result <- list()


# spl = seq(0.5, 0.8, 0.1)

# for (i in 1:length(spl)) {

debug(run_ltm)
#debugging in: run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3, splitdt)
#debug at /home/bparmentier/c_drive/Users/bparmentier/Data/LISER/land-cover-change-modeling-methods/scripts/main_for_3studies_05092018b.R#67: {

names(r)
View(r$ch) #change data

test <- run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3,splitdt)


results_mus_RS = replicate(N, run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3, splitdt))
results_mus_SR_eqP = replicate(N, run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3, stratified_eqP))
results_mus_SR_invP = replicate(N, run_ltm(r$ch, r$no_ch, 6:11, length(6:11), 14, 0.7, 3, stratified_invP))


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

Boston_data_file_name <- "Boston_dataset123.mat"

r2 <- read_data(matfile=Boston_data_file_name,
              xvr=1:6, 
              yvr=11, 
              IndLU_t1=7, 
              IndLU_t2=8, 
              CodeNU=1, 
              CodeU=2,
              in_dir=in_dir)

r2 = read_data("Boston_dataset123.mat", 1:6, 11, 7, 8, 1, 2) 

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