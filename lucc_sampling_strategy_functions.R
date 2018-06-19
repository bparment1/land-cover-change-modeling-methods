# list of steps 
# 1. read the dataset
# 2. normalization of inputs
# 3. split it in L and T with a stratified random sampling
# 4. learn on L and test on T using an optimal threshold, (or by fixing the number of changed cells). 
# 6. compute metrics: FP, FN, TP, and TN 
# 7. plot ROC, TOC

########################################## Here the functions 

#################################################################################### 
# function used to read the data input file and returns the dataframe
#################################################################################### 
fn.read <- function(inputFile){
  
  # read LAND USE dataset 
  inputDF <- read.csv(file=inputFile,sep=",",header=T)
  # check the number of observations
  nrow(inputDF)
  # check the structure of the dataset
  str(inputDF)
  # rename the dataframe columns
  # colnames(inputDF) <- c("square_id","time_interval","country_code","sms_in_activity","sms_out_activity","call_in_activity","call_out_activity","internet_traffic_activity")
  return(inputDF)
}

# read a mat file 

loadMat = function(fileName)
{
  rawData = readMat(fileName)
  data = rawData[[1]]
  return(data)
}


#################################################################################### 
# normalization of inputs
#################################################################################### 

normalize <- function(x){  
  x = ( x - min(x) ) / ( max(x) - min(x) )
  return(x)
}


fn.normalize = function(data, xvr, yvr){ 
  dataN = apply(data[,xvr], 2, normalize)
  data[,xvr] = dataN
  return(data)
  # cbind(dataN, data[,yvr]) )
}


#################################################################################### 
# data split
#################################################################################### 

balanced_dataset  <- function(change, no_change, xvr, yvr){
  spl_ratio = round((nrow(change) * 100) / nrow(no_change))
  r = splitdt( no_change, spl_ratio)
  result= list(change = change, no_change = r$L)
  return(result)
}

# random split to L and T sets 
splitdt <- function( dt, yvr, spl){
  # this function serves to split the dat into training, and testing sets 
  # dat sets. It draws randomly
  # trainingPart and testPart are the share of the full datSet dedicated 
  # for training and test  Data set.
  
  ##  From percentage to fraction
  while (spl >= 1) 
    spl <- spl/100 
  
  set.seed(10)
  
  smp <- runif(nrow(dt)) < spl
  L = dt[smp,] 
  T = dt[!smp,]
  
  ###########################################################################
  #training , and testing sets construction with a random draw 
  #without repitition
  ###########################################################################
  r <- list(L=L, T=T, yvr=yvr)
  return(r)
}

fn.splitSRS <-function(inputFile, yvr, ratio){ 
  # simple random sampling 
  # ratio between 0.1 and 0.9 
  # yvr: index of outcome
  
  set.seed(10)
  
  index <- sample(1:nrow(inputFile), round(ratio*nrow(inputFile)))
  train <- inputFile[index,]
  test <- inputFile[-index,]
  
  LT = list(L = train, T = test)
  return(LT)  
}

stratified = function(df, group, size) { 
  
  #  USE: * Specify your data frame and grouping variable (as column 
  #         number) as the first two arguments.
  #       * Decide on your sample size. For a sample proportional to the
  #         population, enter "size" as a decimal. For an equal number 
  #         of samples from each group, enter "size" as a whole number.
  #
  #  Example 1: Sample 10% of each group from a data frame named "z",
  #             where the grouping variable is the fourth variable, use:
  # 
  #                 > stratified(z, 4, .1)
  #
  #  Example 2: Sample 5 observations from each group from a data frame
  #             named "z"; grouping variable is the third variable:
  #
  #                 > stratified(z, 3, 5)
  #
  g1 = df[df[,group]==1,]
  g0 = df[df[,group]==0,]
  nrow(g1)
  nrow(g0)

  LT1 = splitdt(g1, 1, 1-size)
  LT0 = splitdt(g0, 0, size)

  L1 = LT1$L
	T1 = LT1$T 

      L0 = LT0$L
	T0 = LT0$T 
  train = rbind(L1, L0)
  test = rbind(T1, T0)

  LT = list(L = train, T = test)
  return(LT)  
}


stratified_eqP = function(df, group, size) { 

  g1 = df[df[,group]==1,]
  g0 = df[df[,group]==0,]
  nrow(g1)
  nrow(g0)

  LT1 = splitdt(g1, 1, size)
  LT0 = splitdt(g0, 0, size)

  L1 = LT1$L
	T1 = LT1$T 

  L0 = LT0$L
	T0 = LT0$T 
  train = rbind(L1, L0)
  test = rbind(T1, T0)

  LT = list(L = train, T = test)
  return(LT)  
}


stratified_invP = function(df, group, size) { 
  

  g1 = df[df[,group]==1,]
  G0 = df[df[,group]==0,]

  LT1 = splitdt(g1, 1, size)
  L1 = LT1$L
	T1 = LT1$T 

  n = nrow(L1)

  mysample <- sample(length(G0), n, replace=F)
  g0 <- G0[mysample]
  LT0 = splitdt(g0, 0, size)

  L0 = LT0$L
	T0 = LT0$T 
  train = rbind(L1, L0)
  test = rbind(T1, T0)

  LT = list(L = train, T = test)
  return(LT)  
}

#################################################################################### 
# learn and test
#################################################################################### 

#### functions 

LTM_calibration <- function(trainingSetj, xvrN, yvr, m) {
  
  #
  # xvrC 
  # Detailed explanation goes here
  # nb de neurones dans la couche cach?e
  #  trainingPart=80;

  library("nnet") # load nnet library 

  xx <- trainingSetj[,xvrN]
  yy <- trainingSetj[,yvr]
  m = length(xvrN)

  ## dummies = table(1:length(yy),as.factor(yy))  

  ### CHECK from this LINE #################################################################

  # mynnet  <- train( trainingSetj[,yvr] ~ trainingSetj[,xvrN], data = trainingSetj,
  # method = "nnet", size=m, decay = 0.1)
  # , maxit = nbitemax, decay = 1.0e-5, size= m, trace = F, linout = F)

  mynnet <- nnet(xx, yy, 
			size = m, # yy instead of dummies 
                  softmax = F, # T
                  rang = 0.1, 
                  maxit = 2000, 
                  model=TRUE,
                  decay=1.0e-5)

  return(mynnet)
}

decision <- function(y, th) {
  if (y >th)
    decision = 1
  else 
    decision =0
  return(decision)
}

LTM_validation <- function(mynnet, T, xvrN, yvr, th) { # to check this function 

  mynnet.predict <- predict(mynnet, T[,xvrN], type="raw")
  
  pb2 = mynnet.predict # [,2]
  # dim(pb2) = c(nrow(mynnet.predict),1)
  
  # yhat <- apply(pb2, 1, decision, th) # which.max
  
  # mat = table(T[,yvr], yhat)
  
  # res <- list(cm=mat, yobs=T[,yvr], yhat=yhat, pb=pb2)
  return(pb2)
  
}

pcm_evaluation <- function(cm){
  sumL = apply(cm, 1, sum);
  sumC = apply(cm, 2, sum); 
  
  PCM_0 = cm[1,1] / sumL[1];
  
  if (ncol(cm) == 2) {
    PCM_1 = cm[2,2] / sumL[2];
  }
  if (ncol(cm) == 1) {
    PCM_1 = 0
  }
  return( round(c(pcm0 = PCM_0, pcm1 = PCM_1),3) )
}


metrics <- function(vect, i, j){
  val = 0 
  if ((vect[i] == vect[j]) & (vect[i] ==1) )
    val = 1  # tp
  else 
    if ((vect[i] == vect[j]) & (vect[i] ==0) )
      val = 2 # tn
  else 
    if ((vect[i] != vect[j]) & (vect[i] ==0) )
        val = 3 # fp
  else 
    if ((vect[i] != vect[j]) & (vect[i] ==1) )
        val = 4 # fn
  return(val)
}

# k-means clustering 
cluster <- function(data, xvr, yvr, k){
  # data: dataset like L set
  # xvr: index of x
  # yvr: index of y
  # k: number of cluster
  cl_k <- kmeans(data[,xvr], k);
  tmp1 = list(data_cluster=cbind(data, cl_k$cluster), cluster_center=cl_k$centers)
  return(tmp1)
}
##############################################################################################

##############################
### LTM model 
##############################

fnLUCAnalysis <- function(l, t, xvr, yvr, m) {
  #Data preprocessing
  
  set.seed(2)
  nn = LTM_calibration(l, xvr, yvr, m) 
  
  prediction <- predict(nn, t[,xvr])
  colnames(prediction)<- c("prob")
  
  # mynnet.predict <- predict(mynnet, T[,xvrN], type="raw")
  
  # pb2 = mynnet.predict # [,2]
  # dim(pb2) = c(nrow(mynnet.predict),1)
  
  # yhat <- apply(pb2, 1, decision,ls th) # which.max
  
  tt <- cbind(t, prediction ) # we use the entire dataset for generating the error map. 
  # tt = data.frame(tt) 
  # attach(tt)
  
  # sort by prob (probability value)
  
  N = sum(t[,yvr])     # number of changed cells 
  
  newt <- tt[order(-tt[,yvr+1]),] #  prob
  
  ypred = rep(0, nrow(t)) # check ypred # 
  ypred[1:N] = 1
  
  newt = cbind(newt, ypred ) # list(XY = newt, Yhat = ypred ) # check prob # distribution of prob
  
  return(newt)    # new table with all results 
  
}  
##############################################################################################

fnLUCAnalysis_maxP <- function(l, t, xvr, yvr, m, th) {
  #Data preprocessing
  
  set.seed(2)
  nn = LTM_calibration(l, xvr, yvr, m) 
  
  ## prediction <- predict(nn, t[,xvr])
  ## colnames(prediction)<- c("prob")
  # mynnet.predict <- predict(nn, t[,xvr]) # , type="raw")
  # pb2 = mynnet.predict # [,2]
  # dim(pb2) = c(nrow(mynnet.predict),1)
  
  pb2 <- predict(nn, t[,xvr], type="raw") 
  print(summary(pb2))
  
  ypred <- apply(pb2, 1, decision, th) # which.max
  
  newt <- cbind(t, pb2) # we use the entire dataset for generating the error map. 
  # tt = data.frame(tt) 
  # attach(tt)
  
  # sort by prob (probability value)
  
  ## N = sum(t[,yvr])     # number of changed cells 
  ## newt <- tt[order(-tt[,yvr+1]),] #  prob
  ## ypred = rep(0, nrow(t)) # check ypred # 
  ## ypred[1:N] = 1
  
  newt = cbind(newt, ypred ) # list(XY = newt, Yhat = ypred ) # check prob # distribution of prob
  
  return(newt)    # new table with all results 
  
}  


##############################
### LTM-Cluster model 
##############################
ltm_cluster <- function(learn, test, xvr, yvr, K){

L_with_clusters <- cluster(learn, xvr, yvr, K)   ####### cluster L into k clusters/groups 

test = cbind(test, rep(0, nrow(test)))

# in here, T set has same nb of columns as L_with_clusters, by adding Null values at the end of T columns 

mybiglist <- vector('list', K)
names(mybiglist) <- paste0('k:', seq_along(mybiglist))

res_ltm_cluster = rep(0, K);
res_ltm_cluster_lt = rep(0, K);

for (k in seq_along(mybiglist)){ # k from 1 to 3
lk = L_with_clusters$data_cluster[L_with_clusters$data_cluster[,(yvr+1)]==k, ]

centroids_lk = L_with_clusters$cluster_center

euc.dist <- function(x1, x2) {
sqrt(sum((x1 - x2) ^ 2))
}

### 
# cl_t = c()
# for(cell in 1:nrow(test)){ 
# dist = rep(0, K)
# for(cl in 1:K){
# dist[cl]= euc.dist(T[cell, xvr], centroids_lk[cl,])
#         }
# dist = pdist( t(T[, xvr]), t(centroids_lk) )
# kopt = which.min(dist) # cluster number "kopt" is the closest one to the studied cell in learning set 
# cl_t <- c(cl_t, kopt)
# }

dist = rdist(test[, xvr], centroids_lk)

cl_t = apply(dist, 1, which.min ) 

test[,ncol(test)] = cl_t     # adding class number to the T set in the last column 

tk = test[cl_t == k,] 

### 
threshold_cl = round(nrow(tk[tk[,yvr]==1,]) / nrow(tk), 2)
print("th = ") 
print(threshold_cl) 

newtk = fnLUCAnalysis_maxP(lk, tk, xvr, yvr, m, threshold_cl)

## newtk = fnLUCAnalysis(lk, tk, xvr, yvr, m)

tmp <- list(r_k = newtk)  

mybiglist[[k]] <- tmp
}
return(mybiglist) 
}
##############################################################################################


# xcoord_index = 2
# ycoord_index = 3 
###############################
#### PLOTS 
#################################################################
mapping123 <- function(xcoord_index, ycoord_index){

  ############################### map of LUC of Boston area (B)
  graphics.off()
  color = c("red", "grey50")
  #tiff(filename = "Figures-Ems paper/SEWI/LUC_bis.tiff", units="in", width=5, height=7, res=1500, bg = "white", compression = 'lzw')
  pdf("Figures-Ems paper/SEWI/LUC_bis.pdf", pointsize=11) # , width=5.04, height=7.54) #  width=5.5, height=2.6
  par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))
  plot(change[, xcoord_index], change[, ycoord_index], col="red", cex=0.1, xlab="", ylab="", main="(C)")
  points(no_change[, xcoord_index], no_change[, ycoord_index], col="grey50", type="p", cex=0.01, xlab="", ylab="")
  # points(exc_layer[,2], exc_layer[,3], col="grey90", type="p", cex=0.01, xlab="", ylab="")
  legend("topright", c("Urban-gain", "Non-urban persistence"), col=color, pch=15, cex=1)
  dev.off()
  ###############################
  
  ############################### map of LUC of Muskegon area with cells in three clusters 
  graphics.off()
  color =  c(heat.colors(3))
  tiff(filename = "Figures-Ems paper/SEWI/LUC-with-clusters_bis.tiff", units="in", width=5, height=7, res=1500, bg = "white", compression = 'lzw')
  par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))
  newT_cl = as.matrix(newT_cl)
  plot(newT_cl[, xcoord_index], newT_cl[, ycoord_index], col=color[newT_cl[,yvr+1]], type="p", cex=0.1, xlab="", ylab="", main="(C)")
  # points(exc_layer[,2], exc_layer[,3], col="grey90", type="p", cex=0.1, xlab="", ylab="")
  legend("topright", c("Cluster 1", "Cluster 2", "Cluster 3"), col=color, pch=15, cex=1)
  # hits (H), misses (M), false alarms (FA), and correct rejections (CR)
  dev.off()
  ###############################
  
  ############################### Error map from the LTM model using the Muskegon data
  graphics.off()
  color = c("red", "darkorchid4", "green", "darkgoldenrod1") # "grey90"
  # png("Figures-Ems paper/error-map_LTM_allcells.png") 
  # pdf("Figures-Ems paper/error-map_LTM_allcells.pdf", pointsize=11) # , width=2.6, height=5.5) #  width=5.5, height=2.6
  tiff(filename = "Figures-Ems paper/SEWI/error-map_LTM_allcells.tiff", units="in", width=5, height=7, res=1500, bg = "white", compression = 'lzw')
  par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))
  newT = as.matrix(newT)
  plot(newT[, xcoord_index], newT[, ycoord_index], col=color[errors], type="p", cex=0.1, xlab="", ylab="", main="(A)")
  # points(exc_layer[,2], exc_layer[,3], col="grey90", type="p", cex=0.1, xlab="", ylab="")
  legend("topright", c("H", "CR", "FA", "M"), col=color, pch=15, cex=1)
  # hits (H), misses (M), false alarms (FA), and correct rejections (CR)
  dev.off()
  ###############################
  
  
  ############################### Error map from the LTM-Cluster model using the Muskegon data
  newT_cl1 = as.matrix(newT_cl)
  graphics.off()
  color = c("red", "darkorchid4", "green", "darkgoldenrod1")
  # png("Figures-Ems paper/error-map_LTM_allcells.png") 
  # pdf("Figures-Ems paper/error-map_LTM_allcells.pdf", pointsize=11) # , width=2.6, height=5.5) #  width=5.5, height=2.6
  tiff(filename = "Figures-Ems paper/SEWI/error-map_LTM-Cluster_allcells.tiff", units="in", width=5, height=7, res=1500, bg = "white", compression = 'lzw')
  par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))
  # plot(allN[, xcoord_index], allN[, ycoord_index], col="gray", cex=0.1, xlab="", type="b", ylab="", main="(B)")
  plot(newT_cl1[, xcoord_index], newT_cl1[, ycoord_index], col=color[errors_ltm_cluster], type="p", cex=0.1, xlab="", ylab="", main="(B)")
  # points(exc_layer[,2], exc_layer[,3], col="grey90", type="p", cex=0.1, xlab="", ylab="")
  legend("topright", c("H", "CR", "FA", "M"), col=color, pch=15, cex=1)
  # hits (H), misses (M), false alarms (FA), and correct rejections (CR)
  dev.off()
  ###############################
  
  ############################### TOC curve from the LTM model using the Muskegon data
  graphics.off()
  ##  Width of the figure:  5.5 inches;  height:  2.6 inches
  # pdf("Figures-Ems paper/toc-curve_LTM_allcells.pdf", pointsize=11) #  width=5.5, height=2.6
  # png("Figures-Ems paper/toc-curve_LTM_allcells.png") # , width=5.5, height=5, pointsize=11)
  tiff(filename = "Figures-Ems paper/SEWI/toc-curve_LTM_allcells.tiff", units="in", width=5, height=5, res=1500, bg = "white", compression = 'lzw')
  # filename = "Figures-Ems paper/toc-curve_LTM_allcells.tiff", units="in", width=5, height=5, res=300)
  #     units = "px", pointsize = 2,
  #      bg = "white", res = 800)
  par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))
  plot(tocd, main="(A)") # TOC for the LTM model (a)
  dev.off()
  ###############################
  
  ############################### TOC curve from the LTM-Cluster model using the Muskegon data
  
  graphics.off()
  ##  Width of the figure:  5.5 inches;  height:  2.6 inches
  # pdf("Figures-Ems paper/toc-curve_LTM_allcells.pdf", pointsize=11) #  width=5.5, height=2.6
  # png("Figures-Ems paper/toc-curve_LTM_allcells.png") # , width=5.5, height=5, pointsize=11)
  tiff(filename = "Figures-Ems paper/SEWI/toc-curve_LTM-Cluster_allcells.tiff", units="in", width=5, height=5, res=1500, bg = "white", compression = 'lzw')
  # filename = "Figures-Ems paper/toc-curve_LTM_allcells.tiff", units="in", width=5, height=5, res=300)
  #     units = "px", pointsize = 2,
  #      bg = "white", res = 800)
  par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))
  plot(tocd_ltm_cl, main="(B)") # TOC for the LTM-Cluster model (a)
  dev.off()
  ###############################
  
}

################################## end of mapping  ##################################
