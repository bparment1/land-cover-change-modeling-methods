

# function for data mapping

###############################
#### PLOTS of LUC in Muskegon 
#################################################################

mapping_luc <- function(change1, no_change1, index_xcoord, index_ycoord, color, Foldername, Filename, Figuretitle, legend_name)
{
graphics.off()

# color = c("red", "grey50")
# c("Urban-gain", "Non-urban")
change_and_nochange = rbind(change1, no_change1)

ho <- change_and_nochange[,index_xcoord]  
ve <- change_and_nochange[,index_ycoord] 

# postscript units="in", res=300
# pdf(paste("Figures-Ems paper/", Foldername, "/",Filename, ".pdf", sep=""), width=3.125, height=3.385, pointsize=12) # , horizontal=F)

tiff(paste("Figures-Ems paper/", Foldername, "/",Filename, ".tiff", sep=""), units="in", width=3.125, height=3.385, res=1500, bg = "white", compression = 'lzw') # , horizontal=F)

# par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))
par(mfrow=c(1,1), mar=c(2,2,1,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))

plot(range(ho), range(ve), axes=T, type="n", xlab="", ylab="", main=Figuretitle)

points(change1[, index_xcoord], change1[, index_ycoord], col=color[1], cex=0.15, pch=15, xlab="", ylab="")

points(no_change1[, index_xcoord], no_change1[, index_ycoord], col=color[2], pch=15, cex=0.15, xlab="", ylab="")

legend("topright", legend_name, col=color, pch=15, cex=.75)

dev.off()

}



mapping_cl <- function(newT_cl, index_xcoord, index_ycoord, yvr, color, Foldername, Filename, Figuretitle, legend_name)
{
graphics.off()

# color = c("red", "grey50")
# c("Urban-gain", "Non-urban")

ho <- newT_cl[,index_xcoord]  
ve <- newT_cl[,index_ycoord] 

# postscript units="in", res=300
# pdf(paste("Figures-Ems paper/", Foldername, "/",Filename, ".pdf", sep=""), width=3.125, height=3.385, pointsize=12) # , horizontal=F)

tiff(paste("Figures-Ems paper/", Foldername, "/",Filename, ".tiff", sep=""), units="in", width=3.125, height=3.385, res=1500, bg = "white", compression = 'lzw') # , horizontal=F)

# par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))
par(mfrow=c(1,1), mar=c(2,2,1,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))

plot(range(ho), range(ve), axes=T, type="n", xlab="", ylab="", main=Figuretitle)

points(newT_cl[, index_xcoord], newT_cl[, index_ycoord], col=color[newT_cl[,yvr+1]], cex=0.15, pch=15, xlab="", ylab="")

legend("topright", legend_name, col=color, pch=15, cex=.75)

dev.off()

}

mapping_error_map <- function(newTT, err, index_xcoord, index_ycoord, color, Foldername, Filename, Figuretitle, legend_name)
{

graphics.off()

ho <- newTT[,index_xcoord]  
ve <- newTT[,index_ycoord] 

# postscript units="in", res=300
# pdf(paste("Figures-Ems paper/", Foldername, "/",Filename, ".pdf", sep=""), width=3.125, height=3.385, pointsize=12) # , horizontal=F)

tiff(paste("Figures-Ems paper/", Foldername, "/",Filename, ".tiff", sep=""), units="in", width=3.125, height=3.385, res=1500, bg = "white", compression = 'lzw') # , horizontal=F)

par(mfrow=c(1,1), mar=c(2,2,1,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))

plot(range(ho), range(ve), axes=T, type="n", xlab="", ylab="", main=Figuretitle)

points(newTT[, index_xcoord], newTT[, index_ycoord], col=color[err], cex=0.1, pch=15)

legend("topright", legend_name, col=color, pch=15, cex=.75)

dev.off()
}

###############################


mapping_toc <- function(toc, Foldername, Filename, Figuretitle)
{

graphics.off()

# postscript units="in", res=300
# pdf(paste("Figures-Ems paper/", Foldername, "/",Filename, ".pdf", sep=""), width=5, height=5, pointsize=12) # , horizontal=F)

tiff(paste("Figures-Ems paper/", Foldername, "/",Filename, ".tiff", sep=""), units="in", width=5, height=5, res=1500, bg = "white", compression = 'lzw') # , horizontal=F)

par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))

plot(toc, main=Figuretitle, cex=1) # TOC for the LTM model (a)

dev.off()
}


mapping_2toc <- function(toc1, toc2, Foldername, Filename, Figuretitle)
{

graphics.off()

# postscript units="in", res=300
# pdf(paste("Figures-Ems paper/", Foldername, "/",Filename, ".pdf", sep=""), width=5, height=5, pointsize=12) # , horizontal=F)

tiff(paste("Figures-Ems paper/", Foldername, "/",Filename, ".tiff", sep=""), units="in", width=5, height=5, res=1500, bg = "white", compression = 'lzw') # , horizontal=F)

par(mfrow=c(1,1), mar=c(4,4,2,1), mgp=c(3,1,0)*0.7, lab=c(3,3,1))

tab1 = slot(toc1, "table")
tab2 = slot(toc2, "table")
# plot(toc1, main=Figuretitle, cex=1) # TOC for the LTM model (a)
plot(tab1[,2], tab1[,3], main=Figuretitle, cex=1, col="red") # TOC for the LTM model (a)
lines(tab2[,2], tab2[,3], cex=1, col="black", type="b") # TOC for the LTM model (a)

dev.off()
}


