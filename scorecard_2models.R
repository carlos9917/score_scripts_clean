####################################################################
#### produce scorecards from vfiles/vobs-files                    ##
####################################################################
#Changes (Carlos):
# List of files contains the whole path of the files
# This is to avoid that validdate gets nonsense, since it depends on correct length of file name!!!!!
#                                        
# Notice! Valid dates and lead time are taken from vfld and vobs filename
#         assuming a given length of path+filename (information is not available
#         inside files). Therefore pay attention to the lines setting $validdate and $LT
#         Note also that this script have always used LT=0. Filenames for vfld is given in files below (carra,ERA5,obs)
#
#  General outline
#  1. Read vfld/vobs files and merge in one table xalldata
#  2. Do verification for different station lists/metrics/parameters/significance test: verifout + max/min of conf interv: verifoutmax, verifoutmin
#  3. Prepare Scorecard; for each parameter decide what to be plotted
#  4. Plot scorecard



# settings, what to verify?
# These are the names of the list of stations to verify (to be defined below)
#NOTE: the dimensions of this array will affe
slist <- c("DenmarkALL","DenmarkSkogen","DenmarkRosskilde",
	   "DenmarkHighFreq","DenmarkCoast","DenmarkLand"
	   ,"DenmarkBjarneSel","DenmarkBjarneCoast",
	     "DenmarkBjarneLand")

#slist <- c("CARRASvalbard","CARRANorwayIslands","CARRANorwayCoast","CARRANorwayFjords","CARRANorwayinland","CARRAMountainsNE",
#           "CARRASweFin400800m","CARRASweFin50400m","CARRAGulfOfBothnia")

mod<- c("nea","ec9")                  #models to verify
pv <- c("TT","TTHC","PSS","FF","RH")    # parameters to verify
met<- c("bias","sde")                 # metrics used
mm <- length(mod)*length(met)         # models x metrics
np <- length(pv)                      # number of parameters
verifpara <- matrix(NA,np*length(slist),mm+2)     # matrix with all scores
verifparamin <- matrix(NA,np*length(slist),mm+2)  # matrix with all scores lowest conf int
verifparamax <- matrix(NA,np*length(slist),mm+2)  # matrix with all scores highest conf int


threshcol1 <- 1.20   # % difference to color box in scorecard 1.20 = 20%
threshcol2 <- 0.25   # absolute difference to color box in scorecard, i.e. used for biases


######################################################################################################################

# R-packages needs to be downloaded and installed
#install.packages("gridExtra", lib="/home/morteno/Rpackages")

library(gridExtra)
library(grid)
library(gtools)
library(data.table)

##################################################################
# read vobs-file (observations) from HARMONIE system           ###
##################################################################
datapath <-'/home/cap/data/scripts/R/harp/score_scripts_clean'
# the files indicated in the list must match their corresponding
# timestamps (defined in the filenames)
files      <- list(nea=file.path(datapath,"filelists","nea.vfldfiles.txt"),
		   ec9=file.path(datapath,"filelists","ec9.vfldfiles.txt"))
    #carra="carra.vfldfiles.txt",
    #ERA5="era5.vfldfiles.txt")

files  <- lapply(files, scan, what="character")

# list of vobs files for the verification.
# their names/timestamps must match those of the vfld files above
obsfiles      <- list(
    obs=file.path(datapath,"filelists","vobsfiles_dmi.txt"))

obsfiles  <- lapply(obsfiles, scan, what="character")

####################################################################
####################################################################
####################################################################

fc1tot <- NULL
fc2tot <- NULL
fc1 <- NULL
fc2 <- NULL

for (i in names(files)) {
    cat("model: ",i," \n")
    if (i=="nea") { 

        for (ii in 1:length(files[[i]])) {
            infile <- files[[i]][ii]
            m <- paste(infile,sep="")            
	    cat("reading  ",m, "\n")
            #cat("file to read: ",m)
            x <- readLines(m)
            
            l1 <- read.table(infile,fill=TRUE)
            l1 <- as.numeric(as.character(l1[1,1]))
            l2 <- as.numeric(x[2])+2
            
            d <- read.table(infile,skip=l2,fill=TRUE)
            
### find parameter names
            par <- readLines(infile,n=l2)
            para <- array(NA,l2-3)
            for (k in 3:l2) {
                para[k-2] <- substring(par,1,5)[k]
            }
            para <- gsub(" ", "", para, fixed = TRUE)
###
            
            tot <- dim(d)[1]
            k <- array(TRUE,tot)
            
            for (iii in 1:tot) {
                if (iii > l1) k[iii] <- FALSE
            }
            fc1 <- d[k,]
            names(fc1) <- c("WMO","LAT","LON",para)
            fc1[,] <- lapply(fc1, function(x) {as.numeric(as.character(x))})
	    fcfilename=basename(infile)
	    #print(substring(fcfilename,8,17))
	    #print(substring(infile,19,28))
	    fc1$validdate <-(substring(fcfilename,8,17))
            #fc1$validdate <- (substring(infile,19,28)) #BUG!
	    #fc1$validdate <- (substring(infile,24,33))
            #fc1$LT <- (substring(infile,34,35))
	    #print(substring(infile,19,28))
            #fc1$LT <- (substring(infile,29,30))
	    fc1$LT <-(substring(fcfilename,18,19))
	    #print(substring(infile,29,30))
 
	    #cat("validdate: ", fc1$validdate)
        if ( !("FI" %in% names(fc1)) ) fc1$FI <- -99
        if ( !("NN" %in% names(fc1)) ) fc1$NN <- -99
    	if ( !("DD" %in% names(fc1)) ) fc1$DD <- -99
    	if ( !("FF" %in% names(fc1)) ) fc1$FF <- -99
    	if ( !("TT" %in% names(fc1)) ) fc1$TT <- -99
    	if ( !("RH" %in% names(fc1)) ) fc1$RH <- -99
    	if ( !("PS" %in% names(fc1)) ) fc1$PS <- -99
    	if ( !("PE" %in% names(fc1)) ) fc1$PE <- -99
    	if ( !("QQ" %in% names(fc1)) ) fc1$QQ <- -99
    	if ( !("VI" %in% names(fc1)) ) fc1$VI <- -99
    	if ( !("TD" %in% names(fc1)) ) fc1$TD <- -99
    	if ( !("TX" %in% names(fc1)) ) fc1$TX <- -99
        if ( !("TN" %in% names(fc1)) ) fc1$TN <- -99
        if ( !("GX" %in% names(fc1)) ) fc1$GX <- -99

            if (ii==1) fc1tot <- fc1
            if (ii>1)  fc1tot <- rbind(fc1,fc1tot)
	    #if (ii > 1) fc1tot <- smartbind(fc1,fc1tot)
	    #if (ii > 1) fc1tot <- rbind(fc1, fc1tot[, names(fc1)])
	    #if (ii > 1) fc1tot <-rbindlist(list(fc1,fc1tot), fill = TRUE)
        
        
            
        } 
    }
    if (i=="ec9") { 
        for (ii in 1:length(files[[i]])) {
            infile <- files[[i]][ii]
            m <- paste(infile,sep="")            
            #print(m)
	    cat("reading ",m,"\n")
            x <- readLines(m)
            
            l1 <- read.table(infile,fill=TRUE)
            l1 <- as.numeric(as.character(l1[1,1]))
            l2 <- as.numeric(x[2])+2
            d <- read.table(infile,skip=l2,fill=TRUE)
            
### find parameter names
            par <- readLines(infile,n=l2)
            para <- array(NA,l2-3)
            for (k in 3:l2) {
                para[k-2] <- substring(par,1,5)[k]
            }
            para <- gsub(" ", "", para, fixed = TRUE)
###
            
            tot <- dim(d)[1]
            k <- array(TRUE,tot)
            
            for (iii in 1:tot) {
                if (iii > l1) k[iii] <- FALSE
            }
            fc2 <- d[k,]
            names(fc2) <- c("WMO","LAT","LON",para)
            fc2[,] <- lapply(fc2, function(x) {as.numeric(as.character(x))})
	    fcfilename=basename(infile)
            #fc2$validdate <- (substring(infile,19,28)) #BUG!
	    fc2$validdate <-(substring(fcfilename,8,17))
	    fc2$LT <- (substring(fcfilename,18,19))
	    cat("init ",fc2$LT,"\n")
            #fc2$LT <- (substring(infile,29,30))
 
            if ( !("FI" %in% names(fc2)) ) fc2$FI <- -99
    	    if ( !("NN" %in% names(fc2)) ) fc2$NN <- -99
    	    if ( !("DD" %in% names(fc2)) ) fc2$DD <- -99
    	    if ( !("FF" %in% names(fc2)) ) fc2$FF <- -99
    	    if ( !("TT" %in% names(fc2)) ) fc2$TT <- -99
    	    if ( !("RH" %in% names(fc2)) ) fc2$RH <- -99
    	    if ( !("PS" %in% names(fc2)) ) fc2$PS <- -99
    	    if ( !("PE" %in% names(fc2)) ) fc2$PE <- -99
    	    if ( !("QQ" %in% names(fc2)) ) fc2$QQ <- -99
    	    if ( !("VI" %in% names(fc2)) ) fc2$VI <- -99
    	    if ( !("CH" %in% names(fc2)) ) fc2$CH <- -99
        	if ( !("LC" %in% names(fc2)) ) fc2$LC <- -99
    	    if ( !("TD" %in% names(fc2)) ) fc2$TD <- -99
            if ( !("TX" %in% names(fc2)) ) fc2$TX <- -99
            if ( !("TN" %in% names(fc2)) ) fc2$TN <- -99
            if ( !("GG" %in% names(fc2)) ) fc2$GG <- -99
            if ( !("GX" %in% names(fc2)) ) fc2$GX <- -99

            if (ii==1) fc2tot <- fc2
            if (ii>1)  fc2tot <- rbind(fc2,fc2tot) # this will fail the 2nd time bc second file has more cols
	    #if (ii > 1) fc2tot <-rbindlist(list(fc2,fc2tot), fill = TRUE)
            
        } 
    } 
}

#require(miIO)
print("passed fc reading")
warnings()
#fc <- merge2(fc1tot,fc2tot,by=c("WMO","validdate","LT"),suffixes = c(".fc1",".fc2"))

fc <- merge(fc1tot,fc2tot,by=c("WMO","validdate","LT"),suffixes = c(".fc1",".fc2"))
#fc <- fc1tot
#fc <- fc1
print("passed fc merging")
############################################
#### read observations #####################
############################################

            obs   <- NULL
            obstot<- NULL
            
for (i in names(obsfiles)) {
    for (ii in 1:length(obsfiles[[i]])) {      
        infileobs <- obsfiles[[i]][ii]
        m <- paste(infileobs,sep="")
        print(m)
        x <- readLines(m)
        
        l1 <- read.table(infileobs,fill=TRUE)
        l1 <- as.numeric(as.character(l1[1,1]))
        l2 <- as.numeric(x[2])+2

### find parameter names
        par <- readLines(infileobs,n=l2)
        para <- array(NA,l2-3)
        for (k in 3:l2) {
            para[k-2] <- substring(par,1,5)[k]
        }
        para <- gsub(" ", "", para, fixed = TRUE)
###
 
        
        d <- read.table(infileobs,skip=l2,fill=TRUE)
        
        tot <- dim(d)[1]
        k <- array(TRUE,tot)
        
        for (iii in 1:tot) {
            if (iii > l1) k[iii] <- FALSE
        }
        
        obs <- d[k,]
#        names(obs) <- c("WMO","LAT","LON","AMSL","NN","DD","FF","TT","TD","RH","PSS","PS","VI","PE24","PE","PE1","QQ","TX","TM","GW","GM","WX","PE3")
        names(obs) <- c("WMO","LAT","LON","AMSL",para)
        if ( !("PE" %in% names(obs)) ) obs$PE <- -99
    	if ( !("PE1" %in% names(obs)) ) obs$PE1 <- -99
    	if ( !("PE3" %in% names(obs)) ) obs$PE3 <- -99
    	if ( !("PE6" %in% names(obs)) ) obs$PE6 <- -99
    	if ( !("PE24" %in% names(obs)) ) obs$PE24 <- -99
    	if ( !("TM" %in% names(obs)) ) obs$TM <- -99
    	if ( !("TX" %in% names(obs)) ) obs$TX <- -99
    	if ( !("QQ" %in% names(obs)) ) obs$QQ <- -99
    	if ( !("GW" %in% names(obs)) ) obs$GW <- -99
    	if ( !("GM" %in% names(obs)) ) obs$GM <- -99
    	if ( !("WX" %in% names(obs)) ) obs$WX <- -99
        
        obs[,] <- lapply(obs, function(x) {as.numeric(as.character(x))})
	obsfilename=basename(infileobs)
        #cat("check infileobs ",obsfilename,"\n")
	#print(substring(obsfilename,5,16))
	#stop("check")
        obs$validdate <- (substring(obsfilename,5,16))
        #obs$validdate <- (substring(infileobs,16,25))
	#cat("obs validdate: ", obs$validdate)
        if (ii==1) obstot <- obs
        if (ii>1)  obstot <- rbind(obs,obstot)
        
    }
}


#xalldata <-  merge2(fc,obstot,by=c("WMO","validdate"),suffixes = c("",".obs"))

print("before obstot")

names(obstot) <- ifelse(names(obstot)=="WMO" | names(obstot)=="validdate", names(obstot),paste(names(obstot),".obs",sep=""))
print(obstot)
print("before xalldata")
xalldata <-  merge(fc,obstot,by=c("WMO","validdate"),suffixes = c("",".obs"))
print("after xalldata")

##### all data are read and orginazied in table xalldata
 
########################################
#### Here start verification part   ####
########################################
# station lists:
source("stations.R")

for (st in 1:length(slist)) {
    if (slist[st]=="CARRASvalbard") k <- xalldata$WMO %in% CARRASvalbard 
    if (slist[st]=="CARRANorwayCoast") k <- xalldata$WMO %in% CARRANorwayCoast 
    if (slist[st]=="CARRANorwayFjords") k <- xalldata$WMO %in% CARRANorwayFjords 
    if (slist[st]=="CARRANorwayinland") k <- xalldata$WMO %in%  CARRANorwayinland
    if (slist[st]=="CARRAMountainsNE") k <- xalldata$WMO %in%  CARRAMountainsNE
    if (slist[st]=="CARRASweFin400800m") k <- xalldata$WMO %in% CARRASweFin400800m
    if (slist[st]=="CARRASweFin50400m") k <- xalldata$WMO %in% CARRASweFin50400m
    if (slist[st]=="CARRAGulfOfBothnia") k <- xalldata$WMO %in% CARRAGulfOfBothnia
    if (slist[st]=="DenmarkALL") k <- xalldata$WMO %in% DenmarkALL
    if (slist[st]=="DenmarkSkogen") k <- xalldata$WMO %in% DenmarkSkogen
    if (slist[st]=="DenmarkRosskilde") k <- xalldata$WMO %in% DenmarkRosskilde
    if (slist[st]=="DenmarkHighFreq") k <- xalldata$WMO %in% DenmarkHighFreq
    if (slist[st]=="DenmarkCoast") k <- xalldata$WMO %in% DenmarkCoast
    if (slist[st]=="DenmarkLand") k <- xalldata$WMO %in% DenmarkLand
    if (slist[st]=="DenmarkBjarneSel") k <- xalldata$WMO %in% DenmarkBjarneSel
    if (slist[st]=="DenmarkBjarneCoast") k <- xalldata$WMO %in% DenmarkBjarneCoast
    if (slist[st]=="DenmarkBjarneLand") k <- xalldata$WMO %in% DenmarkBjarneLand

    #if (slist[st]=="Denmark") print("selected DK")
    if (slist[st]=="ALL") k <- xalldata$WMO %in% unique(xalldata$WMO)
    
    x <- xalldata[k,]
    
    for (i in 1:np) {

        verifpara[(np*st-np)+i,1] <- slist[st]
        verifpara[(np*st-np)+i,2] <- pv[i]
        verifparamin[(np*st-np)+i,1] <- slist[st]
        verifparamin[(np*st-np)+i,2] <- pv[i]
        verifparamax[(np*st-np)+i,1] <- slist[st]
        verifparamax[(np*st-np)+i,2] <- pv[i]
        
# TT
        if (pv[i]=="TT") {
            obs <- x$TT.obs
            fc1 <- x$TT.fc1
            fc2 <- x$TT.fc2
            
            k <- (obs > 200. & fc1 > 200. & fc2 > 200.)
            
            obs <- obs[k]-273.14
            fc1 <- fc1[k]-273.14
            fc2 <- fc2[k]-273.14
             if (length(obs) > 100) {
            verifpara[(np*st-np)+i,3] <- round(mean(fc1-obs),2)
            verifpara[(np*st-np)+i,4] <- round(mean(fc2-obs),2)
            verifpara[(np*st-np)+i,5] <- round(sd(fc1-obs),2)
            verifpara[(np*st-np)+i,6] <- round(sd(fc2-obs),2)
            
# confidence intervals:
        rn <- 500
        btmp1 <- array(NA,rn)
            btmp2 <- array(NA,rn)
            
        sde1 <- array(NA,rn)
        sde2 <- array(NA,rn)
        for (rs in 1:rn) {
            k <- sample(1:length(obs),length(obs),replace=TRUE)
            btmp1[rs] <- round(mean(fc1[k]-obs[k]),2)
            btmp2[rs] <- round(mean(fc2[k]-obs[k]),2)
            sde1[rs] <- round(sd(fc1[k]-obs[k]),2)
            sde2[rs] <- round(sd(fc2[k]-obs[k]),2)
        }
            verifparamin[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.025,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.975,na.rm=TRUE),2)
             }
        
    }
         
# TTHC
        if (pv[i]=="TTHC") {
            obs <- x$TT.obs
            fc1 <- x$TT.fc1 + 0.0065*(x$FI.fc1-x$AMSL.obs)
            fc2 <- x$TT.fc2 + 0.0065*(x$FI.fc2-x$AMSL.obs)
            
            k <- (obs > 200. & fc1 > 200. & fc2 > 200.)
            
            obs <- obs[k]-273.14
            fc1 <- fc1[k]-273.14
            fc2 <- fc2[k]-273.14
            
            verifpara[(np*st-np)+i,3] <- round(mean(fc1-obs),2)
            verifpara[(np*st-np)+i,4] <- round(mean(fc2-obs),2)
            verifpara[(np*st-np)+i,5] <- round(sd(fc1-obs),2)
            verifpara[(np*st-np)+i,6] <- round(sd(fc2-obs),2)
            
# confidence intervals:
        rn <- 500
        btmp1 <- array(NA,rn)
        btmp2 <- array(NA,rn)
        sde1 <- array(NA,rn)
        sde2 <- array(NA,rn)
        for (rs in 1:rn) {
            k <- sample(1:length(obs),length(obs),replace=TRUE)
            btmp1[rs] <- round(mean(fc1[k]-obs[k]),2)
            btmp2[rs] <- round(mean(fc2[k]-obs[k]),2)
            sde1[rs] <- round(sd(fc1[k]-obs[k]),2)
            sde2[rs] <- round(sd(fc2[k]-obs[k]),2)
        }
            verifparamin[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.025,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.975,na.rm=TRUE),2)
            
        
    }    
# FF
        if (pv[i]=="FF") {
            obs <- x$FF.obs
            fc1 <- x$FF.fc1
            fc2 <- x$FF.fc2
            
            k <- (obs >= 0. & fc1 >= 0. & fc2 >= 0.)
            
            obs <- obs[k]
            fc1 <- fc1[k]
            fc2 <- fc2[k]
             if (length(obs) > 100) {
            verifpara[(np*st-np)+i,3] <- round(mean(fc1-obs),2)
            verifpara[(np*st-np)+i,4] <- round(mean(fc2-obs),2)
            verifpara[(np*st-np)+i,5] <- round(sd(fc1-obs),2)
            verifpara[(np*st-np)+i,6] <- round(sd(fc2-obs),2)

                     
# confidence intervals:
        rn <- 500
        btmp1 <- array(NA,rn)
        btmp2 <- array(NA,rn)
        sde1 <- array(NA,rn)
        sde2 <- array(NA,rn)
        for (rs in 1:rn) {
            k <- sample(1:length(obs),length(obs),replace=TRUE)
            btmp1[rs] <- round(mean(fc1[k]-obs[k]),2)
            btmp2[rs] <- round(mean(fc2[k]-obs[k]),2)
            sde1[rs] <- round(sd(fc1[k]-obs[k]),2)
            sde2[rs] <- round(sd(fc2[k]-obs[k]),2)
        }
            verifparamin[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.025,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.975,na.rm=TRUE),2)
             }
        }
	
# MSLP / PSS
        if (pv[i]=="PSS") {
            obs <- x$PS.obs
            fc1 <- x$PS.fc1
            fc2 <- x$PS.fc2
            
            k <- (obs >= 0. & fc1 >= 0. & fc2 >= 0.)
            
            obs <- obs[k]
            obs <- ifelse(obs > 2000., obs/100,obs)      # some pss-obs are given in Pa, some in hPa
            fc1 <- fc1[k]
            fc2 <- fc2[k]

            if (length(obs) > 100) {
            verifpara[(np*st-np)+i,3] <- round(mean(fc1-obs),2)
            verifpara[(np*st-np)+i,4] <- round(mean(fc2-obs),2)
            verifpara[(np*st-np)+i,5] <- round(sd(fc1-obs),2)
            verifpara[(np*st-np)+i,6] <- round(sd(fc2-obs),2)

                     
# confidence intervals:
        rn <- 500
        btmp1 <- array(NA,rn)
        btmp2 <- array(NA,rn)
        sde1 <- array(NA,rn)
        sde2 <- array(NA,rn)
        for (rs in 1:rn) {
            k <- sample(1:length(obs),length(obs),replace=TRUE)
            btmp1[rs] <- round(mean(fc1[k]-obs[k]),2)
            btmp2[rs] <- round(mean(fc2[k]-obs[k]),2)
            sde1[rs] <- round(sd(fc1[k]-obs[k]),2)
            sde2[rs] <- round(sd(fc2[k]-obs[k]),2)
        }
            verifparamin[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.025,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.975,na.rm=TRUE),2)
            }
        }
# RH
        if (pv[i]=="RH") {
            obs <- x$RH.obs
            fc1 <- x$RH.fc1
            fc2 <- x$RH.fc2
            
            obs <- ifelse(obs > 100., 100., obs)
            fc1 <- ifelse(fc1 > 100., 100., fc1)
            fc2 <- ifelse(fc2 > 100., 100., fc2)
            
            k <- (obs >= 0. & fc1 >= 0. & fc2 >= 0.)
            
            obs <- obs[k]
            fc1 <- fc1[k]
            fc2 <- fc2[k]
             if (length(obs) > 100) {
            verifpara[(np*st-np)+i,3] <- round(mean(fc1-obs),2)
            verifpara[(np*st-np)+i,4] <- round(mean(fc2-obs),2)
            verifpara[(np*st-np)+i,5] <- round(sd(fc1-obs),2)
            verifpara[(np*st-np)+i,6] <- round(sd(fc2-obs),2)

                     
# confidence intervals:
        rn <- 500
        btmp1 <- array(NA,rn)
        btmp2 <- array(NA,rn)
        sde1 <- array(NA,rn)
        sde2 <- array(NA,rn)
        for (rs in 1:rn) {
            k <- sample(1:length(obs),length(obs),replace=TRUE)
            btmp1[rs] <- round(mean(fc1[k]-obs[k]),2)
            btmp2[rs] <- round(mean(fc2[k]-obs[k]),2)
            sde1[rs] <- round(sd(fc1[k]-obs[k]),2)
            sde2[rs] <- round(sd(fc2[k]-obs[k]),2)
        }
            verifparamin[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.025,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.975,na.rm=TRUE),2)
             }
        }
# QQ
        if (pv[i]=="QQ") {
            obs <- x$QQ.obs
            fc1 <- x$QQ.fc1
            fc2 <- x$QQ.fc2
            
            obs <- ifelse(obs > 100., 100., obs)
            fc1 <- ifelse(fc1 > 100., 100., fc1)
            fc2 <- ifelse(fc2 > 100., 100., fc2)
            
            k <- (obs >= 0. & fc1 >= 0. & fc2 >= 0.)
            
            obs <- obs[k]
            fc1 <- fc1[k]
            fc2 <- fc2[k]
             if (length(obs) > 100) {
            verifpara[(np*st-np)+i,3] <- round(mean(fc1-obs)*1000,2)
            verifpara[(np*st-np)+i,4] <- round(mean(fc2-obs)*1000,2)
            verifpara[(np*st-np)+i,5] <- round(sd(fc1-obs)*1000,2)
            verifpara[(np*st-np)+i,6] <- round(sd(fc2-obs)*1000,2)
                     
# confidence intervals:
        rn <- 500
        btmp1 <- array(NA,rn)
        btmp2 <- array(NA,rn)
        sde1 <- array(NA,rn)
        sde2 <- array(NA,rn)
        for (rs in 1:rn) {
            k <- sample(1:length(obs),length(obs),replace=TRUE)
            btmp1[rs] <- round(mean(fc1[k]-obs[k])*1000,2)
            btmp2[rs] <- round(mean(fc2[k]-obs[k])*1000,2)
            sde1[rs] <- round(sd(fc1[k]-obs[k])*1000,2)
            sde2[rs] <- round(sd(fc2[k]-obs[k])*1000,2)
        }
            
            verifparamin[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.025,na.rm=TRUE),2)
            verifparamin[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.025,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,3] <- round(quantile(btmp1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,4] <- round(quantile(btmp2,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,5] <- round(quantile(sde1,probs=0.975,na.rm=TRUE),2)
            verifparamax[(np*st-np)+i,6] <- round(quantile(sde2,probs=0.975,na.rm=TRUE),2)
             }
        }
    }
}   # slist

verifout <- data.frame(verifpara)
verifoutmin <- data.frame(verifparamin)
verifoutmax <- data.frame(verifparamax)
names(verifout) <- c("STLIST","PARA","m1bias","m2bias","m1std","m2std")

k <- !is.na(verifout$m1bias) | !is.na(verifout$m2bias)
verifout <- verifout[k,]

!is.na(verifoutmin$X3) | !is.na(verifoutmin$X5)
verifoutmin <- verifoutmin[k,]

!is.na(verifoutmax$X3) | !is.na(verifoutmax$X5)
verifoutmax <- verifoutmax[k,]


####################################################################################
# Prepare Scorecard ################################################################
####################################################################################
#install.packages("gridExtra", lib="/home/morteno/Rpackages")
#library(gridExtra, lib="/home/morteno/Rpackages")
#library(grid, lib="/home/morteno/Rpackages")


#threshcol1 <- 1.20   # % difference to color box in scorecard 1.20 = 20%
#threshcol2 <- 0.25   # absolute difference to color box in scorecard, i.e. used for biases

#Temperature;
if ("TT" %in% pv) {
k <- as.character(verifout$PARA)=="TT"
pd <- verifout[k,c(1,5,6,3,4)]
#pd <- verifout[k,c(1,1,1,1,1)] 

print(pd)
print("end")

#pd <- data.frame(verifout[c(1,7,13,19,25,31,37,43,49,55),c(1,5,6,3,4)])
names(pd) <- c("TT.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

#pdmin <- data.frame(verifoutmin[c(1,7,13,19,25,31,37,43,49,55),c(1,5,6,3,4)])
#pdmax <- data.frame(verifoutmax[c(1,7,13,19,25,31,37,43,49,55),c(1,5,6,3,4)])
pdmin <- data.frame(verifoutmin[k,c(1,5,6,3,4)])
pdmax <- data.frame(verifoutmax[k,c(1,5,6,3,4)])
names(pd) <- c("TT.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
names(pdmin) <- c("TT.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
names(pdmax) <- c("TT.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

## significance check; are there overlap in 95%-confidence interval, if not set to FALSE

signi <- data.frame(matrix(NA,dim(pd)[1],dim(pd)[2]))

signi[,2] <- ifelse(as.numeric(as.character(pdmax[,2])) < as.numeric(as.character(pdmin[,3])) |
                    as.numeric(as.character(pdmin[,2])) > as.numeric(as.character(pdmax[,3])),TRUE,FALSE) 
signi[,3] <- signi[,2]
signi[,4] <- ifelse(as.numeric(as.character(pdmax[,4])) < as.numeric(as.character(pdmin[,5])) |
                    as.numeric(as.character(pdmin[,4])) > as.numeric(as.character(pdmax[,5])),TRUE,FALSE) 
signi[,5] <- signi[,4]


##
print("before")
print(pd)
print("printed")
print(substring(pd[,1],1,7))
print(substring(as.character(pd[,1]),8,30))
print(substring(as.character(pd[,1]),1,30))
pd[,1] <- ifelse (substring(pd[,1],1,7)=="Denmark", substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

#cat("first element ",pd[,1],"\n")
pdpara <- cbind(pd[1],sapply(pd[2:5], function(x) {as.numeric(as.character(x))}))
pd <- pdpara
print("first occ")
print(dim(pd))
#pd <- na.omit(pd) #omit missing values

cols <- matrix("black", nrow(pd), ncol(pd))
colsfill <- matrix("lightgrey", nrow(pd), ncol(pd))
print(dim(cols))
print(dim(colsfill))
for (i in 1:dim(pd)[1]) {
    cat("check i: ",i," ",pd[i,1],"\n")
}
for (i in 1:dim(pd)[1]) {
    #if (!is.na(pd[i,2])) print("NA")
    #if (is.na(pd[i,3])) print("NA 2")
    cat("i = ",i,"\n")
    if (pd[i,2] < pd[i,3]) cols[i,1:3] <- c("black","darkgreen", "red")
    if (pd[i,2] > pd[i,3]) cols[i,1:3] <- c("black","red", "darkgreen")
    colsfill[i,1:3] <- c("lightgrey","lightgrey", "lightgrey")
    if (pd[i,3]/pd[i,2]  > threshcol1) colsfill[i,1:3] <- c("lightgrey","green", "pink")
    if (pd[i,2]/pd[i,3] > threshcol1) colsfill[i,1:3] <- c("lightgrey","pink", "green")
    #BIAS
    if (abs(pd[i,4]) < abs(pd[i,5])) cols[i,4:5] <- c("darkgreen", "red")
    if (abs(pd[i,4]) > abs(pd[i,5])) cols[i,4:5] <- c("red", "darkgreen")
    colsfill[i,4:5] <- c("lightgrey", "lightgrey")
    if (abs(pd[i,4]) + threshcol2 < abs(pd[i,5])) colsfill[i,4:5] <- c("green", "pink")
    if (abs(pd[i,4]) > abs(pd[i,5]) + threshcol2 ) colsfill[i,4:5] <- c("pink", "green")
}

print("passed loop")
tt <- ttheme_default(core=list(fg_params = list(col = cols),
                                bg_params = list(fill=colsfill)),
                      rowhead=list(bg_params = list(col=NA)),
                     colhead=list(bg_params = list(col=NA)))
pdTT <- pd
ttTT <- tt
signiTT <- signi
}

#Temperature HC;
print("now for TTHC")
if ("TTHC" %in% pv) {
    
k <- as.character(verifout$PARA)=="TTHC"
pd <- verifout[k,c(1,5,6,3,4)]

#pd <- data.frame(verifout[c(2,8,14,20,26,32,38,44,50,56),c(1,5,6,3,4)])
names(pd) <- c("TTHC.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

#pdmin <- data.frame(verifoutmin[c(2,8,14,20,26,32,38,44,50,56),c(1,5,6,3,4)])
#pdmax <- data.frame(verifoutmax[c(2,8,14,20,26,32,38,44,50,56),c(1,5,6,3,4)])
pdmin <- data.frame(verifoutmin[k,c(1,5,6,3,4)])
pdmax <- data.frame(verifoutmax[k,c(1,5,6,3,4)])
names(pd) <- c("TTHC.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
names(pdmin) <- c("TTHC.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
names(pdmax) <- c("TTHC.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

## significance check

signi <- data.frame(matrix(NA,dim(pd)[1],dim(pd)[2]))

signi[,2] <- ifelse(as.numeric(as.character(pdmax[,2])) < as.numeric(as.character(pdmin[,3])) |
                    as.numeric(as.character(pdmin[,2])) > as.numeric(as.character(pdmax[,3])),TRUE,FALSE) 
signi[,3] <- signi[,2]
signi[,4] <- ifelse(as.numeric(as.character(pdmax[,4])) < as.numeric(as.character(pdmin[,5])) |
                    as.numeric(as.character(pdmin[,4])) > as.numeric(as.character(pdmax[,5])),TRUE,FALSE) 
signi[,5] <- signi[,4]


##

pd[,1] <- ifelse (substring(pd[,1],1,7)=="Denmark", substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

pdpara <- cbind(pd[1],sapply(pd[2:5], function(x) {as.numeric(as.character(x))}))
pd <- pdpara

cols <- matrix("black", nrow(pd), ncol(pd))
colsfill <- matrix("lightgrey", nrow(pd), ncol(pd))

for (i in 1:dim(pd)[1]) {
    if (pd[i,2] < pd[i,3]) cols[i,1:3] <- c("black","darkgreen", "red")
    if (pd[i,2] > pd[i,3]) cols[i,1:3] <- c("black","red", "darkgreen")
    colsfill[i,1:3] <- c("lightgrey","lightgrey", "lightgrey")
    if (pd[i,3]/pd[i,2]  > threshcol1) colsfill[i,1:3] <- c("lightgrey","green", "pink")
    if (pd[i,2]/pd[i,3] > threshcol1) colsfill[i,1:3] <- c("lightgrey","pink", "green")
    #BIAS
    if (abs(pd[i,4]) < abs(pd[i,5])) cols[i,4:5] <- c("darkgreen", "red")
    if (abs(pd[i,4]) > abs(pd[i,5])) cols[i,4:5] <- c("red", "darkgreen")
    colsfill[i,4:5] <- c("lightgrey", "lightgrey")
    if (abs(pd[i,4]) + threshcol2 < abs(pd[i,5])) colsfill[i,4:5] <- c("green", "pink")
    if (abs(pd[i,4]) > abs(pd[i,5]) + threshcol2 ) colsfill[i,4:5] <- c("pink", "green")
}

tt <- ttheme_default(core=list(fg_params = list(col = cols),
                                bg_params = list(fill=colsfill)),
                      rowhead=list(bg_params = list(col=NA)),
                     colhead=list(bg_params = list(col=NA)))
pdTTHC <- pd
ttTTHC <- tt
signiTTHC <- signi
}
######## FF
if ("FF" %in% pv) {
k <- as.character(verifout$PARA)=="FF"
pd <- verifout[k,c(1,5,6,3,4)]

#pd <- verifout[c(4,10,16,22,28,34,40,46,52,58),c(1,5,6,3,4)]
names(pd) <- c("FF.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

#pdmin <- data.frame(verifoutmin[c(4,10,16,22,28,34,40,46,52,58),c(1,5,6,3,4)])
#pdmax <- data.frame(verifoutmax[c(4,10,16,22,28,34,40,46,52,58),c(1,5,6,3,4)])
pdmin <- data.frame(verifoutmin[k,c(1,5,6,3,4)])
pdmax <- data.frame(verifoutmax[k,c(1,5,6,3,4)])
names(pdmin) <- c("FF.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
names(pdmax) <- c("FF.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

## significance check

signi <- data.frame(matrix(NA,dim(pd)[1],dim(pd)[2]))

signi[,2] <- ifelse(as.numeric(as.character(pdmax[,2])) < as.numeric(as.character(pdmin[,3])) |
                    as.numeric(as.character(pdmin[,2])) > as.numeric(as.character(pdmax[,3])),TRUE,FALSE) 
signi[,3] <- signi[,2]
signi[,4] <- ifelse(as.numeric(as.character(pdmax[,4])) < as.numeric(as.character(pdmin[,5])) |
                    as.numeric(as.character(pdmin[,4])) > as.numeric(as.character(pdmax[,5])),TRUE,FALSE) 
signi[,5] <- signi[,4]


##
pd[,1] <- ifelse (substring(pd[,1],1,7)=="Denmark", substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

pdpara <- cbind(pd[1],sapply(pd[2:5], function(x) {as.numeric(as.character(x))}))
pd <- pdpara

cols <- matrix("black", nrow(pd), ncol(pd))
colsfill <- matrix("lightgrey", nrow(pd), ncol(pd))


for (i in 1:dim(pd)[1]) {
    if (pd[i,2] < pd[i,3]) cols[i,1:3] <- c("black","darkgreen", "red")
    if (pd[i,2] > pd[i,3]) cols[i,1:3] <- c("black","red", "darkgreen")
    colsfill[i,1:3] <- c("lightgrey","lightgrey", "lightgrey")
    if (pd[i,3]/pd[i,2]  > threshcol1) colsfill[i,1:3] <- c("lightgrey","green", "pink")
    if (pd[i,2]/pd[i,3] > threshcol1) colsfill[i,1:3] <- c("lightgrey","pink", "green")
    #BIAS
    if (abs(pd[i,4]) < abs(pd[i,5])) cols[i,4:5] <- c("darkgreen", "red")
    if (abs(pd[i,4]) > abs(pd[i,5])) cols[i,4:5] <- c("red", "darkgreen")
    colsfill[i,4:5] <- c("lightgrey", "lightgrey")
    if (abs(pd[i,4]) + threshcol2 < abs(pd[i,5])) colsfill[i,4:5] <- c("green", "pink")
    if (abs(pd[i,4]) > abs(pd[i,5]) + threshcol2 ) colsfill[i,4:5] <- c("pink", "green")
}

tt <- ttheme_default(core=list(fg_params = list(col = cols),
                                bg_params = list(fill=colsfill)),
                      rowhead=list(bg_params = list(col=NA)),
                     colhead=list(bg_params = list(col=NA)))
pdFF <- pd
ttFF <- tt
signiFF <- signi

}
print("2nd occ")
######## MSLP
if ("PSS" %in% pv) {
k <- as.character(verifout$PARA)=="PSS"
pd <- verifout[k,c(1,5,6,3,4)]

#pd <- verifout[c(3,9,15,21,27,33,45,51,57),c(1,5,6,3,4)]
#pd <- verifout[c(3,9,15,21,27),c(1,5,6,3,4)]
names(pd) <- c("MSLP.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

#pdmin <- data.frame(verifoutmin[c(3,9,15,21,27,33,45,51,57),c(1,5,6,3,4)])
#pdmax <- data.frame(verifoutmax[c(3,9,15,21,27,33,45,51,57),c(1,5,6,3,4)])
pdmin <- data.frame(verifoutmin[k,c(1,5,6,3,4)])
pdmax <- data.frame(verifoutmax[k,c(1,5,6,3,4)])
names(pdmin) <- c("MSLP.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
names(pdmax) <- c("MSLP.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

## significance check

signi <- data.frame(matrix(NA,dim(pd)[1],dim(pd)[2]))

signi[,2] <- ifelse(as.numeric(as.character(pdmax[,2])) < as.numeric(as.character(pdmin[,3])) |
                    as.numeric(as.character(pdmin[,2])) > as.numeric(as.character(pdmax[,3])),TRUE,FALSE) 
signi[,3] <- signi[,2]
signi[,4] <- ifelse(as.numeric(as.character(pdmax[,4])) < as.numeric(as.character(pdmin[,5])) |
                    as.numeric(as.character(pdmin[,4])) > as.numeric(as.character(pdmax[,5])),TRUE,FALSE) 
signi[,5] <- signi[,4]

#
pd[,1] <- ifelse (substring(pd[,1],1,7)=="Denmark", substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

pdpara <- cbind(pd[1],sapply(pd[2:5], function(x) {as.numeric(as.character(x))}))
pd <- pdpara

cols <- matrix("black", nrow(pd), ncol(pd))
colsfill <- matrix("lightgrey", nrow(pd), ncol(pd))

for (i in 1:dim(pd)[1]) {
    if (pd[i,2] < pd[i,3]) cols[i,1:3] <- c("black","darkgreen", "red")
    if (pd[i,2] > pd[i,3]) cols[i,1:3] <- c("black","red", "darkgreen")
    colsfill[i,1:3] <- c("lightgrey","lightgrey", "lightgrey")
    if (pd[i,3]/pd[i,2]  > threshcol1) colsfill[i,1:3] <- c("lightgrey","green", "pink")
    if (pd[i,2]/pd[i,3] > threshcol1) colsfill[i,1:3] <- c("lightgrey","pink", "green")
    #BIAS
    if (abs(pd[i,4]) < abs(pd[i,5])) cols[i,4:5] <- c("darkgreen", "red")
    if (abs(pd[i,4]) > abs(pd[i,5])) cols[i,4:5] <- c("red", "darkgreen")
    colsfill[i,4:5] <- c("lightgrey", "lightgrey")
    if (abs(pd[i,4]) + threshcol2 < abs(pd[i,5])) colsfill[i,4:5] <- c("green", "pink")
    if (abs(pd[i,4]) > abs(pd[i,5]) + threshcol2 ) colsfill[i,4:5] <- c("pink", "green")
}

tt <- ttheme_default(core=list(fg_params = list(col = cols),
                                bg_params = list(fill=colsfill)),
                      rowhead=list(bg_params = list(col=NA)),
                     colhead=list(bg_params = list(col=NA)))
pdMSLP <- pd
ttMSLP <- tt
signiMSLP  <- signi
}
######## RH
if ("RH" %in% pv) {
k <- as.character(verifout$PARA)=="RH"
pd <- verifout[k,c(1,5,6,3,4)]

#pd <- verifout[c(5,11,17,23,29,35,41,47,53,59),c(1,5,6,3,4)]
names(pd) <- c("RH.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

#pdmin <- data.frame(verifoutmin[c(5,11,17,23,29,35,41,47,53,59),c(1,5,6,3,4)])
#pdmax <- data.frame(verifoutmax[c(5,11,17,23,29,35,41,47,53,59),c(1,5,6,3,4)])
pdmin <- data.frame(verifoutmin[k,c(1,5,6,3,4)])
pdmax <- data.frame(verifoutmax[k,c(1,5,6,3,4)])
names(pdmin) <- c("RH.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
names(pdmax) <- c("RH.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

## significance check

signi <- data.frame(matrix(NA,dim(pd)[1],dim(pd)[2]))

signi[,2] <- ifelse(as.numeric(as.character(pdmax[,2])) < as.numeric(as.character(pdmin[,3])) |
                    as.numeric(as.character(pdmin[,2])) > as.numeric(as.character(pdmax[,3])),TRUE,FALSE) 
signi[,3] <- signi[,2]
signi[,4] <- ifelse(as.numeric(as.character(pdmax[,4])) < as.numeric(as.character(pdmin[,5])) |
                    as.numeric(as.character(pdmin[,4])) > as.numeric(as.character(pdmax[,5])),TRUE,FALSE) 
signi[,5] <- signi[,4]


##
pd[,1] <- ifelse (substring(pd[,1],1,7)=="Denmark", substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

pdpara <- cbind(pd[1],sapply(pd[2:5], function(x) {as.numeric(as.character(x))}))
pd <- pdpara

cols <- matrix("black", nrow(pd), ncol(pd))
colsfill <- matrix("lightgrey", nrow(pd), ncol(pd))

for (i in 1:dim(pd)[1]) {
    if (pd[i,2] < pd[i,3]) cols[i,1:3] <- c("black","darkgreen", "red")
    if (pd[i,2] > pd[i,3]) cols[i,1:3] <- c("black","red", "darkgreen")
    colsfill[i,1:3] <- c("lightgrey","lightgrey", "lightgrey")
    if (pd[i,3]/pd[i,2]  > threshcol1) colsfill[i,1:3] <- c("lightgrey","green", "pink")
    if (pd[i,2]/pd[i,3] > threshcol1) colsfill[i,1:3] <- c("lightgrey","pink", "green")
    #BIAS
    if (abs(pd[i,4]) < abs(pd[i,5])) cols[i,4:5] <- c("darkgreen", "red")
    if (abs(pd[i,4]) > abs(pd[i,5])) cols[i,4:5] <- c("red", "darkgreen")
    colsfill[i,4:5] <- c("lightgrey", "lightgrey")
    if (abs(pd[i,4]) + threshcol2 < abs(pd[i,5])) colsfill[i,4:5] <- c("green", "pink")
    if (abs(pd[i,4]) > abs(pd[i,5]) + threshcol2 ) colsfill[i,4:5] <- c("pink", "green")
}

tt <- ttheme_default(core=list(fg_params = list(col = cols),
                                bg_params = list(fill=colsfill)),
                      rowhead=list(bg_params = list(col=NA)),
                     colhead=list(bg_params = list(col=NA)))

pdRH <- pd
ttRH <- tt
signiRH <- signi
}


######## QQ

if ("QQ" %in% pv) {
k <- as.character(verifout$PARA)=="QQ"
pd <- verifout[k,c(1,5,6,3,4)]

#pd <- verifout[c(6,12,18,24,30,36,42,48,54,60),c(1,5,6,3,4)]
names(pd) <- c("QQ.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

#pdmin <- data.frame(verifoutmin[c(6,12,18,24,30,36,42,48,54,60),c(1,5,6,3,4)])
#pdmax <- data.frame(verifoutmax[c(6,12,18,24,30,36,42,48,54,60),c(1,5,6,3,4)])
pdmin <- data.frame(verifoutmin[k,c(1,5,6,3,4)])
pdmax <- data.frame(verifoutmax[k,c(1,5,6,3,4)])
names(pdmin) <- c("QQ.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
names(pdmax) <- c("QQ.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

## significance check

signi <- data.frame(matrix(NA,dim(pd)[1],dim(pd)[2]))

signi[,2] <- ifelse(as.numeric(as.character(pdmax[,2])) < as.numeric(as.character(pdmin[,3])) |
                    as.numeric(as.character(pdmin[,2])) > as.numeric(as.character(pdmax[,3])),TRUE,FALSE) 
signi[,3] <- signi[,2]
signi[,4] <- ifelse(as.numeric(as.character(pdmax[,4])) < as.numeric(as.character(pdmin[,5])) |
                    as.numeric(as.character(pdmin[,4])) > as.numeric(as.character(pdmax[,5])),TRUE,FALSE) 
signi[,5] <- signi[,4]


##
pd[,1] <- ifelse (substring(pd[,1],1,7)=="Denmark", substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

pdpara <- cbind(pd[1],sapply(pd[2:5], function(x) {as.numeric(as.character(x))}))
pd <- pdpara

cols <- matrix("black", nrow(pd), ncol(pd))
colsfill <- matrix("lightgrey", nrow(pd), ncol(pd))

for (i in 1:dim(pd)[1]) {
    if (pd[i,2] < pd[i,3]) cols[i,1:3] <- c("black","darkgreen", "red")
    if (pd[i,2] > pd[i,3]) cols[i,1:3] <- c("black","red", "darkgreen")
    colsfill[i,1:3] <- c("lightgrey","lightgrey", "lightgrey")
    if (pd[i,3]/pd[i,2]  > threshcol1) colsfill[i,1:3] <- c("lightgrey","green", "pink")
    if (pd[i,2]/pd[i,3] > threshcol1) colsfill[i,1:3] <- c("lightgrey","pink", "green")
    #BIAS
    if (abs(pd[i,4]) < abs(pd[i,5])) cols[i,4:5] <- c("darkgreen", "red")
    if (abs(pd[i,4]) > abs(pd[i,5])) cols[i,4:5] <- c("red", "darkgreen")
    colsfill[i,4:5] <- c("lightgrey", "lightgrey")
    if (abs(pd[i,4]) + threshcol2 < abs(pd[i,5])) colsfill[i,4:5] <- c("green", "pink")
    if (abs(pd[i,4]) > abs(pd[i,5]) + threshcol2 ) colsfill[i,4:5] <- c("pink", "green")
}

tt <- ttheme_default(core=list(fg_params = list(col = cols),
                                bg_params = list(fill=colsfill)),
                      rowhead=list(bg_params = list(col=NA)),
                     colhead=list(bg_params = list(col=NA)))

pdQQ <- pd
ttQQ <- tt
signiQQ <- signi
}

####################################################################
####################################################################

#### PLOT:


## version II

#X11(height=18,width=12)
filename <- paste("Summary.local.",min(xalldata$validdate),"-",max(xalldata$validdate),".pdf",sep="")

pdf(filename,height=18,width=12)
par(mfrow=c(2,4), mar=c(6,6,6,6), oma=c(2,2,2,2), cex=0.8, lwd=0.25)

for (i in 1:dim(signiFF)[1]) {
    for (j in 1:dim(signiFF)[2]) {
        if (!is.na(signiMSLP[i,j])) {
            pdMSLP[i,j] <- ifelse(signiMSLP[i,j]==TRUE,paste(pdMSLP[i,j],"*",sep=""),pdMSLP[i,j])
        }
        if (!is.na(signiFF[i,j])) {
            pdFF[i,j] <- ifelse(signiFF[i,j]==TRUE,paste(pdFF[i,j],"*",sep=""),pdFF[i,j])
        }
        if (!is.na(signiTT[i,j])) {
            pdTT[i,j] <- ifelse(signiTT[i,j]==TRUE,paste(pdTT[i,j],"*",sep=""),pdTT[i,j])
        }
        if (!is.na(signiTTHC[i,j])) {
            pdTTHC[i,j] <- ifelse(signiTTHC[i,j]==TRUE,paste(pdTTHC[i,j],"*",sep=""),pdTTHC[i,j])
        }
        if (!is.na(signiRH[i,j])) {
            pdRH[i,j] <- ifelse(signiRH[i,j]==TRUE,paste(pdRH[i,j],"*",sep=""),pdRH[i,j])
        }
    }
}
 
MSLP <- tableGrob(pdMSLP, theme=ttMSLP,rows=NULL)
TT <- tableGrob(pdTT, theme=ttTT,rows=NULL)
TTHC <- tableGrob(pdTTHC, theme=ttTTHC,rows=NULL)
FF <- tableGrob(pdFF, theme=ttFF,rows=NULL)
RH <- tableGrob(pdRH, theme=ttRH,rows=NULL)


grid.arrange(
    MSLP,
    FF,
    TT,
    RH,
    nrow=4,ncol=2)

dev.off()
