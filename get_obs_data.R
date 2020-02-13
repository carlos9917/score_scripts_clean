####################################################################
# Read observation data and place it in a dataframe
get_obs_data <- function(obsfiles)
{
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
    #print(obstot)
    return(obstot)
}

