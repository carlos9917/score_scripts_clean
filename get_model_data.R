####################################################################
get_model_data <- function(model,files)
{
fctot <- NULL
fc <- NULL

if (model=="nea") {
    print(model)
        for (ii in 1:length(files)) {
            infile <- files[[model]][ii]
            m <- paste(infile,sep="")            
	    cat("reading  ",m, "\n")
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
            fc <- d[k,]
            names(fc) <- c("WMO","LAT","LON",para)
            fc[,] <- lapply(fc, function(x) {as.numeric(as.character(x))})
	    fcfilename=basename(infile)
	    fc$validdate <-(substring(fcfilename,8,17))
	    fc$LT <-(substring(fcfilename,18,19))
        if ( !("FI" %in% names(fc)) ) fc$FI <- -99
        if ( !("NN" %in% names(fc)) ) fc$NN <- -99
    	if ( !("DD" %in% names(fc)) ) fc$DD <- -99
    	if ( !("FF" %in% names(fc)) ) fc$FF <- -99
    	if ( !("TT" %in% names(fc)) ) fc$TT <- -99
    	if ( !("RH" %in% names(fc)) ) fc$RH <- -99
    	if ( !("PS" %in% names(fc)) ) fc$PS <- -99
    	if ( !("PE" %in% names(fc)) ) fc$PE <- -99
    	if ( !("QQ" %in% names(fc)) ) fc$QQ <- -99
    	if ( !("VI" %in% names(fc)) ) fc$VI <- -99
    	if ( !("TD" %in% names(fc)) ) fc$TD <- -99
    	if ( !("TX" %in% names(fc)) ) fc$TX <- -99
        if ( !("TN" %in% names(fc)) ) fc$TN <- -99
        if ( !("GX" %in% names(fc)) ) fc$GX <- -99

            if (ii==1) fctot <- fc
            if (ii>1)  fctot <- rbind(fc,fctot)
    }
 } # nea model
 if (model=="ec9") {
for (ii in 1:length(files)) {
            infile <- files[[model]][ii]
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
            fc <- d[k,]
            names(fc) <- c("WMO","LAT","LON",para)
            fc[,] <- lapply(fc, function(x) {as.numeric(as.character(x))})
            fcfilename=basename(infile)
            fc$validdate <-(substring(fcfilename,8,17))
            fc$LT <- (substring(fcfilename,18,19))
            
            if ( !("FI" %in% names(fc)) ) fc$FI <- -99
            if ( !("NN" %in% names(fc)) ) fc$NN <- -99
            if ( !("DD" %in% names(fc)) ) fc$DD <- -99
            if ( !("FF" %in% names(fc)) ) fc$FF <- -99
            if ( !("TT" %in% names(fc)) ) fc$TT <- -99
            if ( !("RH" %in% names(fc)) ) fc$RH <- -99
            if ( !("PS" %in% names(fc)) ) fc$PS <- -99
            if ( !("PE" %in% names(fc)) ) fc$PE <- -99
            if ( !("QQ" %in% names(fc)) ) fc$QQ <- -99
            if ( !("VI" %in% names(fc)) ) fc$VI <- -99
            if ( !("CH" %in% names(fc)) ) fc$CH <- -99
            if ( !("LC" %in% names(fc)) ) fc$LC <- -99
            if ( !("TD" %in% names(fc)) ) fc$TD <- -99
            if ( !("TX" %in% names(fc)) ) fc$TX <- -99
            if ( !("TN" %in% names(fc)) ) fc$TN <- -99
            if ( !("GG" %in% names(fc)) ) fc$GG <- -99
            if ( !("GX" %in% names(fc)) ) fc$GX <- -99

            if (ii==1) fctot <- fc
            if (ii>1)  fctot <- rbind(fc,fctot) # this will fail the 2nd time bc second file has more cols

        }



 } #ec9 model

     return(fctot)
}
