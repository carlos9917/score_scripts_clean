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
