####################################################################################
# Prepare Scorecard ################################################################
####################################################################################

#prep_scorecard <- function(ver_region,pv)
#{

ver_region <- "Denmark"
score_header <- c("MSLP.in.region","nea.std","ec9.std","nea.bias","ec9.bias")
#threshcol1 <- 1.20   # % difference to color box in scorecard 1.20 = 20%
#threshcol2 <- 0.25   # absolute difference to color box in scorecard, i.e. used for biases

#Temperature;
if ("TT" %in% pv) {
k <- as.character(verifout$PARA)=="TT"
pd <- verifout[k,c(1,5,6,3,4)]
names(pd) <- c("TT.in.region","nea.std","ec9.std","nea.bias","ec9.bias")

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
pd[,1] <- ifelse (substring(pd[,1],1,7)==ver_region, substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

pdpara <- cbind(pd[1],sapply(pd[2:5], function(x) {as.numeric(as.character(x))}))
pd <- pdpara

cols <- matrix("black", nrow(pd), ncol(pd))
colsfill <- matrix("lightgrey", nrow(pd), ncol(pd))
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

#print("passed loop")
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

pd[,1] <- ifelse (substring(pd[,1],1,7)==ver_region, substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

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
pd[,1] <- ifelse (substring(pd[,1],1,7)==ver_region, substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

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
pd[,1] <- ifelse (substring(pd[,1],1,7)==ver_region, substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

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
pd[,1] <- ifelse (substring(pd[,1],1,7)==ver_region, substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

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
pd[,1] <- ifelse (substring(pd[,1],1,7)==ver_region, substring(as.character(pd[,1]),8,30),substring(as.character(pd[,1]),1,30))

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
#}
