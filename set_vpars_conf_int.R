####################################################################
#initialize the verifpara arrays for the scores
#and their confidence intervals
# Note the thresholds set by hand here

#set_vpars_conf_int <- function(x,np,st,pv,slist,vpar,vparmin,vparmax)
#{
	TT_thres <- 200.
	FF_thres <- 0.
	RH_thres <- 0.

for (i in 1:np) {
      cat("Going through variable ",pv[i],"\n")
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
            
            k <- (obs > TT_thres & fc1 > TT_thres & fc2 > TT_thres)
            
            obs <- obs[k]-273.14 #convert to Celsius
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
        
    } #TT
         
# TTHC
        if (pv[i]=="TTHC") {
            obs <- x$TT.obs
            fc1 <- x$TT.fc1 + 0.0065*(x$FI.fc1-x$AMSL.obs)
            fc2 <- x$TT.fc2 + 0.0065*(x$FI.fc2-x$AMSL.obs)
            
            k <- (obs > TT_thres  & fc1 > TT_thres  & fc2 > TT_thres)
            obs <- obs[k]-273.14 # convert to Celsius
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
            
            k <- (obs >= FF_thres & fc1 >= FF_thres & fc2 >= FF_thres)
            
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
        } #PSS
# RH
        if (pv[i]=="RH") {
            obs <- x$RH.obs
            fc1 <- x$RH.fc1
            fc2 <- x$RH.fc2
            
            obs <- ifelse(obs > 100., 100., obs) #correct for unphysical values
            fc1 <- ifelse(fc1 > 100., 100., fc1)
            fc2 <- ifelse(fc2 > 100., 100., fc2)
            
            k <- (obs >= RH_thres & fc1 >= RH_thres & fc2 >= RH_thres)
            
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
        } #RH
# QQ
        if (pv[i]=="QQ") {
            obs <- x$QQ.obs
            fc1 <- x$QQ.fc1
            fc2 <- x$QQ.fc2
            
            obs <- ifelse(obs > 100., 100., obs) #correct for unphysical values
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
        } #QQ

} #for loop

    
      
#}
