fitDem <- result1[[2]]
 library(rethinking)
 library(Cairo)
 
  Samples <- ((Iter-Warmup)*Chains)
  
###################################################################### Check Estimates
 print(fitDem,digits_summary=3,pars=c("Beta_males","Beta_females"))

##################################################################### Plot Densities
 BM<-extract(fitDem,pars="Beta_males")$Beta_males
 BF<-extract(fitDem,pars="Beta_females")$Beta_females
 x <- data.frame(Male=exp(BM[,4]),Female=exp(BF[,4]))
 library(ggplot2);library(reshape2);library(Cairo)
 data<- melt(x)
 p1<-ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.4, colour=NA)

 q5a <- quantile(x$Male,.05)
 q95a <- quantile(x$Male,.95)
 medxa <- median(x$Male)
 x.densa <- density(x$Male)
 df.densa <- data.frame(x = x.densa$x, y = x.densa$y)
 p1<-p1 + geom_area(data = subset(df.densa, x >= q5a & x <= q95a),
 aes(x=x,y=y), fill = '#253494',alpha=0.4)
 q5b <- quantile(x$Female,.05)
 q95b <- quantile(x$Female,.95)
 medxb <- median(x$Female)
 x.densb <- density(x$Female)
 df.densb <- data.frame(x = x.densb$x, y = x.densb$y)
 p1<-p1 + geom_area(data = subset(df.densb, x >= q5b & x <= q95b),
 aes(x=x,y=y), fill = '#e31a1c',alpha=0.6)   + ylab("Density")+xlab("Elasticity: Timing- and Quality-\nWeighted Years Married")+
 scale_fill_manual("Sex", values = c("#253494","#e31a1c"))+geom_vline(xintercept = 1) +
 theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
 legend.text=element_text(size=12), legend.title=element_text(size=14,face="bold"))
# CairoPDF("ElastSY",height=8,width=8)
#  p1
# dev.off()



#############################################################################


library(rethinking)
MaxExposure<-86

EffectiveSpousesByAge_males<-array(NA,c(N_males,MaxExposure,Samples))
 ESA<-c()
 Scrap<-c()

################################################################################################################################
################################################################################ Effective Spouses by Age - Males - Mate Quality
################################################################################################################################
for(k in 1:Samples){
 for (i in 1:N_males){
  for (a in 1:Age_males[i]){
   if(PartnersByAge1_males[i,a+11]==0){ESA[1] <-0;}
    else{ ESA[1] <-1;}

   if(PartnersByAge2_males[i,a+11]==0){ESA[2] <-0;}
    else{ ESA[2] <-1;}

   if(PartnersByAge3_males[i,a+11]==0){ESA[3] <-0;}
    else{ ESA[3] <-1;}

   if(PartnersByAge4_males[i,a+11]==0){ESA[4] <-0;}
    else{ ESA[4] <-1;}

   if(PartnersByAge5_males[i,a+11]==0){ESA[5] <-0;}
    else{ ESA[5] <-1;}

   EffectiveSpousesByAge_males[i,a,k] <- ESA[1]+ESA[2]+ESA[3]+ESA[4]+ESA[5];
   }}}

################################################################################################################################
############################################################################## Effective Spouses by Age - Females - Mate Quality
################################################################################################################################

EffectiveSpousesByAge_females<-array(NA,c(N_females,MaxExposure,Samples))

for(k in 1:Samples){
 for (i in 1:N_females){
  for (a in 1:Age_females[i]){
   if(PartnersByAge1_females[i,a+11]==0){EffectiveSpousesByAge_females[i,a,k] <-0;}
     else{EffectiveSpousesByAge_females[i,a,k] <-1;}
     }}
   }

################################################################################################################################
################################################################################## Effective Partner Years - Males - Mate Timing
################################################################################################################################
SpouseYears_males2<-array(NA,c(N_males,Samples))
SpouseYears_males<-array(NA,c(N_males,Samples))

for(k in 1:Samples){
 for (i in 1:N_males){
   Ticker <- 0;
   Ticker2 <- 0;
   Ticker3 <- 0;
   Ticker4 <- 0;
 for (a in 1:Age_males[i]){
   Ticker2 <- Ticker2 + EffectiveSpousesByAge_males[i,a,k];
   Ticker4 <- Ticker4 +  ifelse(PartnersByAge1_males[i,a+11]==0,0,1)+ifelse(PartnersByAge2_males[i,a+11]==0,0,1)+ifelse(PartnersByAge3_males[i,a+11]==0,0,1)+ifelse(PartnersByAge4_males[i,a+11]==0,0,1)+ifelse(PartnersByAge5_males[i,a+11]==0,0,1)
   }

   SpouseYears_males2[i,k] <- Ticker2;
   SpouseYears_males[i,k] <- Ticker4;
   }
   }

SpouseYears_males_median <- SQ_SpouseYears_males_median <- PQ_SpouseYears_males_median <- SQ_PQ_SpouseYears_males_median <- c()
for(i in 1:N_males){
SpouseYears_males_median[i]<-median(SpouseYears_males[i,])
   }

################################################################################################################################
################################################################################ Effective Partner Years - Females - Mate Timing
################################################################################################################################

SpouseYears_females2<-array(NA,c(N_females,Samples))
SpouseYears_females<-array(NA,c(N_females,Samples))

for(k in 1:Samples){
 for (i in 1:N_females){
   Ticker <- 0;
   Ticker2 <- 0;
   Ticker3 <- 0;
   Ticker4 <- 0;
 for (a in 1:Age_females[i]){
   Ticker2 <- Ticker2 + EffectiveSpousesByAge_females[i,a,k];
   Ticker4 <- Ticker4 + ifelse(PartnersByAge1_females[i,a+11]==0,0,1)
   }
   SpouseYears_females2[i,k] <- Ticker2;
   SpouseYears_females[i,k] <- Ticker4;
   }
   }


 SpouseYears_females_median <- PQ_SpouseYears_females_median <- SQ_SpouseYears_females_median <- SQ_PQ_SpouseYears_females_median <- c()
for(i in 1:N_females){
SpouseYears_females_median[i]<-median(SpouseYears_females[i,])
   }
   
######################################################################### Slopes
 predMaleSN <-matrix(NA,nrow=Samples,ncol=40)
 for(X in 1:40){
 for(i in 1:Samples){
  predMaleSN[i,X] <- exp(BM[i,1])*((45-11)^BM[i,2])*exp(BM[i,4])*(X^(exp(BM[i,4])-1))
 }
 }
 
medm   <- c() 
hdpiLm <- c() 
hpdiHm <- c() 

for(X in 1:40){
medm[X] <- median( predMaleSN[,X] )
hdpiLm[X] <- PCI( predMaleSN[,X],0.9 )[1]
hpdiHm[X] <- PCI( predMaleSN[,X],0.9 )[2]
}


 predFemaleSN <-matrix(NA,nrow=Samples,ncol=40)
 for(X in 1:40){
 for(i in 1:Samples){
  predFemaleSN[i,X] <-exp(BF[i,1])*((45-11)^BF[i,2])*exp(BF[i,4])*(X^(exp(BF[i,4])-1))
 }
 }
 
medf   <- c() 
hdpiLf <- c() 
hpdiHf <- c() 

for(X in 1:40){
medf[X] <- median( predFemaleSN[,X] )
hdpiLf[X] <- PCI( predFemaleSN[,X],0.9 )[1]
hpdiHf[X] <- PCI( predFemaleSN[,X],0.9 )[2]
}


dfplot <-data.frame(Median=c(medm,medf),Low=c(hdpiLm,hdpiLf), High=c(hpdiHm,hpdiHf), Sex=c(rep(c("Male","Female"),each=40)),YearsMarried=c(1:40,1:40))

Mzz <- PCI(SpouseYears_males_median[which(Age_males>32 & Age_males<36 & SpouseYears_males_median>0)],0.9)
Fzz <- PCI(SpouseYears_females_median[which(Age_females>32 & Age_females<36 & SpouseYears_females_median>0)],0.9) 


p1x<-ggplot(data=dfplot,aes(x=YearsMarried, fill=Sex, y=Median)) + geom_line(aes(color=Sex)) +  geom_ribbon(aes(ymin=Low, ymax=High),alpha=0.4) +
  ylab("Slope of RS on Years Married")+xlab("Years Married")+  scale_color_manual("Sex", values = c("#253494","#e31a1c"))  +
 scale_fill_manual("Sex", values = c("#253494","#e31a1c"))+geom_hline(yintercept = 0) +  ylim(0,0.6) +
 theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
 legend.text=element_text(size=12), legend.title=element_text(size=14,face="bold"))  +
 geom_vline(xintercept = Mzz,color="#e31a1c",linetype="dashed") + geom_vline(xintercept = Fzz,color="#253494",linetype="dashed")
 
 CairoPDF("ElastSY_slope1",height=8,width=8)
  print(p1x)
 dev.off() 
 
 
########
###################################################################### Print Estimates
library(xtable)
library(SkewCalc)
library(rethinking)
cv<-function(x){var(x)/(mean(x)^2)}

 BM<-extract(fitDem,pars="Beta_males")$Beta_males
 BF<-extract(fitDem,pars="Beta_females")$Beta_females

 Irs.m<-c()
 Irs.f<-c()
   for(i in 1:Samples){
  Irs.m[i]<- cv(sample(RS_males[which(Age_males >(45-11))], replace = TRUE))
  Irs.f[i]<- cv(sample(RS_females[which(Age_females >(45-11))], replace = TRUE))
                              }

 Mrs.m<-c()
 Mrs.f<-c()
   for(i in 1:Samples){
     xm1 <- sample(c(1:length(RS_males)), replace = TRUE)
     xf1 <- sample(c(1:length(RS_females)), replace = TRUE)
   Mrs.m[i]<- (M_index(RS_males[xm1],Age_males[xm1])/sqrt(mean(RS_males[xm1])))^2
   Mrs.f[i]<- (M_index(RS_females[xf1],Age_females[xf1])/sqrt(mean(RS_females[xf1])))^2
                              }

 Ims.m<-c()
 Ims.f<-c()
      for(i in 1:Samples){
 Ims.m[i]<- cv(sample(SpouseYears_males[which(Age_males >(45-11)),i], replace = TRUE))
 Ims.f[i]<- cv(sample(SpouseYears_females[which(Age_females >(45-11)),i], replace = TRUE))
                              }

 Mms.m<-c()
 Mms.f<-c()
   for(i in 1:Samples){
     xm1 <- sample(c(1:length(RS_males)), replace = TRUE)
     xf1 <- sample(c(1:length(RS_females)), replace = TRUE)
   Mms.m[i]<- (M_index(SpouseYears_males[xm1,i],Age_males[xm1])/sqrt(mean(SpouseYears_males[xm1,i])))^2
   Mms.f[i]<- (M_index(SpouseYears_females[xf1,i],Age_females[xf1])/sqrt(mean(SpouseYears_females[xf1,i])))^2
                              }
 pc<-0.9
 parM <-   round(c(
  c(median(Irs.m),HPDI(Irs.m,pc)), # I RS Male
  c(median(Mrs.m),HPDI(Mrs.m,pc)), # M RS Male

  c(median(Ims.m),HPDI(Ims.m,pc)), # I MS Male
  c(median(Mms.m),HPDI(Mms.m,pc)), # M MS Male

  c(median(BM[,1]),HPDI(BM[,1],pc)),     # Intercept Male
  c(median(BM[,2]),HPDI(BM[,2],pc)),     # Age Male
  c(median(BM[,3]),HPDI(BM[,3],pc)),     # MS Male
  c(median(exp(BM[,4])),HPDI(exp(BM[,4]),pc))      # MS Male
  ),2)

 parM <- as.vector(parM)


 parF <-   round(c(
  c(median(Irs.f),HPDI(Irs.f,pc)), # I RS Female
  c(median(Mrs.f),HPDI(Mrs.f,pc)), # M RS Female

  c(median(Ims.f),HPDI(Ims.f,pc)), # I MS Female
  c(median(Mms.f),HPDI(Mms.f,pc)), # M MS Female

  c(median(BF[,1]),HPDI(BF[,1],pc)),
  c(median(BF[,2]),HPDI(BF[,2],pc)),
  c(median(BF[,3]),HPDI(BF[,3],pc)),
  c(median(exp(BF[,4])),HPDI(exp(BF[,4]),pc))
     ),2)
  parF <-  as.vector(parF)


   parD <- round(c(
  c(median(log(Irs.m/Irs.f)),HPDI(log(Irs.m/Irs.f),pc)), # dI RS
  c(median(log(Mrs.m/Mrs.f)),HPDI(log(Mrs.m/Mrs.f),pc)), # dM RS

  c(median(log(Ims.m/Ims.f)),HPDI(log(Ims.m/Ims.f),pc)), # dI RS
  c(median(log(Mms.m/Mms.f)),HPDI(log(Mms.m/Mms.f),pc)), # dM RS

  c(median(BM[,1]-BF[,1]),HPDI(BM[,1]-BF[,1],pc)),
  c(median(BM[,2]-BF[,2]),HPDI(BM[,2]-BF[,2],pc)),
  c(median(BM[,3]-BF[,3]),HPDI(BM[,3]-BF[,3],pc)),
  c(median(exp(BM[,4])-exp(BF[,4])),HPDI(exp(BM[,4])-exp(BF[,4]),pc))
     ),2)
  parD <-  as.vector(parD)



########################################################################## WAIC
Beta_males<-extract(fitDem,pars="Beta_males")$Beta_males
Sigma_males<-extract(fitDem,pars="Sigma_males")$Sigma_males

Beta_females<-extract(fitDem,pars="Beta_females")$Beta_females
Sigma_females<-extract(fitDem,pars="Sigma_females")$Sigma_females

A_males<-array(NA,c(N_males,Samples))
B_males<-array(NA,c(N_males,Samples))

A_females<-array(NA,c(N_females,Samples))
B_females<-array(NA,c(N_females,Samples))

###### Male Link Functions
for(k in 1:Samples){
 for (i in 1:N_males){
  if(Zeros_males[i]==0){
   B_males[i,k]<-1/Sigma_males[k,1];
   A_males[i,k] <- exp(Beta_males[k,1] + Beta_males[k,2]*log(Age_males[i]) + exp(Beta_males[k,4])*log(SpouseYears_males[i,k]));
   } else{
   B_males[i,k]<- -1
   A_males[i,k] <- -1
   }
   }  }

###### Female Link Functions
for(k in 1:Samples){
 for (i in 1:N_females){
 if(Zeros_females[i]==0){
   B_females[i,k] <- 1/Sigma_females[k,1];
   A_females[i,k] <- exp(Beta_females[k,1] + Beta_females[k,2]*log(Age_females[i]) + exp(Beta_females[k,4])*log(SpouseYears_females[i,k]));
    } else{
   B_females[i,k] <- -1
   A_females[i,k] <- -1
   }
   } }


# Set Range of Samples to Use for WAIC Computation
Q1 <-1
Q <- Samples

# Then for each data point, for each sample, compute log-likelihood
# pD is sum over data of variance of log-lik
pD <- 0

for ( i in 1:N_males ) {
 if(  A_males[i,1]>0){
    ll<-c()
# compute variance of log-lik for point i over posterior
 for(q in Q1:Q){
    ll[q] <- dgampois(RS_males[i], A_males[i,q],(1/B_males[i,q]), log=TRUE )
               }
    pD <- pD + var(ll,na.rm=T)
}}

# Then for each data point, for each sample, compute log of average likelihood
# lppd is log posterior likelihood
lppd <- 0
for ( i in 1:N_males ){
 if(  A_males[i,1]>0){
  ll<-c()
    # compute log of average likelihood
    for(q in Q1:Q){
    ll[q] <- dgampois(RS_males[i], A_males[i,q],(1/B_males[i,q]), log=FALSE )
                 }
    lppd <- lppd + log(mean(ll,na.rm=T))
}  }

# WAIC
WAIC<- -2*(lppd - pD)

# Return Results
waicM<-round(cbind(pD,lppd,WAIC),2)


# Set Range of Samples to Use for WAIC Computation
Q1 <-1
Q <- Samples

# Then for each data point, for each sample, compute log-likelihood
# pD is sum over data of variance of log-lik
pD <- 0

for ( i in 1:N_females ) {
 if(  A_females[i,1]>0){
    ll<-c()
# compute variance of log-lik for point i over posterior
 for(q in Q1:Q){
    ll[q] <- dgampois(RS_females[i], A_females[i,q],(1/B_females[i,q]), log=TRUE )
               }
    pD <- pD + var(ll,na.rm=T)
}}

# Then for each data point, for each sample, compute log of average likelihood
# lppd is log posterior likelihood
lppd <- 0
for ( i in 1:N_females ){
 if(  A_females[i,1]>0){
  ll<-c()
    # compute log of average likelihood
    for(q in Q1:Q){
    ll[q] <- dgampois(RS_females[i], A_females[i,q],(1/B_females[i,q]), log=FALSE )
                 }
    lppd <- lppd + log(mean(ll,na.rm=T))
}  }

# WAIC
WAIC<- -2*(lppd - pD)

# Return Results
waicF<-round(cbind(pD,lppd,WAIC),2)

ResTab <- rbind(c(parM,waicM),c(parF,waicF), c(parD,rep(NA,3)))
colnames(ResTab) <- c("Irs","IrsL","IrsH","Mrs","MrsL","MrsH","Ims","ImsL","ImsH","Mms","MmsL","MmsH",
                       "Incpt","IncptL","IncptH","Age","AgeL","AgeH","Spouses","SpousesL","SpousesH",
                       "SpouseYears","SpouseYearsL","SpouseYearsH","pD","lppd","WAIC" )
rownames(ResTab) <- c("Males","Females","Delta")

write.csv(ResTab, "Bateman-SY.csv")



