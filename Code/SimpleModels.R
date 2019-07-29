################################################################# Load Libraries
library(GGally)
library(rethinking)

MaxExposure<-86

EffectiveSpousesByAge_males<-array(NA,c(N_males,MaxExposure))
 ESA<-c()
 Scrap<-c()

################################################################################
################################ Effective Spouses by Age - Males - Mate Quality

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

   EffectiveSpousesByAge_males[i,a] <- ESA[1]+ESA[2]+ESA[3]+ESA[4]+ESA[5];
   }}

################################################################################
############################## Effective Spouses by Age - Females - Mate Quality

EffectiveSpousesByAge_females<-array(NA,c(N_females,MaxExposure))


 for (i in 1:N_females){
  for (a in 1:Age_females[i]){
   if(PartnersByAge1_females[i,a+11]==0){EffectiveSpousesByAge_females[i,a] <-0;}
     else{EffectiveSpousesByAge_females[i,a] <-1;}
     }}
   

  YearsMarried_males <- rowSums(EffectiveSpousesByAge_males,na.rm=TRUE)
  YearsMarried_females <- rowSums(EffectiveSpousesByAge_females,na.rm=TRUE)
  
  YearsPoly_males <- rowSums(ifelse(EffectiveSpousesByAge_males>1,1,0),na.rm=TRUE)

############################################################## Correlation plots
 library(GGally)
 library(stargazer)
 df_m <- data.frame(Age=Age_males,RS=RS_males, SpouseNumber=NumberSpouses_males, SpouseYears=YearsMarried_males,Sex="M")
 df_f <- data.frame(Age=Age_females,RS=RS_females, SpouseNumber=NumberSpouses_females, SpouseYears=YearsMarried_females,Sex="F")

 df<-rbind(df_m,df_f)
 aa<- ggpairs(df, lower=list(mapping=aes(colour = factor(Sex), alpha = 0.4)))
 ggsave("Cor_Basic.PDF",aa,height=10,width=10)
 
 df<-rbind(df_m,df_f)
 df <- df[which(df$Age>(45-11)),]
 bb<-ggpairs(df, lower=list(mapping=aes(colour = Sex, alpha = 0.4)))
 ggsave("Cor_PostRep.PDF",bb,height=10,width=10)
 
 df<-rbind(df_m,df_f)
 df <- df[which(df$SpouseYears>0),]
 cc<-ggpairs(df, lower=list(mapping=aes(colour = Sex, alpha = 0.4)))
 ggsave("Cor_Married.PDF",cc,height=10,width=10)

 df<-rbind(df_m,df_f)
 df <- df[which(df$SpouseNumber>1),]
 dd<-ggpairs(df, lower=list(mapping=aes(colour = Sex, alpha = 0.4)))
 ggsave("Cor_Poly.PDF",dd,height=10,width=10) 
 
 df<-rbind(df_m,df_f)
 df <- df[which(df$SpouseNumber==1),]
 ee<-ggpairs(df, lower=list(mapping=aes(colour = Sex, alpha = 0.4)))
 ggsave("Cor_Monog.PDF",ee,height=10,width=10) 
 
####################################################################### Analysis


 m1 <- lm(data=df_m, RS ~ Age + SpouseNumber )
 m2 <- lm(data=df_m, RS ~ Age + SpouseYears)
 m3 <- lm(data=df_m, RS ~ Age + SpouseNumber + SpouseYears)

 f1 <- lm(data=df_f, RS ~ Age + SpouseNumber )
 f2 <- lm(data=df_f, RS ~ Age + SpouseYears)
 f3 <- lm(data=df_f, RS ~ Age + SpouseNumber + SpouseYears)

 
 m1z <- lm(data=df_m[which(df_m$SpouseYears>0),], RS ~ Age + SpouseNumber )
 m2z <- lm(data=df_m[which(df_m$SpouseYears>0),], RS ~ Age + SpouseYears)
 m3z <- lm(data=df_m[which(df_m$SpouseYears>0),], RS ~ Age + SpouseNumber + SpouseYears)

 f1z <-  lm(data=df_f[which(df_f$SpouseYears>0),], RS ~ Age + SpouseNumber )
 f2z <-  lm(data=df_f[which(df_f$SpouseYears>0),], RS ~ Age + SpouseYears)
 f3z <-  lm(data=df_f[which(df_f$SpouseYears>0),], RS ~ Age + SpouseNumber + SpouseYears)

 
 m1o <- lm(data=df_m[which(df_m$Age>(45-11)),], RS ~ Age + SpouseNumber )
 m2o <- lm(data=df_m[which(df_m$Age>(45-11)),], RS ~ Age + SpouseYears)
 m3o <- lm(data=df_m[which(df_m$Age>(45-11)),], RS ~ Age + SpouseNumber + SpouseYears)

 f1o <- lm(data=df_f[which(df_f$Age>(45-11)),], RS ~ Age + SpouseNumber )
 f2o <- lm(data=df_f[which(df_f$Age>(45-11)),], RS ~ Age + SpouseYears)
 f3o <- lm(data=df_f[which(df_f$Age>(45-11)),], RS ~ Age + SpouseNumber + SpouseYears)

 
  stargazer(m1,m2,m3, type = "latex")
  stargazer(f1,f2,f3, type = "latex")
  
  stargazer(m1z,m2z,m3z, type = "latex")
  stargazer(f1z,f2z,f3z, type = "latex")
  
  stargazer(m1o,m2o,m3o, type = "latex")
  stargazer(f1o,f2o,f3o, type = "latex")
  
 
################# SOM density plots
 
 dens(RS_females[which(Zeros_females==1)], xlab="RS")
 dens(RS_males[which(Zeros_males==1)],xlab="RS")



################################################################################
# Start by loading data
yob<-model_dat$YOB_scaled_females + 1915
fr <-I(model_dat$RS_females/(model_dat$Age_females ))
ag <- model_dat$Age_females

thresh <- 2010
Athresh <- 19
yob2f <- yob[which(yob<thresh)]
fr2f <- fr[which(yob<thresh)]
ag2f <- ag[which(yob<thresh)]

resF <- lm(fr2f[which(ag2f>Athresh)]~yob2f[which(ag2f>Athresh)])
summary(resF)

plot(fr2f[which(ag2f>Athresh)]~yob2f[which(ag2f>Athresh)], main="Women", xlab="Year of Birth", ylab="Fertility rate (RS/(Age-11))")


################################################################################
# Start by loading data
yob<-model_dat$YOB_scaled_males + 1911
fr <-I(model_dat$RS_males/(model_dat$Age_males))
ag <- model_dat$Age_males

yob2m <- yob[which(yob<thresh)]
fr2m <- fr[which(yob<thresh)]
ag2m <- ag[which(yob<thresh)]

resM <- lm(fr2m[which(ag2m>Athresh)]~yob2m[which(ag2m>Athresh)])

summary(resM)

plot(fr2m[which(ag2m>Athresh)]~yob2m[which(ag2m>Athresh)], main="Men", xlab="Year of Birth", ylab="Fertility rate (RS/(Age-11))")



  stargazer(resM,resF, type = "latex")













