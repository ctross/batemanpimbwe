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

 df_m <- data.frame(Age=Age_males,RS=RS_males, SpouseNumber=NumberSpouses_males, SpouseYears=YearsMarried_males,Sex="M",YearsPoly=YearsPoly_males)
 df_f <- data.frame(Age=Age_females,RS=RS_females, SpouseNumber=NumberSpouses_females, SpouseYears=YearsMarried_females,Sex="F",YearsPoly=rep(NA,length(YearsMarried_females)))

 df<-rbind(df_m,df_f)
 ggpairs(df, aes(colour = Sex, alpha = 0.4))
 ggsave("Cor_Basic.PDF",height=10,width=10)
 
 df<-rbind(df_m,df_f)
 df <- df[which(df$Age>45),]
 ggpairs(df, aes(colour = Sex, alpha = 0.4))
 ggsave("Cor_PostRep.PDF",height=10,width=10)
 
 df<-rbind(df_m,df_f)
 df <- df[which(df$SpouseYears>0),]
 ggpairs(df, aes(colour = Sex, alpha = 0.4))
 ggsave("Cor_Married.PDF",height=10,width=10)

 df<-rbind(df_m,df_f)
 df <- df[which(df$SpouseNumber>1),]
 ggpairs(df, aes(colour = Sex, alpha = 0.4))
 ggsave("Cor_Poly.PDF",height=10,width=10) 
 
 df<-rbind(df_m,df_f)
 df <- df[which(df$SpouseNumber==1),]
 ggpairs(df, aes(colour = Sex, alpha = 0.4))
 ggsave("Cor_Monog.PDF",height=10,width=10) 
 
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

 
 m1o <- lm(data=df_m[which(df_m$Age>45),], RS ~ Age + SpouseNumber )
 m2o <- lm(data=df_m[which(df_m$Age>45),], RS ~ Age + SpouseYears)
 m3o <- lm(data=df_m[which(df_m$Age>45),], RS ~ Age + SpouseNumber + SpouseYears)

 f1o <- lm(data=df_f[which(df_f$Age>45),], RS ~ Age + SpouseNumber )
 f2o <- lm(data=df_f[which(df_f$Age>45),], RS ~ Age + SpouseYears)
 f3o <- lm(data=df_f[which(df_f$Age>45),], RS ~ Age + SpouseNumber + SpouseYears)

 
 m1o2 <- lm(data=df_m[which(df_m$Age>30),], RS ~ Age + SpouseNumber )
 m2o2 <- lm(data=df_m[which(df_m$Age>30),], RS ~ Age + SpouseYears)
 m3o2 <- lm(data=df_m[which(df_m$Age>30),], RS ~ Age + SpouseNumber + SpouseYears)

 f1o2 <- lm(data=df_f[which(df_f$Age>30),], RS ~ Age + SpouseNumber )
 f2o2 <- lm(data=df_f[which(df_f$Age>30),], RS ~ Age + SpouseYears)
 f3o2 <- lm(data=df_f[which(df_f$Age>30),], RS ~ Age + SpouseNumber + SpouseYears)


  stargazer(m1,m2,m3, type = "latex")
  stargazer(f1,f2,f3, type = "latex")
  
  stargazer(m1z,m2z,m3z, type = "latex")
  stargazer(f1z,f2z,f3z, type = "latex")
  
  stargazer(m1o,m2o,m3o, type = "latex")
  stargazer(f1o,f2o,f3o, type = "latex")
  
  stargazer(m1o2,m2o2,m3o2, type = "latex")
  stargazer(f1o2,f2o2,f3o2, type = "latex")

  pdf("SY-SN.pdf")
  par(mfrow=c(1,2))
df_m2 <- df_f[which(df_f$SpouseYears>0 & df_f$RS>0 & df_f$Age>15),]
rbPal <- colorRampPalette(c('blue','red'))
df_m2$Col <- rbPal(10)[as.numeric(cut(c((df_m2$RS)),breaks = 15))][1:length(df_m2$RS)]
   plot(log(df_m2$SpouseYears)~jitter((df_m2$SpouseNumber),1.5),pch=20, col=df_m2$Col,cex=2, main="Female",ylim=c(log(1),log(80)),xlab="Spouse Number", ylab="Spouse Years")
  abline(h=20,lty=2)
  
  df_m2 <- df_m[which(df_m$SpouseYears>0 & df_m$RS>0 & df_m$Age>15),]
rbPal <- colorRampPalette(c('blue','red'))
df_m2$Col <- rbPal(10)[as.numeric(cut(c((df_m2$RS)),breaks = 15))][1:length(df_m2$RS)]
   plot(log(df_m2$SpouseYears)~jitter((df_m2$SpouseNumber),1.5),pch=20, col=df_m2$Col,cex=2, main="Male",ylim=c(log(1),log(80)),xlab="Spouse Number", ylab="Spouse Years")
  abline(h=20,lty=2)
 dev.off()




  pdf("SY-SN.pdf")
  
  par(mfrow=c(2,2))
df_m2 <- df_m[which(df_m$SpouseYears>0  & df_m$Age>25 & df_m$YearsPoly>3),]
rbPal <- colorRampPalette(c('blue','red'))
df_m2$Col <- rbPal(10)[as.numeric(cut(c(c(df_m2$RS,0,15)),breaks = 15))][1:length(df_m2$RS)]
   plot(log(df_m2$SpouseYears)~jitter((df_m2$SpouseNumber),1),pch=20, col=df_m2$Col,cex=2, main="Polygynous Males",ylim=c(log(1),log(80)),xlab="Spouse Number", ylab="Log Spouse Years")
  abline(h=20,lty=2)
  
  df_m2 <- df_m[which(df_m$SpouseYears>0 & df_m$Age>25 & df_m$YearsPoly<=3),]
rbPal <- colorRampPalette(c('blue','red'))
df_m2$Col <- rbPal(10)[as.numeric(cut(c(c(df_m2$RS,0,15)),breaks = 15))][1:length(df_m2$RS)]
   plot(log(df_m2$SpouseYears)~jitter((df_m2$SpouseNumber),1),pch=20, col=df_m2$Col,cex=2, main="Monogamous Males",ylim=c(log(1),log(80)),xlab="Spouse Number", ylab="Log Spouse Years")
  abline(h=20,lty=2)
  
  df_m2 <- df_f[which(df_f$SpouseYears>0 & df_f$Age>25),]
rbPal <- colorRampPalette(c('blue','red'))
df_m2$Col <- rbPal(10)[as.numeric(cut(c(c(df_m2$RS,0,15)),breaks = 15))][1:length(df_m2$RS)]
   plot(log(df_m2$SpouseYears)~jitter((df_m2$SpouseNumber),1),pch=20, col=df_m2$Col,cex=2, main="Monogamous Females",ylim=c(log(1),log(80)),xlab="Spouse Number", ylab="Log Spouse Years")
  abline(h=20,lty=2)
  
 dev.off()




















