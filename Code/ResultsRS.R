
# Results 1
ofs<-function(x){var(x)/(mean(x)^2)}
Samples<-1000
set.seed(1)

 Irs.m<-c()
 Irs.f<-c()
   for(i in 1:Samples){
  Irs.m[i]<- ofs(sample(RS_males[which(Age_males >(45-11))], replace = TRUE))
  Irs.f[i]<- ofs(sample(RS_females[which(Age_females >(45-11))], replace = TRUE))
                              }
                              
 Mrs.m<-c()
 Mrs.f<-c()
   for(i in 1:Samples){
  Mrs.m[i]<- mean(sample(RS_males[which(Age_males >(45-11))], replace = TRUE))
  Mrs.f[i]<- mean(sample(RS_females[which(Age_females >(45-11))], replace = TRUE))
                              }
  pc <-0.9                            
  print( c(mean(Irs.m),HPDI(Irs.m,pc)) )
  print(  c(mean(Irs.f),HPDI(Irs.f,pc))) 
  
  print( c(mean(log(Irs.m/Irs.f)),HPDI(log(Irs.m/Irs.f),pc)) ) 
   
  print( c(mean(Mrs.m),HPDI(Mrs.m,pc)) ) 
  print( c(mean(Mrs.f),HPDI(Mrs.f,pc)) )  
  
  print(length(RS_males[which(Age_males >(45-11))]) )
  print(length(RS_females[which(Age_females >(45-11))]))
  
  print(cor(SQ_PQ_SpouseYears_females_median[which(SpouseYears_females_median>0)], NumberSpouses_females[which(SpouseYears_females_median>0)]))
  print(cor(SQ_PQ_SpouseYears_males_median[which(SpouseYears_males_median>0)], NumberSpouses_males[which(SpouseYears_males_median>0)]))
   
####################################################################### Males                             
  datZ <- data.frame(YearsMarried=SQ_PQ_SpouseYears_males_median,SpouseNumber=NumberSpouses_males,RS=RS_males,Age=Age_males)   
  datZ2 <- datZ[which(datZ$YearsMarried>0),] 
    datZ2 <- datZ2[which(datZ2$Age>0),]                        
                             
datZ2$Grid<-round(datZ2$YearsMarried/5)*5
 JJ<-aggregate(datZ2$RS/datZ2$Age,list(datZ2$SpouseNumber,datZ2$Grid),mean)
 JJ2<-aggregate(datZ2$RS/datZ2$Age,list(datZ2$SpouseNumber,datZ2$Grid),length)
 colnames(JJ)<-c("SN","YM","RS")
 JJ<-data.frame(JJ)
 JJ$N<-JJ2[,3]
JJ$Sex <- rep("Males",length(JJ$N))

JJm<-JJ

p1x<-ggplot(data=JJ, aes(x=YM, y=SN)) + geom_point(aes(color=RS, size=N)) +
              scale_size_continuous(range=c(2,15)) + scale_color_continuous(low="purple",high="orange") 
p1x

####################################################################### Males
 datZ <- data.frame(YearsMarried=SQ_PQ_SpouseYears_females_median,SpouseNumber=NumberSpouses_females,RS=RS_females,Age=Age_females)   
  datZ2 <- datZ[which(datZ$YearsMarried>0),] 
    datZ2 <- datZ2[which(datZ2$Age>0),]                        
                             
p1x<-ggplot(data=datZ2, aes(x=YearsMarried, fill=RS, y=SpouseNumber)) + geom_point(aes(color=RS),position = "jitter",size=2) 
p1x



datZ2$Grid<-round(datZ2$YearsMarried/2)*2
 JJ<-aggregate(datZ2$RS/datZ2$Age,list(datZ2$SpouseNumber,datZ2$Grid),mean)
 JJ2<-aggregate(datZ2$RS/datZ2$Age,list(datZ2$SpouseNumber,datZ2$Grid),length)
 colnames(JJ)<-c("SN","YM","RS")
 JJ<-data.frame(JJ)
 JJ$N<-JJ2[,3]
 JJ$Sex <- rep("Females",length(JJ$N))
 
 JJf<-JJ
 
 JJ <- rbind(JJm,JJf)


p1x<-ggplot(data=JJ, aes(x=YM, y=SN)) + geom_point(aes(color=RS, size=N)) + facet_grid(.~Sex,scales="free")+
              scale_size_continuous(range=c(2,15)) + scale_color_continuous(low="purple",high="orange") +
            theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
            legend.text=element_text(size=12), legend.title=element_text(size=14,face="bold"))                           
                             
  p1x                           
                             
                             
                             
                             
                             
                             
                             
                             