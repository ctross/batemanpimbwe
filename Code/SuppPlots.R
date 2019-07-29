
##################################################################### Plot Densities
library(ggplot2);library(reshape2);library(Cairo)
 dfz <- data.frame(RS=c(RS_males,RS_females), Sex = c(rep("Male",length(RS_males)),rep("Female",length(RS_females)) ))
 
 dfz2 <- data.frame(RS=c(rpois(length(RS_males),mean(RS_males)),rpois(length(RS_females), mean(RS_females))), 
                         Sex = c(rep("Male",length(RS_males)),rep("Female",length(RS_females)) ))
 

 p0<-ggplot(dfz2) + geom_histogram(aes(x=RS),alpha=0.6, color=NA, fill="black")  + facet_grid(. ~ Sex)   + ylab("Count")+ xlab("RS")+
 scale_fill_manual("Sex", values = c("#253494","#e31a1c"),guide=FALSE)+geom_vline(xintercept = 0,linetype=2) + theme_light() +
 theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+ 
 geom_histogram(data=dfz,aes(x=RS, fill=Sex),binwidth = 1,alpha=0.4, color=NA) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"),
 legend.text=element_text(size=18), legend.title=element_text(size=18,face="bold")) 


CairoPDF("RS_Check",height=8,width=8)
  plot(p0)
 dev.off() 
 
 
 


 CairoPDF("Trace1",height=8,width=8)
 traceplot(result1[[1]],pars=c("Beta_males","Beta_females"))
  dev.off() 
  
  CairoPDF("Trace2",height=8,width=8)
 traceplot(result1[[2]],pars=c("Beta_males","Beta_females"))
 dev.off()
 
 CairoPDF("Trace3",height=8,width=8)
 traceplot(result1[[3]],pars=c("Beta_males","Beta_females"))
   dev.off()  
   
 CairoPDF("Trace4",height=8,width=8) 
 traceplot(result1[[4]],pars=c("Beta_males","Beta_females"))
  dev.off()  

CairoPDF("Trace5",height=8,width=8)
 traceplot(result1[[5]],pars=c("Beta_males","Beta_females"))
 dev.off()  
    
 CairoPDF("Trace6",height=8,width=8)
 traceplot(result1[[5]],pars=c("Beta_males","Beta_females")) 
  dev.off() 
 
 
 
 