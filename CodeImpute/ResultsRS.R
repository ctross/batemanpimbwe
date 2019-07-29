
# Results 1
ofs<-function(x){var(x)/(mean(x)^2)}
Samples<-1000
set.seed(1)

 Irs.m<-c()
 Irs.f<-c()
   for(i in 1:Samples){
  Irs.m[i]<- ofs(sample(RS_males[which(Age_males >45)], replace = TRUE))
  Irs.f[i]<- ofs(sample(RS_females[which(Age_females >45)], replace = TRUE))
                              }
                              
 Mrs.m<-c()
 Mrs.f<-c()
   for(i in 1:Samples){
  Mrs.m[i]<- mean(sample(RS_males[which(Age_males >45)], replace = TRUE))
  Mrs.f[i]<- mean(sample(RS_females[which(Age_females >45)], replace = TRUE))
                              }
                              
  print( c(mean(Irs.m),HPDI(Irs.m,0.95)) )
  print(  c(mean(Irs.f),HPDI(Irs.f,0.95)))  
   
  print( c(mean(Mrs.m),HPDI(Mrs.m,0.95)) ) 
  print( c(mean(Mrs.f),HPDI(Mrs.f,0.95)) )  
  
  print(length(RS_males[which(Age_males >45)]) )
  print(length(RS_females[which(Age_females >45)]))
   
                             