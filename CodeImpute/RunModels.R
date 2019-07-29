
Iter <- 3000
Warmup <- 1500   
Thin <- 1

Cores <- 2
Chains <- 2
Refresh <- 1
Seed <- 123
MTD <- 14
AD <- 0.95
Metric <- "diag_e"

model_dat$A<-100

library(rethinking)
library(parallel)

model_code <- vector("list",6)

model_code[[1]] <- readLines(file('./CodeImpute/Model1.stan'))
model_code[[2]] <- readLines(file('./CodeImpute/Model2.stan'))
model_code[[3]] <- readLines(file('./CodeImpute/Model3.stan'))
model_code[[4]] <- readLines(file('./CodeImpute/Model4.stan'))
model_code[[5]] <- readLines(file('./CodeImpute/Model5.stan'))
model_code[[6]] <- readLines(file('./CodeImpute/Model6.stan'))

result1 <- mclapply(1:6,function(z){
 stan(model_code=model_code[[z]], data = model_dat,thin=Thin, iter = Iter, warmup=Warmup, cores=Cores, chains=Chains, seed=33402, refresh=Refresh,control=list(max_treedepth=MTD, adapt_delta=AD))
 },
 mc.cores = 6*Cores)  
 
 save.image("PimbRes-2-17-19.RData")  
  
 
 
 
 
 
 
 