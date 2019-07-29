
Iter <- 3000
Warmup <- 1500   
Thin <- 1
Init <- 0
Cores <- 2
Chains <- 2
Refresh <- 1
Seed <- 123
MTD <- 15
AD <- 0.96
Metric <- "diag_e"

library(rethinking)
library(parallel)

model_code <- vector("list",6)

model_code[[1]] <- readLines(file('./Code/Model1.stan'))
model_code[[2]] <- readLines(file('./Code/Model2.stan'))
model_code[[3]] <- readLines(file('./Code/Model3.stan'))
model_code[[4]] <- readLines(file('./Code/Model4.stan'))
model_code[[5]] <- readLines(file('./Code/Model5.stan'))
model_code[[6]] <- readLines(file('./Code/Model6.stan'))

result1 <- mclapply(1:6,function(z){
 stan(model_code=model_code[[z]], data = model_dat,thin=Thin, iter = Iter, warmup=Warmup, cores=Cores, chains=Chains, refresh=Refresh,init=Init,control=list(max_treedepth=MTD, adapt_delta=AD, metric=Metric))
 },
 mc.cores = 6*Cores)    
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 