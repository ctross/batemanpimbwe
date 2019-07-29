
model_dat$RS_males <- RS_males[which(Age_males>(45-11))];                                       
model_dat$NumberSpouses_males <- NumberSpouses_males[which(Age_males>(45-11))];           
model_dat$R_Age_males <- R_Age_males[which(Age_males>(45-11))];                
model_dat$Zeros_males <- Zeros_males[which(Age_males>(45-11))];                   

model_dat$PartnersByAge1_males <- PartnersByAge1_males[which(Age_males>(45-11)),];       
model_dat$PartnersByAge2_males <- PartnersByAge2_males[which(Age_males>(45-11)),];       
model_dat$PartnersByAge3_males <- PartnersByAge3_males[which(Age_males>(45-11)),];       
model_dat$PartnersByAge4_males <- PartnersByAge4_males[which(Age_males>(45-11)),];       
model_dat$PartnersByAge5_males <- PartnersByAge5_males[which(Age_males>(45-11)),]; 

model_dat$Age_males <- Age_males[which(Age_males>(45-11))];        

model_dat$N_males <- length(model_dat$Age_males) 
  
  

model_dat$RS_females <- RS_females[which(Age_females>(45-11))]                              
model_dat$NumberSpouses_females <- NumberSpouses_females[which(Age_females>(45-11))]     
model_dat$R_Age_females <- R_Age_females[which(Age_females>(45-11))]           
model_dat$Zeros_females <- Zeros_females[which(Age_females>(45-11))]         

model_dat$PartnersByAge1_females <- PartnersByAge1_females[which(Age_females>(45-11)),];  

model_dat$Age_females <- Age_females[which(Age_females>(45-11))]   

model_dat$N_females <- length(model_dat$Age_females) 





RS_males <- RS_males[which(Age_males>(45-11))];                                       
NumberSpouses_males <- NumberSpouses_males[which(Age_males>(45-11))];           
R_Age_males <- R_Age_males[which(Age_males>(45-11))];                
Zeros_males <- Zeros_males[which(Age_males>(45-11))];                   

PartnersByAge1_males <- PartnersByAge1_males[which(Age_males>(45-11)),];       
PartnersByAge2_males <- PartnersByAge2_males[which(Age_males>(45-11)),];       
PartnersByAge3_males <- PartnersByAge3_males[which(Age_males>(45-11)),];       
PartnersByAge4_males <- PartnersByAge4_males[which(Age_males>(45-11)),];       
PartnersByAge5_males <- PartnersByAge5_males[which(Age_males>(45-11)),]; 

Age_males <- Age_males[which(Age_males>(45-11))];        

N_males <- length(Age_males) 
  
  

RS_females <- RS_females[which(Age_females>(45-11))]                              
NumberSpouses_females <- NumberSpouses_females[which(Age_females>(45-11))]     
R_Age_females <- R_Age_females[which(Age_females>(45-11))]           
Zeros_females <- Zeros_females[which(Age_females>(45-11))]         

PartnersByAge1_females <- PartnersByAge1_females[which(Age_females>(45-11)),];  

Age_females <- Age_females[which(Age_females>(45-11))]   

N_females <- length(Age_females) 

