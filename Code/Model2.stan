//##################################################################### Data Block
data {
//######################################################################## Indexes
  int N_males;                                    // Indexes
  int N_females;                                  //
  int MaxExposure;                                //
  int A;                                          //

  int RS_males[N_males];                          // Time invariant male data
  int Age_males[N_males];                         //
  int NumberSpouses_males[N_males];               //
  vector[N_males] R_Age_males;                    //
  int Zeros_males[N_males];                       //

  int PartnersByAge1_males [N_males,A];           // Time varying male data
  int PartnersByAge2_males [N_males,A];           //
  int PartnersByAge3_males [N_males,A];           //
  int PartnersByAge4_males [N_males,A];           //
  int PartnersByAge5_males [N_males,A];           //

  int RS_females[N_females];                      // Time invariant female data
  int Age_females[N_females];                     //
  int NumberSpouses_females[N_females];           //
  vector[N_females] R_Age_females;                //
  int Zeros_females[N_females];                   //

  int PartnersByAge1_females [N_females,A];       // Time varying female data
}

//############################################################### Parameters Block
parameters{
//################################################################ Male Parameters
  vector[4] Beta_males;                          // RS Model Parameters - Married
  vector[2] Deta_males;                          // RS Model Parameters - Unmarried
  vector<lower=0>[2] Sigma_males;                // RS Model Dispersion Parameter
  vector[2] Ceta_males;                          // Marriage Model Parameters

//################################################################ Female Parameters
  vector[4] Beta_females;                        // RS Model Parameters - Married
  vector[2] Deta_females;                        // RS Model Parameters - Unmarried
  vector<lower=0>[2] Sigma_females;              // RS Model Dispersion Parameter
  vector[2] Ceta_females;                        // Marriage Model Parameters
}

//############################################################### Model Block
model{
//######################################################## Local Storage
  vector[N_males] EffectiveSpouseYears_males;
  matrix[N_males,MaxExposure] EffectiveSpousesByAge_males;

  vector[N_females] EffectiveSpouseYears_females;
  matrix[N_females,MaxExposure] EffectiveSpousesByAge_females;

  vector[N_males] A_males;
  vector[N_males] B_males;

  vector[N_females] A_females;
  vector[N_females] B_females;

  real Ticker;
  vector[5] ESA;

//######################################################## Male Priors
  Beta_males  ~ normal(0,5);
  Deta_males  ~ normal(0,5);
  Ceta_males  ~ normal(0,5);
  Sigma_males ~ normal(0,10);

//######################################################## Female Priors
  Beta_females  ~ normal(0,5);
  Deta_females  ~ normal(0,5);
  Ceta_females  ~ normal(0,5);
  Sigma_females ~ normal(0,10);

//################################################################################################################################
//################################################################################ Effective Spouses by Age - Males - Mate Quality
//################################################################################################################################
//  A little tricky. Men can have up to 5 wives. So we load five separate arrays.
//  If they are unmarried, effective spouse years, ESA, are not added.

 for (i in 1:N_males){
  for (a in 1:Age_males[i]){
   if(PartnersByAge1_males[i,a+11]==0){ESA[1] = 0;}
    else{ ESA[1] = 1;}

   if(PartnersByAge2_males[i,a+11]==0){ESA[2] = 0;}
    else{ ESA[2] = 1;}

   if(PartnersByAge3_males[i,a+11]==0){ESA[3] = 0;}
    else{ ESA[3] = 1;}

   if(PartnersByAge4_males[i,a+11]==0){ESA[4] = 0;}
    else{ ESA[4] = 1;}

   if(PartnersByAge5_males[i,a+11]==0){ESA[5] = 0;}
    else{ ESA[5] = 1;}

   EffectiveSpousesByAge_males[i,a] = ESA[1]+ESA[2]+ESA[3]+ESA[4]+ESA[5];
   }
   }

//################################################################################################################################
//############################################################################## Effective Spouses by Age - Females - Mate Quality
//################################################################################################################################
 for (i in 1:N_females){
  for (a in 1:Age_females[i]){
   if(PartnersByAge1_females[i,a+11]==0){EffectiveSpousesByAge_females[i,a] = 0;}
     else{EffectiveSpousesByAge_females[i,a] = 1;}
     }}

//################################################################################################################################
//################################################################################## Effective Partner Years - Males - Mate Timing
//################################################################################################################################
 for (i in 1:N_males){
   Ticker = 0;
 for (a in 1:Age_males[i]){
   Ticker = Ticker + EffectiveSpousesByAge_males[i,a];
   }
   EffectiveSpouseYears_males[i] = Ticker;
   }


//################################################################################################################################
//################################################################################ Effective Partner Years - Females - Mate Timing
//################################################################################################################################
 for (i in 1:N_females){
   Ticker = 0;
 for (a in 1:Age_females[i]){
   Ticker = Ticker + EffectiveSpousesByAge_females[i,a];
   }
   EffectiveSpouseYears_females[i] = Ticker;
   }

//################################################################################################################################
//###################################################################################################### Main Regression Functions
//################################################################################################################################
//###### Male Link Functions
 for (i in 1:N_males){
  if(Zeros_males[i]==0){
   B_males[i] = 1/Sigma_males[1];
   A_males[i] = exp(Beta_males[1] + Beta_males[2]*log(Age_males[i])  + exp(Beta_males[4])*log(EffectiveSpouseYears_males[i]))*B_males[i];
   } else{
   B_males[i] = 1/Sigma_males[2];
   A_males[i] = exp(Deta_males[1] + Deta_males[2]*log(Age_males[i]))*B_males[i];
   }
   }

//###### Female Link Functions
 for (i in 1:N_females){
 if(Zeros_females[i]==0){
   B_females[i] = 1/Sigma_females[1];
   A_females[i] = exp(Beta_females[1] + Beta_females[2]*log(Age_females[i]) + exp(Beta_females[4])*log(EffectiveSpouseYears_females[i]))*B_females[i];
    } else{
   B_females[i] = 1/Sigma_females[2];
   A_females[i] = exp(Deta_females[1] + Deta_females[2]*log(Age_females[i]))*B_females[i];
   }
   }

//###### Model Outcomes
   RS_males ~ neg_binomial(A_males,B_males);
   RS_females ~ neg_binomial(A_females,B_females);

   Zeros_males ~ bernoulli_logit(1-(Ceta_males[1] + Ceta_males[2]*log(R_Age_males)));
   Zeros_females ~ bernoulli_logit(1-(Ceta_females[1] + Ceta_females[2]*log(R_Age_females)));
}

 
