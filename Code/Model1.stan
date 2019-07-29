//##################################################################### Data Block
data {
//######################################################################## Indexes
  int N_males;                                    // Indexes
  int N_females;                                  //

  int RS_males[N_males];                          // Time invariant male data
  int Age_males[N_males];                         //
  int NumberSpouses_males[N_males];               //
  int Zeros_males[N_males];                       //
  vector[N_males] R_Age_males;                    //

  int RS_females[N_females];                      // Time invariant female data
  int Age_females[N_females];                     //
  int NumberSpouses_females[N_females];           //
  int Zeros_females[N_females];                   //
  vector[N_females] R_Age_females;                //
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
  vector[N_males] A_males;
  vector[N_males] B_males;

  vector[N_females] A_females;
  vector[N_females] B_females;

//######################################################## Male Priors
  Deta_males  ~ normal(0,5);
  Beta_males  ~ normal(0,5);
  Ceta_males  ~ normal(0,5);
  Sigma_males ~ normal(0,10);

//######################################################## Female Priors
  Deta_females  ~ normal(0,5);
  Beta_females  ~ normal(0,5);
  Ceta_females  ~ normal(0,5);
  Sigma_females ~ normal(0,10);

//################################################################################################################################
//###################################################################################################### Main Regression Functions
//################################################################################################################################
//###### Male Link Functions
 for (i in 1:N_males){
  if(Zeros_males[i]==0){
   B_males[i] = 1/Sigma_males[1];
   A_males[i] = exp(Beta_males[1] + Beta_males[2]*log(Age_males[i])  + Beta_males[3]*log(NumberSpouses_males[i]) )*B_males[i];
   } else{
   B_males[i] = 1/Sigma_males[2];
   A_males[i] = exp(Deta_males[1] + Deta_males[2]*log(Age_males[i]))*B_males[i];
   }
   }

//###### Female Link Functions
 for (i in 1:N_females){
 if(Zeros_females[i]==0){
   B_females[i] = 1/Sigma_females[1];
   A_females[i] = exp(Beta_females[1] + Beta_females[2]*log(Age_females[i]) + Beta_females[3]*log(NumberSpouses_females[i]) )*B_females[i];
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

 