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
  vector<lower=0>[2] Sigma_males;                // RS Model Dispersion Parameter
  
  real<lower=1, upper=5> ImpNS[5];

//################################################################ Female Parameters
  vector[4] Beta_females;                        // RS Model Parameters - Married
  vector<lower=0>[2] Sigma_females;              // RS Model Dispersion Parameter
}

//############################################################### Model Block
model{
//######################################################## Local Storage
  vector[N_males] A_males;
  vector[N_males] B_males;

  vector[N_females] A_females;
  vector[N_females] B_females;

//######################################################## Male Priors
  Beta_males  ~ normal(0,5);
  Sigma_males ~ normal(0,10);
  
   

//######################################################## Female Priors
  Beta_females  ~ normal(0,5);
  Sigma_females ~ normal(0,10);

//################################################################################################################################
//###################################################################################################### Main Regression Functions
//################################################################################################################################
//###### Male Link Functions
 for (i in 1:N_males){
   B_males[i] = 1/Sigma_males[1];
if(Zeros_males[i] != 1)
   A_males[i] = exp(Beta_males[1] + Beta_males[2]*log(Age_males[i])  + Beta_males[3]*log(NumberSpouses_males[i]) )*B_males[i];
   }
   
A_males[15] = exp(Beta_males[1] + Beta_males[2]*log(Age_males[15])  + Beta_males[3]*log(ImpNS[1]) )*B_males[15];
A_males[78] = exp(Beta_males[1] + Beta_males[2]*log(Age_males[78])  + Beta_males[3]*log(ImpNS[2]) )*B_males[78];
A_males[111] = exp(Beta_males[1] + Beta_males[2]*log(Age_males[111])  + Beta_males[3]*log(ImpNS[3]) )*B_males[111];
A_males[134] = exp(Beta_males[1] + Beta_males[2]*log(Age_males[134])  + Beta_males[3]*log(ImpNS[4]) )*B_males[134];


//###### Female Link Functions
 for (i in 1:N_females){
   B_females[i] = 1/Sigma_females[1];
if(Zeros_females[i] != 1)
   A_females[i] = exp(Beta_females[1] + Beta_females[2]*log(Age_females[i]) + Beta_females[3]*log(NumberSpouses_females[i]) )*B_females[i];
   }

 A_females[94] = exp(Beta_females[1] + Beta_females[2]*log(Age_females[94]) + Beta_females[3]*log(ImpNS[5]) )*B_females[94];
  

//###### Model Outcomes
   RS_males ~ neg_binomial(A_males,B_males);
   RS_females ~ neg_binomial(A_females,B_females);
}

 