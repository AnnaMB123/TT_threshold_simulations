#Functions for running simulations for observed TT data

# Function to simulate observed data if want to use a distribution for diagnostic sensitivity and specificity
# Input population size, coverage, alpha and beta parameters to desribe distributions of sensitivity and specificity of diagnostic
#output binary 1=n or below cases observed, 0=more than n cases observed

simulate_observed_data_diagnostic_dist<- function(Pop,coverage,pi,n_TT_data,SeCE_alpha,SeCE_beta,SpCE_alpha,Sp_CE_beta)
{
  
  #Draw from beta distributions for sens and spec
  
  SeCE<-rbeta(1,shape1=SeCE_alpha,shape2=SeCE_beta)
  
  SpCE<-rbeta(1,shape1=SpCE_alpha,shape2=SpCE_beta)
  
  #simulate a number of people with clincally observable TT based on underlying prevalence, sensitivity and specifity of the clinical presentation
  
  prob_clinical_TT<-pi*SeCE + (1-pi)*(1-SpCE) ###prob_clinical_TT=prob of being clinically positive, pi is true prevalence 
  
  n_clinical_TT<-rbinom(n=1,size=Pop,prob=prob_clinical_TT) 
  
  #simulate how many would be observed and recorded at health facility as a function of coverage
  
  n_observed_TT<-rbinom(n=1,size=n_clinical_TT,prob=coverage)
  
  #record if simulated data returns equal to or less than no. of TT in the data
  
  observed_below_n<-ifelse(n_observed_TT<=n_TT_data,1,0)
  
  return("observed_below_n"=observed_below_n)
  
  
}

#Function to run multiple times for pi values and a given coverage value: distribution of sp and sens
simulate_multi_pi_coverage_diag_range<-function(pi,coverage) #other values should be drawn fron whatever fixed in globals
{simulate_multi<-replicate(n=no_reps,simulate_observed_data_diagnostic_dist(Pop,coverage,pi,n_TT_data,SeCE_alpha,SeCE_beta,SpCE_alpha,Sp_CE_beta))
proportion_below_n<-sum(simulate_multi)/no_reps
return(c("pi"=pi,"cov"=coverage,"proportion_below_n"=proportion_below_n))
}

#Function to run for range of pi and given coverage value, return prob of n or below fixed value sens and spec
sapply_simulate_pi_range_n_or_below_diag<-function(pi_range,coverage){
  results_pi_range<-sapply(X=pi_range,FUN=simulate_multi_pi_coverage_diag_range,coverage)
  results<-results_pi_range[3,]#proportion n or below, n in globals
  return(results)
}


###### Fixed values of sens and spec #####

#Function to simulate obserced data using fixed values for sensitivity and specificity
simulate_observed_data<- function(Pop,coverage,SeCE,SpCE,pi,n_TT_data)
{
  #simulate a number of people with clincal TT based on underlying prevalence, sensitivity and specifity of a clinical examination
  
  prob_clinical_TT<-pi*SeCE + (1-pi)*(1-SpCE) ###prob_clinical_TT=prob of being clinically positive
  
  ###pi= true underlying prevalence 
  
  n_clinical_TT<-rbinom(n=1,size=Pop,prob=prob_clinical_TT) 
  
  #simulate how many would be observed and recorded at health facility as a function of coverage
  
  n_observed_TT<-rbinom(n=1,size=n_clinical_TT,prob=coverage)
  
  #record if simulated data returns less than no. of TT in the data
  
  observed_below_n<-ifelse(n_observed_TT<=n_TT_data,1,0)
  
  return("observed_below_n"=observed_below_n)
  
  
}

#Function to run multiple times for pi values and a given coverage value
simulate_multi_pi_coverage<-function(pi,coverage) #other values should be drawn fron whatever fixed in globals
{simulate_multi<-replicate(n=no_reps,simulate_observed_data(Pop,coverage,SeCE,SpCE,pi,n_TT_data))
proportion_below_n<-sum(simulate_multi)/no_reps
return(c("pi"=pi,"cov"=coverage,"proportion_below_n"=proportion_below_n))
}

#Function to run for range of pi and given coverage value, return prob of n or below fixed value sens and spec
sapply_simulate_pi_range_n_or_below<-function(pi_range,coverage){
  results_pi_range<-sapply(X=pi_range,FUN=simulate_multi_pi_coverage,coverage)
  results<-results_pi_range[3,]#proportion n or below, n in globals
  return(results)
}


#Functions to run multiple simulations varying a single variable
#can output proportion which are below a given value (n_TT_data) against whatever other variable you want to change e.g. pi

simulate_vary_pi<-function(pi) #other values should be drawn fron whatever fixed in globals
{simulate_multi<-replicate(n=no_reps,simulate_observed_data(Pop,coverage,SeCE,SpCE,pi,n_TT_data))
proportion_below_n<-sum(simulate_multi)/no_reps
return(c("pi"=pi,"proportion_below_n"=proportion_below_n))
}

simulate_vary_coverage<-function(coverage) #other values should be drawn fron whatever fixed in globals
{simulate_multi<-replicate(n=no_reps,simulate_observed_data(Pop,coverage,SeCE,SpCE,pi,n_TT_data))
proportion_below_n<-sum(simulate_multi)/no_reps
return(c("coverage"=coverage,"proportion_below_n"=proportion_below_n))
}
