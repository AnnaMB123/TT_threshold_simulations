#Making fast version using prob rather than sims

###### Fixed values of sens and spec #####

#Function to simulate obserced data using fixed values for sensitivity and specificity
simulate_observed_data<- function(Pop,coverage,SeCE,SpCE,pi,n_TT_data)
{
  #simulate a number of people with clincal TT based on underlying prevalence, sensitivity and specifity of a clinical examination
  
  prob_clinical_TT<-pi*SeCE + (1-pi)*(1-SpCE) ###prob_clinical_TT=prob of being clinically positive
  
  prob_n_or_below_observed<-pbinom(n_TT_data,size=Pop,prob=prob_clinical_TT*coverage, lower.tail = TRUE) 
  
  return("prob_n_or_below_observed"=prob_n_or_below_observed)
  
}

#Function to run for range of pi and given coverage value, return prob of n or below fixed value sens and spec
sapply_simulate_pi_range_n_or_below<-function(pi_range,coverage){
  results_pi_range<-sapply(pi=pi_range,FUN=simulate_observed_data,Pop,coverage,SeCE,SpCE,n_TT_data)
  return(results_pi_range)
}


