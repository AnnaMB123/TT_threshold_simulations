#### Code to run simulations to determine probability of observed TT data given assumed prevalence (pi) and coverage levels ###
#### Can either asssume fixed values for sensitivity and specificity (lines 53 to 73)
### Or can assume a distribution of sensitivity and specificity: either input shape parameters mannually (lines 30-34) or use lines 22 to 34 to generate shape parameters, then lines 79-100 to generate results and plot

source("Functions_threshold_simulations.R")
library(prevalence)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

######################################################################################################################################################### 
##########################INPUT VALUES###################################################################################################################
######################################################################################################################################################### 

#Input values for number of repetition of each parameter set you want, population size, and number of observed cases in data
no_reps<-10000 #How many times to repeat simulations
Pop<-100000 #Population size
n_TT_data<-10 #Input either zero if want to explore probability of observed zero, or any other value you want to look at the probability of observing given true prevalence and coverage
threshold<-0.2 #Threshold you want to explore (%)

#If you want to assume fixed values for test sensitivity and specificity input here:
SeCE<-1 #Sensitivity of clinical examination
SpCE<-1 #Specificity of clinical examination

#If you want to use a distribution of sensitivity and specificity, use this to generate alpha and beta parameters for beta distributions based on upper and lower bounds and "best guess"
lower_bound_sens<-0.75
upper_bound_sens<-0.999
best_estimate_sens<-0.8

lower_bound_spec<-0.99
upper_bound_spec<-0.99999
best_estimate_spec<-0.99999

SeCE_alpha<-betaExpert(best=best_estimate_sens, lower=lower_bound_sens, upper=upper_bound_sens, method='mode')$alpha
SeCE_beta<-betaExpert(best=best_estimate_sens, lower=lower_bound_sens, upper=upper_bound_sens, method='mode')$beta

SpCE_alpha<-betaExpert(best=best_estimate_spec, lower=lower_bound_spec, upper=upper_bound_spec, method='mode')$alpha
SpCE_beta<-betaExpert(best=best_estimate_spec, lower=lower_bound_spec, upper=upper_bound_spec, method='mode')$beta

#Specify range of true prevalence values that you want to explore (pi is true prevalence)
pi_range<-seq(from=0, to=0.0025, by=0.00005)
pi_length<-length(pi_range)

#Specify range of coverage values that you want to explore
coverage_range<-seq(from=0, to=1, by=0.02)
coverage_length<-length(coverage_range)

######################################################################################################################################################### 
#### GENERATE RESULTS AND PLOT: varying both pi and coverage assuming fixed values of sensitivity and specificity #######################################
######################################################################################################################################################### 

#Make a matrix to store results
name_matrix<-list(coverage_range,pi_range)
output_prob_n_or_below<-matrix(,nrow=length(coverage_range),ncol=length(pi_range),dimnames = name_matrix)

#Now run multiple simulations for range of coverage values and pi values and store in matrix, fixed values sens and spec
for (i in 1:length(coverage_range))
{
  output_prob_n_or_below[i,]<-sapply_simulate_pi_range_n_or_below(pi_range, coverage=coverage_range[i])
}

#Make into long format for tile plot and bin into discrete values- can edit bins as required
long_n_or_below<-melt(output_prob_n_or_below) #make into long format
names(long_n_or_below)<-c("Coverage","True_prevalence",'Val')
output_prob_n_or_below.df<-as.data.frame(long_n_or_below)
output_prob_n_or_below.df$discrete<-cut(output_prob_n_or_below.df$Val,c(0,0.0001,0.01,0.05,0.2,0.5,0.75,1), include.lowest=TRUE)

prob_n_or_below_plot<-ggplot(data=output_prob_n_or_below.df, aes(x=True_prevalence*100, y=Coverage*100, fill=discrete))+
  geom_tile()+
  scale_fill_manual(values=(brewer.pal(7,"YlGnBu")))+
  ggtitle(paste("Probability of observing n or below cases (n=",n_TT_data,")"))+
  geom_vline(xintercept=threshold,colour="red",linetype="dashed")
prob_n_or_below_plot

######################################################################################################################################################### 
#### GENERATE RESULTS AND PLOT: varying both pi and coverage and assuming distributions of sensitivity and specificity #######################################
######################################################################################################################################################### 

#Make a matrix to store results
name_matrix<-list(coverage_range,pi_range)
output_prob_n_or_below<-matrix(,nrow=length(coverage_range),ncol=length(pi_range),dimnames = name_matrix)

#Now run multiple simulations for range of coverage values and pi values and store in matrix, distributions of sensitivity and specificity
for (i in 1:length(coverage_range))
{
  output_prob_n_or_below[i,]<-sapply_simulate_pi_range_n_or_below_diag(pi_range, coverage=coverage_range[i])
}

#Make into long format for tile plot and bin into discrete values- can edit bins as required
long_n_or_below<-melt(output_prob_n_or_below) #make into long format
names(long_n_or_below)<-c("Coverage","True_prevalence",'Val')
output_prob_n_or_below.df<-as.data.frame(long_n_or_below)
output_prob_n_or_below.df$discrete<-cut(output_prob_n_or_below.df$Val,c(0,0.0001,0.01,0.05,0.2,0.5,0.75,1), include.lowest=TRUE)

prob_n_or_below_plot<-ggplot(data=output_prob_n_or_below.df, aes(x=True_prevalence*100, y=Coverage*100, fill=discrete))+
  geom_tile()+
  scale_fill_manual(values=(brewer.pal(7,"YlGnBu")))+
  ggtitle(paste("Probability of observing n or below cases (n=",n_TT_data,")"))+
  geom_vline(xintercept=threshold,colour="red",linetype="dashed")
prob_n_or_below_plot

######################################################################################################################################################### 
########################### Varying single parameters #####################################################################################################
#########################################################################################################################################################

#Use this if want to just vary pi (true prevalence) between 0 and x%
#Will need to add a fixed coverage value to globals
coverage<-0.5
pi_range<-seq(from=0, to=0.0025, by=0.00005)
pi_length<-length(pi_range)
results_pi_range<-lapply(pi_range,simulate_vary_pi)
df_pi_range<-data.frame(matrix(unlist(results_pi_range),nrow = pi_length, byrow=T))

#Plot probability of observing n positives against varying pi#
vary_pi_prob_below_n<-ggplot(data=df_pi_range, aes(x=X1*100))+
  geom_line(aes(y=X2))+
  xlab("True Prevalence TT (%)")+
  ylab(paste("Probability of observing",n_TT_data,"or below cases"))+
  ggtitle(paste("Coverage=",coverage))
vary_pi_prob_below_n

#Use this if want to vary coverage between 0 and ?%
#Will need to add a fixed pi value to globals
pi<-0.002
coverage_range<-seq(from=0, to=1, by=0.01)
coverage_length<-length(coverage_range)
results_coverage_range<-lapply(coverage_range,simulate_vary_coverage)
df_cov_range<-data.frame(matrix(unlist(results_coverage_range),nrow = coverage_length, byrow=T))

#Plot probability of observing below given value of TT in data against varying coverage levels#
vary_cov_prob_below_n<-ggplot(data=df_cov_range, aes(x=X1))+
  geom_line(aes(y=X2))+
  xlab("Coverage")+
  ylab(paste("Probability of observing",n_TT_data,"or below cases"))+
  ggtitle(paste("True Prevalence=",pi))
vary_cov_prob_below_n


