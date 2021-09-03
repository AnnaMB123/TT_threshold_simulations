source("Functions_quick_version.R")
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
n_TT_data<-0 #Input either zero if want to explore probability of observed zero, or any other value you want to look at the probability of observing given true prevalence and coverage
threshold<-0.2 #Threshold you want to explore (%)

#If you want to assume fixed values for test sensitivity and specificity input here:
SeCE<-1 #Sensitivity of clinical examination
SpCE<-1 #Specificity of clinical examination

pi_range<-seq(from=0, to=0.0025, by=0.00005)
pi_length<-length(pi_range)

#Specify range of coverage values that you want to explore
coverage_range<-seq(from=0, to=1, by=0.02)
coverage_length<-length(coverage_range)

######################################################################################################################################################### 
#### GENERATE RESULTS AND PLOT: varying both pi and coverage and fixed values of sensitivity and specificity #######################################
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

