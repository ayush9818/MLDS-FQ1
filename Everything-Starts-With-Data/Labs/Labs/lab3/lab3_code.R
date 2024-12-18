set.seed(400) # Seed RNG 
n = 8000
x1 = runif(n,-0.5,0.5)
x2 = runif(n,-0.5,0.5)
z = x1^2+x2^2
Pi = 4*sum(z<=0.25)/n 
Pi

library(LearnBayes)
puffin[1:5,]

#Frequentist view
pfit = glm(Nest~Grass+Soil+Angle+Distance,poisson,data=puffin) 
summary(pfit)

#Bayesian method
library(MCMCpack)
#Assumes normal prior
Bpfit = MCMCpoisson(Nest~Grass+Soil+Angle+Distance,data=puffin,burnin=1000,mcmc=25000,thin=25)
summary(Bpfit)

