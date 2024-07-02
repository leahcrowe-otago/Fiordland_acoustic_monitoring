#### Dagg comparison

ch<-readRDS("./data/ch.rds")

library(RPresence)
library(jagsUI)
library(R2OpenBUGS)
library(dplyr)

s<-nrow(ch)
samp<-ch$samp ## 1 = ST corvariate, sampled 15 out of 30 min, 0 = continuous
fpod<-ch$FPOD
ST<-ch$ST

h<-as.matrix(ch%>%dplyr::select(-date,-samp))
K = 2
h[1,2]
model<-function(){
  
  ## model structure
  for (i in 1:s){
    #logit(psi[i])<-a1+a2*samp[i]
    z[i]~dbern(psi)
    mu[i] <- p*z[i]
    for(j in 1:K){
      #logit(p[i,j])<-b1+b2*fpod[i]+b3*ST[i]
      #p.temp[i,j]<-p[i,j]*z[i]
      h[i,j]~dbern(mu[i])
      
      }
  }
  
  #priors
  p~dunif(0,1)
  psi~dunif(0,1)
  
  #a1~dnorm(0,1)
  #a2~dnorm(0,1)
  
  #b1~dnorm(0,1)
  #b2~dnorm(0,1)
  #b3~dnorm(0,1)
  
  #logit(psi1[1])<-a1
  #logit(psi1[2])<-a1+a2
  
  #logit(p1[1])<-b1
  #logit(p1[2])<-b1+b2
  #logit(p1[3])<-b1+b3
}

write.model(model,con="dagg_model.txt") # write JAGS model code to file
mcmc.data<-list(s=s,K=K,h=h)# samp=samp,fpod=fpod,ST=ST) # define input variables for model
mcmc.params<-c("psi","p")
#mcmc.params<-c("psi1","p1","a1","a2","b1","b2","b3") # define which node values to store
mcmc.inits<-function() {list(z=rep(0.5,s))}

Sys.time()
dagg_samp <- jags(data=mcmc.data, inits = mcmc.inits, parameters.to.save=mcmc.params,
              n.iter=11000, model.file="dagg_model.txt",n.chains=3,parallel=TRUE,verbose=TRUE,n.burnin = 1000)
Sys.time()

bayesplot::mcmc_trace(dagg_samp$samples)
bayesplot::mcmc_dens(dagg_samp$samples)
dagg_samp
