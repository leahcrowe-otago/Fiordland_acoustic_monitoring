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

model<-function(){
  
  ## model structure
  for (ii in 1:s){
    logit(psi[ii])<-a1+a2*samp[ii]
    z[ii]~dbern(psi[ii])
    for(jj in 1:K){
      logit(p[ii,jj])<-b1+b2*fpod[ii]+b3*ST[ii]
      p.temp[ii,jj]<-p[ii,jj]*z[ii]
      h[ii,jj]~dbern(p.temp[ii,jj])
      
      }
  }
  
  a1~dnorm(0,1)
  a2~dnorm(0,1)
  
  b1~dnorm(0,1)
  b2~dnorm(0,1)
  b3~dnorm(0,1)
  
  logit(psi1[1])<-a1
  logit(psi1[2])<-a1+a2
  
  logit(p1[1])<-b1
  logit(p1[2])<-b1+b2
  logit(p1[3])<-b1+b3
}

write.model(model,con="dagg_model.txt") # write JAGS model code to file
mcmc.data<-list(s=s,K=K, samp=samp,fpod=fpod,ST=ST) # define input variables for model
mcmc.params<-c("psi1","p1","a1","a2","b1","b2","b3") # define which node values to store
mcmc.inits<-function() {list(z=rep(1,s))}

Sys.time()
dagg_samp <- jags(data=mcmc.data, inits=mcmc.inits, parameters.to.save=mcmc.params,
              n.iter=11000, model.file="dagg_model.txt",n.chains=3,parallel=TRUE,verbose=TRUE,n.burnin = 1000)
Sys.time()

bayesplot::mcmc_trace(dagg_samp$samples)
bayesplot::mcmc_dens(dagg_samp$samples)
dagg_samp
