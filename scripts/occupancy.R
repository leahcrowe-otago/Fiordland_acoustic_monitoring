#### Dagg comparison

library(RPresence)
library(jagsUI)
library(R2OpenBUGS)
library(dplyr)

dat = vector("list",8)
dat[[1]]=readRDS("./data/dagg_ch.rds")
dat[[2]] = readRDS("./data/nancy_ch.rds")
dat[[3]] = readRDS("./data/charles_ch.rds")
dat[[4]] = readRDS("./data/chalky_ch.rds")
dat[[5]] = readRDS("./data/pres_ch.rds")
dat[[6]] = readRDS("./data/dusky_ch.rds")
dat[[7]] = readRDS("./data/mr1_ch.rds")
dat[[8]] = readRDS("./data/mr2_ch.rds")
names(dat) = c("dagg", "nancy", "charles","chalky","pres","dusky","mr1","mr2")

ns = length(dat)

s = rep(NA, ns)

for(j in 1:ns){
  s[j] = nrow(dat[[j]])
}

K=2 #for Dagg only, K is different gear at same site

model<-function(){

## model structure
  ## first dagg
  for (i in 1:s[1]){
    zd[i]~dbern(psi[1])
    
    logit(pd[i,1]) <- beta[1] # FPOD: not sure why I'm putting this on logit scale -- prob to be comparable to ST
    logit(pd[i,2]) <- beta[2] + beta[3] * sampd[i] + beta[4] * analysisd[i]   
    for(k in 1:K){
      yd[i,k]~dbern(pd[i,k]*zd[i]) 
    }
  }
  
  ## then nancy
  for (i in 1:s[2]){
    zn[i]~dbern(psi[2])
    
    logit(pn[i]) <- beta[2] + beta[3] * sampn[i]
    yn[i]~dbern(pn[i]*zn[i]) 
  }
  
  ## then charles
  for (i in 1:s[3]){
    zc[i]~dbern(psi[3])
    
    logit(pc[i]) <- beta[1]
    yc[i]~dbern(pc[i]*zc[i]) 
  }
  
  ## then chalky
  for (i in 1:s[4]){
    zch[i]~dbern(psi[4])
    
    logit(pch[i]) <- beta[2] + beta[3] * sampch[i] + beta[4] * analysisch[i]
    ych[i]~dbern(pch[i]*zch[i]) 
  }
  
  ## then preservation
  for (i in 1:s[5]){
    zp[i]~dbern(psi[5])
    
    logit(pp[i]) <- beta[1]
    yp[i]~dbern(pp[i]*zp[i]) 
  }
  
  ## then dusky/anchor site  
  for (i in 1:s[6]){
    za[i]~dbern(psi[6])
    
    logit(pa[i]) <- beta[2]
    ya[i]~dbern(pa[i]*za[i]) 
  }
  
  ## then dusky MR-1
  for (i in 1:s[7]){
    zm1[i]~dbern(psi[7])
    
    logit(pm1[i]) <- beta[2]
    ym1[i]~dbern(pm1[i]*zm1[i]) 
  }

  ## then dusky MR-2  

  for (i in 1:s[8]){
    zm2[i]~dbern(psi[8])
    
    logit(pm2[i]) <- beta[2]
    ym2[i]~dbern(pm2[i]*zm2[i]) 
  }
  
  #priors
  for(j in 1:8){
    psi[j] ~ dbeta(0.5,0.5)
  }
  
  for(j in 1:4){
    beta[j] ~ dt(0,1,3) #Matt's
    #beta[j] ~ dt(0,1,1) #cauchy
    #beta[j] ~ dnorm(0,1) #tau = precision = 1/sigma^2
  }
  
  #transformed parameters
  logit(pie[1])<-beta[1] # probability of FPOD
  logit(pie[2])<-beta[2] # probability of ST, no sampling
  logit(pie[3])<-beta[2] + beta[3] # probability of ST, sampling
  logit(pie[4])<-beta[2] + beta[3] + beta[4] # probability of ST, sampling, handbrowse
}


mcmc.data<-list(s=s,
                yd = dat$dagg[,2:3],
                yn = dat$nancy[,2],
                yc = dat$charles[,2],
                ych = dat$chalky[,2],
                yp = dat$pres[,2],
                ya = dat$dusky[,2],
                ym1 = dat$mr1[,2],
                ym2 = dat$mr2[,2],
                sampd = dat$dagg$samp,
                sampn = dat$nancy$samp,
                sampch = dat$chalky$samp,
                analysisd = dat$dagg$analysis,
                analysisch = dat$chalky$analysis,
                K=K) #samp=samp, define input variables for model
mcmc.params<-c("psi","beta","pie")
mcmc.inits<-function() {list(zd=rep(1,s[1]), zn = rep(1,s[2]), zc = rep(1,s[3]), zch = rep(1,s[4]), 
                             zp = rep(1,s[5]), za = rep(1,s[6]), zm1 = rep(1,s[7]), zm2 = rep(1,s[8]))} # z has to be 0 or 1

write.model(model,con="FAM_model.txt") # write JAGS model code to file
FAM_samp <- jags(data=mcmc.data, inits = mcmc.inits, parameters.to.save=mcmc.params,
                  n.iter=55000, model.file="FAM_model.txt",n.chains=3,parallel=TRUE,verbose=TRUE,n.burnin = 5000)
saveRDS(FAM_samp, file = paste0("./data/FAM_samp_dt1_50k_",Sys.Date(),".rds"))

FAM_samp$samples
FAM_samp$summary

bayesplot::mcmc_trace(FAM_samp$samples)
bayesplot::mcmc_dens(FAM_samp$samples)

#matt's way below
m1 = jags.model("FAM_model.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 50000)
out1_df = as_draws_df(out1)

mcmc_trace(out1_df)
summary(out1_df)

mean(out1_df$`beta[1]` > out1_df$`beta[2]`)

## Read results ----
read.date = "2024-07-08"
occ.results<-readRDS(paste0("./data/FAM_samp_dt3_50k_",read.date,".rds"))

summ.occ<-as.data.frame(summary(occ.results))

max(summ.occ$Rhat)
min(summ.occ$n.eff)

table1.occ<-summ.occ%>%
  mutate(Median = `50%`,
         Fiord_recorder = c("DAGG_BOTH", "NANCY_ST", "CHARLES_FPOD",
                   "CHALKY_ST", "PRESERVATION_FPOD","DUSKY_ST",
                   "MARINE-RESERVE-1_ST", "MARINE-RESERVE-2_ST",
                   "$\\beta_0$","$\\beta_1$","$\\beta_2$","$\\beta_3$",
                   "$\\pi_0$","$\\pi_1$","$\\pi_2$","$\\pi_3$","deviance"))%>%
  dplyr::select(Median,`2.5%CI` = `2.5%`,`97.5%CI` = `97.5%`, Fiord_recorder)

table1.occ$Median<-round(table1.occ$Median, 2)
table1.occ$`2.5%CI`<-round(table1.occ$`2.5%CI`, 2)
table1.occ$`97.5%CI`<-round(table1.occ$`97.5%CI`, 2)

saveRDS(table1.occ, file = paste0("./tables/table1.occ.rds"))
