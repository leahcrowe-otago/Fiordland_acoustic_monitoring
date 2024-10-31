library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# data ----
dat = vector("list",8)
dat[[1]] = readRDS("./data/dagg_ch.rds")
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
    
    logit(pd[i,1]) <- beta[1] # FPOD
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
    beta[j] ~ dt(0,1,3) 
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

## run model ----
#norm for appendix
#R2OpenBUGS::write.model(model,con="FAM_model_norm.txt") # write JAGS model code to file
#m1 = rjags::jags.model("FAM_model_norm.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
#student-t for main
R2OpenBUGS::write.model(model,con="FAM_model_dt.txt") # write JAGS model code to file
m1 = rjags::jags.model("FAM_model_dt.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
##

out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 50000)
out1_df = posterior::as_draws_df(out1)

#saveRDS(out1_df, file = paste0("./data/FAM_samp_n_50k_",Sys.Date(),".rds"))
saveRDS(out1_df, file = paste0("./data/FAM_samp_dt3_50k_",Sys.Date(),".rds"))

bayesplot::mcmc_trace(out1_df)
summary(out1_df)

mean(out1_df$`beta[1]` > out1_df$`beta[2]`)

## Read results ----
read.date = "2024-10-29"

occ.results<-readRDS(paste0("./data/FAM_samp_dt3_50k_",read.date,".rds"))

summ.occ<-as.data.frame(summary(occ.results))

max(summ.occ$rhat)
min(summ.occ$ess_bulk)

table1.occ<-summ.occ%>%
  mutate(Median = median,
         Fiord_recorder = c(
           "$\\beta_1$","$\\beta_2$","$\\beta_3$","$\\beta_4$",
           "$\\pi_1$","$\\pi_2$","$\\pi_3$","$\\pi_4$",
           "DAGG_BOTH", "NANCY_ST", "CHARLES_F-POD",
              "CHALKY_ST", "PRESERVATION_F-POD","DUSKY_ST",
              "MARINE-RESERVE-1_ST", "MARINE-RESERVE-2_ST"
                   ))%>%
  dplyr::select(Median,`2.5%CI` = q5,`97.5%CI` = q95, Fiord_recorder)

table1.occ$Median<-round(table1.occ$Median, 3)
table1.occ$`2.5%CI`<-round(table1.occ$`2.5%CI`, 3)
table1.occ$`97.5%CI`<-round(table1.occ$`97.5%CI`, 3)

saveRDS(table1.occ, file = paste0("./tables/table1.occ.rds"))

## beta priors ----
#normal beta prior pi rank is the same
occ.results_norm<-readRDS(paste0("./data/FAM_samp_n_50k_",read.date,".rds"))
summary(occ.results_norm)
summ.occ_norm<-as.data.frame(summary(occ.results_norm))
supp.occ_norm<-summ.occ_norm%>%
  mutate(Median = median,
         Fiord_recorder = c("$\\beta_1$","$\\beta_2$","$\\beta_3$","$\\beta_4$",
                            "$\\pi_1$","$\\pi_2$","$\\pi_3$","$\\pi_4$",
                            "$\\psi_{DAGG}$", "$\\psi_{NANCY}$", "$\\psi_{CHARLES}$",
                            "$\\psi_{CHALKY}$", "$\\psi_{PRESERVATION}$","$\\psi_{DUSKY}$",
                            "$\\psi_{MARINE-RESERVE-1}$", "$\\psi_{MARINE-RESERVE-2}$"
                            ))%>%
  dplyr::select(Median,`2.5%CI` = q5,`97.5%CI` = q95, Fiord_recorder, Rhat = rhat, ESS = ess_bulk)

supp.occ_norm$Median<-round(supp.occ_norm$Median, 2)
supp.occ_norm$`2.5%CI`<-round(supp.occ_norm$`2.5%CI`, 2)
supp.occ_norm$`97.5%CI`<-round(supp.occ_norm$`97.5%CI`, 2)

saveRDS(supp.occ_norm, file = paste0("./tables/supp.occ_norm.rds"))

## dagg stats ----

dagg<-dat[[1]]
dagg_samp<-dagg

dagg_samp<-dagg%>%
  filter(analysis == 1)

nrow(dagg_samp)

dagg_samp%>%
  filter((`DAGG_F-POD` == 1 & DAGG_ST == 1) | (`DAGG_F-POD` == 0 & DAGG_ST == 0))%>%
  distinct(date)

dagg_samp%>%
  filter(!((`DAGG_F-POD` == 1 & DAGG_ST == 1) | (`DAGG_F-POD` == 0 & DAGG_ST == 0)))%>%
  arrange(`DAGG_F-POD`)

### between the two marine reserve sites
mr1<-dat[[7]]
mr2<-dat[[8]]

mr<-mr1%>%
  dplyr::rename('ST_MR1' = 'ST', 'ST_samp' = 'samp')%>%
  left_join(mr2, by = 'date')

nrow(mr)

mr%>%
  filter(ST_MR1 != ST)

mr%>%
  filter(ST == 1 & ST_MR1 == 1)

mr%>%
  filter(ST == 0 & ST_MR1 == 1)

mr%>%
  filter(ST == 1 & ST_MR1 == 0)

## FPOD detections on performance dates

dagg<-dat[[1]]

head(dagg)

dagg%>%
  filter(date %in% c('2022-04-21', '2022-04-26', '2022-05-10', '2022-06-11', '2022-07-30', '2022-08-10', '2022-11-29','2023-01-04', '2023-05-13', '2023-05-23'))

fpod_perf<-all_FPOD_Dol%>%
  filter(Fiord == "DAGG")%>%
  filter(Date %in% c('2022-04-21', '2022-04-26', '2022-05-10', '2022-06-11', '2022-07-30', '2022-08-10', '2022-11-29','2023-01-04', '2023-05-13', '2023-05-23'))
  
#write.csv(fpod_perf,"./fpod_performance_dates.csv", row.names = F, na = "")

# N ----

nrow(dat$dagg)
nrow(dat$nancy)
nrow(dat$charles)
nrow(dat$chalky)
nrow(dat$pres)
nrow(dat$dusky)
nrow(dat$mr1)
nrow(dat$mr2)

nrow(dat$dagg)+
nrow(dat$nancy)+
nrow(dat$charles)+
nrow(dat$chalky)+
nrow(dat$pres)+
nrow(dat$dusky)+
nrow(dat$mr1)+
nrow(dat$mr2)

head(dat$dagg)
head(dat$nancy)
head(dat$charles)
head(dat$chalky)
head(dat$pres)
head(dat$dusky)
head(dat$mr1)
head(dat$mr2)

