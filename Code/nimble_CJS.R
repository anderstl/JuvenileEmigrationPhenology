#2019 - 30 May 2019. Recaptures began opportunistically 29 April 2019 and animals 
##          were actively removed starting on 10 May 2019. 
## 
##    Analysis summary: We assessed surivival using CJS Modles as outlined in Bayesian Population
##          Analysis using WinBUGS by Kery & Schaub. Growth was assessed using _____.
##
## Edited by: Jake Burkhart, Tom Anderson, and Arianne Messerman
## ----------------------------------------------------------------------------------------------

## Load Packages:
## --------------
require(tidyr)
require(readxl)
require(ggplot2)
require(dplyr)

library(lattice)
library(coda)
library(jagsUI)
library(mcmcplots)
## --------------


## Set Directories and Import Data:
## --------------------------------

# aa_recaps <- read.csv("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/AMAN_phenology_recap_data_master.csv", header=T)           ## recapture data
# #aa_weather <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/AaCOE_CapenPark_Daily-Weather_20180601-20190531.xlsx")  ## weather data
# 
# aa_assign <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Pen_Assignments.xlsx")             ## initial pen assignments
# aa_treats <- read.csv("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Treatments.csv", header=T)           ## treatment data
# aa_tagged <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Tagged_Animals.xlsx")              ## all tagged animals and metamorphosis sizes


aa_recaps <- read.csv("~/GitRepos/JuvenileEmigrationPhenology/Data/AMAN/AMAN_phenology_recap_data_master.csv", header=T)           ## recapture data
#aa_weather <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/AaCOE_CapenPark_Daily-Weather_20180601-20190531.xlsx")  ## weather data

aa_assign <- read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Pen_Assignments.xlsx")             ## initial pen assignments
aa_treats <- read.csv("~/GitRepos/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Treatments.csv", header=T)           ## treatment data
aa_tagged <- read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Tagged_Animals.xlsx")              ## all tagged animals and metamorphosis sizes
## --------------------------------

## Format/Merge Data:
## ------------------
doy.adj <- 172                           ## DOY value to standardize values to June 21, 2018

aa_recaps <- subset(aa_recaps, aa_recaps$Notes != "Arianne Salamander")
aa_recaps$DOY <- as.numeric(format(as.Date(as.character(aa_recaps$Recap_Date),"%m/%d/%Y"),"%j"))
aa_recaps$DOY_adj <- ifelse(aa_recaps$DOY >= doy.adj, yes=aa_recaps$DOY-(doy.adj-1), no=aa_recaps$DOY+(365-doy.adj+1))
aa_recaps$DOY_adj[aa_recaps$Recap_Date=="7/6/2019"]<-365+aa_recaps$DOY_adj[aa_recaps$Recap_Date=="7/6/2019"] #adjust one value that spanned years
aa_recaps$Rel.DOY <- as.numeric(format(as.Date(as.character(aa_recaps$Release),"%m/%d/%Y"),"%j"))
aa_recaps$Rel.DOY_adj <- ifelse(aa_recaps$Rel.DOY >= doy.adj, yes=aa_recaps$Rel.DOY-(doy.adj-1), no=recaps$Rel.DOY+(365-doy.adj+1))
aa_recaps$Recap_Loc <- as.character(aa_recaps$Recap_Loc)

aa_recaps$Re.Block <- NULL
aa_recaps$Re.Pen <- NULL
aa_recaps$Re.Micro <- NULL

for(i in 1:dim(aa_recaps)[1]) {
  aa_recaps$Re.Block[i] <- unlist(strsplit(aa_recaps$Recap_Loc[i], ","))[1]
  aa_recaps$Re.Pen[i] <- unlist(strsplit(aa_recaps$Recap_Loc[i], ","))[2]
  aa_recaps$Re.Micro[i] <- unlist(strsplit(aa_recaps$Recap_Loc[i], ","))[3]
}

#aa_weather$DOY <- as.numeric(format(as.Date(as.character(aa_weather$DATE), "%m/%d/%Y"), "%j"))
#aa_weather$DOY_adj <- ifelse(aa_weather$DOY >= doy.adj, yes=aa_weather$DOY-(doy.adj-1), no=ifelse(aa_weather$DOY < 152, yes=aa_weather$DOY+(365-doy.adj+1), no=aa_weather$DOY-(doy.adj-1)))

aa_assign$Release_DOY <- as.numeric(format(as.Date(as.character(aa_assign$Release_Date), "%Y-%m-%d"),"%j"))
aa_assign$DOY_adj <- ifelse(aa_assign$Release_DOY >= doy.adj, yes=aa_assign$Release_DOY-(doy.adj-1), no=aa_assign$Release_DOY+(365-doy.adj+1))

aa_tagged$Meta.DOY <- as.numeric(format(as.Date(as.character(aa_tagged$Meta.Date), "%Y-%m-%d"),"%j"))
aa_tagged$Meta.DOY_adj <- ifelse(aa_tagged$Meta.DOY >= doy.adj, yes=aa_tagged$Meta.DOY-(doy.adj-1), no=aa_tagged$Meta.DOY+(365-doy.adj+1))
aa_tagged$Tag.DOY <- as.numeric(format(as.Date(as.character(aa_tagged$Tag.Date), "%Y-%m-%d"),"%j"))
aa_tagged$Tag.DOY_adj <- ifelse(aa_tagged$Tag.DOY >= doy.adj, yes=aa_tagged$Tag.DOY-(doy.adj-1), no=aa_tagged$Tag.DOY+(365-doy.adj+1))

df.a <- merge(aa_assign, aa_tagged, by="PIT_Tag", type="left")
aa_df <- merge(aa_recaps, df.a, by="PIT_Tag", type="left")

aa_df$Moved <- ifelse(is.na(aa_df$Moved)==T, yes=0, no=aa_df$Moved)
aa_df$Pre.Alive <- aa_df$Moved + aa_df$Pre_Alive
aa_df$Pre.Alive <- ifelse(aa_df$Pre.Alive >= 1, yes=1, no=0)

## create a matrix of zeros with # inds X # periods dimensions
## loop over individuals, populate the cells with data
## concatenate data on the back end to create the capture history cells

## Create Capture History Matrix (messy... needs cleaned later)
aa_ch.pa <- matrix(0, nrow=dim(aa_assign)[1], ncol=max(as.numeric(aa_recaps$Period), na.rm=T)+6)
aa_ch.pa <- as.data.frame(aa_ch.pa)
colnames(aa_ch.pa)[1:5] <- c("PIT_Tag", "Treatment", "Block", "Pen", "Release")
aa_ch.pa$PIT_Tag <- aa_assign$PIT_Tag
aa_ch.pa$Treatment <- aa_assign$Treatment
aa_ch.pa$Block <- aa_assign$Block
aa_ch.pa$Pen <- aa_assign$Pen
aa_ch.pa$Release <- aa_assign$Release_DOY
aa_ch.pa[,6] <- 1

aa_recaps$Moved <- ifelse(is.na(aa_recaps$Moved)==T, yes=0, no=aa_recaps$Moved)

# recaps$Visual <- ifelse(recaps$Visual == 2, yes=0, no=recaps$Visual)          
aa_recaps$Pre.Alive <- aa_recaps$Moved + aa_recaps$Pre_Alive
aa_recaps$Pre.Alive <- ifelse(aa_recaps$Pre.Alive >= 1, yes=1, no=0)

ag.recaps <- aggregate(aa_recaps$Pre.Alive, by=list(aa_recaps$PIT_Tag, aa_recaps$Period), FUN='sum')
colnames(ag.recaps) <- c("PIT_Tag", "Period", "Nobs")
ag.recaps$Pre.Alive <- ag.recaps$Nobs
ag.recaps$Pre.Alive <- ifelse(ag.recaps$Pre.Alive >= 1, yes=1, no=ag.recaps$Pre.Alive)

for(i in 1:dim(aa_ch.pa)[1]){
  ind <- aa_ch.pa$PIT_Tag[i]
  sdf <- subset(ag.recaps, ag.recaps$PIT_Tag == ind)
  
  if(dim(sdf)[1]>0){
    for(c in 7:dim(aa_ch.pa)[2]){
      for(s in 1:dim(sdf)[1]){
        p <- sdf$Period[s]
        if((c-5) == p){
          aa_ch.pa[i,c] <- sdf$Pre.Alive[s]
        }
      }
    }
  }
}


#sort by treatment
#aa_ch.pa<-arrange(aa_ch.pa,c(Treatment))

## isolate capture history matrix data only.
aa_CH <- as.matrix(aa_ch.pa[,6:dim(aa_ch.pa)[2]])   

#add mass to data frame
aa_ch.pa<-merge(aa_ch.pa,aa_tagged[,c("PIT_Tag","Meta.Mass","Meta.Date")],by=c("PIT_Tag"))
aa_ch.pa<-aa_ch.pa[-(aa_ch.pa$PIT_Tag=="x0966" & aa_ch.pa$Meta.Mass==2.21),]#drop duplicate mass?

#sort by treatment again
#aa_ch.pa<-arrange(aa_ch.pa,c(Treatment))

## ------------------
## Analyze Survival:
## -----------------
# Initial values
# In JAGS we have to give good initial values for the latent state z. At all occasions when an individual was observed, its state
# is z = 1 for sure. In addition, if an individual was not observed at an occasion, but was alive for sure, because it was 
# observed before and thereafter (i.e. has a capture history of e.g. {101} or {10001}), then we know that the individual was alive 
# at all of these occasions, and thus z = 1. Therefore, we should provide initial values of z = 1 at these positions as well. The 
# following function provides such initial values from the observed capture histories:
known.state.cjs <- function(ch){
  state <- ch
  for (i in 1:dim(ch)[1]){
    n1 <- min(which(ch[i,]==1))
    n2 <- max(which(ch[i,]==1))
    state[i,n1:n2] <- 1
    state[i,n1] <- NA
  }
  state[state==0] <- NA
  return(state)
}

# Function to create a matrix of initial values for latent state z
cjs.init.z <- function(ch,f){
  for (i in 1:dim(ch)[1]){
    if (sum(ch[i,])==1) next
    n2 <- max(which(ch[i,]==1))
    ch[i,f[i]:n2] <- NA
  }
  for (i in 1:dim(ch)[1]){
    ch[i,1:f[i]] <- NA
  }
  return(ch)
}

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f_aa <- apply(aa_CH, 1, get.first)

# Create matrix m to indicate when an individual was captured, to address test 2.ct trap effect
m_aa<-aa_CH[,1:(dim(aa_CH)[2]-1)]
u_aa<-which (m_aa==0)
m_aa[u_aa]<-2

#calculate unequal interval lengths 
#interval next to event 1 was calculated from the time of release (event 0). 
#The last interval is then between the last and second to last events)
interval_aa<-aa_recaps%>%
  group_by(as.factor(Period))%>%
  dplyr::summarise(min.int=min(DOY_adj,na.rm=T),max.int=max(DOY_adj,na.rm=T))

interval_aa$days<-c(rep(NA,length=nrow(interval_aa)))
interval_aa$days[1]<-14
#int.days<-diff(interval$DOY_adj)
for(i in 1:(nrow(interval_aa)-1)){
  interval_aa$days[i+1]<-(interval_aa$min.int[i+1]-interval_aa$max.int[i])
}
interval_aa$int<-interval_aa$days/mean(interval_aa$days)

#metamorph size used in terrestrial pens
aa.mass<-aa_ch.pa%>%
  #group_by(Treatment)%>%
  summarise(meanMass=mean(Meta.Mass,na.rm=T),
            sdMass=sd(Meta.Mass,na.rm=T),
            minMass=min(Meta.Mass),
            maxMass=max(Meta.Mass))

#############################################################
## Format data to add predictors
#############################################################
#define groups, blocks and pens
aa_ch.pa$Treatment2<-ifelse(aa_ch.pa$Treatment=="L1J1", "J1", aa_ch.pa$Treatment) #Create juvenile-only treatment factor
aa_ch.pa$Treatment2<-ifelse(aa_ch.pa$Treatment=="L3J1", "J1", aa_ch.pa$Treatment2)
aa_ch.pa$Treatment2<-ifelse(aa_ch.pa$Treatment=="L3J3", "J3", aa_ch.pa$Treatment2)
aa_ch.pa$Treatment2<-ifelse(aa_ch.pa$Treatment=="L1J3", "J3", aa_ch.pa$Treatment2)

group_aa<-as.numeric(as.factor(aa_ch.pa$Treatment))
group2_aa<-as.numeric(as.factor(aa_ch.pa$Treatment2))
block_aa<-as.numeric(aa_ch.pa$Block)
pen_aa<-as.numeric(as.factor(paste(aa_ch.pa$Block,aa_ch.pa$Pen,sep="")))
rel_aa<-as.numeric(as.factor(aa_ch.pa$Release))

#define abiotic covariates
#aa_abiotic <- readRDS("~/GitHub/JuvenileEmigrationPhenology/aa_abiotic.rds")
aa_abiotic <- readRDS("~/GitRepos/JuvenileEmigrationPhenology/aa_abiotic.rds")
aa_abiotic$propMax <- c(0.14, 0.27, 0.33, rep(0,13)) #add row of proportion of previous interval days with max temp. above 35C
str(aa_abiotic)
cor.test(as.numeric(aa_abiotic$Tmin), as.numeric(aa_abiotic$Prcp)) #Not autocorrelated

aa_abiotic$Tmin <- as.numeric(aa_abiotic$Tmin)
aa_stdtempc<-rep(NA,length(aa_abiotic$Tmin))
aa_abiotic$Prcp <- as.numeric(aa_abiotic$Prcp)
aa_stdprecip<-rep(NA,length(aa_abiotic$Prcp))
aa_stdprecip<-rep(NA,length(aa_abiotic$Prcp))
aa_stdpropMax<-rep(NA,length(aa_abiotic$propMax))
aa_reachMax<-as.numeric(factor(c(1, 1, 1, rep(0,13)))) #factor indicating whether CTmax (max temp. above 35C) reached in previous interval days
#aa_abiotic$temp.sd <- as.numeric(aa_abiotic$temp.sd)
#aa_stdtempsd<-rep(NA,length(aa_abiotic$temp.sd))
hist(aa_abiotic$Tmin)
shapiro.test(aa_abiotic$Tmin)

hist(aa_abiotic$Prcp)
shapiro.test(aa_abiotic$Prcp)
hist(log(aa_abiotic$Prcp+1))
shapiro.test(log(aa_abiotic$Prcp+1))

aa_abiotic$log.Prcp<-log(aa_abiotic$Prcp+1)

#Scale temp. and Prcp. covariates
for (i in 1:length(aa_abiotic$Tmin)) {
  aa_stdtempc[i] <- (aa_abiotic$Tmin[i]-mean(aa_abiotic$Tmin[]))/sd(aa_abiotic$Tmin[])
  #stdtempsd[i] <- (aa_abiotic$temp.sd[i]-mean(aa_abiotic$temp.sd[]))/sd(aa_abiotic$temp.sd[])
  aa_stdprecip[i] <- (aa_abiotic$log.Prcp[i]-mean(aa_abiotic$log.Prcp[]))/sd(aa_abiotic$log.Prcp[])
  aa_stdpropMax[i] <- (aa_abiotic$propMax[i]-mean(aa_abiotic$propMax[]))/sd(aa_abiotic$propMax[])
}

#define body mass covariate
#amb$mass <- as.numeric(aa_$mass)
aa_ch.pa$Meta.Mass[is.na(aa_ch.pa$Meta.Mass)]<-mean(aa_ch.pa$Meta.Mass,na.rm=T)
stdmass_aa<-rep(NA,length(aa_ch.pa$Meta.Mass))

#Scale mass covariate
for (i in 1:length(aa_ch.pa$Meta.Mass)) {
  stdmass_aa[i] <- (aa_ch.pa$Meta.Mass[i]-mean(aa_ch.pa$Meta.Mass[]))/sd(aa_ch.pa$Meta.Mass[])
}

# # Specify model in nimble language
# library(nimble)
# aamod <- nimbleCode({
#     
#     ## Priors and constraints
#     for (i in 1:nind){
#       for (t in f[i]:(n.occasions-1)){
#         phi[i,t] <- (1/(1+exp(-(group.phi[group[i]] + beta.mass*mass[i] + beta.temp*temp[t] + beta.pre*precip[t] + block.phi[block[i]] + pen.phi[pen[i]] + epsilon.phi[t]))))^int[t]
#         p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + group.p[group[i,t]] + block.p[block[i,t]] + pen.p[pen[i,t]]+ epsilon.p[t])))
#         
#         #phi[i,t] <- (1/(1+exp(-(beta.g1[group[i]] + beta.mass*mass[i] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
#         #p[i,t] <- 1/(1+exp(-(beta.g2[group[i]] + beta.m[m[i,t]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
#       } #t
#     } #i
#     
#     ## For recapture
#   
#     #mean.p~dnorm(0,0.001)
#     #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
#     
#     for (u in 1:2){
#       beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
#     }
#     
#     group.p[1] ~ dnorm(0, 0.01)#I(-10,10)                   
#     group.p[2] ~ dnorm(0, 0.01)#I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
#     group.p[3] ~ dnorm(0, 0.01)#I(-10,10)
#     group.p[4] ~ dnorm(0, 0.01)#I(-10,10)
#     
#     for (b in 1:nblock){
#       block.p[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
#     }
#     sigma.beta.bl1~dunif(0,5)
#     tau.beta.bl1<-pow(sigma.beta.bl1,-2)
#     sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
#     
#     for (p in 1:npen){
#       pen.p[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
#     }
#     sigma.beta.pen1~dunif(0,5)
#     tau.beta.pen1<-pow(sigma.beta.pen1,-2)
#     sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
#     
#     ##For survival
#     
#     #mean.phi~dnorm(0,0.001)
#     #mu.phi<-1/(1+exp(-mean.phi))             # Logit transformed survival grand mean/intercept
#     
#     group.phi[1] ~ dnorm(0, 0.01)#I(-10,10)
#     group.phi[2] ~ dnorm(0, 0.01)#I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
#     group.phi[3] ~ dnorm(0, 0.01)#I(-10,10)
#     group.phi[4] ~ dnorm(0, 0.01)#I(-10,10)
#     
#     beta.mass ~ dnorm(0, 0.01)#I(-10, 10)      # Prior for mass slope parameter
# 
#     for (t in 1:(n.occasions-1)){
#       epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
#     }
#     sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
#     tau.phi <- pow(sigma.phi, -2)
#     sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
#     for (t in 1:(n.occasions-1)){
#       epsilon.p[t] ~ dnorm(0, tau.p)      # Prior for recapture residuals
#     }
#     sigma.p ~ dunif(0,5)                    # Prior on standard deviation
#     tau.p <- pow(sigma.p, -2)
#     sigma2.p <- pow(sigma.p, 2)           # Residual temporal variance
#     
#     for (b in 1:nblock){
#       for (t in 1:(n.occasions-1)){
#         block.phi[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
#       }
#     }
#     sigma.beta.bl~dunif(0,5)
#     tau.beta.bl<-pow(sigma.beta.bl,-2)
#     sigma2.beta.bl <- pow(sigma.beta.bl, 2)
#     
#     for (p in 1:npen){
#       for (t in 1:(n.occasions-1)){
#         pen.phi[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
#       }
#     }
#     sigma.beta.pen~dunif(0,10)
#     tau.beta.pen<-pow(sigma.beta.pen,-2)
#     sigma2.beta.pen <- pow(sigma.beta.pen, 2)
# 
#     # Likelihood 
#     for (i in 1:nind){
#       # Define latent state at first capture
#       z[i,f[i]] <- 1
#       for (t in (f[i]+1):n.occasions){
#         # State process
#         z[i,t] ~ dbern(mu1[i,t])
#         mu1[i,t] <- phi[i,t-1] * z[i,t-1]
#         # Observation process
#         y[i,t] ~ dbern(mu2[i,t])
#         mu2[i,t] <- p[i,t-1] * z[i,t]
#       } #t
#     } #i
#   }
#   )
# 
# # Bundle data
# aa.data <- list(y = aa_CH, int=interval_aa$int, 
#                 z = known.state.cjs(aa_CH), 
#                 block = as.numeric(block_aa), 
#                 pen = as.numeric(pen_aa),
#                 mass=stdmass_aa, 
#                 group=group_aa, 
#                 m=m_aa,
#                 temp=aa_stdtempc,
#                 precip=aa_stdprecip,
#                 f = f_aa,
#                 nind = dim(aa_CH)[1], 
#                 n.occasions = dim(aa_CH)[2], 
#                 nblock = length(unique(block_aa)), 
#                 npen = length(unique(pen_aa)), 
#                 g = length(unique(group_aa)))
# 
# # Initial values (probably need to adjust thse to match dimensions of certain parameters)
# aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
#                             sigma.phi = runif(1, 0, 2), 
#                             sigma.p = runif(1, 0, 2), 
#                             mean.phi = runif(1, 0, 1),
#                             mean.p = runif(1, 0, 1), 
#                             group.p = runif(length(unique(group_aa)), 0, 1), 
#                             group.phi = runif(length(unique(group_aa)), 0, 1), 
#                             beta.mass = runif(1, -5, 5),
#                             beta.pre = runif(1, -5, 5), 
#                             beta.temp = runif(1, -5, 5),
#                             block.p = array(runif(68, 0, 1),dim=c(4,16)),
#                             block.phi = runif(length(unique(block_aa)), 0, 1), 
#                             sigma.beta.c= runif(1, 0, 2), 
#                             sigma.beta.h= runif(1, 0, 2), 
#                             pen.p = array(runif(384, 0, 1),dim=c(24,16)), 
#                             pen.phi = runif(length(unique(pen_aa)), 0, 1), 
#                             sigma.beta.d= runif(1, 0, 2), 
#                             sigma.beta.j= runif(1, 0, 2), 
#                             beta.m = runif (2, 0, 1))}
# 
# # Parameters monitored
# parameters <- c( "group.p",
#                  "group.phi",
#                  "beta.mass",
#                  "beta.pre",
#                  "beta.temp",
#                  "phi", 
#                  "p")
# 
# # MCMC settings
# ni <- 5000
# nt <- 5
# nb <- 1000
# nc <- 3
# 
# samples <- nimbleMCMC(
#   code = aamod,
#   constants = aa.data, ## provide the combined data & constants as constants
#   #data = aa.data,
#   inits = aa.inits,
#   monitors = parameters,
#   nchains = nc,
#   niter = ni,
#   nburnin = nb,
#   thin = nt)
# 
# library(MCMCvis)
# 
# MCMCsummary(object = samples, params="beta.a",round = 3)
# MCMCsummary(object = samples, params="beta.b",round = 3)
# MCMCsummary(object = samples, params="beta.e",round = 3)
# MCMCsummary(object = samples, params="beta.f",round = 3)
# MCMCsummary(object = samples, params="beta.g",round = 3)

#aa.cjs.trt.mass.cov.add5a<-readRDS("Results/aman.finalmod.rds")
#saveRDS(aa.cjs.trt.mass.cov.add5,file = "Results/aman.finalmod.rds")
#plot(aa.cjs.trt.mass.cov.add5)

###AMOP Model
recaps <- read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMOP/Recap_Database_2019-2020-Experiments_Master_20200125.xlsx", sheet=2,na=c("NA", ""))## recapture data
penID <- read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMOP/Pens_Assignments_2019-2020-Experiments.xlsx", na=c("NA", ""))
endfates<-read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMOP/BreakdownFates_2019-2020-Experiments.xlsx",na=c("NA",""))
sex<-read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMOP/Siegel Salamander data.xlsx",na=c("NA",""))
endfates<-merge(endfates,sex,by="PIT_Tag")
endfates<-merge(endfates,penID,by=c("PIT_Tag","Juv.Treat","Juv.Pen","Treatment","Species"))

## Format/Merge Data:
## ------------------
DOY.adj <- 157                           ## Standardize DOY to date of first release- 07 June 2019

recaps<-recaps%>%
  #mutate(mutate(across(where(is.character), as.factor)))%>%
  filter(!grepl("^UNK.",PIT_Tag))%>% #untagged animals we caught
  filter(!grepl("Arianne",Notes))%>%
  filter(!(Period%in%c("R1","R2","R3")))%>% #release periods
  filter(PIT_Tag!="x0992")%>% #tag from last year
  mutate(DOY = as.numeric(format(as.Date(as.character(Recap_Date),"%Y-%m-%d"),"%j")),
         DOY_adj = ifelse(DOY >= DOY.adj, yes=DOY-(DOY.adj-1), no=DOY+(365-DOY.adj+1)),
         Rel.DOY = as.numeric(format(as.Date(as.character(Release),"%Y-%m-%d"),"%j")),
         Rel.DOY_adj = ifelse(Rel.DOY >= DOY.adj, yes=Rel.DOY-(DOY.adj-1), no=Rel.DOY+(365-DOY.adj+1)),
         Recap_Loc = as.character(Recap_Loc),
         Rc.Block = sapply(strsplit(Recap_Loc, ","), function(x) x[1]),
         Rc.Pen = sapply(strsplit(Recap_Loc, ","), function(x) x[2]),
         Rc.Micro = sapply(strsplit(Recap_Loc, ","), function(x) x[3]),
         Moved = ifelse(is.na(Moved)==T, yes=0, no=Moved),
         Visual = ifelse(is.na(Visual)==T | Visual==2, yes=0, no=Visual),
         Burr_Vis = ifelse(is.na(Burr_Vis)==T, yes=0, no=Burr_Vis),
         Pre.Alive = (Moved + Visual + Burr_Vis),
         Pre.Alive = ifelse(Pre.Alive >= 1, yes=1, no=0))
recaps<-recaps%>%
  mutate(DOY_adj1=if_else(Recap_Date>"2020-06-06",DOY_adj+365,DOY_adj))

penID<-penID%>%
  select("PIT_Tag","Species","Juv.Pen","Juv.Treat","Meta.Mass.g", "Rel.Cohort")%>%
  mutate(Rel.Block = sapply(strsplit(Juv.Pen, ","), function(x) x[1]),
         Rel.Pen = sapply(strsplit(Juv.Pen, ","), function(x) x[2]))%>%
  mutate(across(where(is.character), as.factor))

df <- merge(recaps, penID, by=c("PIT_Tag"),all.x=T)
# df<-df%>%
#   mutate(Species=Species.y)%>%
#   select(!c(Species.x,Species.y))

ao_df<- df%>%
  filter(as.factor(Juv.Treat)%in%c("L3-J1","L3-J3","L1-J1","L1-J3"))

#metamorph sizes used in terrestrial pens
ao_mass<-penID%>%filter(as.factor(Juv.Treat)%in%c("L3-J1","L3-J3","L1-J1","L1-J3"))%>%
  #group_by(Juv.Treat)%>%
  summarise(meanMass=mean(Meta.Mass.g,na.rm=T),
            sdMass=sd(Meta.Mass.g,na.rm=T),
            minMass=min(Meta.Mass.g),
            maxMass=max(Meta.Mass.g))

#Build capture history matrix
# ao_ch.pa <- matrix(0, nrow=dim(ao_penID)[1], ncol=max(as.numeric(ao_recaps$Period), na.rm=T))
# ao_ch.pa <- as.data.frame(cbind(ao_penID[,c("PIT_Tag", "Juv.Treat", "Rel.Block", "Rel.Pen")],ao_ch.pa))
# ao_ch.pa[,5] <- 1
# 
# 
# ag.recaps <- aggregate(ao_recaps$Pre.Alive, by=list(ao_recaps$PIT_Tag, ao_recaps$Period), FUN='sum')
# colnames(ag.recaps) <- c("PIT_Tag", "Period", "Nobs")
# ag.recaps$Pre.Alive <- ag.recaps$Nobs
# ag.recaps$Pre.Alive <- ifelse(ag.recaps$Pre.Alive >= 1, yes=1, no=ag.recaps$Pre.Alive)
# for(i in 1:dim(ao_ch.pa)[1]){
#   ind <- ao_ch.pa$PIT_Tag[i]
#   sdf <- filter(ag.recaps, PIT_Tag == ind)
#   
#   if(dim(sdf)[1]>0){
#     for(c in 6:dim(ao_ch.pa)[2]){
#       for(s in 1:dim(sdf)[1]){
#         p <- sdf$Period[s]
#         if((c-5) == p){
#           ao_ch.pa[i,c] <- sdf$Pre.Alive[s]
#         }
#       }
#     }
#   }
# }

ag.recaps<-ao_df%>%
  mutate(Period=as.numeric(as.character(Period)))%>%
  group_by(PIT_Tag, Juv.Treat,Rel.Block,Rel.Pen,Period, Meta.Mass.g, Rel.Cohort)%>%
  summarise(Nobs=sum(Pre.Alive))%>%
  mutate(Pre.Alive=Nobs)%>%
  mutate(Pre.Alive=if_else(Pre.Alive>=1,1,Pre.Alive))%>%
  select(!Nobs)%>%
  droplevels()

#pivot to wide format
ao_ch.pa<-ag.recaps%>%
  pivot_wider(names_from = Period,values_from=Pre.Alive,values_fill=0,names_sort=TRUE,names_prefix="R")%>%
  mutate(`R1`=rep(1))%>% #starting vector where they are all alive
  relocate(`R1`, .after = Rel.Pen)

#recombine with originally released animals
ao_penID<-filter(penID,as.factor(Juv.Treat)%in%c("L3-J1","L3-J3","L1-J1","L1-J3"))
ao_wide<-merge(ao_penID[,c("PIT_Tag", "Juv.Treat", "Rel.Block", "Rel.Pen","Meta.Mass.g", "Rel.Cohort")],ao_ch.pa,all=T)

ao_wide<-ao_wide%>%
  mutate(R1=replace_na(R1,1))%>%
  mutate_at(vars(R2:R15), ~replace_na(., 0))%>%
  droplevels()

## isolate capture history matrix data only.
ao_CH <- ao_wide%>%
  select(R1:R15)
ao_CH<-as.matrix(ao_CH)


## ------------------
## Analyze Survival:
## -----------------
# Initial values
# In JAGS we have to give good initial values for the latent state z. At all occasions when an individual was observed, its state
# is z = 1 for sure. In addition, if an individual was not observed at an occasion, but was alive for sure, because it was 
# observed before and thereafter (i.e. has a capture history of e.g. {101} or {10001}), then we know that the individual was alive 
# at all of these occasions, and thus z = 1. Therefore, we should provide initial values of z = 1 at these positions as well. The 
# following function provides such initial values from the observed capture histories:
known.state.cjs <- function(ch){
  state <- ch
  for (i in 1:dim(ch)[1]){
    n1 <- min(which(ch[i,]==1))
    n2 <- max(which(ch[i,]==1))
    state[i,n1:n2] <- 1
    state[i,n1] <- NA
  }
  state[state==0] <- NA
  return(state)
}

# Function to create a matrix of initial values for latent state z
cjs.init.z <- function(ch,f){
  for (i in 1:dim(ch)[1]){
    if (sum(ch[i,])==1) next
    n2 <- max(which(ch[i,]==1))
    ch[i,f[i]:n2] <- NA
  }
  for (i in 1:dim(ch)[1]){
    ch[i,1:f[i]] <- NA
  }
  return(ch)
}

# # Create group variable
# group_ao.L <- character(length = dim(ao_ch.pa)[1])      ## breeding phenology group
# group_ao.J <- character(length = dim(ao_ch.pa)[1])      ## emigration phenology group
# 
# for(i in 1:dim(ao_ch.pa)[1]){
#   group_ao.L[i] <- unlist(strsplit(as.character(ao_ch.pa$Juv.Treat[i]), "[-]", perl=T))[[1]]
#   group_ao.J[i] <- unlist(strsplit(as.character(ao_ch.pa$Juv.Treat[i]), "[-]", perl=T))[[2]]
# }
# 
# group_ao.L <- as.factor(group_ao.L)
# group_ao.J <- as.factor(group_ao.J)

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f_ao <- apply(ao_CH, 1, get.first)

# Create matrix m to indicate when an individual was captured, to address test 2.ct trap effect
m_ao<-ao_CH[,1:(dim(ao_CH)[2]-1)]
u_ao<-which (m_ao==0)
m_ao[u_ao]<-2

# Grand means
# With immediate trap response 

group_ao<-as.numeric(ao_wide$Juv.Treat)
block_ao<-as.numeric(as.factor(ao_wide$Rel.Block))
pen_ao<-as.numeric(as.factor(paste(ao_wide$Rel.Block,ao_wide$Rel.Pen,sep="")))

#load weather data
#ao_abiotic <- readRDS("~/GitHub/JuvenileEmigrationPhenology/ao_abiotic.rds")
ao_abiotic <- readRDS("~/GitRepos/JuvenileEmigrationPhenology/ao_abiotic.rds")
cor.test(as.numeric(ao_abiotic$Tmin), as.numeric(ao_abiotic$Prcp)) #Not autocorrelated

ao_stdtempc<-rep(NA,length(ao_abiotic$Tavg))
ao_stdprecip<-rep(NA,length(ao_abiotic$Prcp))
ao_stdtempsd <- as.numeric(ao_abiotic$temp.sd)
# stdtempsd<-rep(NA,length(abiotic$temp.sd))

#Scale temp. and precip. covariates
for (i in 1:length(ao_abiotic$Tmin)) {
  ao_stdtempc[i] <- (ao_abiotic$Tmin[i]-mean(ao_abiotic$Tmin[]))/sd(ao_abiotic$Tmin[])
  ao_stdtempsd[i] <- (ao_abiotic$temp.sd[i]-mean(ao_abiotic$temp.sd[]))/sd(ao_abiotic$temp.sd[])
  ao_stdprecip[i] <- (ao_abiotic$Prcp[i]-mean(ao_abiotic$Prcp[]))/sd(ao_abiotic$Prcp[])
}

stdmass_ao<-rep(NA,length(ao_wide$Meta.Mass.g))

#Scale mass covariate
for (i in 1:length(ao_wide$Meta.Mass)) {
  stdmass_ao[i] <- (ao_wide$Meta.Mass[i]-mean(ao_wide$Meta.Mass[]))/sd(ao_wide$Meta.Mass[])
}

#calculate unequal interval lengths 
#interval next to event 1 was calculated from the time of release (event 0). 
#The last interval is then between the last and second to last events)
interval_ao<-recaps%>%
  mutate(Period=as.numeric(as.character(Period)))%>%
  group_by(Period)%>%
  dplyr::summarise(min.int=min(DOY_adj1,na.rm=T),max.int=max(DOY_adj1,na.rm=T))
interval_ao$days[1]<-NA
interval_ao$days<-c(rep(NA,length=nrow(interval_ao)))
for(i in 1:(nrow(interval_ao)-1)){
  interval_ao$days[i+1]<-(interval_ao$min.int[i+1]-interval_ao$max.int[i])
}
interval_ao$days[1]<-14
interval_ao$int<-interval_ao$days/mean(interval_ao$days,na.rm=T)

#define groups, blocks and pens
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L1-J1", "J1", ao_wide$Juv.Treat) #Create juvenile-only treatment factor
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L3-J1", "J1", ao_wide$Treatment2)
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L3-J3", "J3", ao_wide$Treatment2)
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L1-J3", "J3", ao_wide$Treatment2)

group_ao<-as.numeric(ao_wide$Juv.Treat)
group2_ao<-as.numeric(as.factor(ao_wide$Treatment2))
block_ao<-as.numeric(ao_wide$Rel.Block)
pen_ao<-as.numeric(as.factor(paste(ao_wide$Rel.Block,ao_wide$Rel.Pen,sep="")))
rel_ao<-as.numeric(ao_wide$Rel.Cohort)

#define abiotic covariates
#ao_abiotic <- readRDS("~/GitHub/JuvenileEmigrationPhenology/ao_abiotic.rds")
ao_abiotic <- readRDS("~/GitRepos/JuvenileEmigrationPhenology/ao_abiotic.rds")
#ao_abiotic$propMax <- c(0.14, 0.27, 0.33, rep(0,13)) #add row of proportion of previous interval days with max temp. above 35C
str(ao_abiotic)
cor.test(as.numeric(ao_abiotic$Tmin), as.numeric(ao_abiotic$Prcp)) #Not autocorrelated

ao_abiotic$Tmin <- as.numeric(ao_abiotic$Tmin)
ao_stdtempc<-rep(NA,length(ao_abiotic$Tmin))
ao_abiotic$Prcp <- as.numeric(ao_abiotic$Prcp)
ao_stdprecip<-rep(NA,length(ao_abiotic$Prcp))
ao_stdprecip<-rep(NA,length(ao_abiotic$Prcp))
#ao_stdpropMax<-rep(NA,length(ao_abiotic$propMax))
#ao_reachMax<-as.numeric(factor(c(1, 1, 1, rep(0,13)))) #factor indicating whether CTmax (max temp. above 35C) reached in previous interval days
#ao_abiotic$temp.sd <- as.numeric(ao_abiotic$temp.sd)
#ao_stdtempsd<-rep(NA,length(ao_abiotic$temp.sd))
hist(ao_abiotic$Tmin)
shapiro.test(ao_abiotic$Tmin)

hist(ao_abiotic$Prcp)
shapiro.test(ao_abiotic$Prcp)
hist(log(ao_abiotic$Prcp+1))
shapiro.test(log(ao_abiotic$Prcp+1))

ao_abiotic$log.Prcp<-log(ao_abiotic$Prcp+1)

#Scale temp. and Prcp. covariates
for (i in 1:length(ao_abiotic$Tmin)) {
  ao_stdtempc[i] <- (ao_abiotic$Tmin[i]-mean(ao_abiotic$Tmin[]))/sd(ao_abiotic$Tmin[])
  ao_stdtempsd[i] <- (ao_abiotic$temp.sd[i]-mean(ao_abiotic$temp.sd[]))/sd(ao_abiotic$temp.sd[])
  ao_stdprecip[i] <- (ao_abiotic$log.Prcp[i]-mean(ao_abiotic$log.Prcp[]))/sd(ao_abiotic$log.Prcp[])
  #ao_stdpropMax[i] <- (ao_abiotic$propMax[i]-mean(ao_abiotic$propMax[]))/sd(ao_abiotic$propMax[])
}

#define body mass covariate
#amb$mass <- as.numeric(ao_$mass)
ao_wide$Meta.Mass.g[is.na(ao_wide$Meta.Mass.g)]<-mean(ao_wide$Meta.Mass.g,na.rm=T)
stdmass_ao<-rep(NA,length(ao_wide$Meta.Mass.g))

#Scale mass covariate
for (i in 1:length(ao_wide$Meta.Mass.g)) {
  stdmass_ao[i] <- (ao_wide$Meta.Mass.g[i]-mean(ao_wide$Meta.Mass.g[]))/sd(ao_wide$Meta.Mass.g[])
}

library(nimble)
cjs_timedep_allcovs <- nimbleCode({
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(group.phi[group[i]] + beta.mass*mass[i] + block.phi[block[i]] + pen.phi[pen[i]] + epsilon.phi[t])))) ^int[t]
        p[i,t] <- 1/(1+exp(-(group.p[group[i]] + beta.m[m[i,t]] + temp.p*temp[t] + pcp.p*precip[t] + block.p[block[i],t] + pen.p[pen[i],t] + epsilon.p[t])))
      } #t
    } #i
    
    
    for (b in 1:nblock){
      block.phi[b] ~ dnorm(0,tau.beta.c)      #Prior for logit of mean survival with random effect of block (random effect of block on phi)
    }
    sigma.beta.c~dunif(0,5)
    tau.beta.c<-pow(sigma.beta.c,-2)
    sigma2.beta.c <- pow(sigma.beta.c, 2)
    
    for (p in 1:npen){
      pen.phi[p] ~ dnorm(0,tau.beta.d)    #Prior for logit of mean survival with random effect of pen given block
    }
    sigma.beta.d~dunif(0,5)
    tau.beta.d<-pow(sigma.beta.d,-2)
    sigma2.beta.d <- pow(sigma.beta.d, 2)
    
    beta.mass ~ dnorm(0, 0.001)#I(-10, 10)         # Prior for mass slope parameter

    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for time-specific recapture
    }
    
    for (u in 1:g){
      group.phi[u] ~ dnorm(0, 0.1)#I(-10,10)          # Prior for group-spec. recapture
      group.p[u] ~ dnorm(0, 0.1)#I(-10,10)          # Prior for group-spec. survival
    } #g
    
    for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, 0.1)#I(-10,10)          # Prior for time on recapture
      epsilon.phi[t] ~ dnorm(0, 0.1)#I(-10,10)          # Prior for time on survival
    }
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        block.p[b,t] ~ dnorm(0,tau.beta.h)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
      }
    }
    sigma.beta.h~dunif(0,5)
    tau.beta.h<-pow(sigma.beta.h,-2)
    sigma2.beta.h <- pow(sigma.beta.h, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        pen.p[p,t] ~ dnorm(0,tau.beta.j)    #Prior for logit of mean recapture with random effect of pen given block
      }
    }
    sigma.beta.j~dunif(0,5)
    tau.beta.j<-pow(sigma.beta.j,-2)
    sigma2.beta.j <- pow(sigma.beta.j, 2)

    temp.p ~ dnorm(0, 0.01)#I(-10, 10)         # Prior for temp slope parameter on survival
    pcp.p ~ dnorm(0, 0.01)#I(-10, 10)         # Prior for pcp slope parameter on survival
    temp.phi ~ dnorm(0, 0.01)#I(-10, 10)         # Prior for temp slope parameter on survival
    pcp.phi ~ dnorm(0, 0.01)#I
    
    # Likelihood 
    for (i in 1:nind){
    # Define latent state at first capture
      z[i,f[i]] <- 1
      for (t in (f[i]+1):n.occasions){
        # State process
        z[i,t] ~ dbern(mu1[i,t])
        mu1[i,t] <- phi[i,t-1] * z[i,t-1]
        # Observation process
        y[i,t] ~ dbern(mu2[i,t])
        mu2[i,t] <- p[i,t-1] * z[i,t]
      } #t
    } #i
  }
)

# Bundle data
ao.data <- list(y = ao_CH, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                nblock = length(unique(block_ao)), block = as.numeric(block_ao), npen = length(unique(pen_ao)), pen = as.numeric(pen_ao),
                mass=stdmass_ao, g = length(unique(group_ao)), group=group_ao, m=m_ao,
                temp = ao_stdtempc,precip=ao_stdprecip)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
ao.inits <- function(){list(z = cjs.init.z(ao_CH,f_ao), 
                            temp.phi = runif(1, -5, 5), 
                            pcp.phi = runif(1, -5, 5), 
                            temp.p = runif(1, -5, 5), 
                            pcp.p = runif(1, -5, 5), 
                            group.p = runif(length(unique(group_ao)), 0, 1),
                            group.phi = runif(length(unique(group_ao)), 0, 1),
                            block.phi = runif(length(unique(block_ao)), 0, 1), 
                            block.p = array(runif(28, 0, 1),dim=c(2,14)), 
                            sigma.beta.c= runif(1, 0, 2), 
                            sigma.beta.h= runif(1, 0, 2), 
                            beta.mass = runif(1, -5, 5),
                            pen.p = array(runif(288, 0, 1),dim=c(16,14)), 
                            pen.phi = runif(length(unique(pen_ao)), 0, 1), 
                            sigma.beta.d= runif(1, 0, 2), 
                            sigma.beta.j= runif(1, 0, 2), 
                            beta.m = runif (2, 0, 1))}  

# aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_ao), 
#                             beta.temp = runif(1, -5, 5), 
#                             beta.pcp = runif(1, -5, 5), 
#                             group_p = rnorm(4),
#                             group_phi = rnorm(4),
#                             block.phi = runif(length(unique(block_ao)), 0, 1), 
#                             block.p = array(runif(28, 0, 1),dim=c(2,14)), 
#                             sigma.beta.c= runif(1, 0, 2), 
#                             sigma.beta.h= runif(1, 0, 2), 
#                             beta.mass = runif(1, -5, 5),
#                             pen.p = array(runif(288, 0, 1),dim=c(16,14)), 
#                             pen.phi = runif(length(unique(pen_ao)), 0, 1), 
#                             sigma.beta.d= runif(1, 0, 2), 
#                             sigma.beta.j= runif(1, 0, 2), 
#                             beta.m = runif (2, 0, 1))}  

# Bundle data
aa.data <- list(y = aa_CH, int=interval_aa$int,
                z = known.state.cjs(aa_CH),
                block = as.numeric(block_aa),
                pen = as.numeric(pen_aa),
                mass=stdmass_aa,
                group=group_aa,
                m=m_aa,
                temp=aa_stdtempc,
                precip=aa_stdprecip,
                f = f_aa,
                nind = dim(aa_CH)[1],
                n.occasions = dim(aa_CH)[2],
                nblock = length(unique(block_aa)),
                npen = length(unique(pen_aa)),
                g = length(unique(group_aa)))

aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2),
                            group.p = runif(length(unique(group_aa)), 0, 1), 
                            group.phi = runif(length(unique(group_aa)), 0, 1), 
                            beta.mass = runif(1, -5, 5),
                            temp.phi = runif(1, -5, 5), 
                            temp.p = runif(1, -5, 5),
                            pcp.phi = runif(1, -5, 5), 
                            pcp.p = runif(1, -5, 5),
                            block.p = array(runif(68, 0, 1),dim=c(4,16)),
                            block.phi = runif(length(unique(block_aa)), 0, 1), 
                            sigma.beta.c= runif(1, 0, 2), 
                            sigma.beta.h= runif(1, 0, 2), 
                            pen.p = array(runif(384, 0, 1),dim=c(24,16)), 
                            pen.phi = runif(length(unique(pen_aa)), 0, 1), 
                            sigma.beta.d= runif(1, 0, 2), 
                            sigma.beta.j= runif(1, 0, 2), 
                            beta.m = runif (2, 0, 1))}

# Parameters monitored
parameters <- c("group.phi", "group.p", "beta.mass", 
                "temp.p","pcp.p","phi", "p") 

ni=150000
nc=3
nb=50000
nt=5

samples_ao <- nimbleMCMC(
  code = cjs_timedep_allcovs,
  constants = ao.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = ao.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt)

samples_aa <- nimbleMCMC(
  code = cjs_timedep_allcovs,
  constants = aa.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = aa.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt)

library(MCMCvis)

MCMCsummary(object = samples_ao, params="group.phi",round = 3)
MCMCsummary(object = samples_ao, params="group.p",round = 3)
MCMCsummary(object = samples_ao, params="beta.mass",round = 3)
#MCMCsummary(object = samples_ao, params="pcp.phi",round = 3)
MCMCsummary(object = samples_ao, params="pcp.p",round = 3)
#MCMCsummary(object = samples_ao, params="temp.phi",round = 3)
MCMCsummary(object = samples_ao, params="temp.p",round = 3)

MCMCtrace(samples_ao, 
          params = c('group.phi[1]','group.phi[2]','group.phi[3]','group.phi[4]'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)

MCMCtrace(samples_ao, 
          params = c('group.p[1]','group.p[2]','group.p[3]','group.p[4]'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)

MCMCtrace(samples_ao, 
          params = c('pcp.phi','temp.phi','pcp.p','temp.p'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)

#annulatum results
MCMCsummary(object = samples_aa, params="group.phi",round = 3)
MCMCsummary(object = samples_aa, params="group.p",round = 3)
MCMCsummary(object = samples_aa, params="beta.mass",round = 3)
MCMCsummary(object = samples_aa, params="pcp.phi",round = 3)
MCMCsummary(object = samples_aa, params="pcp.p",round = 3)
MCMCsummary(object = samples_aa, params="temp.phi",round = 3)
MCMCsummary(object = samples_aa, params="temp.p",round = 3)

MCMCtrace(samples_aa, 
          params = c('group.phi[1]','group.phi[2]','group.phi[3]','group.phi[4]'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)

MCMCtrace(samples_aa, 
          params = c('group.p[1]','group.p[2]','group.p[3]','group.p[4]'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)

MCMCtrace(samples_aa, 
          params = c('pcp.phi','pcp.p','temp.phi','temp.p'), 
          ISB = FALSE, 
          exact = TRUE,
          pdf = FALSE)


#group averages for both species
MCMCplot(object=samples_aa,
         object2=samples_ao,
         params = 'group.phi', 
         ci = c(50, 90))

##Graph phi over time by treatment
#opacum
ao_phi <- MCMCchains(samples_ao, #get phi values for all individuals
                  params = 'phi',  
                  mcmc.list = F)

#organize into dataframe that is individual X time period
#??does this fill the matrix in the right order?? 
aophi.mean<-matrix(apply(ao_phi,2,mean),144,14) #does this put things in the right order? 

#combine with group 
ao_plotdat<-as.data.frame(cbind(ao.data$group,aophi.mean))

#average by period and group
ao_avgplotdat<-ao_plotdat%>%
  gather(key="Period",value="Survival",-1)%>%
  mutate(Period=as.numeric(as.factor(Period)))%>%
  group_by(Period,V1)%>%
  summarise(meanS=mean(Survival))%>%
  rename(Group=V1)

#plot data- doesn't look right relative to what JAGS plot looks like
ggplot(ao_avgplotdat,aes(Period,meanS,group=Group,color=as.factor(Group)))+
  geom_point()+
  geom_line()+
  theme_classic()+
  lims(y=c(0,1))+
  scale_color_manual(values=c('salmon1','deepskyblue3','midnightblue','orangered4'))+
  labs(y=expression("Survival probability ("~italic(phi)~")"),x="Recapture Period")


#annulatum
aa_phi <- MCMCchains(samples_aa, #get phi values for all individuals
                     params = 'phi',  
                     mcmc.list = F)

#organize into dataframe that is individual X time period
#??does this fill the matrix in the right order?? 
aaphi.mean<-matrix(apply(aa_phi,2,mean),192,16) 

#combine with group 
aa_plotdat<-as.data.frame(cbind(aa.data$group,aaphi.mean))

#average by period and group
aa_avgplotdat<-aa_plotdat%>%
  gather(key="Period",value="Survival",-1)%>%
  mutate(Period=as.numeric(as.factor(Period)))%>%
  group_by(Period,V1)%>%
  summarise(meanS=mean(Survival))%>%
  rename(Group=V1)

#plot data- doesn't look right relative to what JAGS plot looks like
ggplot(aa_avgplotdat,aes(Period,meanS,group=Group,color=as.factor(Group)))+
  geom_point()+
  geom_line()+
  theme_classic()+
  lims(y=c(0,1))+
  scale_color_manual(values=c('salmon1','deepskyblue3','midnightblue','orangered4'))+
  labs(y=expression("Survival probability ("~italic(phi)~")"),x="Recapture Period")


