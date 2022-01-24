## Load Packages:
## --------------
if(!require(tidyr)) install.packages('tidyr'); library("tidyr")
if(!require(readxl)) install.packages('readxl'); library("readxl")
if(!require(ggplot2)) install.packages('ggplot2'); library("ggplot2")
if(!require(dplyr)) install.packages('dplyr'); library("dplyr")
if(!require(viridis)) install.packages('viridis'); library("viridis")
library(lattice)
library(coda)
library(jagsUI)
library(mcmcplots)

#install.packages("devtools")
#devtools::install_github("tidyverse/dplyr")

## --------------
#Nick's IPR theme
my_theme2 <- function()
{
  cowplot::theme_cowplot() %+replace%
    theme(panel.background = element_rect(color = "black", size=0.75,
                                          linetype=1),
          panel.border = element_rect(colour = "black", fill=NA, size=0.75),
          axis.line=element_blank(),
          strip.background=element_rect(fill='white'),
          strip.text=element_text(size=12, face='bold'))
}

## Import Data:
## --------------------------------
recaps <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMOP/Recap_Database_2019-2020-Experiments_Master_20200125.xlsx", sheet=2,na=c("NA", ""))## recapture data
penID <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMOP/Pens_Assignments_2019-2020-Experiments.xlsx", na=c("NA", ""))
endfates<-read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMOP/BreakdownFates_2019-2020-Experiments.xlsx",na=c("NA",""))
sex<-read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMOP/Siegel Salamander data.xlsx",na=c("NA",""))
endfates<-merge(endfates,sex,by="PIT_Tag")
endfates<-merge(endfates,penID,by=c("PIT_Tag","Juv.Treat","Juv.Pen","Treatment","Species"))

## --------------------------------

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
  select("PIT_Tag","Species","Juv.Pen","Juv.Treat","Meta.Mass.g")%>%
  mutate(Rel.Block = sapply(strsplit(Juv.Pen, ","), function(x) x[1]),
         Rel.Pen = sapply(strsplit(Juv.Pen, ","), function(x) x[2]))%>%
  mutate(across(where(is.character), as.factor))
  
df <- merge(recaps, penID, by=c("PIT_Tag"),all.x=T)
# df<-df%>%
#   mutate(Species=Species.y)%>%
#   select(!c(Species.x,Species.y))

ao_df<- df%>%
  filter(as.factor(Juv.Treat)%in%c("L3-J1","L3-J3","L1-J1","L1-J3"))

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
  group_by(PIT_Tag, Juv.Treat,Rel.Block,Rel.Pen,Period, Meta.Mass.g)%>%
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
ao_wide<-merge(ao_penID[,c("PIT_Tag", "Juv.Treat", "Rel.Block", "Rel.Pen","Meta.Mass.g")],ao_ch.pa,all=T)

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
ao_abiotic <- readRDS("~/GitHub/JuvenileEmigrationPhenology/ao_abiotic.rds")
cor.test(as.numeric(ao_abiotic$Tmin), as.numeric(ao_abiotic$Prcp)) #Not autocorrelated

ao_stdtempc<-rep(NA,length(ao_abiotic$Tavg))
ao_stdprecip<-rep(NA,length(ao_abiotic$Prcp))
# abiotic$temp.sd <- as.numeric(abiotic$temp.sd)
# stdtempsd<-rep(NA,length(abiotic$temp.sd))

#Scale temp. and precip. covariates
for (i in 1:length(ao_abiotic$Tmin)) {
  ao_stdtempc[i] <- (ao_abiotic$Tmin[i]-mean(ao_abiotic$Tmin[]))/sd(ao_abiotic$Tmin[])
  #stdtempsd[i] <- (abiotic$temp.sd[i]-mean(abiotic$temp.sd[]))/sd(abiotic$temp.sd[])
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

### Basic models
#############################################################
# 1. Phi(.)P(.): Model with constant parameters (from Kery & Schaub 7.3)
# With immediate trap response
#############################################################

sink("amb-cjs-c-c.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (mean.phi)^int[t]               # Constant survival
        p[i,t] <- beta[m[i,t]]                  # Constant recapture
      } #t
    } #i
    
    mean.phi ~ dunif(0, 1)         # Prior for mean survival
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)         # Priors for recapture
    }
    
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
  ",fill = TRUE)
sink()

# Bundle data

ao_jags.data <- list(y = ao_CH, int=interval_ao$int, f = f_ao, m=m_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH))

# Initial values
inits <- function(){list(mean.phi = runif(1, 0, 1), beta = runif(2, 0, 1), z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("mean.phi", "beta", "phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 1 min)
amb.cjs.c.c <- jags(ao_jags.data, inits, parallel=TRUE, parameters, "amb-cjs-c-c.jags", 
                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Summarize posteriors
print(amb.cjs.c.c, digits = 3)#DIC = 1828.8


##########################################################################
# 2. Phi(t)P(t): Model with fixed time-dependent parameters (from Kery & Schaub 7.4.1)
# With immediate trap response 
# Best based on DIC
##########################################################################

sink("amb-cjs-t-t.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (alpha[t])^int[t]
        logit(p[i,t]) <- beta[m[i,t]] + epsilon[t]
      } #t
    } #i

    for (t in 1:(n.occasions-1)){
      alpha[t] ~ dunif(0, 1)        # Priors for time-specific survival
    }

    for (u in 1:2){
      beta[u] ~ dunif(0, 1)        # Priors for time-specific recapture
    }
    
    for (t in 1:(n.occasions-1)){
      epsilon[t] ~ dnorm(0, tau)
    }
    
    sigma ~ dunif(0, 10)                     # Prior for standard deviation
    tau <- pow(sigma, -2)
    sigma2 <- pow(sigma, 2)                  # Residual temporal variance
    
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2])

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), sigma = runif(1,0,2), 
                         alpha = runif(dim(ao_CH)[2]-1, 0, 1), z = known.state.cjs(ao_CH))}

# Parameters monitored
parameters <- c("alpha", "beta", "sigma2", "phi", "p")

## MCMC settings
ni <- 5000
nt <- 10
nb <- 2500
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.t <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-t.jags", 
                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

print(amb.cjs.t.t)#DIC = 1361.6

#####################################################################################################
# 3. Phi(t)P(.): Model with fixed time-dependent survival and constant recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
####################################################################################################

sink("amb-cjs-t-c.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (alpha[t])^int[t]              # Time-dependent survival
        p[i,t] <- beta[m[i,t]]                  # Constant recapture
      } #t
    } #i
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)         # Priors for recapture
    }
    
    for (t in 1:(n.occasions-1)){
      alpha[t] ~ dunif(0, 1)        # Prior for time-dependent survival
    }
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], n.occasions = dim(ao_CH)[2])

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = runif(dim(ao_CH)[2]-1, 0, 1), 
                         z = known.state.cjs(ao_CH))}

# Parameters monitored
parameters <- c("alpha", "mean.p", "beta","phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.c <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-c.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.c)#DIC = 1573.5
plot(amb.cjs.t.c)

#####################################################################################################
# 4. Phi(.)P(t): Model with fixed constant survival and time-dependent recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
####################################################################################################

sink("amb-cjs-c-t.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (mean.phi)^int[t]              # Constant survival
        logit(p[i,t]) <- beta[m[i,t]] + epsilon[t]  # Time-dependent recapture
      } #t
    } #i
    
    mean.phi ~ dunif(0,1)         # Prior for time-constant survival
    
    for (u in 1:2){
      beta[u] ~ dunif(0, 1)        # Priors for time-dependent recapture
    }

    for (t in 1:(n.occasions-1)){
      epsilon[t] ~ dnorm(0, tau)
    }
    
    sigma ~ dunif(0, 10)                     # Prior for standard deviation
    tau <- pow(sigma, -2)
    sigma2 <- pow(sigma, 2)                  # Residual temporal variance
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], n.occasions = dim(ao_CH)[2], m=m_ao)

# Initial values
inits <- function(){list(mean.phi = runif(1, 0, 1), beta = runif(2, 0, 1), z = known.state.cjs(ao_CH),
                         sigma = runif(1, 0, 2))}

# Parameters monitored
parameters <- c("beta", "mean.phi", "sigma2", "phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 1.87 min)
amb.cjs.c.t <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-c-t.jags", 
                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

print(amb.cjs.c.t)#DIC = 1462.38

#############################################################
## Format data to add predictors
#############################################################
#define groups, blocks and pens
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L1-J1", "J1", ao_wide$Juv.Treat) #Create juvenile-only treatment factor
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L3-J1", "J1", ao_wide$Treatment2)
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L3-J3", "J3", ao_wide$Treatment2)
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L1-J3", "J3", ao_wide$Treatment2)

group_ao<-as.numeric(as.factor(ao_wide$Juv.Treat))
group2_ao<-as.numeric(as.factor(ao_wide$Treatment2))
block_ao<-as.numeric(ao_wide$Rel.Block)
pen_ao<-as.numeric(as.factor(paste(ao_wide$Rel.Block,ao_wide$Rel.Pen,sep="")))

#define abiotic covariates
ao_abiotic <- readRDS("~/GitHub/JuvenileEmigrationPhenology/ao_abiotic.rds")
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
  #stdtempsd[i] <- (ao_abiotic$temp.sd[i]-mean(ao_abiotic$temp.sd[]))/sd(ao_abiotic$temp.sd[])
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

#############################################################
# 5. Phi(.+g)P(.): Model with constant parameters (from Kery & Schaub 7.3)
# With immediate trap response
# With treatment group effect
#############################################################

sink("amb-cjs-c-c-trt.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (alpha[group[i]])^int[t]    # Constant survival
        p[i,t] <- beta[m[i,t]]                  # Constant recapture
      } #t
    } #i
    
    for (u in 1:g){
      alpha[u] ~ dunif(0, 1)         # Prior for treatment-specific survival
    }
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)         # Priors for recapture
    }
    
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
  ",fill = TRUE)
sink()

# Bundle data

ao_jags.data <- list(y = ao_CH, int=interval_ao$int, f = f_ao, m=m_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group = group_ao)

# Initial values
inits <- function(){list(alpha = runif(4, 0, 1), beta = runif(2, 0, 1), z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha", "beta", "phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 1 min)
amb.cjs.c.c.trt <- jags(ao_jags.data, inits, parallel=TRUE, parameters, "amb-cjs-c-c-trt.jags", 
                        n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Summarize posteriors
print(amb.cjs.c.c.trt, digits = 3)

plot(density(amb.cjs.c.c.trt$sims.list$alpha[,1]))#L1J1
lines(density(amb.cjs.c.c.trt$sims.list$alpha[,2]), col=2)#L1J3
lines(density(amb.cjs.c.c.trt$sims.list$alpha[,3]), col=3)#L3J1, differs from other groups; closest to L3J3
lines(density(amb.cjs.c.c.trt$sims.list$alpha[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,1]-amb.cjs.c.c.trt$sims.list$alpha[,2]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,1]-amb.cjs.c.c.trt$sims.list$alpha[,3]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,1]-amb.cjs.c.c.trt$sims.list$alpha[,4]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,2]-amb.cjs.c.c.trt$sims.list$alpha[,3]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,2]-amb.cjs.c.c.trt$sims.list$alpha[,4]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,3]-amb.cjs.c.c.trt$sims.list$alpha[,4]))


#############################################################
# 5.1. Phi(.+g)P(.): Model with constant parameters (from Kery & Schaub 7.3)
# With immediate trap response
# With juvenile-only treatment group effect
#############################################################

sink("amb-cjs-c-c-trt2.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (alpha[group[i]])^int[t]    # Constant survival
        p[i,t] <- beta[m[i,t]]                  # Constant recapture
      } #t
    } #i
    
    for (u in 1:g){
      alpha[u] ~ dunif(0, 1)         # Prior for treatment-specific survival
    }
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)         # Priors for recapture
    }
    
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
  ",fill = TRUE)
sink()

# Bundle data

ao_jags.data <- list(y = ao_CH, int=interval_ao$int, f = f_ao, m=m_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                     g = length(unique(group2_ao)), group=group2_ao)

# Initial values
inits <- function(){list(alpha = runif(2, 0, 1), beta = runif(2, 0, 1), z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha", "beta", "phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.c.c.trt2 <- jags(ao_jags.data, inits, parallel=TRUE, parameters, "amb-cjs-c-c-trt2.jags", 
                         n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Summarize posteriors
print(amb.cjs.c.c.trt2, digits = 3)

plot(density(amb.cjs.c.c.trt2$sims.list$alpha[,1]))#J1
lines(density(amb.cjs.c.c.trt2$sims.list$alpha[,2]), col=2)#J3


#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.c.c.trt2$sims.list$alpha[,1]-amb.cjs.c.c.trt2$sims.list$alpha[,2]))
#No juvenile-only effect

#####################################################################################################
# 6. Phi(g+t)P(.): Model with fixed time-dependent survival and constant recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With fixed group and time effects on survival
####################################################################################################

sink("amb-cjs-t-c-trt.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(alpha[group[i]] + gamma[t]))))^int[t]     # Time and treatment-dependent survival
        p[i,t] <- beta[m[i,t]]                                     # Constant recapture
      } #t
    } #i
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)              # Priors for recapture
    }
    
    alpha[1] <- 0                        # Corner constraint
    alpha[2] ~ dnorm(0, 0.01)I(-10,10)   # Priors for difference in treatment-spec. survival compared to treatment 1
    alpha[3] ~ dnorm(0, 0.01)I(-10,10)
    alpha[4] ~ dnorm(0, 0.001)I(-10,10)
    
    for(t in 1:(n.occasions-1)){
      gamma[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      
      phi.trt1[t] <- 1/(1 + exp(-gamma[t]))             # Back-transformed survival of treatment 1
      phi.trt2[t] <- 1/(1 + exp(-gamma[t]-alpha[2]))    # Back-transformed survival of treatment 2
      phi.trt3[t] <- 1/(1 + exp(-gamma[t]-alpha[3]))    # Back-transformed survival of treatment 3
      phi.trt4[t] <- 1/(1 + exp(-gamma[t]-alpha[4]))    # Back-transformed survival of treatment 4
    }
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group=group_ao)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = c(NA, rnorm(3)), 
                         gamma = rnorm(14), z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha", "phi.trt1", "phi.trt2", "phi.trt3", "phi.trt4", "gamma", "beta","phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.c.trt <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-c-trt.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.c.trt)

plot(c(1:14), amb.cjs.t.c.trt$mean$phi.trt1, lty=1)
lines(c(1:14), amb.cjs.t.c.trt$mean$phi.trt1, col=1)
lines(c(1:14), amb.cjs.t.c.trt$mean$phi.trt2, col=2)
lines(c(1:14), amb.cjs.t.c.trt$mean$phi.trt3, col=3)
lines(c(1:14), amb.cjs.t.c.trt$mean$phi.trt4, col=4)

plot(density(amb.cjs.t.c.trt$sims.list$alpha[,1]), xlim=c(-3,3))#L3J3
lines(density(amb.cjs.t.c.trt$sims.list$alpha[,2]), col=2)#L3J1
lines(density(amb.cjs.t.c.trt$sims.list$alpha[,3]), col=3)#L1J3
lines(density(amb.cjs.t.c.trt$sims.list$alpha[,4]), col=4)#L1J1

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.c.trt$sims.list$alpha[,1]-amb.cjs.t.c.trt$sims.list$alpha[,2]))
plot(density(amb.cjs.t.c.trt$sims.list$alpha[,1]-amb.cjs.t.c.trt$sims.list$alpha[,3]))
plot(density(amb.cjs.t.c.trt$sims.list$alpha[,1]-amb.cjs.t.c.trt$sims.list$alpha[,4]))
plot(density(amb.cjs.t.c.trt$sims.list$alpha[,2]-amb.cjs.t.c.trt$sims.list$alpha[,3]))
plot(density(amb.cjs.t.c.trt$sims.list$alpha[,2]-amb.cjs.t.c.trt$sims.list$alpha[,4]))
plot(density(amb.cjs.t.c.trt$sims.list$alpha[,3]-amb.cjs.t.c.trt$sims.list$alpha[,4]))
#All overlap zero

#####################################################################################################
# 7. Phi(g+t)P(t): Model with fixed time-dependent survival and recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With fixed group and time effects on survival and time effects on recapture
####################################################################################################

sink("amb-cjs-t-t-trt.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(alpha[group[i]] + gamma[t]))))^int[t]     # Time and treatment-dependent survival
        p[i,t] <- (1/(1+exp(-(beta[m[i,t]] + gamma.p[t]))))               # Time-dependentrecapture
      } #t
    } #i
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)              # Priors for recapture
    }
    
    alpha[1] <- 0                        # Corner constraint
    for(u in 2:4){
      alpha[u] ~ dnorm(0, 0.01)I(-10,10)   # Priors for difference in treatment-spec. survival compared to treatment 1
    }
    
    for(t in 1:(n.occasions-1)){
      gamma[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      gamma.p[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      
      phi.trt21[t] <- 1/(1 + exp(-gamma[t]))             # Back-transformed survival of treatment 1
      phi.trt22[t] <- 1/(1 + exp(-gamma[t]-alpha[2]))    # Back-transformed survival of treatment 2
      phi.trt23[t] <- 1/(1 + exp(-gamma[t]-alpha[3]))    # Back-transformed survival of treatment 3
      phi.trt24[t] <- 1/(1 + exp(-gamma[t]-alpha[4]))    # Back-transformed survival of treatment 4
    }
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group=group_ao)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = c(NA, rnorm(3)), 
                         gamma = rnorm(14), gamma.p = rnorm(14), z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha", "phi.trt21", "phi.trt22", "phi.trt23", "phi.trt24", "gamma",  "gamma.p", "beta","phi", "p")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 30000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.t.trt <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-t-trt.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.t.trt) #DIC = 1576.59

plot(c(1:14), amb.cjs.t.t.trt$mean$phi.trt21, lty=1)
lines(c(1:14), amb.cjs.t.t.trt$mean$phi.trt21, col=1)
lines(c(1:14), amb.cjs.t.t.trt$mean$phi.trt22, col=2)
lines(c(1:14), amb.cjs.t.t.trt$mean$phi.trt23, col=3)
lines(c(1:14), amb.cjs.t.t.trt$mean$phi.trt24, col=4)

plot(density(amb.cjs.t.t.trt$sims.list$alpha[,1]), xlim=c(-3,3))#L1J1
lines(density(amb.cjs.t.t.trt$sims.list$alpha[,2]), col=2)#L1J3
lines(density(amb.cjs.t.t.trt$sims.list$alpha[,3]), col=3)#L3J1
lines(density(amb.cjs.t.t.trt$sims.list$alpha[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.t.trt$sims.list$alpha[,1]-amb.cjs.t.t.trt$sims.list$alpha[,2]))
plot(density(amb.cjs.t.t.trt$sims.list$alpha[,1]-amb.cjs.t.t.trt$sims.list$alpha[,3]))
plot(density(amb.cjs.t.t.trt$sims.list$alpha[,1]-amb.cjs.t.t.trt$sims.list$alpha[,4]))
plot(density(amb.cjs.t.t.trt$sims.list$alpha[,3]-amb.cjs.t.t.trt$sims.list$alpha[,4]))

#####################################################################################################
# 8. Phi(t)P(g+t): Model with fixed time-dependent survival and recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With fixed group and time effects on recapture and time effects on survival
####################################################################################################

sink("amb-cjs-t-t-trt2.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(gamma[t]))))^int[t]     # Time-dependent survival
        p[i,t] <- (1/(1+exp(-(beta[m[i,t]] + alpha[group[i]] + gamma.p[t]))))         # Time and treatment-dependentrecapture
      } #t
    } #i
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)              # Priors for recapture
    }
    
    alpha[1] <- 0                        # Corner constraint
    for(u in 2:4){
      alpha[u] ~ dnorm(0, 0.01)I(-10,10)   # Priors for difference in treatment-spec. survival compared to treatment 1
    }
    
    for(t in 1:(n.occasions-1)){
      gamma[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      gamma.p[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      
      p.trt21[t] <- 1/(1 + exp(-gamma.p[t]-beta[1]))             # Back-transformed survival of treatment 1
      p.trt22[t] <- 1/(1 + exp(-gamma.p[t]-beta[1]-alpha[2]))    # Back-transformed survival of treatment 2
      p.trt23[t] <- 1/(1 + exp(-gamma.p[t]-beta[1]-alpha[3]))    # Back-transformed survival of treatment 3
      p.trt24[t] <- 1/(1 + exp(-gamma.p[t]-beta[1]-alpha[4]))    # Back-transformed survival of treatment 4
    }
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group=group_ao)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = c(NA, rnorm(3)), 
                         gamma = rnorm(14), gamma.p = rnorm(14), z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha", "p.trt21", "p.trt22", "p.trt23", "p.trt24", "gamma",  "gamma.p", "beta","phi", "p")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 30000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.t.trt2 <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-t-trt2.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.t.trt2)#DIC = 1548.32

plot(c(1:14), amb.cjs.t.t.trt2$mean$p.trt21, lty=1)
lines(c(1:14), amb.cjs.t.t.trt2$mean$p.trt21, col=1)
lines(c(1:14), amb.cjs.t.t.trt2$mean$p.trt22, col=2)
lines(c(1:14), amb.cjs.t.t.trt2$mean$p.trt23, col=3)
lines(c(1:14), amb.cjs.t.t.trt2$mean$p.trt24, col=4)

plot(density(amb.cjs.t.t.trt2$sims.list$alpha[,1]), xlim=c(-3,3))#L1J1
lines(density(amb.cjs.t.t.trt2$sims.list$alpha[,2]), col=2)#L1J3
lines(density(amb.cjs.t.t.trt2$sims.list$alpha[,3]), col=3)#L3J1
lines(density(amb.cjs.t.t.trt2$sims.list$alpha[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.t.trt2$sims.list$alpha[,1]-amb.cjs.t.t.trt2$sims.list$alpha[,2]))
plot(density(amb.cjs.t.t.trt2$sims.list$alpha[,1]-amb.cjs.t.t.trt2$sims.list$alpha[,3]))
plot(density(amb.cjs.t.t.trt2$sims.list$alpha[,1]-amb.cjs.t.t.trt2$sims.list$alpha[,4]))
plot(density(amb.cjs.t.t.trt2$sims.list$alpha[,3]-amb.cjs.t.t.trt2$sims.list$alpha[,4]))

#####################################################################################################
# 9. Phi(g+t)P(g+t): Model with fixed time-dependent survival and recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With fixed group and time effects on recapture and  survival
####################################################################################################

sink("amb-cjs-t-t-trt3.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(alpha[group[i]] + gamma[t]))))^int[t]     # Time-dependent survival
        p[i,t] <- (1/(1+exp(-(beta[m[i,t]] + alpha.p[group[i]] + gamma.p[t]))))         # Time and treatment-dependentrecapture
      } #t
    } #i
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)              # Priors for recapture
    }
    
    alpha[1] <- 0                        # Corner constraint
    alpha.p[1] <- 0  
    for(u in 2:4){
      alpha[u] ~ dnorm(0, 0.01)I(-10,10)   # Priors for difference in treatment-spec. survival compared to treatment 1
      alpha.p[u] ~ dnorm(0, 0.01)I(-10,10)   # Priors for difference in treatment-spec. recapture compared to treatment 1
    }
    
    
    for(t in 1:(n.occasions-1)){
      gamma[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      gamma.p[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      
      phi.trt31[t] <- 1/(1 + exp(-gamma[t]-beta[1]))             # Back-transformed survival of treatment 1
      phi.trt32[t] <- 1/(1 + exp(-gamma[t]-beta[1]-alpha[2]))    # Back-transformed survival of treatment 2
      phi.trt33[t] <- 1/(1 + exp(-gamma[t]-beta[1]-alpha[3]))    # Back-transformed survival of treatment 3
      phi.trt34[t] <- 1/(1 + exp(-gamma[t]-beta[1]-alpha[4]))    # Back-transformed survival of treatment 4
      
      p.trt31[t] <- 1/(1 + exp(-gamma.p[t]-beta[1]))             # Back-transformed recapture of treatment 1
      p.trt32[t] <- 1/(1 + exp(-gamma.p[t]-beta[1]-alpha.p[2]))    # Back-transformed recapture of treatment 2
      p.trt33[t] <- 1/(1 + exp(-gamma.p[t]-beta[1]-alpha.p[3]))    # Back-transformed recapture of treatment 3
      p.trt34[t] <- 1/(1 + exp(-gamma.p[t]-beta[1]-alpha.p[4]))    # Back-transformed recapture of treatment 4
    }
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group=group_ao)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = c(NA, rnorm(3)), alpha.p = c(NA, rnorm(3)),
                         gamma = rnorm(14), gamma.p = rnorm(14), z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha", "alpha.p", "beta", "phi.trt31", "phi.trt32", "phi.trt33", "phi.trt34", 
                "p.trt31", "p.trt32", "p.trt33", "p.trt34", "gamma", "gamma.p","phi", "p")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 30000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.t.trt3 <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-t-trt3.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.t.trt3)#DIC = 1547.44

plot(amb.cjs.t.t.trt3)

#Recapture
plot(c(1:14), amb.cjs.t.t.trt3$mean$p.trt31, lty=1)
lines(c(1:14), amb.cjs.t.t.trt3$mean$p.trt31, col=1)
lines(c(1:14), amb.cjs.t.t.trt3$mean$p.trt32, col=2)
lines(c(1:14), amb.cjs.t.t.trt3$mean$p.trt33, col=3)
lines(c(1:14), amb.cjs.t.t.trt3$mean$p.trt34, col=4)

#Survival
plot(c(1:14), amb.cjs.t.t.trt3$mean$phi.trt31, lty=1)
lines(c(1:14), amb.cjs.t.t.trt3$mean$phi.trt31, col=1)
lines(c(1:14), amb.cjs.t.t.trt3$mean$phi.trt32, col=2)
lines(c(1:14), amb.cjs.t.t.trt3$mean$phi.trt33, col=3)
lines(c(1:14), amb.cjs.t.t.trt3$mean$phi.trt34, col=4)

#Group effect on recapture
plot(density(amb.cjs.t.t.trt3$sims.list$alpha.p[,1]), xlim=c(-3,3))#L1J1
lines(density(amb.cjs.t.t.trt3$sims.list$alpha.p[,2]), col=2)#L1J3
lines(density(amb.cjs.t.t.trt3$sims.list$alpha.p[,3]), col=3)#L3J1
lines(density(amb.cjs.t.t.trt3$sims.list$alpha.p[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.t.trt3$sims.list$alpha.p[,1]-amb.cjs.t.t.trt3$sims.list$alpha.p[,2]))
plot(density(amb.cjs.t.t.trt3$sims.list$alpha.p[,1]-amb.cjs.t.t.trt3$sims.list$alpha.p[,3]))
plot(density(amb.cjs.t.t.trt3$sims.list$alpha.p[,1]-amb.cjs.t.t.trt3$sims.list$alpha.p[,4]))
plot(density(amb.cjs.t.t.trt3$sims.list$alpha.p[,3]-amb.cjs.t.t.trt3$sims.list$alpha.p[,4]))

#Group effect on survival
plot(density(amb.cjs.t.t.trt3$sims.list$alpha[,1]), xlim=c(-3,3))#L1J1
lines(density(amb.cjs.t.t.trt3$sims.list$alpha[,2]), col=2)#L1J3
lines(density(amb.cjs.t.t.trt3$sims.list$alpha[,3]), col=3)#L3J1
lines(density(amb.cjs.t.t.trt3$sims.list$alpha[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.t.trt3$sims.list$alpha[,1]-amb.cjs.t.t.trt3$sims.list$alpha[,2]))
plot(density(amb.cjs.t.t.trt3$sims.list$alpha[,1]-amb.cjs.t.t.trt3$sims.list$alpha[,3]))
plot(density(amb.cjs.t.t.trt3$sims.list$alpha[,1]-amb.cjs.t.t.trt3$sims.list$alpha[,4]))
plot(density(amb.cjs.t.t.trt3$sims.list$alpha[,3]-amb.cjs.t.t.trt3$sims.list$alpha[,4]))

#####################################################################################################
# 10. Phi(g*t)P(.): Model with random time-dependent survival and constant recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With treatment group * time interaction term
####################################################################################################

sink("amb-cjs-t-c-int.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(eta.phi[group[i],t]))))^int[t]              # Time-dependent survival
        p[i,t] <- beta[m[i,t]]                  # Constant recapture
      } #t
    } #i
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)         # Priors for recapture
    }
    
    for (u in 1:g){
      for (t in 1:(n.occasions-1)){
        eta.phi[u,t] <- mu.phi[u] + epsilon[u,t]
        epsilon[u,t] ~ dnorm(0, tau[u])
      }#t
      mean.phi[u] ~ dunif(0, 1)                     #Priors on mean treatment-spec. survival
      mu.phi[u] <- log(mean.phi[u]/(1-mean.phi[u]))
      sigma[u] ~ dunif(0, 10)                       #Prior for treatment-spec. SD
      tau[u] <- pow(sigma[u], -2)
      sigma2[u] <-pow(sigma[u], 2)                  #Treatment-spec. temporal variance
    }#g
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2],  z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group=group_ao)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), mean.phi = runif(4, 0, 1),
                         z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("mu.phi", "mean.phi", "sigma2", "beta","phi", "p")

# MCMC settings
ni <- 20000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.c.int <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-c-int.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.c.int)

plot(amb.cjs.t.c.int)

plot(density(amb.cjs.t.c.int$sims.list$mean.phi[,1]))#L3J3
lines(density(amb.cjs.t.c.int$sims.list$mean.phi[,2]), col=2)#L3J1
lines(density(amb.cjs.t.c.int$sims.list$mean.phi[,3]), col=3)#L1J3
lines(density(amb.cjs.t.c.int$sims.list$mean.phi[,4]), col=4)#L1J1

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.c.int$sims.list$mean.phi[,1]-amb.cjs.t.c.int$sims.list$mean.phi[,2]))
plot(density(amb.cjs.t.c.int$sims.list$mean.phi[,1]-amb.cjs.t.c.int$sims.list$mean.phi[,3]))
plot(density(amb.cjs.t.c.int$sims.list$mean.phi[,1]-amb.cjs.t.c.int$sims.list$mean.phi[,4]))
plot(density(amb.cjs.t.c.int$sims.list$mean.phi[,2]-amb.cjs.t.c.int$sims.list$mean.phi[,3]))
plot(density(amb.cjs.t.c.int$sims.list$mean.phi[,2]-amb.cjs.t.c.int$sims.list$mean.phi[,4]))
plot(density(amb.cjs.t.c.int$sims.list$mean.phi[,3]-amb.cjs.t.c.int$sims.list$mean.phi[,4]))
#All overlap zero

#Calculate phi distributions
phi.list<-as.data.frame(amb.cjs.t.c.int$mean$phi)
phi.l<-as.data.frame(amb.cjs.t.c.int$q2.5$phi)
phi.h<-as.data.frame(amb.cjs.t.c.int$q97.5$phi)
phi.listv<-as.matrix(amb.cjs.t.c.int$mean$phi)
phi.lv<-as.matrix(amb.cjs.t.c.int$q2.5$phi)
phi.hv<-as.matrix(amb.cjs.t.c.int$q97.5$phi)
phi.mean <- phi.list %>% summarise_all(mean, na.rm=TRUE) 
phi.med <- phi.list %>% summarise_all(median, na.rm=TRUE) 
phi.lower <- as.numeric(phi.l %>% summarise_all(mean, na.rm=TRUE))
phi.higher <- as.numeric(phi.h %>% summarise_all(mean, na.rm=TRUE))
mean(phi.listv, na.rm = TRUE) #mean survival = 0.90
median(phi.listv, na.rm = TRUE) #median survival= 0.93
sd(phi.listv, na.rm = TRUE)#0.12
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.69
phi.ci.high<-mean(phi.hv, na.rm = TRUE)#0.99

g1.phi<-as.matrix(subset(phi.list[1:36,]))
g2.phi<-as.matrix(subset(phi.list[37:72,]))
g3.phi<-as.matrix(subset(phi.list[73:108,]))
g4.phi<-as.matrix(subset(phi.list[109:144,]))
g1.phi.dat<-as.data.frame(subset(phi.list[1:36,]))
g2.phi.dat<-as.data.frame(subset(phi.list[37:72,]))
g3.phi.dat<-as.data.frame(subset(phi.list[73:108,]))
g4.phi.dat<-as.data.frame(subset(phi.list[109:144,]))
g1.phil<-as.data.frame(subset(phi.l[1:36,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[37:72,]))
g3.phil<-as.data.frame(subset(phi.l[73:108,]))
g4.phil<-as.data.frame(subset(phi.l[109:144,]))
g1.phih<-as.data.frame(subset(phi.h[1:36,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[37:72,]))
g3.phih<-as.data.frame(subset(phi.h[73:108,]))
g4.phih<-as.data.frame(subset(phi.h[109:144,]))
phi.g1 <- g1.phi.dat %>% summarise_all(mean)
phi.g2 <- g2.phi.dat %>% summarise_all(mean)
phi.g3 <- g3.phi.dat %>% summarise_all(mean)
phi.g4 <- g4.phi.dat %>% summarise_all(mean)
phi.g1.med <- g1.phi.dat %>% summarise_all(median)
phi.g2.med <- g2.phi.dat %>% summarise_all(median)
phi.g3.med <- g3.phi.dat %>% summarise_all(median)
phi.g4.med <- g4.phi.dat %>% summarise_all(median)
g1.low<-as.numeric(g1.phil %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.phih %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.phil %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.phih %>% summarise_all(mean, na.rm=TRUE))
g3.low<-as.numeric(g3.phil %>% summarise_all(mean, na.rm=TRUE))
g3.high<-as.numeric(g3.phih %>% summarise_all(mean, na.rm=TRUE))
g4.low<-as.numeric(g4.phil %>% summarise_all(mean, na.rm=TRUE))
g4.high<-as.numeric(g4.phih %>% summarise_all(mean, na.rm=TRUE))
x.g1.phi<-mean(g1.phi)#Overall spp. means
x.g2.phi<-mean(g2.phi)
x.g3.phi<-mean(g3.phi)
x.g4.phi<-mean(g4.phi)
med.g1.phi<-median(g1.phi)#Overall spp. medians
med.g2.phi<-median(g2.phi)
med.g3.phi<-median(g3.phi)
med.g4.phi<-median(g4.phi)
means.phi<-c(x.g1.phi, x.g2.phi, x.g3.phi, x.g4.phi)#0.8811616 0.9136432 0.8980440 0.8929457
meds.phi<-c(med.g1.phi, med.g2.phi, med.g3.phi, med.g4.phi) #0.9233746 0.9511189 0.9379565 0.9310718
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.g3.phi<-sd(g3.phi)
sd.g4.phi<-sd(g4.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi, sd.g3.phi, sd.g4.phi)#0.1231950 0.1105236 0.1169268 0.1172422

#Figure of treatment-specific temporal survival
par(mai=c(2,2,1,1), mgp=c(5,2,0))
plot(x=(1:14),y= phi.g1.med, type="b", pch=1, col="salmon1",lty=3, cex=2.5, lwd=3, bty='l',
     ylim=c(0,1), ylab="Survival probability", xlab="Recapture occasion", cex.lab=2.5, cex.axis=2.5)
segments((1:14), g1.low, (1:14), g1.high, col="salmon1", lwd=2)
points(x=(1:14)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2, cex=2.5, lwd=2)
segments((1:14)+.1, g2.low, (1:14)+.1, g2.high, col="deepskyblue3", lwd=2)
points(x=(1:14)+.2,phi.g3.med, type="b", pch=0, col="midnightblue", lty=1, cex=2.5, lwd=2)
segments((1:14)+.2, g3.low, (1:14)+.2, g3.high, col="midnightblue", lwd=2)
points(x=(1:14)+.3,phi.g4.med, type="b", pch=5, col="orangered4", lty=4, cex=2.5, lwd=2)
segments((1:14)+.3, g4.low, (1:14)+.3, g4.high, col="orangered4", lwd=2)
legend(x = 10, y=.5, bty = 'n',
       #legend=c(as.expression(bquote(italic(.("Ambystoma annulatum")))),as.expression(bquote(italic(.("Ambystoma maculatum")))), as.expression(bquote(italic(.("Ambystoma texanum"))))),
       legend=c("L1J1", "L1J3", "L3J1", "L3J3"),
       lwd=c(3,2,2), pch=c(1,6,0,5), lty=c(3,2,1,4), cex=2.5,  col=c("salmon1", "deepskyblue3", "midnightblue", "orangered4"))
#dev.off()

#####################################################################################################
# 11. Phi(t*g)P(t): Model with random time-dependent survival and recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With treatment group * time interaction term on survival
####################################################################################################

sink("amb-cjs-t-t-int.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(eta.phi[group[i],t]))))^int[t]              # Time-dependent survival
        p[i,t] <- 1/(1+exp(-(mean.p + beta.m[m[i,t]] + epsilon.p[t])))      # Time-dependen recapture
      } #t
    } #i
    
    for(u in 1:2){
      beta.m[u] ~ dunif(0, 1)         # Priors for recapture
    }
    
    mean.p~dnorm(0,0.001)
    mu.p<-1/(1+exp(-mean.p))              # Logit transformed Recapture grand mean/intercept
    
    for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, tau.p)        # Prior for recapture residuals
    }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)             # Residual temporal variance
    
    for (u in 1:g){
      for (t in 1:(n.occasions-1)){
        eta.phi[u,t] <- mu.phi[u] + epsilon[u,t]
        epsilon[u,t] ~ dnorm(0, tau[u])
      }#t
      mean.phi[u] ~ dunif(0, 1)                     #Priors on mean treatment-spec. survival
      mu.phi[u] <- log(mean.phi[u]/(1-mean.phi[u]))
      sigma[u] ~ dunif(0, 10)                       #Prior for treatment-spec. SD
      tau[u] <- pow(sigma[u], -2)
      sigma2[u] <-pow(sigma[u], 2)                  #Treatment-spec. temporal variance
    }#g
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2],  z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group=group_ao)

# Initial values
inits <- function(){list(beta.m = runif(2, 0, 1), mean.phi = runif(4, 0, 1),
                         z = cjs.init.z(ao_CH,f_ao), mean.p = runif(1, 0, 1))}

# Parameters monitored
parameters <- c("mu.phi", "mean.phi", "mean.p", "sigma2", "beta.m","phi", "p")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 20000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.t.int <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-t-int.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.t.int)

plot(amb.cjs.t.t.int)

plot(density(amb.cjs.t.t.int$sims.list$mean.phi[,1]))#L3J3
lines(density(amb.cjs.t.t.int$sims.list$mean.phi[,2]), col=2)#L3J1
lines(density(amb.cjs.t.t.int$sims.list$mean.phi[,3]), col=3)#L1J3
lines(density(amb.cjs.t.t.int$sims.list$mean.phi[,4]), col=4)#L1J1

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.t.int$sims.list$mean.phi[,1]-amb.cjs.t.t.int$sims.list$mean.phi[,2]))
plot(density(amb.cjs.t.t.int$sims.list$mean.phi[,1]-amb.cjs.t.t.int$sims.list$mean.phi[,3]))
plot(density(amb.cjs.t.t.int$sims.list$mean.phi[,1]-amb.cjs.t.t.int$sims.list$mean.phi[,4]))
plot(density(amb.cjs.t.t.int$sims.list$mean.phi[,2]-amb.cjs.t.t.int$sims.list$mean.phi[,3]))
plot(density(amb.cjs.t.t.int$sims.list$mean.phi[,2]-amb.cjs.t.t.int$sims.list$mean.phi[,4]))
plot(density(amb.cjs.t.t.int$sims.list$mean.phi[,3]-amb.cjs.t.t.int$sims.list$mean.phi[,4]))
#All overlap zero

#Calculate phi distributions
phi.list<-as.data.frame(amb.cjs.t.t.int$mean$phi)
phi.l<-as.data.frame(amb.cjs.t.t.int$q2.5$phi)
phi.h<-as.data.frame(amb.cjs.t.t.int$q97.5$phi)
phi.listv<-as.matrix(amb.cjs.t.t.int$mean$phi)
phi.lv<-as.matrix(amb.cjs.t.t.int$q2.5$phi)
phi.hv<-as.matrix(amb.cjs.t.t.int$q97.5$phi)
phi.mean <- phi.list %>% summarise_all(mean, na.rm=TRUE) 
phi.med <- phi.list %>% summarise_all(median, na.rm=TRUE) 
phi.lower <- as.numeric(phi.l %>% summarise_all(mean, na.rm=TRUE))
phi.higher <- as.numeric(phi.h %>% summarise_all(mean, na.rm=TRUE))
mean(phi.listv, na.rm = TRUE) #mean survival = 0.90
median(phi.listv, na.rm = TRUE) #median survival= 0.93
sd(phi.listv, na.rm = TRUE)#0.12
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.70
phi.ci.high<-mean(phi.hv, na.rm = TRUE)#0.99

g1.phi<-as.matrix(subset(phi.list[1:36,]))
g2.phi<-as.matrix(subset(phi.list[37:72,]))
g3.phi<-as.matrix(subset(phi.list[73:108,]))
g4.phi<-as.matrix(subset(phi.list[109:144,]))
g1.phi.dat<-as.data.frame(subset(phi.list[1:36,]))
g2.phi.dat<-as.data.frame(subset(phi.list[37:72,]))
g3.phi.dat<-as.data.frame(subset(phi.list[73:108,]))
g4.phi.dat<-as.data.frame(subset(phi.list[109:144,]))
g1.phil<-as.data.frame(subset(phi.l[1:36,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[37:72,]))
g3.phil<-as.data.frame(subset(phi.l[73:108,]))
g4.phil<-as.data.frame(subset(phi.l[109:144,]))
g1.phih<-as.data.frame(subset(phi.h[1:36,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[37:72,]))
g3.phih<-as.data.frame(subset(phi.h[73:108,]))
g4.phih<-as.data.frame(subset(phi.h[109:144,]))
phi.g1 <- g1.phi.dat %>% summarise_all(mean)
phi.g2 <- g2.phi.dat %>% summarise_all(mean)
phi.g3 <- g3.phi.dat %>% summarise_all(mean)
phi.g4 <- g4.phi.dat %>% summarise_all(mean)
phi.g1.med <- g1.phi.dat %>% summarise_all(median)
phi.g2.med <- g2.phi.dat %>% summarise_all(median)
phi.g3.med <- g3.phi.dat %>% summarise_all(median)
phi.g4.med <- g4.phi.dat %>% summarise_all(median)
g1.low<-as.numeric(g1.phil %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.phih %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.phil %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.phih %>% summarise_all(mean, na.rm=TRUE))
g3.low<-as.numeric(g3.phil %>% summarise_all(mean, na.rm=TRUE))
g3.high<-as.numeric(g3.phih %>% summarise_all(mean, na.rm=TRUE))
g4.low<-as.numeric(g4.phil %>% summarise_all(mean, na.rm=TRUE))
g4.high<-as.numeric(g4.phih %>% summarise_all(mean, na.rm=TRUE))
x.g1.phi<-mean(g1.phi)#Overall spp. means
x.g2.phi<-mean(g2.phi)
x.g3.phi<-mean(g3.phi)
x.g4.phi<-mean(g4.phi)
med.g1.phi<-median(g1.phi)#Overall spp. medians
med.g2.phi<-median(g2.phi)
med.g3.phi<-median(g3.phi)
med.g4.phi<-median(g4.phi)
means.phi<-c(x.g1.phi, x.g2.phi, x.g3.phi, x.g4.phi)#0.8821195 0.9147134 0.8990304 0.8938436
meds.phi<-c(med.g1.phi, med.g2.phi, med.g3.phi, med.g4.phi) #0.9240564 0.9472223 0.9391936 0.9279482
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.g3.phi<-sd(g3.phi)
sd.g4.phi<-sd(g4.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi, sd.g3.phi, sd.g4.phi)#0.1231950 0.1105236 0.1169268 0.1172422

#Figure of treatment-specific temporal survival
par(mai=c(2,2,1,1), mgp=c(5,2,0))
plot(x=(1:14),y= phi.g1.med, type="b", pch=1, col="salmon1",lty=3, cex=2.5, lwd=3, bty='l',
     ylim=c(0,1), ylab="Survival probability", xlab="Recapture occasion", cex.lab=2.5, cex.axis=2.5)
segments((1:14), g1.low, (1:14), g1.high, col="salmon1", lwd=2)
points(x=(1:14)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2, cex=2.5, lwd=2)
segments((1:14)+.1, g2.low, (1:14)+.1, g2.high, col="deepskyblue3", lwd=2)
points(x=(1:14)+.2,phi.g3.med, type="b", pch=0, col="midnightblue", lty=1, cex=2.5, lwd=2)
segments((1:14)+.2, g3.low, (1:14)+.2, g3.high, col="midnightblue", lwd=2)
points(x=(1:14)+.3,phi.g4.med, type="b", pch=5, col="orangered4", lty=4, cex=2.5, lwd=2)
segments((1:14)+.3, g4.low, (1:14)+.3, g4.high, col="orangered4", lwd=2)
legend(x = 10, y=.5, bty = 'n',
       #legend=c(as.expression(bquote(italic(.("Ambystoma annulatum")))),as.expression(bquote(italic(.("Ambystoma maculatum")))), as.expression(bquote(italic(.("Ambystoma texanum"))))),
       legend=c("L1J1", "L1J3", "L3J1", "L3J3"),
       lwd=c(3,2,2), pch=c(1,6,0,5), lty=c(3,2,1,4), cex=2.5,  col=c("salmon1", "deepskyblue3", "midnightblue", "orangered4"))
#dev.off()

#####################################################################################################
# 12. Phi(t*g)P(t*g): Model with random time-dependent survival and recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With treatment group * time interaction term on survival
####################################################################################################

sink("amb-cjs-t-t-int1.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(mean.phi + alpha.phi[group[i],t] + epsilon.phi[t]))))^int[t]              # Time-dependent survival
        p[i,t] <- 1/(1+exp(-(mean.p + beta.m[m[i,t]] + alpha.p[group[i],t] + epsilon.p[t])))      # Time-dependent recapture
      } #t
    } #i
    
    for(u in 1:2){
      beta.m[u] ~ dunif(0, 1)         # Priors for recapture
    }
    
    for (u in 1:g){
      for (t in 1:(n.occasions-1)){
        alpha.phi[u,t] ~ dunif(0, 1)          # Prior for time and group-spec. survival
        alpha.p[u,t] ~ dunif(0, 1)          # Prior for time and group-spec. recapture
      } #t
    } #g
    
    mean.p~dnorm(0,0.001)
    mu.p<-1/(1+exp(-mean.p))              # Logit transformed Recapture grand mean/intercept
    mean.phi~dnorm(0,0.001)
    mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed Recapture grand mean/intercept
    
    for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, tau.p)        # Prior for recapture residuals
      epsilon.phi[t] ~ dnorm(0, tau.phi)        # Prior for recapture residuals
    }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)             # Residual temporal variance
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)             # Residual temporal variance
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2],  z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group=group_ao)

# Initial values
inits <- function(){list(beta.m = runif(2, 0, 1), mean.phi = runif(1, 0, 1),
                         z = cjs.init.z(ao_CH,f_ao), mean.p = runif(1, 0, 1),
                         alpha.phi = array(runif(64, 0, 1),dim=c(4,14)), 
                         alpha.p = array(runif(64, 0, 1),dim=c(4,14)))}

# Parameters monitored
parameters <- c("mu.phi", "mean.phi", "mu.p", "mean.p", "alpha.phi", "alpha.p", 
                "sigma.p", "sigma.phi", "beta.m","phi", "p")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 20000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.t.int1 <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-t-int1.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.t.int1)

plot(amb.cjs.t.t.int1)

plot(density(amb.cjs.t.t.int1$sims.list$mean.phi[,1]))#L3J3
lines(density(amb.cjs.t.t.int1$sims.list$mean.phi[,2]), col=2)#L3J1
lines(density(amb.cjs.t.t.int1$sims.list$mean.phi[,3]), col=3)#L1J3
lines(density(amb.cjs.t.t.int1$sims.list$mean.phi[,4]), col=4)#L1J1

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.t.int1$sims.list$mean.phi[,1]-amb.cjs.t.t.int1$sims.list$mean.phi[,2]))
plot(density(amb.cjs.t.t.int1$sims.list$mean.phi[,1]-amb.cjs.t.t.int1$sims.list$mean.phi[,3]))
plot(density(amb.cjs.t.t.int1$sims.list$mean.phi[,1]-amb.cjs.t.t.int1$sims.list$mean.phi[,4]))
plot(density(amb.cjs.t.t.int1$sims.list$mean.phi[,2]-amb.cjs.t.t.int1$sims.list$mean.phi[,3]))
plot(density(amb.cjs.t.t.int1$sims.list$mean.phi[,2]-amb.cjs.t.t.int1$sims.list$mean.phi[,4]))
plot(density(amb.cjs.t.t.int1$sims.list$mean.phi[,3]-amb.cjs.t.t.int1$sims.list$mean.phi[,4]))
#All overlap zero

#Calculate phi distributions
phi.list<-as.data.frame(amb.cjs.t.t.int1$mean$phi)
phi.l<-as.data.frame(amb.cjs.t.t.int1$q2.5$phi)
phi.h<-as.data.frame(amb.cjs.t.t.int1$q97.5$phi)
phi.listv<-as.matrix(amb.cjs.t.t.int1$mean$phi)
phi.lv<-as.matrix(amb.cjs.t.t.int1$q2.5$phi)
phi.hv<-as.matrix(amb.cjs.t.t.int1$q97.5$phi)
phi.mean <- phi.list %>% summarise_all(mean, na.rm=TRUE) 
phi.med <- phi.list %>% summarise_all(median, na.rm=TRUE) 
phi.lower <- as.numeric(phi.l %>% summarise_all(mean, na.rm=TRUE))
phi.higher <- as.numeric(phi.h %>% summarise_all(mean, na.rm=TRUE))
mean(phi.listv, na.rm = TRUE) #mean survival = 0.90
median(phi.listv, na.rm = TRUE) #median survival= 0.93
sd(phi.listv, na.rm = TRUE)#0.12
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.70
phi.ci.high<-mean(phi.hv, na.rm = TRUE)#0.99

g1.phi<-as.matrix(subset(phi.list[1:36,]))
g2.phi<-as.matrix(subset(phi.list[37:72,]))
g3.phi<-as.matrix(subset(phi.list[73:108,]))
g4.phi<-as.matrix(subset(phi.list[109:144,]))
g1.phi.dat<-as.data.frame(subset(phi.list[1:36,]))
g2.phi.dat<-as.data.frame(subset(phi.list[37:72,]))
g3.phi.dat<-as.data.frame(subset(phi.list[73:108,]))
g4.phi.dat<-as.data.frame(subset(phi.list[109:144,]))
g1.phil<-as.data.frame(subset(phi.l[1:36,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[37:72,]))
g3.phil<-as.data.frame(subset(phi.l[73:108,]))
g4.phil<-as.data.frame(subset(phi.l[109:144,]))
g1.phih<-as.data.frame(subset(phi.h[1:36,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[37:72,]))
g3.phih<-as.data.frame(subset(phi.h[73:108,]))
g4.phih<-as.data.frame(subset(phi.h[109:144,]))
phi.g1 <- g1.phi.dat %>% summarise_all(mean)
phi.g2 <- g2.phi.dat %>% summarise_all(mean)
phi.g3 <- g3.phi.dat %>% summarise_all(mean)
phi.g4 <- g4.phi.dat %>% summarise_all(mean)
phi.g1.med <- g1.phi.dat %>% summarise_all(median)
phi.g2.med <- g2.phi.dat %>% summarise_all(median)
phi.g3.med <- g3.phi.dat %>% summarise_all(median)
phi.g4.med <- g4.phi.dat %>% summarise_all(median)
g1.low<-as.numeric(g1.phil %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.phih %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.phil %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.phih %>% summarise_all(mean, na.rm=TRUE))
g3.low<-as.numeric(g3.phil %>% summarise_all(mean, na.rm=TRUE))
g3.high<-as.numeric(g3.phih %>% summarise_all(mean, na.rm=TRUE))
g4.low<-as.numeric(g4.phil %>% summarise_all(mean, na.rm=TRUE))
g4.high<-as.numeric(g4.phih %>% summarise_all(mean, na.rm=TRUE))
x.g1.phi<-mean(g1.phi)#Overall spp. means
x.g2.phi<-mean(g2.phi)
x.g3.phi<-mean(g3.phi)
x.g4.phi<-mean(g4.phi)
med.g1.phi<-median(g1.phi)#Overall spp. medians
med.g2.phi<-median(g2.phi)
med.g3.phi<-median(g3.phi)
med.g4.phi<-median(g4.phi)
means.phi<-c(x.g1.phi, x.g2.phi, x.g3.phi, x.g4.phi)#0.8821195 0.9147134 0.8990304 0.8938436
meds.phi<-c(med.g1.phi, med.g2.phi, med.g3.phi, med.g4.phi) #0.9240564 0.9472223 0.9391936 0.9279482
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.g3.phi<-sd(g3.phi)
sd.g4.phi<-sd(g4.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi, sd.g3.phi, sd.g4.phi)#0.1231950 0.1105236 0.1169268 0.1172422

#Figure of treatment-specific temporal survival
par(mai=c(2,2,1,1), mgp=c(5,2,0))
plot(x=(1:14),y= phi.g1.med, type="b", pch=1, col="salmon1",lty=3, cex=2.5, lwd=3, bty='l',
     ylim=c(0,1), ylab="Survival probability", xlab="Recapture occasion", cex.lab=2.5, cex.axis=2.5)
segments((1:14), g1.low, (1:14), g1.high, col="salmon1", lwd=2)
points(x=(1:14)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2, cex=2.5, lwd=2)
segments((1:14)+.1, g2.low, (1:14)+.1, g2.high, col="deepskyblue3", lwd=2)
points(x=(1:14)+.2,phi.g3.med, type="b", pch=0, col="midnightblue", lty=1, cex=2.5, lwd=2)
segments((1:14)+.2, g3.low, (1:14)+.2, g3.high, col="midnightblue", lwd=2)
points(x=(1:14)+.3,phi.g4.med, type="b", pch=5, col="orangered4", lty=4, cex=2.5, lwd=2)
segments((1:14)+.3, g4.low, (1:14)+.3, g4.high, col="orangered4", lwd=2)
legend(x = 10, y=.5, bty = 'n',
       #legend=c(as.expression(bquote(italic(.("Ambystoma annulatum")))),as.expression(bquote(italic(.("Ambystoma maculatum")))), as.expression(bquote(italic(.("Ambystoma texanum"))))),
       legend=c("L1J1", "L1J3", "L3J1", "L3J3"),
       lwd=c(3,2,2), pch=c(1,6,0,5), lty=c(3,2,1,4), cex=2.5,  col=c("salmon1", "deepskyblue3", "midnightblue", "orangered4"))
#dev.off()

###################################################################################################
#Messerman et al., (2020) Full Model ****Does not converge
#8. Phi(.*g+mass+block+pen)P(t*g+cov+block+pen): 
# FULL MODEL
# Treatment effect
# Survival by mass
# Temperature and precipitation covariates
# Time-dependent recapture and survival (best model based on DIC from Messerman et al., 2020)
# Grand means
# With immediate trap response
###################################################################################################

# Specify model in BUGS language
sink("ao-cjs-trt-mass-cov-rand.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(mean.phi + beta.a[group[i]] + beta.b*mass[i] + beta.c[block[i]] + beta.d[pen[i]] + epsilon.phi[i])))) ^int[t]
        p[i,t] <- 1/(1+exp(-(mean.p + beta.m[m[i,t]] + beta.e[group[i],t] + beta.f*temp[t] + beta.g*precip[t] + beta.h[block[i],t] + beta.j[pen[i],t] + epsilon.p[t])))
      } #t
    } #i
    
    ## For survival
    
    mean.phi~dnorm(0,0.001)
    mu.phi<-1/(1+exp(-mean.phi))           # Logit transformed Survival grand mean/intercept
    
    for (i in 1:nind){
      epsilon.phi[i] ~ dnorm(0, tau.phi)         # Prior for individual survival residuals
    }
    sigma.phi ~ dunif(0,5)                     # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)            # Residual temporal variance
    
    for (u in 1:g){
      beta.a[u] ~ dunif(0, 1)          # Priors for treatment-specific survival
    }
    
    for (b in 1:nblock){
      beta.c[b] ~ dnorm(0,tau.beta.c)      #Prior for logit of mean survival with random effect of block (random effect of block on phi)
    }
    sigma.beta.c~dunif(0,5)
    tau.beta.c<-pow(sigma.beta.c,-2)
    sigma2.beta.c <- pow(sigma.beta.c, 2)
    
    for (p in 1:npen){
      beta.d[p] ~ dnorm(0,tau.beta.d)    #Prior for logit of mean survival with random effect of pen given block
    }
    sigma.beta.d~dunif(0,5)
    tau.beta.d<-pow(sigma.beta.d,-2)
    sigma2.beta.d <- pow(sigma.beta.d, 2)
    
    beta.b ~ dnorm(0, 0.001)I(-10, 10)         # Prior for mass slope parameter

    ##For overall recapture
    
    mean.p~dnorm(0,0.001)
    mu.p<-1/(1+exp(-mean.p))              # Logit transformed Recapture grand mean/intercept
    
    for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, tau.p)          # Prior for recapture residuals
    }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)             # Residual temporal variance
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for time-specific recapture
    }
    
    for (u in 1:g){
      for (t in 1:(n.occasions-1)){
        beta.e[u,t] ~ dunif(0, 1)          # Prior for time and group-spec. recapture
      } #t
    } #g
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.h[b,t] ~ dnorm(0,tau.beta.h)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
      }
    }
    sigma.beta.h~dunif(0,5)
    tau.beta.h<-pow(sigma.beta.h,-2)
    sigma2.beta.h <- pow(sigma.beta.h, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.j[p,t] ~ dnorm(0,tau.beta.j)    #Prior for logit of mean recapture with random effect of pen given block
      }
    }
    sigma.beta.j~dunif(0,5)
    tau.beta.j<-pow(sigma.beta.j,-2)
    sigma2.beta.j <- pow(sigma.beta.j, 2)
    
    #For covariates
    beta.f ~ dnorm(0, 0.001)I(-10, 10)         # Prior for temp slope parameter
    beta.g ~ dnorm(0, 0.001)I(-10, 10)         # Prior for precip slope parameter

    
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
",fill = TRUE)
sink()


# Bundle data
ao.data <- list(y = ao_CH, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                nblock = length(unique(block_ao)), block = as.numeric(block_ao), npen = length(unique(pen_ao)), pen = as.numeric(pen_ao),
                mass=stdmass_ao, g = length(unique(group_ao)), group=group_ao, m=m_ao,
                temp = ao_stdtempc, precip = ao_stdprecip)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
ao.inits <- function(){list(z = cjs.init.z(ao_CH,f_ao), 
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            mean.phi = runif(1, 0, 1),
                            mean.p = runif(1, 0, 1), 
                            beta.f = runif(1, -5, 5), 
                            beta.g = runif(1, -5, 5),
                            beta.e = array(runif(60, 0, 1),dim=c(4,14)), 
                            beta.a = runif(length(unique(group_ao)), 0, 1), 
                            beta.c = runif(length(unique(block_ao)), 0, 1), 
                            beta.h = array(runif(28, 0, 1),dim=c(2,14)), 
                            sigma.beta.c= runif(1, 0, 2), 
                            sigma.beta.h= runif(1, 0, 2), 
                            beta.b = runif(1, -5, 5),
                            beta.j = array(runif(288, 0, 1),dim=c(16,14)), 
                            beta.d = runif(length(unique(pen_ao)), 0, 1), 
                            sigma.beta.d= runif(1, 0, 2), 
                            sigma.beta.j= runif(1, 0, 2), 
                            beta.m = runif (2, 0, 1))}  

# Parameters monitored
parameters <- c("mu.phi", "mean.phi", "beta.b", "mu.p",
                "mean.p", "beta.m", "sigma2.phi", "sigma2.p", 
                "beta.f","beta.g", "beta.a", "sigma2.beta.c", 
                "sigma2.beta.d", "sigma2.beta.h", "sigma2.beta.j", 
                "beta.c", "beta.d", "beta.e", "beta.h", "beta.j", 
                "epsilon.phi", "epsilon.p", "phi", "p") 

# MCMC settings
ni <- 50000
nt <- 5
nb <- 20000
nc <- 3

# Call JAGS from R (JRT 55 min)
ao.cjs.trt.mass.cov.rand <- jags(ao.data, parallel=TRUE, ao.inits, parameters, "ao-cjs-trt-mass-cov-rand.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(ao.cjs.trt.mass.cov.rand)#DIC=1287

plot(ao.cjs.trt.mass.cov.rand)

#Calculate phi distributions
phi.list<-as.data.frame(ao.cjs.trt.mass.cov.rand$mean$phi)
phi.l<-as.data.frame(ao.cjs.trt.mass.cov.rand$q2.5$phi)
phi.h<-as.data.frame(ao.cjs.trt.mass.cov.rand$q97.5$phi)
phi.listv<-as.matrix(ao.cjs.trt.mass.cov.rand$mean$phi)
phi.lv<-as.matrix(ao.cjs.trt.mass.cov.rand$q2.5$phi)
phi.hv<-as.matrix(ao.cjs.trt.mass.cov.rand$q97.5$phi)
phi.mean <- phi.list %>% summarise_all(mean, na.rm=TRUE) 
phi.med <- phi.list %>% summarise_all(median, na.rm=TRUE) 
phi.lower <- as.numeric(phi.l %>% summarise_all(mean, na.rm=TRUE))
phi.higher <- as.numeric(phi.h %>% summarise_all(mean, na.rm=TRUE))
mean(phi.listv, na.rm = TRUE) #mean survival = 0.76
median(phi.listv, na.rm = TRUE) #median survival= 0.83
sd(phi.listv, na.rm = TRUE)#0.22
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.45
phi.ci.high<-mean(phi.hv, na.rm = TRUE)#0.98

g1.phi<-as.matrix(subset(phi.list[1:36,]))
g2.phi<-as.matrix(subset(phi.list[37:72,]))
g3.phi<-as.matrix(subset(phi.list[73:108,]))
g4.phi<-as.matrix(subset(phi.list[109:144,]))
g1.phi.dat<-as.data.frame(subset(phi.list[1:36,]))
g2.phi.dat<-as.data.frame(subset(phi.list[37:72,]))
g3.phi.dat<-as.data.frame(subset(phi.list[73:108,]))
g4.phi.dat<-as.data.frame(subset(phi.list[109:144,]))
g1.phil<-as.data.frame(subset(phi.l[1:36,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[37:72,]))
g3.phil<-as.data.frame(subset(phi.l[73:108,]))
g4.phil<-as.data.frame(subset(phi.l[109:144,]))
g1.phih<-as.data.frame(subset(phi.h[1:36,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[37:72,]))
g3.phih<-as.data.frame(subset(phi.h[73:108,]))
g4.phih<-as.data.frame(subset(phi.h[109:144,]))
phi.g1 <- g1.phi.dat %>% summarise_all(mean)
phi.g2 <- g2.phi.dat %>% summarise_all(mean)
phi.g3 <- g3.phi.dat %>% summarise_all(mean)
phi.g4 <- g4.phi.dat %>% summarise_all(mean)
phi.g1.med <- g1.phi.dat %>% summarise_all(median)
phi.g2.med <- g2.phi.dat %>% summarise_all(median)
phi.g3.med <- g3.phi.dat %>% summarise_all(median)
phi.g4.med <- g4.phi.dat %>% summarise_all(median)
g1.low<-as.numeric(g1.phil %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.phih %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.phil %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.phih %>% summarise_all(mean, na.rm=TRUE))
g3.low<-as.numeric(g3.phil %>% summarise_all(mean, na.rm=TRUE))
g3.high<-as.numeric(g3.phih %>% summarise_all(mean, na.rm=TRUE))
g4.low<-as.numeric(g4.phil %>% summarise_all(mean, na.rm=TRUE))
g4.high<-as.numeric(g4.phih %>% summarise_all(mean, na.rm=TRUE))
x.g1.phi<-mean(g1.phi)#Overall spp. means
x.g2.phi<-mean(g2.phi)
x.g3.phi<-mean(g3.phi)
x.g4.phi<-mean(g4.phi)
med.g1.phi<-median(g1.phi)#Overall treatment medians
med.g2.phi<-median(g2.phi)
med.g3.phi<-median(g3.phi)
med.g4.phi<-median(g4.phi)
means.phi<-c(x.g1.phi, x.g2.phi, x.g3.phi, x.g4.phi)#0.7446462 0.8008541 0.7494634 0.7362286
meds.phi<-c(med.g1.phi, med.g2.phi, med.g3.phi, med.g4.phi) #0.8081852 0.8953175 0.8232520 0.7688689
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.g3.phi<-sd(g3.phi)
sd.g4.phi<-sd(g4.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi, sd.g3.phi, sd.g4.phi)#0.2156200 0.2040436 0.2258321 0.2272138

#Figure of treatment-specific temporal survival
par(mai=c(2,2,1,1), mgp=c(5,2,0))
plot(x=(1:14),y= phi.g1.med, type="b", pch=1, col="salmon1",lty=3, cex=2.5, lwd=3, bty='l',
     ylim=c(0,1), ylab="Survival probability", xlab="Recapture occasion", cex.lab=2.5, cex.axis=2.5)
segments((1:14), g1.low, (1:14), g1.high, col="salmon1", lwd=2)
points(x=(1:14)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2, cex=2.5, lwd=2)
segments((1:14)+.1, g2.low, (1:14)+.1, g2.high, col="deepskyblue3", lwd=2)
points(x=(1:14)+.2,phi.g3.med, type="b", pch=0, col="midnightblue", lty=1, cex=2.5, lwd=2)
segments((1:14)+.2, g3.low, (1:14)+.2, g3.high, col="midnightblue", lwd=2)
points(x=(1:14)+.3,phi.g4.med, type="b", pch=5, col="orangered4", lty=4, cex=2.5, lwd=2)
segments((1:14)+.3, g4.low, (1:14)+.3, g4.high, col="orangered4", lwd=2)
legend(x = 10, y=.5, bty = 'n',
       #legend=c(as.expression(bquote(italic(.("Ambystoma annulatum")))),as.expression(bquote(italic(.("Ambystoma maculatum")))), as.expression(bquote(italic(.("Ambystoma texanum"))))),
       legend=c("L1J1", "L1J3", "L3J1", "L3J3"),
       lwd=c(3,2,2), pch=c(1,6,0,5), lty=c(3,2,1,4), cex=2.5,  col=c("salmon1", "deepskyblue3", "midnightblue", "orangered4"))
#dev.off()
#####################################################################################################
# 14. Phi(g+t)P(g+t+cov): Model with fixed time-dependent survival and recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With fixed group and time effects on recapture and survival
#With temperature and precipitation effects on recapture
####################################################################################################

sink("amb-cjs-t-t-cov.jags")
cat("
    model {
    
    # Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(alpha[group[i]] + gamma[t]))))^int[t]     # Time-dependent survival
        p[i,t] <- (1/(1+exp(-(beta[m[i,t]] + alpha.p[group[i]] + gamma.p[t]))))         # Time and treatment-dependentrecapture
      } #t
    } #i
    
    for(u in 1:2){
      beta[u] ~ dunif(0, 1)              # Priors for recapture
    }
    
    alpha[1] <- 0                        # Corner constraint
    alpha.p[1] <- 0  
    for(u in 2:4){
      alpha[u] ~ dnorm(0, 0.01)I(-10,10)   # Priors for difference in treatment-spec. survival compared to treatment 1
      alpha.p[u] ~ dnorm(0, 0.01)I(-10,10)   # Priors for difference in treatment-spec. recapture compared to treatment 1
    }
    
    
    for(t in 1:(n.occasions-1)){
      gamma[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      gamma.p[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
    }
    
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
    ",fill = TRUE)
sink()

# Bundle data
ao_jags.data <- list(y = ao_CH, m=m_ao, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], 
                     n.occasions = dim(ao_CH)[2], z = known.state.cjs(ao_CH), 
                     g = length(unique(group_ao)), group=group_ao)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = c(NA, rnorm(3)), alpha.p = c(NA, rnorm(3)),
                         gamma = rnorm(14), gamma.p = rnorm(14), z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha", "alpha.p", "beta", "phi.cov1", "phi.cov2", "phi.cov3", "phi.cov4", 
                "p.cov1", "p.cov2", "p.cov3", "p.cov4", "gamma", "gamma.p","phi", "p")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 30000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.t.cov <- jags(ao_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-t-cov.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.t.cov)#DIC = 1547.44

plot(amb.cjs.t.t.cov)

#Recapture
plot(c(1:14), amb.cjs.t.t.cov$mean$p.cov1, lty=1)
lines(c(1:14), amb.cjs.t.t.cov$mean$p.cov1, col=1)
lines(c(1:14), amb.cjs.t.t.cov$mean$p.cov2, col=2)
lines(c(1:14), amb.cjs.t.t.cov$mean$p.cov3, col=3)
lines(c(1:14), amb.cjs.t.t.cov$mean$p.cov4, col=4)

#Survival
plot(c(1:14), amb.cjs.t.t.cov$mean$phi.cov1, lty=1)
lines(c(1:14), amb.cjs.t.t.cov$mean$phi.cov1, col=1)
lines(c(1:14), amb.cjs.t.t.cov$mean$phi.cov2, col=2)
lines(c(1:14), amb.cjs.t.t.cov$mean$phi.cov3, col=3)
lines(c(1:14), amb.cjs.t.t.cov$mean$phi.cov4, col=4)

#Group effect on recapture
plot(density(amb.cjs.t.t.cov$sims.list$alpha.p[,1]), xlim=c(-3,3))#L1J1
lines(density(amb.cjs.t.t.cov$sims.list$alpha.p[,2]), col=2)#L1J3
lines(density(amb.cjs.t.t.cov$sims.list$alpha.p[,3]), col=3)#L3J1
lines(density(amb.cjs.t.t.cov$sims.list$alpha.p[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.t.cov$sims.list$alpha.p[,1]-amb.cjs.t.t.cov$sims.list$alpha.p[,2]))
plot(density(amb.cjs.t.t.cov$sims.list$alpha.p[,1]-amb.cjs.t.t.cov$sims.list$alpha.p[,3]))
plot(density(amb.cjs.t.t.cov$sims.list$alpha.p[,1]-amb.cjs.t.t.cov$sims.list$alpha.p[,4]))
plot(density(amb.cjs.t.t.cov$sims.list$alpha.p[,3]-amb.cjs.t.t.cov$sims.list$alpha.p[,4]))

#Group effect on survival
plot(density(amb.cjs.t.t.cov$sims.list$alpha[,1]), xlim=c(-3,3))#L1J1
lines(density(amb.cjs.t.t.cov$sims.list$alpha[,2]), col=2)#L1J3
lines(density(amb.cjs.t.t.cov$sims.list$alpha[,3]), col=3)#L3J1
lines(density(amb.cjs.t.t.cov$sims.list$alpha[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.t.cov$sims.list$alpha[,1]-amb.cjs.t.t.cov$sims.list$alpha[,2]))
plot(density(amb.cjs.t.t.cov$sims.list$alpha[,1]-amb.cjs.t.t.cov$sims.list$alpha[,3]))
plot(density(amb.cjs.t.t.cov$sims.list$alpha[,1]-amb.cjs.t.t.cov$sims.list$alpha[,4]))
plot(density(amb.cjs.t.t.cov$sims.list$alpha[,3]-amb.cjs.t.t.cov$sims.list$alpha[,4]))