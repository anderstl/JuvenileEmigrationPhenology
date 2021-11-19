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
  group_by(PIT_Tag,Juv.Treat,Rel.Block,Rel.Pen,Period)%>%
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
print(amb.cjs.c.c, digits = 3)


##########################################################################
# 2. Phi(t)P(t): Model with fixed time-dependent parameters (from Kery & Schaub 7.4.1)
# With immediate trap response
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
print(amb.cjs.t.t)

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
print(amb.cjs.t.c)
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
ao_jags.data <- list(y = ao_CH, int=interval_ao$int, f = f_ao, nind = dim(ao_CH)[1], n.occasions = dim(ao_CH)[2], m=m)

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
print(amb.cjs.c.t)

#############################################################
## Format data to add predictors
#############################################################
#define groups, blocks and pens
ao_ch.pa$Treatment2<-ifelse(ao_ch.pa$Treatment=="L1J1", "J1", ao_ch.pa$Treatment) #Create juvenile-only treatment factor
ao_ch.pa$Treatment2<-ifelse(ao_ch.pa$Treatment=="L3J1", "J1", ao_ch.pa$Treatment2)
ao_ch.pa$Treatment2<-ifelse(ao_ch.pa$Treatment=="L3J3", "J3", ao_ch.pa$Treatment2)
ao_ch.pa$Treatment2<-ifelse(ao_ch.pa$Treatment=="L1J3", "J3", ao_ch.pa$Treatment2)

group_ao<-as.numeric(as.factor(ao_ch.pa$Treatment))
group2_ao<-as.numeric(as.factor(ao_ch.pa$Treatment2))
block_ao<-as.numeric(ao_ch.pa$Block)
pen_ao<-as.numeric(as.factor(paste(ao_ch.pa$Block,ao_ch.pa$Pen,sep="")))

#define abiotic covariates
ao_abiotic <- readRDS("~/GitHub/JuvenileEmigrationPhenology/ao_abiotic.rds")
ao_abiotic$propMax <- c(0.14, 0.27, 0.33, rep(0,13)) #add row of proportion of previous interval days with max temp. above 35C
str(ao_abiotic)
cor.test(as.numeric(ao_abiotic$Tmin), as.numeric(ao_abiotic$Prcp)) #Not autocorrelated

ao_abiotic$Tmin <- as.numeric(ao_abiotic$Tmin)
ao_stdtempc<-rep(NA,length(ao_abiotic$Tmin))
ao_abiotic$Prcp <- as.numeric(ao_abiotic$Prcp)
ao_stdprecip<-rep(NA,length(ao_abiotic$Prcp))
ao_stdprecip<-rep(NA,length(ao_abiotic$Prcp))
ao_stdpropMax<-rep(NA,length(ao_abiotic$propMax))
ao_reachMax<-as.numeric(factor(c(1, 1, 1, rep(0,13)))) #factor indicating whether CTmax (max temp. above 35C) reached in previous interval days
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
  ao_stdpropMax[i] <- (ao_abiotic$propMax[i]-mean(ao_abiotic$propMax[]))/sd(ao_abiotic$propMax[])
}

#define body mass covariate
#amb$mass <- as.numeric(ao_$mass)
ao_ch.pa$Meta.Mass[is.na(ao_ch.pa$Meta.Mass)]<-mean(ao_ch.pa$Meta.Mass,na.rm=T)
stdmass_ao<-rep(NA,length(ao_ch.pa$Meta.Mass))

#Scale mass covariate
for (i in 1:length(ao_ch.pa$Meta.Mass)) {
  stdmass_ao[i] <- (ao_ch.pa$Meta.Mass[i]-mean(ao_ch.pa$Meta.Mass[]))/sd(ao_ch.pa$Meta.Mass[])
}
