## ----------------------------------------------------------------------------------------------
## Ambystoma annulatum Carry-over Effects Terrestrial Pen Anaylsis
##    Data Summary: Estimating Growth and Survival of A. annulatum under different emigration 
##          phenologies (1 or 3 dates) which are crossed with breeding phenology treatments 
##          (1 or 3 breeding dates). Pen treatments are 1 larval, 1 juvenile; 1 larval, 3 
##          juvenile; 3 larval, 1 juvenile; or 3 larval, 3 juvenile (L1J1, L1J3, L3J1, and L3J3,
##          respectively). Pens were tracked appx. bi-weekly from 01 Aug 2018 - 31 Dec 2018, 
##          opportunistically on 01 Jan XX Jan 2019, XX Feb 2019, and bi-weekly from 01 April
##          2019 - 30 May 2019. Recaptures began opportunistically 29 April 2019 and animals 
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

aa_recaps <- read.csv("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/AMAN_phenology_recap_data_master.csv", header=T)           ## recapture data
#aa_weather <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/AaCOE_CapenPark_Daily-Weather_20180601-20190531.xlsx")  ## weather data

aa_assign <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Pen_Assignments.xlsx")             ## initial pen assignments
aa_treats <- read.csv("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Treatments.csv", header=T)           ## treatment data
aa_tagged <- read_excel("~/GitHub/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Tagged_Animals.xlsx")              ## all tagged animals and metamorphosis sizes
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
aa_ch.pa <- matrix(0, nrow=dim(aa_assign)[1], ncol=max(as.numeric(aa_recaps$Period), na.rm=T)+5)
aa_ch.pa <- as.data.frame(aa_ch.pa)
colnames(aa_ch.pa)[1:4] <- c("PIT_Tag", "Treatment", "Block", "Pen")
aa_ch.pa$PIT_Tag <- aa_assign$PIT_Tag
aa_ch.pa$Treatment <- aa_assign$Treatment
aa_ch.pa$Block <- aa_assign$Block
aa_ch.pa$Pen <- aa_assign$Pen
aa_ch.pa[,5] <- 1

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
    for(c in 6:dim(aa_ch.pa)[2]){
      for(s in 1:dim(sdf)[1]){
        p <- sdf$Period[s]
        if((c-5) == p){
          aa_ch.pa[i,c] <- sdf$Pre.Alive[s]
        }
      }
    }
  }
}


## isolate capture history matrix data only.
aa_CH <- as.matrix(aa_ch.pa[,5:dim(aa_ch.pa)[2]])        

#add mass to data frame
aa_ch.pa<-merge(aa_ch.pa,aa_tagged[,c("PIT_Tag","Meta.Mass","Meta.Date")],by=c("PIT_Tag"))
aa_ch.pa<-aa_ch.pa[-(aa_ch.pa$PIT_Tag=="x0966" & aa_ch.pa$Meta.Mass==2.21),]#drop duplicate mass?

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

aa_jags.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, m=m_aa, nind = dim(aa_CH)[1], 
                     n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH))

# Initial values
inits <- function(){list(mean.phi = runif(1, 0, 1), beta = runif(2, 0, 1), z = cjs.init.z(aa_CH,f_aa))}

# Parameters monitored
parameters <- c("mean.phi", "beta", "phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 1 min)
amb.cjs.c.c <- jags(aa_jags.data, inits, parallel=TRUE, parameters, "amb-cjs-c-c.jags", 
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
aa_jags.data <- list(y = aa_CH, m=m_aa, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], 
                     n.occasions = dim(aa_CH)[2])

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), sigma = runif(1,0,2), 
                         alpha = runif(dim(aa_CH)[2]-1, 0, 1), z = known.state.cjs(aa_CH))}

# Parameters monitored
parameters <- c("alpha", "beta", "sigma2", "phi", "p")

## MCMC settings
ni <- 5000
nt <- 10
nb <- 2500
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.t <- jags(aa_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-t.jags", 
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
aa_jags.data <- list(y = aa_CH, m=m_aa, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2])

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = runif(dim(aa_CH)[2]-1, 0, 1), 
                         z = known.state.cjs(aa_CH))}

# Parameters monitored
parameters <- c("alpha", "mean.p", "beta","phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.c <- jags(aa_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-c.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
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
aa_jags.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], m=m_aa)

# Initial values
inits <- function(){list(mean.phi = runif(1, 0, 1), beta = runif(2, 0, 1), z = known.state.cjs(aa_CH),
                         sigma = runif(1, 0, 2))}

# Parameters monitored
parameters <- c("beta", "mean.phi", "sigma2", "phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 1.87 min)
amb.cjs.c.t <- jags(aa_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-c-t.jags", 
                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.c.t)

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

#define abiotic covariates
aa_abiotic <- readRDS("~/GitHub/JuvenileEmigrationPhenology/aa_abiotic.rds")
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

aa_jags.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, m=m_aa, nind = dim(aa_CH)[1], 
                     n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                     g = length(unique(group_aa)), group=group_aa)

# Initial values
inits <- function(){list(alpha = runif(4, 0, 1), beta = runif(2, 0, 1), z = cjs.init.z(aa_CH,f_aa))}

# Parameters monitored
parameters <- c("alpha", "beta", "phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 1 min)
amb.cjs.c.c.trt <- jags(aa_jags.data, inits, parallel=TRUE, parameters, "amb-cjs-c-c-trt.jags", 
                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Summarize posteriors
print(amb.cjs.c.c.trt, digits = 3)

plot(density(amb.cjs.c.c.trt$sims.list$alpha[,1]))#L3J3
lines(density(amb.cjs.c.c.trt$sims.list$alpha[,2]), col=2)#L3J1
lines(density(amb.cjs.c.c.trt$sims.list$alpha[,3]), col=3)#L1J3
lines(density(amb.cjs.c.c.trt$sims.list$alpha[,4]), col=4)#L1J1

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,1]-amb.cjs.c.c.trt$sims.list$alpha[,2]))# Most different combination
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,1]-amb.cjs.c.c.trt$sims.list$alpha[,3]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,1]-amb.cjs.c.c.trt$sims.list$alpha[,4]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,2]-amb.cjs.c.c.trt$sims.list$alpha[,3]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,2]-amb.cjs.c.c.trt$sims.list$alpha[,4]))
plot(density(amb.cjs.c.c.trt$sims.list$alpha[,3]-amb.cjs.c.c.trt$sims.list$alpha[,4]))
#All overlap zero

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

aa_jags.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, m=m_aa, nind = dim(aa_CH)[1], 
                     n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                     g = length(unique(group2_aa)), group=group2_aa)

# Initial values
inits <- function(){list(alpha = runif(2, 0, 1), beta = runif(2, 0, 1), z = cjs.init.z(aa_CH,f_aa))}

# Parameters monitored
parameters <- c("alpha", "beta", "phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.c.c.trt2 <- jags(aa_jags.data, inits, parallel=TRUE, parameters, "amb-cjs-c-c-trt2.jags", 
                        n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

# Summarize posteriors
print(amb.cjs.c.c.trt2, digits = 3)

plot(density(amb.cjs.c.c.trt2$sims.list$alpha[,1]))#J1
lines(density(amb.cjs.c.c.trt2$sims.list$alpha[,2]), col=2)#J3


#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.c.c.trt2$sims.list$alpha[,1]-amb.cjs.c.c.trt2$sims.list$alpha[,2]))

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
aa_jags.data <- list(y = aa_CH, m=m_aa, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], 
                     n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                     g = length(unique(group_aa)), group=group_aa)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = c(NA, rnorm(3)), 
                         gamma = rnorm(16), z = cjs.init.z(aa_CH,f_aa))}

# Parameters monitored
parameters <- c("alpha", "phi.trt1", "phi.trt2", "phi.trt3", "phi.trt4", "gamma", "beta","phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.c.trt <- jags(aa_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-c-trt.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.c.trt)

plot(c(1:16), amb.cjs.t.c.trt$mean$phi.trt1, lty=1)
lines(c(1:16), amb.cjs.t.c.trt$mean$phi.trt1, col=1)
lines(c(1:16), amb.cjs.t.c.trt$mean$phi.trt2, col=2)
lines(c(1:16), amb.cjs.t.c.trt$mean$phi.trt3, col=3)
lines(c(1:16), amb.cjs.t.c.trt$mean$phi.trt4, col=4)

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
# 6.1. Phi(g+t)P(.): Model with fixed time-dependent survival and constant recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With fixed juvenile-only group and time effects on survival
####################################################################################################

sink("amb-cjs-t-c-trt2.jags")
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
    
    for(t in 1:(n.occasions-1)){
      gamma[t] ~ dnorm(0, 0.01)I(-10,10)       # Prior for time-dependent survival
      
      phi.trt21[t] <- 1/(1 + exp(-gamma[t]))             # Back-transformed survival of treatment 1
      phi.trt22[t] <- 1/(1 + exp(-gamma[t]-alpha[2]))    # Back-transformed survival of treatment 2
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
aa_jags.data <- list(y = aa_CH, m=m_aa, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], 
                     n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                     g = length(unique(group2_aa)), group=group2_aa)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), alpha = c(NA, rnorm(1)), 
                         gamma = rnorm(16), z = cjs.init.z(aa_CH,f_aa))}

# Parameters monitored
parameters <- c("alpha", "phi.trt21", "phi.trt22", "gamma", "beta","phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.c.trt2 <- jags(aa_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-c-trt2.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.c.trt2)

plot(c(1:16), amb.cjs.t.c.trt2$mean$phi.trt21, lty=1)
lines(c(1:16), amb.cjs.t.c.trt2$mean$phi.trt21, col=1)
lines(c(1:16), amb.cjs.t.c.trt2$mean$phi.trt22, col=2)

plot(density(amb.cjs.t.c.trt2$sims.list$alpha[,1]), xlim=c(-3,3))#J1
lines(density(amb.cjs.t.c.trt2$sims.list$alpha[,2]), col=2)#J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.c.trt2$sims.list$alpha[,1]-amb.cjs.t.c.trt2$sims.list$alpha[,2]))

#####################################################################################################
# 7. Phi(g*t)P(.): Model with random time-dependent survival and constant recapture (edited from Kery & Schaub 7.4.1)
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
aa_jags.data <- list(y = aa_CH, m=m_aa, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], 
                     n.occasions = dim(aa_CH)[2],  z = known.state.cjs(aa_CH), 
                     g = length(unique(group_aa)), group=group_aa)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), mean.phi = runif(4, 0, 1),
                         z = cjs.init.z(aa_CH,f_aa))}

# Parameters monitored
parameters <- c("mu.phi", "mean.phi", "sigma2", "beta","phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.c.int <- jags(aa_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-c-int.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
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
mean(phi.listv, na.rm = TRUE) #mean survival = 0.85
median(phi.listv, na.rm = TRUE) #median survival= 0.92
sd(phi.listv, na.rm = TRUE)#0.18
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.46
phi.ci.high<-mean(phi.hv, na.rm = TRUE)#0.97

g1.phi<-as.matrix(subset(phi.list[1:48,]))
g2.phi<-as.matrix(subset(phi.list[49:96,]))
g3.phi<-as.matrix(subset(phi.list[97:144,]))
g4.phi<-as.matrix(subset(phi.list[145:192,]))
g1.phi.dat<-as.data.frame(subset(phi.list[1:48,]))
g2.phi.dat<-as.data.frame(subset(phi.list[49:96,]))
g3.phi.dat<-as.data.frame(subset(phi.list[97:144,]))
g4.phi.dat<-as.data.frame(subset(phi.list[145:192,]))
g1.phil<-as.data.frame(subset(phi.l[1:48,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[49:96,]))
g3.phil<-as.data.frame(subset(phi.l[97:144,]))
g4.phil<-as.data.frame(subset(phi.l[145:192,]))
g1.phih<-as.data.frame(subset(phi.h[1:48,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[49:96,]))
g3.phih<-as.data.frame(subset(phi.h[97:144,]))
g4.phih<-as.data.frame(subset(phi.h[145:192,]))
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
means.phi<-c(x.g1.phi, x.g2.phi, x.g3.phi, x.g4.phi)#0.8526413 0.8472176 0.8523415 0.8555185
meds.phi<-c(med.g1.phi, med.g2.phi, med.g3.phi, med.g4.phi) #0.9130953 0.9130953 0.9190088 0.9198113
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.g3.phi<-sd(g3.phi)
sd.g4.phi<-sd(g4.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi, sd.g3.phi, sd.g4.phi)#0.1701492 0.1814983 0.1856068 0.1815513

#Figure of treatment-specific temporal survival
par(mai=c(2,2,1,1), mgp=c(5,2,0))
plot(x=(1:16),y= phi.g1.med, type="b", pch=1, col="salmon1",lty=3, cex=2.5, lwd=3, bty='l',
     ylim=c(0,1), ylab="Survival probability", xlab="Recapture occasion", cex.lab=2.5, cex.axis=2.5)
segments((1:16), g1.low, (1:16), g1.high, col="salmon1", lwd=2)
points(x=(1:16)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2, cex=2.5, lwd=2)
segments((1:16)+.1, g2.low, (1:16)+.1, g2.high, col="deepskyblue3", lwd=2)
points(x=(1:16)+.2,phi.g3.med, type="b", pch=0, col="midnightblue", lty=1, cex=2.5, lwd=2)
segments((1:16)+.2, g3.low, (1:16)+.2, g3.high, col="midnightblue", lwd=2)
points(x=(1:16)+.3,phi.g4.med, type="b", pch=5, col="orangered4", lty=4, cex=2.5, lwd=2)
segments((1:16)+.3, g4.low, (1:16)+.3, g4.high, col="orangered4", lwd=2)
legend(x = 10, y=.5, bty = 'n',
       #legend=c(as.expression(bquote(italic(.("Ambystoma annulatum")))),as.expression(bquote(italic(.("Ambystoma maculatum")))), as.expression(bquote(italic(.("Ambystoma texanum"))))),
       legend=c("L3J3", "L3J1", "L1J3", "L1J1"),
       lwd=c(3,2,2), pch=c(1,6,0,5), lty=c(3,2,1,4), cex=2.5,  col=c("salmon1", "deepskyblue3", "midnightblue", "orangered4"))
#dev.off()

#####################################################################################################
# 7.1. Phi(g*t)P(.): Model with random time-dependent survival and constant recapture (edited from Kery & Schaub 7.4.1)
# With immediate trap response
# With juvenile-only treatment group * time interaction term
####################################################################################################

sink("amb-cjs-t-c-int2.jags")
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
aa_jags.data <- list(y = aa_CH, m=m_aa, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], 
                     n.occasions = dim(aa_CH)[2],  z = known.state.cjs(aa_CH), 
                     g = length(unique(group2_aa)), group=group2_aa)

# Initial values
inits <- function(){list(beta = runif(2, 0, 1), mean.phi = runif(2, 0, 1),
                         z = cjs.init.z(aa_CH,f_aa))}

# Parameters monitored
parameters <- c("mu.phi", "mean.phi", "sigma2", "beta","phi", "p")

# MCMC settings
ni <- 15000
nt <- 10
nb <- 7000
nc <- 3

# Call JAGS from R (BRT 2 min)
amb.cjs.t.c.int2 <- jags(aa_jags.data, parallel=TRUE, inits, parameters, "amb-cjs-t-c-int2.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(amb.cjs.t.c.int2)

plot(amb.cjs.t.c.int2)

plot(density(amb.cjs.t.c.int2$sims.list$mean.phi[,1]))#J1
lines(density(amb.cjs.t.c.int2$sims.list$mean.phi[,2]), col=2)#J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(amb.cjs.t.c.int2$sims.list$mean.phi[,1]-amb.cjs.t.c.int2$sims.list$mean.phi[,2]))


#Calculate phi distributions
phi.list<-as.data.frame(amb.cjs.t.c.int2$mean$phi)
phi.l<-as.data.frame(amb.cjs.t.c.int2$q2.5$phi)
phi.h<-as.data.frame(amb.cjs.t.c.int2$q97.5$phi)
phi.listv<-as.matrix(amb.cjs.t.c.int2$mean$phi)
phi.lv<-as.matrix(amb.cjs.t.c.int2$q2.5$phi)
phi.hv<-as.matrix(amb.cjs.t.c.int2$q97.5$phi)
phi.mean <- phi.list %>% summarise_all(mean, na.rm=TRUE) 
phi.med <- phi.list %>% summarise_all(median, na.rm=TRUE) 
phi.lower <- as.numeric(phi.l %>% summarise_all(mean, na.rm=TRUE))
phi.higher <- as.numeric(phi.h %>% summarise_all(mean, na.rm=TRUE))
mean(phi.listv, na.rm = TRUE) #mean survival = 0.86
median(phi.listv, na.rm = TRUE) #median survival= 0.94
sd(phi.listv, na.rm = TRUE)#0.18
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.57
phi.ci.high<-mean(phi.hv, na.rm = TRUE)#0.97

g1.phi<-as.matrix(subset(phi.list[1:96,]))
g2.phi<-as.matrix(subset(phi.list[97:192,]))
g1.phi.dat<-as.data.frame(subset(phi.list[1:96,]))
g2.phi.dat<-as.data.frame(subset(phi.list[97:192,]))
g1.phil<-as.data.frame(subset(phi.l[1:96,])) #Treatment-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[97:192,]))
g1.phih<-as.data.frame(subset(phi.h[1:96,])) #Treatment-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[97:192,]))
phi.g1 <- g1.phi.dat %>% summarise_all(mean)
phi.g2 <- g2.phi.dat %>% summarise_all(mean)
phi.g1.med <- g1.phi.dat %>% summarise_all(median)
phi.g2.med <- g2.phi.dat %>% summarise_all(median)
g1.low<-as.numeric(g1.phil %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.phih %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.phil %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.phih %>% summarise_all(mean, na.rm=TRUE))
x.g1.phi<-mean(g1.phi)#Overall spp. means
x.g2.phi<-mean(g2.phi)
med.g1.phi<-median(g1.phi)#Overall spp. medians
med.g2.phi<-median(g2.phi)
means.phi<-c(x.g1.phi, x.g2.phi)#0.8526413 0.8472176 0.8523415 0.8555185
meds.phi<-c(med.g1.phi, med.g2.phi) #0.9130953 0.9130953 0.9190088 0.9198113
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi)#0.1701492 0.1814983 0.1856068 0.1815513

#Figure of treatment-specific temporal survival
par(mai=c(2,2,1,1), mgp=c(5,2,0))
plot(x=(1:16),y= phi.g1.med, type="b", pch=1, col="salmon1",lty=3, cex=2.5, lwd=3, bty='l',
     ylim=c(0,1), ylab="Survival probability", xlab="Recapture occasion", cex.lab=2.5, cex.axis=2.5)
segments((1:16), g1.low, (1:16), g1.high, col="salmon1", lwd=2)
points(x=(1:16)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2, cex=2.5, lwd=2)
segments((1:16)+.1, g2.low, (1:16)+.1, g2.high, col="deepskyblue3", lwd=2)
legend(x = 10, y=.5, bty = 'n',
       legend=c("J1", "J3"),
       lwd=c(3,2), pch=c(1,6), lty=c(3,2), cex=2.5,  col=c("salmon1", "deepskyblue3"))
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
sink("aa-cjs-trt-mass-cov-rand.jags")
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa,
                temp = aa_stdtempc, precip = aa_stdprecip)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            mean.phi = runif(1, 0, 1),
                            mean.p = runif(1, 0, 1), 
                            beta.f = runif(1, -5, 5), 
                            beta.g = runif(1, -5, 5),
                            beta.e = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.a = runif(length(unique(group_aa)), 0, 1), 
                            beta.c = runif(length(unique(block_aa)), 0, 1), 
                            beta.h = array(runif(68, 0, 1),dim=c(4,16)), 
                            sigma.beta.c= runif(1, 0, 2), 
                            sigma.beta.h= runif(1, 0, 2), 
                            beta.b = runif(1, -5, 5),
                            beta.j = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.d = runif(length(unique(pen_aa)), 0, 1), 
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
ni <- 15000
nt <- 5
nb <- 7000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.rand <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-rand.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.rand)#DIC=1287

#treatment contrasts
trtvals<-combn(1:4,m=2,simplify=F)
trt.cont<-matrix(NA,dim(aa.cjs.trt.mass.cov.rand$sims.list$beta.a)[1],length(trtvals))
for(i in 1:length(trtvals)){
  trt.cont[,i]<-(aa.cjs.trt.mass.cov.rand$sims.list$beta.a[,trtvals[[i]][[1]]]-aa.cjs.trt.mass.cov.rand$sims.list$beta.a[,trtvals[[i]][[2]]])
}

    
###################################################################################################
#Best supported model structure based on DIC ****Does not converge
#9. Phi(t+g+mass+temp+precip+block+pen)P(.+g+block+pen): 
# FULL MODEL
# Treatment additive effect
# Survival by mass, temperature, and precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# Grand means
# With immediate trap response 
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-add.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(mean.phi + beta.g1[group[i]] + beta.mass*mass[i] + beta.temp*temp[t] + beta.pre*precip[t] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(mean.p + beta.m[m[i,t]] + beta.g2[group[i]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
      } #t
    } #i
    
    
    
    ## For recapture
  
    mean.p~dnorm(0,0.001)
    mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] <- 0                           # Corner constraint
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.001)I(-10,10)
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    
    
    ##For survival
    
    mean.phi~dnorm(0,0.001)
    mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed survival grand mean/intercept
    
    beta.g1[1] <- 0                           # Corner constraint
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.001)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.001)I(-10, 10)     # Prior for mass slope parameter
    beta.temp ~ dnorm(0, 0.001)I(-10, 10)     # Prior for temp slope parameter
    beta.pre ~ dnorm(0, 0.001)I(-10, 10)      # Prior for precip slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,5)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa,
                temp = aa_stdtempc, precip = aa_stdprecip)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            mean.phi = runif(1, 0, 1),
                            mean.p = runif(1, 0, 1), 
                            beta.temp = runif(1, -5, 5), 
                            beta.pre = runif(1, -5, 5),
                            beta.g1 = c(NA, rnorm(3)), 
                            beta.g2 = c(NA, rnorm(3)), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c("mu.phi", "mean.phi", "beta.g1","beta.mass", "beta.temp",
                "beta.pre", "sigma2.phi", "mu.p","mean.p", "beta.m", 
                "beta.g2", "sigma2.p", "sigma2.beta.bl1", 
                "sigma2.beta.pen1", "sigma2.beta.bl", "sigma2.beta.pen", 
                "beta.bl1", "beta.pen1", "beta.bl", "beta.pen", 
                "epsilon.phi", "epsilon.p", "phi", "p") 

# MCMC settings
ni <- 15000
nt <- 5
nb <- 7000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.add <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-add.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.add)#DIC=977.29

#treatment contrasts
trtvals<-combn(1:4,m=2,simplify=F)
trt.cont1<-matrix(NA,dim(aa.cjs.trt.mass.cov.add$sims.list$beta.g1)[1],length(trtvals))
for(i in 1:length(trtvals)){
  trt.cont[,i]<-(aa.cjs.trt.mass.cov.add$sims.list$beta.g1[,trtvals[[i]][[1]]]-aa.cjs.trt.mass.cov.add$sims.list$beta.g1[,trtvals[[i]][[2]]])
}

###################################################################################################
#Best supported model structure based on DIC
#10. Phi(t+g+mass+temp+precip+block+pen)P(.+g+block+pen): 
# FULL MODEL
# Treatment additive effect
# Survival by mass, temperature, and precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# NO grand means
# With immediate trap response 
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-add1.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-( beta.g1[group[i]] + beta.mass*mass[i] + beta.temp*temp[t] + beta.pre*precip[t] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + beta.g2[group[i]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
      } #t
    } #i
    
    
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] <- 0                           # Corner constraint
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.01)I(-10,10)
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed survival grand mean/intercept
    
    beta.g1[1] <- 0                           # Corner constraint
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.01)I(-10, 10)     # Prior for mass slope parameter
    beta.temp ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter
    beta.pre ~ dnorm(0, 0.01)I(-10, 10)      # Prior for precip slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,10)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa,
                temp = aa_stdtempc, precip = aa_stdprecip)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            beta.temp = runif(1, -5, 5), 
                            beta.pre = runif(1, -5, 5),
                            beta.g1 = c(NA, rnorm(3)), 
                            beta.g2 = c(NA, rnorm(3)), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c( "beta.g1","beta.mass", "beta.temp",
                "beta.pre", "sigma2.phi", "beta.m", 
                "beta.g2", "sigma2.p", "beta.bl1", 
                "beta.pen1", "beta.bl", "beta.pen", 
                "epsilon.phi", "epsilon.p", "phi", "p") 


# MCMC settings
ni <- 20000
nt <- 5
nb <- 8000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.add1 <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-add1.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.add1)#DIC=1006.3
plot(aa.cjs.trt.mass.cov.add1)

###################################################################################################
#Best supported model structure based on DIC
#11. Phi(t+g+mass+temp+block+pen)P(.+g+block+pen): 
# Treatment additive effect
# Survival by mass and temperature, no precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# NO grand means
# With immediate trap response 
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-add2.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-( beta.g1[group[i]] + beta.mass*mass[i] + beta.temp*temp[t] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + beta.g2[group[i]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
      } #t
    } #i
    
    
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] <- 0                           # Corner constraint
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.01)I(-10,10)
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed survival grand mean/intercept
    
    beta.g1[1] <- 0                           # Corner constraint
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.01)I(-10, 10)     # Prior for mass slope parameter
    beta.temp ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,10)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa,
                temp = aa_stdtempc)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            beta.temp = runif(1, -5, 5), 
                            beta.g1 = c(NA, rnorm(3)), 
                            beta.g2 = c(NA, rnorm(3)), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c( "beta.g1","beta.mass", "beta.temp",
                  "sigma2.phi", "beta.m", 
                 "beta.g2", "sigma2.p", "beta.bl1", 
                 "beta.pen1", "beta.bl", "beta.pen", 
                 "epsilon.phi", "epsilon.p", "phi", "p") 


# MCMC settings
ni <- 20000
nt <- 5
nb <- 8000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.add2 <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-add2.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.add2)#DIC=997.1
plot(aa.cjs.trt.mass.cov.add2)

###################################################################################################
#Best supported model structure based on DIC
#12. Phi(t+g+mass+precip+block+pen)P(.+g+block+pen): 
# Treatment additive effect
# Survival by mass and precipitation, NO temperature
# Time-dependent survival and constant recapture (best model based on DIC)
# NO grand means
# With immediate trap response 
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-add3.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-( beta.g1[group[i]] + beta.mass*mass[i] + beta.pre*precip[t] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + beta.g2[group[i]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
      } #t
    } #i
    
    
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] <- 0                           # Corner constraint
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.01)I(-10,10)
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed survival grand mean/intercept
    
    beta.g1[1] <- 0                           # Corner constraint
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.01)I(-10, 10)     # Prior for mass slope parameter
    beta.pre ~ dnorm(0, 0.01)I(-10, 10)      # Prior for precip slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,10)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa,
                precip = aa_stdprecip)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            beta.pre = runif(1, -5, 5),
                            beta.g1 = c(NA, rnorm(3)), 
                            beta.g2 = c(NA, rnorm(3)), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c( "beta.g1","beta.mass", 
                 "beta.pre", "sigma2.phi", "beta.m", 
                 "beta.g2", "sigma2.p", "beta.bl1", 
                 "beta.pen1", "beta.bl", "beta.pen", 
                 "epsilon.phi", "epsilon.p", "phi", "p") 


# MCMC settings
ni <- 20000
nt <- 5
nb <- 8000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.add3 <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-add3.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.add3)#DIC=997.3
plot(aa.cjs.trt.mass.cov.add3)

###################################################################################################
#Best supported model structure based on DIC, and best supported model overall?
#12. Phi(t+g+mass+block+pen)P(.+g+block+pen): 
# Treatment additive effect
# Survival by mass, NO temperature or precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# NO grand means
# With immediate trap response 
# With corner constraint
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-add4.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-( beta.g1[group[i]] + beta.mass*mass[i] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(beta.g2[group[i]] + beta.m[m[i,t]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
      } #t
    } #i
    
    
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] <- 0                           # Corner constraint
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.01)I(-10,10)
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))             # Logit transformed survival grand mean/intercept
    
    beta.g1[1] <- 0                           # Corner constraint
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.01)I(-10, 10)      # Prior for mass slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,10)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            beta.g1 = c(NA, rnorm(3)), 
                            beta.g2 = c(NA, rnorm(3)), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c( "beta.g1","beta.mass", 
                 "sigma2.phi", "beta.m", 
                 "beta.g2", "sigma2.p", "beta.bl1", 
                 "beta.pen1", "beta.bl", "beta.pen", 
                 "epsilon.phi", "epsilon.p", "phi", "p") 


# MCMC settings
ni <- 20000
nt <- 5
nb <- 8000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.add4 <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-add4.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.add4)#DIC=996.72
plot(aa.cjs.trt.mass.cov.add4)

###################################################################################################
#Best supported model structure based on DIC, and best supported model overall?
#12. Phi(t+g+mass+block+pen)P(.+g+block+pen): 
# Treatment additive effect
# Survival by mass, NO temperature or precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# NO grand means
# With immediate trap response 
# With corner constraint
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-add5.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(beta.g1[group[i]] + beta.mass*mass[i] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(beta.g2[group[i]] + beta.m[m[i,t]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
      } #t
    } #i
    
    
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] ~ dnorm(0, 0.01)I(-10,10)                   
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.01)I(-10,10)
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))             # Logit transformed survival grand mean/intercept
    
    beta.g1[1] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.01)I(-10, 10)      # Prior for mass slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,10)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            beta.g1 = rnorm(4), 
                            beta.g2 = rnorm(4), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c( "beta.g1","beta.mass", 
                 "beta.g2", "beta.m",
                 "sigma2.phi", "sigma2.p", "beta.bl1", 
                 "beta.pen1", "beta.bl", "beta.pen", 
                 "epsilon.phi", "epsilon.p", "phi", "p") 


# MCMC settings
ni <- 20000
nt <- 5
nb <- 8000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.add5 <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-add5.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.add5)#DIC=996.72
plot(aa.cjs.trt.mass.cov.add5)

#Calculate phi distributions
phi.list<-as.data.frame(aa.cjs.trt.mass.cov.add5$mean$phi)
phi.l<-as.data.frame(aa.cjs.trt.mass.cov.add5$q2.5$phi)
phi.h<-as.data.frame(aa.cjs.trt.mass.cov.add5$q97.5$phi)
phi.listv<-as.matrix(aa.cjs.trt.mass.cov.add5$mean$phi)
phi.lv<-as.matrix(aa.cjs.trt.mass.cov.add5$q2.5$phi)
phi.hv<-as.matrix(aa.cjs.trt.mass.cov.add5$q97.5$phi)
phi.mean <- phi.list %>% summarise_all(mean, na.rm=TRUE) 
phi.med <- phi.list %>% summarise_all(median, na.rm=TRUE) 
phi.lower <- as.numeric(phi.l %>% summarise_all(mean, na.rm=TRUE))
phi.higher <- as.numeric(phi.h %>% summarise_all(mean, na.rm=TRUE))
mean(phi.listv, na.rm = TRUE) #mean survival = 0.87
median(phi.listv, na.rm = TRUE) #median survival= 0.95
sd(phi.listv, na.rm = TRUE)#0.20
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.53
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
means.phi<-c(x.g1.phi, x.g2.phi, x.g3.phi, x.g4.phi)#0.8700113 0.8553893 0.8569909 0.8669944
meds.phi<-c(med.g1.phi, med.g2.phi, med.g3.phi, med.g4.phi) #0.9507267 0.9405564 0.9442335 0.9517203
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.g3.phi<-sd(g3.phi)
sd.g4.phi<-sd(g4.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi, sd.g3.phi, sd.g4.phi)#0.1978787 0.2033943 0.2055896 0.2017135

#Figure of treatment-specific temporal survival
par(mai=c(2,2,1,1), mgp=c(5,2,0))
plot(x=(1:16),y= phi.g1.med, type="b", pch=1, col="salmon1",lty=3, cex=2.5, lwd=3, bty='l',
     ylim=c(0,1), ylab="Survival probability", xlab="Recapture occasion", cex.lab=2.5, cex.axis=2.5)
segments((1:16), g1.low, (1:16), g1.high, col="salmon1", lwd=2)
points(x=(1:16)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2, cex=2.5, lwd=2)
segments((1:16)+.1, g2.low, (1:16)+.1, g2.high, col="deepskyblue3", lwd=2)
points(x=(1:16)+.2,phi.g3.med, type="b", pch=0, col="midnightblue", lty=1, cex=2.5, lwd=2)
segments((1:16)+.2, g3.low, (1:16)+.2, g3.high, col="midnightblue", lwd=2)
points(x=(1:16)+.3,phi.g4.med, type="b", pch=5, col="orangered4", lty=4, cex=2.5, lwd=2)
segments((1:16)+.3, g4.low, (1:16)+.3, g4.high, col="orangered4", lwd=2)
legend(x = 10, y=.5, bty = 'n',
       #legend=c(as.expression(bquote(italic(.("Ambystoma annulatum")))),as.expression(bquote(italic(.("Ambystoma maculatum")))), as.expression(bquote(italic(.("Ambystoma texanum"))))),
       legend=c("L1J1", "L1J3", "L3J1", "L3J3"),
       lwd=c(3,2,2), pch=c(1,6,0,5), lty=c(3,2,1,4), cex=2.5,  col=c("salmon1", "deepskyblue3", "midnightblue", "orangered4"))
#dev.off()

#Calculate recapture distributions
p.list<-as.data.frame(aa.cjs.trt.mass.cov.add5$mean$p)
p.l<-as.data.frame(aa.cjs.trt.mass.cov.add5$q2.5$p)
p.h<-as.data.frame(aa.cjs.trt.mass.cov.add5$q97.5$p)
p.listv<-as.matrix(aa.cjs.trt.mass.cov.add5$mean$p)
p.lv<-as.matrix(aa.cjs.trt.mass.cov.add5$q2.5$p)
p.hv<-as.matrix(aa.cjs.trt.mass.cov.add5$q97.5$p)
p.mean <- p.list %>% summarise_all(mean, na.rm=TRUE) 
p.med <- p.list %>% summarise_all(median, na.rm=TRUE) 
p.lower <- as.numeric(p.l %>% summarise_all(mean, na.rm=TRUE))
p.higher <- as.numeric(p.h %>% summarise_all(mean, na.rm=TRUE))
mean(p.listv, na.rm = TRUE) #mean survival = 0.30
median(p.listv, na.rm = TRUE) #median survival= 0.27
sd(p.listv, na.rm = TRUE)#0.09
p.ci.low<-mean(p.lv, na.rm = TRUE)#0.19
p.ci.high<-mean(p.hv, na.rm = TRUE)#0.45

g1.p<-as.matrix(subset(p.list[1:36,]))
g2.p<-as.matrix(subset(p.list[37:72,]))
g3.p<-as.matrix(subset(p.list[73:108,]))
g4.p<-as.matrix(subset(p.list[109:164,]))
g1.p.dat<-as.data.frame(subset(p.list[1:36,]))
g2.p.dat<-as.data.frame(subset(p.list[37:72,]))
g3.p.dat<-as.data.frame(subset(p.list[73:108,]))
g4.p.dat<-as.data.frame(subset(p.list[109:164,]))
g1.pl<-as.data.frame(subset(p.l[1:36,])) #Spp.-specific lower CI
g2.pl<-as.data.frame(subset(p.l[37:72,]))
g3.pl<-as.data.frame(subset(p.l[73:108,]))
g4.pl<-as.data.frame(subset(p.l[109:164,]))
g1.ph<-as.data.frame(subset(p.h[1:36,])) #spp.-specific upper CI
g2.ph<-as.data.frame(subset(p.h[37:72,]))
g3.ph<-as.data.frame(subset(p.h[73:108,]))
g4.ph<-as.data.frame(subset(p.h[109:164,]))
p.g1 <- g1.p.dat %>% summarise_all(mean)
p.g2 <- g2.p.dat %>% summarise_all(mean)
p.g3 <- g3.p.dat %>% summarise_all(mean)
p.g4 <- g4.p.dat %>% summarise_all(mean)
p.g1.med <- g1.p.dat %>% summarise_all(median)
p.g2.med <- g2.p.dat %>% summarise_all(median)
p.g3.med <- g3.p.dat %>% summarise_all(median)
p.g4.med <- g4.p.dat %>% summarise_all(median)
g1.low<-as.numeric(g1.pl %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.ph %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.pl %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.ph %>% summarise_all(mean, na.rm=TRUE))
g3.low<-as.numeric(g3.pl %>% summarise_all(mean, na.rm=TRUE))
g3.high<-as.numeric(g3.ph %>% summarise_all(mean, na.rm=TRUE))
g4.low<-as.numeric(g4.pl %>% summarise_all(mean, na.rm=TRUE))
g4.high<-as.numeric(g4.ph %>% summarise_all(mean, na.rm=TRUE))
x.g1.p<-mean(g1.p)#Overall spp. means
x.g2.p<-mean(g2.p)
x.g3.p<-mean(g3.p)
x.g4.p<-mean(g4.p)
med.g1.p<-median(g1.p)#Overall treatment medians
med.g2.p<-median(g2.p)
med.g3.p<-median(g3.p)
med.g4.p<-median(g4.p)
means.p<-c(x.g1.p, x.g2.p, x.g3.p, x.g4.p)#0.2820788 0.2848755 0.2966071 0.3276752
meds.p<-c(med.g1.p, med.g2.p, med.g3.p, med.g4.p) #0.2540890 0.2540890 0.2790220 0.2879549
sd.g1.p<-sd(g1.p)
sd.g2.p<-sd(g2.p)
sd.g3.p<-sd(g3.p)
sd.g4.p<-sd(g4.p)
sd.p<-c(sd.g1.p, sd.g2.p, sd.g3.p, sd.g4.p)#0.2372395 0.2419790 0.2360986 0.2347370


#Group effect on recapture
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]), xlim=c(-3,3))#L1J1
lines(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2]), col=2)#L1J3
lines(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3]), col=3)#L3J1
lines(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2]))
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3]))
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4]))
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4]))
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4]))

#Calculate % of posterior of differences overlapping zero
(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4])) * 100

library(overlapping)
library(lattice)


#Group effect on survival
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]), xlim=c(-3,8))#L3J3
lines(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2]), col=2)#L3J1
lines(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3]), col=3)#L1J3
lines(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4]), col=4)#L1J1

#If difference of posteriors overlaps zero, no significant difference
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2]))
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3]))
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4]))
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4]))
plot(density(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3]))

#Calculate % of posterior of differences overlapping zero
(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4])) * 100

(sum(ifelse((aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2])<=0,1,0))/
    sum(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1]-aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2])) * 100

###################################################################################################
#Best supported model structure based on DIC
#12. Phi(t+g+mass+ctmax+block+pen)P(.+g+block+pen): 
# Treatment additive effect
# Survival by mass and days reaching CTmax, NO temperature or precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# NO grand means
# With immediate trap response 
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-ctmax.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-( beta.g1[group[i]] + beta.mass*mass[i] + beta.ctm*ctmax[t] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + beta.g2[group[i]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
      } #t
    } #i
    
    
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] <- 0                           # Corner constraint
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.01)I(-10,10)
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed survival grand mean/intercept
    
    beta.g1[1] <- 0                           # Corner constraint
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.01)I(-10, 10)     # Prior for mass slope parameter
    beta.ctm ~ dunif(-10, 10)     # Prior for proportion of days reaching CTmax slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,10)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass = stdmass_aa, ctmax = aa_stdpropMax, g = length(unique(group_aa)), group = group_aa, m=m_aa)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            beta.g1 = c(NA, rnorm(3)), 
                            beta.g2 = c(NA, rnorm(3)), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.ctm = runif(1, -5, 5),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c( "beta.g1","beta.mass", "beta.ctm", 
                 "sigma2.phi", "beta.m", 
                 "beta.g2", "sigma2.p", "beta.bl1", 
                 "beta.pen1", "beta.bl", "beta.pen", 
                 "epsilon.phi", "epsilon.p", "phi", "p") 


# MCMC settings
ni <- 20000
nt <- 5
nb <- 8000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.ctmax <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-ctmax.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.ctmax)#DIC=1006.887
plot(aa.cjs.trt.mass.cov.ctmax)

###################################################################################################
#Best supported model structure based on DIC
#13. Phi(t+g+mass+reachMax+block+pen)P(.+g+block+pen): 
# Treatment additive effect
# Survival by mass and factor indicating if any interval days reached CTmax, NO temperature or precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# NO grand means
# With immediate trap response 
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-ctmax1.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-( beta.g1[group[i]] + beta.mass*mass[i] + beta.ctm[reach[t]] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + beta.g2[group[i]] + beta.bl1[block[i]] + beta.pen1[pen[i]])))
      } #t
    } #i
    
    
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] <- 0                           # Corner constraint
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.01)I(-10,10)
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed survival grand mean/intercept
    
    beta.g1[1] <- 0                           # Corner constraint
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.01)I(-10, 10)     # Prior for mass slope parameter
    for (u in 1:2){
        beta.ctm[u] ~ dunif(0, 1)      # Prior for factor of reaching CTmax slope parameter
    }
    
    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,10)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass = stdmass_aa, reach = aa_reachMax, g = length(unique(group_aa)), group = group_aa, m=m_aa)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            beta.g1 = c(NA, rnorm(3)), 
                            beta.g2 = c(NA, rnorm(3)), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.ctm = runif(2, 0, 1),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c( "beta.g1","beta.mass", "beta.ctm", 
                 "sigma2.phi", "beta.m", 
                 "beta.g2", "sigma2.p", "beta.bl1", 
                 "beta.pen1", "beta.bl", "beta.pen", 
                 "epsilon.phi", "epsilon.p", "phi", "p") 


# MCMC settings
ni <- 20000
nt <- 5
nb <- 8000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.ctmax1 <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-ctmax1.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.ctmax1)#DIC=1003.89
plot(aa.cjs.trt.mass.cov.ctmax1)

###################################################################################################
#14. Phi(t+g+mass+temp+precip+block+pen)P(t+g+block+pen): 
# Fully time-dependent
# Treatment additive effect
# Survival by mass, temperature, and precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# NO grand means
# With immediate trap response 
###################################################################################################

# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-tt.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(beta.g1[group[i]] + beta.mass*mass[i] + beta.temp*temp[t] + beta.pre*precip[t] + beta.bl[block[i],t] + beta.pen[pen[i],t] + epsilon.phi[t]))))^int[t]
        p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + beta.g2[group[i]] + beta.temp1*temp[t] + beta.pre1*precip[t] + beta.bl1[block[i]] + beta.pen1[pen[i]] + epsilon.p[t])))
      } #t
    } #i
    
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    beta.g2[1] <- 0                           # Corner constraint
    beta.g2[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    beta.g2[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g2[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.temp1 ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter
    beta.pre1 ~ dnorm(0, 0.01)I(-10, 10)      # Prior for precip slope parameter
    
    for (b in 1:nblock){
      beta.bl1[b] ~ dnorm(0,tau.beta.bl1)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.bl1~dunif(0,5)
    tau.beta.bl1<-pow(sigma.beta.bl1,-2)
    sigma2.beta.bl1 <- pow(sigma.beta.bl1, 2)
    
    for (p in 1:npen){
      beta.pen1[p] ~ dnorm(0,tau.beta.pen1)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.pen1~dunif(0,5)
    tau.beta.pen1<-pow(sigma.beta.pen1,-2)
    sigma2.beta.pen1 <- pow(sigma.beta.pen1, 2)
    
    for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, tau.p)      # Prior for recapture residuals
    }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)           # Residual temporal variance
    
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed survival grand mean/intercept
    
    beta.g1[1] <- 0                           # Corner constraint
    beta.g1[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    beta.g1[3] ~ dnorm(0, 0.01)I(-10,10)
    beta.g1[4] ~ dnorm(0, 0.01)I(-10,10)
    
    beta.mass ~ dnorm(0, 0.01)I(-10, 10)     # Prior for mass slope parameter
    beta.temp ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter
    beta.pre ~ dnorm(0, 0.01)I(-10, 10)      # Prior for precip slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.bl[b,t] ~ dnorm(0,tau.beta.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.bl~dunif(0,5)
    tau.beta.bl<-pow(sigma.beta.bl,-2)
    sigma2.beta.bl <- pow(sigma.beta.bl, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.pen[p,t] ~ dnorm(0,tau.beta.pen)    #Prior for logit of mean survival with random effect of pen given block
      }
    }
    sigma.beta.pen~dunif(0,10)
    tau.beta.pen<-pow(sigma.beta.pen,-2)
    sigma2.beta.pen <- pow(sigma.beta.pen, 2)

    
    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa,
                temp = aa_stdtempc, precip = aa_stdprecip)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            beta.temp = runif(1, -5, 5), 
                            beta.pre = runif(1, -5, 5),
                            beta.temp1 = runif(1, -5, 5), 
                            beta.pre1 = runif(1, -5, 5),
                            beta.g1 = c(NA, rnorm(3)), 
                            beta.g2 = c(NA, rnorm(3)), 
                            beta.bl1 = runif(length(unique(block_aa)), 0, 1), 
                            beta.bl = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.mass = runif(1, -5, 5),
                            beta.pen = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.pen1 = runif(length(unique(pen_aa)), 0, 1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.beta.bl1= runif(1, 0, 2), 
                            sigma.beta.bl= runif(1, 0, 2), 
                            sigma.beta.pen1= runif(1, 0, 2), 
                            sigma.beta.pen= runif(1, 0, 2))}

# Parameters monitored
parameters <- c( "beta.g1","beta.mass", "beta.temp",
                 "beta.pre", "sigma2.phi", "beta.m", 
                 "beta.temp1","beta.pre1",
                 "beta.g2", "sigma2.p", "beta.bl1", 
                 "beta.pen1", "beta.bl", "beta.pen", 
                 "epsilon.phi", "epsilon.p", "phi", "p") 


# MCMC settings
ni <- 25000
nt <- 5
nb <- 10000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.tt <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-tt.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.tt)#DIC=1001.37
plot(aa.cjs.trt.mass.cov.tt)

###################################################################################################
#Best supported model structure based on DIC ****Does not converge
#15. Phi(t*g+mass+cov+block+pen)P(.+g+block+pen): 
# FULL MODEL
# Treatment*time effect
# Survival by mass, temperature, and precipitation
# Time-dependent survival and constant recapture (best model based on DIC)
# Grand means
# With immediate trap response 
###################################################################################################
  
# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-rand1.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(mean.phi + beta.e[group[i],t] + beta.b*mass[i] + beta.f*temp[t] + beta.g*precip[t] + beta.h[block[i],t] + beta.j[pen[i],t] + epsilon.phi[t])))) ^int[t]
        p[i,t] <- 1/(1+exp(-(mean.p + beta.m[m[i,t]] + beta.a[group[i]] + beta.c[block[i]] + beta.d[pen[i]])))
      } #t
    } #i
    
    ## For recapture
  
    mean.p~dnorm(0,0.001)
    mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    for (u in 1:g){
      beta.a[u] ~ dunif(0, 1)          # Priors for group-specific recapture
    }
    
    for (b in 1:nblock){
      beta.c[b] ~ dnorm(0,tau.beta.c)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
    }
    sigma.beta.c~dunif(0,5)
    tau.beta.c<-pow(sigma.beta.c,-2)
    sigma2.beta.c <- pow(sigma.beta.c, 2)
    
    for (p in 1:npen){
      beta.d[p] ~ dnorm(0,tau.beta.d)    #Prior for logit of mean recapture with random effect of pen given block
    }
    sigma.beta.d~dunif(0,5)
    tau.beta.d<-pow(sigma.beta.d,-2)
    sigma2.beta.d <- pow(sigma.beta.d, 2)
    
    
    ##For overall survival
    
    mean.phi~dnorm(0,0.001)
    mu.phi<-1/(1+exp(-mean.phi))              # Logit transformed survival grand mean/intercept
    
    beta.b ~ dnorm(0, 0.001)I(-10, 10)         # Prior for mass slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)          # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                      # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)             # Residual temporal variance
    
    for (u in 1:g){
      for (t in 1:(n.occasions-1)){
        beta.e[u,t] ~ dunif(0, 1)          # Prior for time and group-spec. survival
      } #t
    } #g
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.h[b,t] ~ dnorm(0,tau.beta.h)      #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      }
    }
    sigma.beta.h~dunif(0,5)
    tau.beta.h<-pow(sigma.beta.h,-2)
    sigma2.beta.h <- pow(sigma.beta.h, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.j[p,t] ~ dnorm(0,tau.beta.j)    #Prior for logit of mean survival with random effect of pen given block
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa,
                temp = aa_stdtempc, precip = aa_stdprecip)

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            mean.phi = runif(1, 0, 1),
                            mean.p = runif(1, 0, 1), 
                            beta.f = runif(1, -5, 5), 
                            beta.g = runif(1, -5, 5),
                            beta.e = array(runif(68, 0, 1),dim=c(4,16)), 
                            beta.a = runif(length(unique(group_aa)), 0, 1), 
                            beta.c = runif(length(unique(block_aa)), 0, 1), 
                            beta.h = array(runif(68, 0, 1),dim=c(4,16)), 
                            sigma.beta.c= runif(1, 0, 2), 
                            sigma.beta.h= runif(1, 0, 2), 
                            beta.b = runif(1, -5, 5),
                            beta.j = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.d = runif(length(unique(pen_aa)), 0, 1), 
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
ni <- 15000
nt <- 5
nb <- 7000
nc <- 3

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.rand1 <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-rand1.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.rand1)#DIC=977.29

#treatment contrasts
trtvals<-combn(1:4,m=2,simplify=F)
trt.cont1<-matrix(NA,dim(aa.cjs.trt.mass.cov.rand1$sims.list$beta.e)[1],length(trtvals))
for(i in 1:length(trtvals)){
  trt.cont[,i]<-(aa.cjs.trt.mass.cov.rand1$sims.list$beta.e[,trtvals[[i]][[1]]]-aa.cjs.trt.mass.cov.rand1$sims.list$beta.e[,trtvals[[i]][[2]]])
}
###################################################################################################
#Fully time-dependent model ****Does not converge
#16. Phi(t*g+mass+cov+block+pen)P(t*g+cov+block+pen): 
# FULL MODEL
# Treatment effect
# Survival by mass
# Temperature and precipitation covariates
# Grand means
# With immediate trap response 
###################################################################################################
# Specify model in BUGS language
sink("aa-cjs-trt-mass-cov-rand-tt.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(mean.phi + beta.trt[group[i]] + beta.a[t] + beta.b*mass[i] + beta.f*temp[t] + beta.g*precip[t] + beta.c[block[i]] + beta.d[pen[i]])))) ^int[t]
        p[i,t] <- 1/(1+exp(-(mean.p + beta.m[m[i,t]] + beta.e[t] + beta.k*temp[t] + beta.l*precip[t] + beta.h[block[i],t] + beta.j[pen[i],t])))
      } #t
    } #i
    
    ## For survival
    
    mean.phi~dnorm(0,0.001)
    mu.phi<-1/(1+exp(-mean.phi))               # Logit transformed survival grand mean/intercept
    
    #for (t in 1:(n.occasions-1)){
    #  epsilon.phi[t] ~ dnorm(0, tau.phi)       # Prior for survival temporal residuals
    #}
    #sigma.phi ~ dunif(0,5)                     # Prior on standard deviation
    #tau.phi <- pow(sigma.phi, -2)
    #sigma2.phi <- pow(sigma.phi, 2)            # Residual temporal variance
    
    beta.trt[1] <- 0                            # Corner constraint
    for (u in 2:g){
      beta.trt[u] ~ dnorm(0, 0.01)I(-10,10)     # Prior for difference in mean treatment effects on survival from treatment 1
    } #u
    
    for (t in 1:(n.occasions-1)){
      beta.a[t] ~ dnorm(0, 0.01)I(-10,10)               # Prior for time effect on survival
    } #t
    
    
    #For covariates
    beta.b ~ dnorm(0, 0.001)I(-10, 10)         # Prior for survival mass slope parameter
    beta.f ~ dnorm(0, 0.001)I(-10, 10)         # Prior for survival temp slope parameter
    beta.g ~ dnorm(0, 0.001)I(-10, 10)         # Prior for survival precip slope parameter
        
    for (b in 1:nblock){
      beta.c[b] ~ dnorm(0,tau.beta.c)            #Prior for logit of mean survival with random effect of block (random effect of block on phi)
    }
    sigma.beta.c~dunif(0,5)
    tau.beta.c<-pow(sigma.beta.c,-2)
    sigma2.beta.c <- pow(sigma.beta.c, 2)
    
    for (p in 1:npen){
      beta.d[p] ~ dnorm(0,tau.beta.d)            #Prior for logit of mean survival with random effect of pen given block
    }
    sigma.beta.d~dunif(0,5)
    tau.beta.d<-pow(sigma.beta.d,-2)
    sigma2.beta.d <- pow(sigma.beta.d, 2)
    

    ##For recapture
    
    mean.p~dnorm(0,0.001)
    mu.p<-1/(1+exp(-mean.p))                # Logit transformed Recapture grand mean/intercept
    
    #for (t in 1:(n.occasions-1)){
    #  epsilon.p[t] ~ dnorm(0, tau.p)          # Prior for recapture residuals
    #}
    #sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    #tau.p <- pow(sigma.p, -2)
    #sigma2.p <- pow(sigma.p, 2)             # Residual temporal variance
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)                 # Prior for effect of trapping history
    }
    
    for (t in 1:(n.occasions-1)){
       beta.e[t] ~ dnorm(0, 0.01)I(-10,10)    # Prior for time-spec. recapture
    } #t
    
    for (b in 1:nblock){
      for (t in 1:(n.occasions-1)){
        beta.h[b,t] ~ dnorm(0,tau.beta.h)       #Prior for logit of mean recapture with random effect of block (random effect of block on p)
      }
    }
    sigma.beta.h~dunif(0,5)
    tau.beta.h<-pow(sigma.beta.h,-2)
    sigma2.beta.h <- pow(sigma.beta.h, 2)
    
    for (p in 1:npen){
      for (t in 1:(n.occasions-1)){
        beta.j[p,t] ~ dnorm(0,tau.beta.j)       #Prior for logit of mean recapture with random effect of pen given block
      }
    }
    sigma.beta.j~dunif(0,5)
    tau.beta.j<-pow(sigma.beta.j,-2)
    sigma2.beta.j <- pow(sigma.beta.j, 2)
    
    #For covariates
    beta.k ~ dnorm(0, 0.001)I(-10, 10)         # Prior for recapture temp slope parameter
    beta.l ~ dnorm(0, 0.001)I(-10, 10)         # Prior for recapture precip slope parameter

    
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
aa.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa,
                temp = aa_stdtempc, precip = aa_stdprecip)

# Initial values
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            #sigma.phi = runif(1, 0, 2), 
                            #sigma.p = runif(1, 0, 2), 
                            mean.phi = runif(1, 0, 1),
                            mean.p = runif(1, 0, 1), 
                            beta.f = runif(1, -5, 5), 
                            beta.g = runif(1, -5, 5),
                            beta.k = runif(1, -5, 5), 
                            beta.l = runif(1, -5, 5),
                            beta.e = rnorm(16, 0, 1), 
                            beta.a = rnorm(16, 0, 1), 
                            beta.trt = c(NA, rnorm(3,0,1)),
                            beta.c = runif(length(unique(block_aa)), 0, 1), 
                            beta.h = array(runif(68, 0, 1),dim=c(4,16)), 
                            sigma.beta.c= runif(1, 0, 2), 
                            sigma.beta.h= runif(1, 0, 2), 
                            beta.b = runif(1, -5, 5),
                            beta.j = array(runif(384, 0, 1),dim=c(24,16)), 
                            beta.d = runif(length(unique(pen_aa)), 0, 1), 
                            sigma.beta.d= runif(1, 0, 2), 
                            sigma.beta.j= runif(1, 0, 2), 
                            beta.m = runif (2, 0, 1))}  

# Parameters monitored
parameters <- c("mu.phi", "mean.phi", "beta.trt", "beta.a", "beta.b", "beta.f","beta.g", 
                "mu.p", "mean.p", "beta.m", "beta.e", "beta.k","beta.l", #"sigma2.phi", "sigma2.p", "epsilon.phi", "epsilon.p",
                "sigma2.beta.c", "sigma2.beta.d", 
                "sigma2.beta.h", "sigma2.beta.j", "beta.c", "beta.d", "beta.h", 
                "beta.j", "phi", "p") 

# MCMC settings
ni <- 15000
nt <- 5
nb <- 7000
nc <- 4

# Call JAGS from R (JRT 55 min)
aa.cjs.trt.mass.cov.rand.tt <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-rand-tt.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(aa.cjs.trt.mass.cov.rand.tt)#DIC=1043

#treatment contrasts
trtvals<-combn(1:4,m=2,simplify=F)
trt.cont<-matrix(NA,dim(aa.cjs.trt.mass.cov.rand.tt$sims.list$beta.a)[1],length(trtvals))
for(i in 1:length(trtvals)){
  trt.cont[,i]<-(aa.cjs.trt.mass.cov.rand.tt$sims.list$beta.a[,trtvals[[i]][[1]]]-aa.cjs.trt.mass.cov.rand.tt$sims.list$beta.a[,trtvals[[i]][[2]]])
}
