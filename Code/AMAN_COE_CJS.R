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
## Edited by: Jake Burkhart and Tom Anderson
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
    
    # # Create group variable
    # group_aa <- as.factor(aa_ch.pa$Treatment)
    # 
    # group_aa.L <- character(length = dim(aa_ch.pa)[1])      ## breeding phenology group
    # group_aa.J <- character(length = dim(aa_ch.pa)[1])      ## emigration phenology group
    # 
    # for(i in 1:dim(aa_ch.pa)[1]){
    #   group_aa.L[i] <- unlist(strsplit(aa_ch.pa$Treatment[i], "(?<=.{2})", perl=T))[[1]]
    #   group_aa.J[i] <- unlist(strsplit(aa_ch.pa$Treatment[i], "(?<=.{2})", perl=T))[[2]]
    # }
    # 
    # group_aa.L <- as.factor(group_aa.L)
    # group_aa.J <- as.factor(group_aa.J)
    
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
    
    #define groups, blocks and pens
    group_aa<-as.numeric(as.factor(aa_ch.pa$Treatment))
    block_aa<-as.numeric(aa_ch.pa$Block)
    pen_aa<-as.numeric(as.factor(paste(aa_ch.pa$Block,aa_ch.pa$Pen,sep="")))
    
    
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
    aa_jags.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], m=m)
    
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
    
###################################################################################################
#ARIANNE'S Full Model
#5. Phi(.*g+mass+block+pen)P(t*g+cov+block+pen): 
# FULL MODEL
# Treatment effect
# Survival by mass
# Temperature and precipitation covariates
# Time-dependent recapture and survival (best model based on DIC from Messerman et al., 2020)
# Grand means
# With immediate trap response 
###################################################################################################
    
    group_aa<-as.numeric(as.factor(aa_ch.pa$Treatment))
    block_aa<-as.numeric(aa_ch.pa$Block)
    pen_aa<-as.numeric(as.factor(paste(aa_ch.pa$Block,aa_ch.pa$Pen,sep="")))
    
    
    aa_abiotic <- readRDS("~/GitHub/JuvenileEmigrationPhenology/aa_abiotic.rds")
    str(aa_abiotic)
    cor.test(as.numeric(aa_abiotic$Tmin), as.numeric(aa_abiotic$Prcp)) #Not autocorrelated

    aa_abiotic$Tmin <- as.numeric(aa_abiotic$Tmin)
    aa_stdtempc<-rep(NA,length(aa_abiotic$Tmin))
    aa_abiotic$Prcp <- as.numeric(aa_abiotic$Prcp)
    aa_stdprecip<-rep(NA,length(aa_abiotic$Prcp))
    #aa_abiotic$temp.sd <- as.numeric(aa_abiotic$temp.sd)
    #aa_stdtempsd<-rep(NA,length(aa_abiotic$temp.sd))

    #Scale temp. and Prcp. covariates
    for (i in 1:length(aa_abiotic$Tmin)) {
      aa_stdtempc[i] <- (aa_abiotic$Tmin[i]-mean(aa_abiotic$Tmin[]))/sd(aa_abiotic$Tmin[])
      #stdtempsd[i] <- (aa_abiotic$temp.sd[i]-mean(aa_abiotic$temp.sd[]))/sd(aa_abiotic$temp.sd[])
      aa_stdprecip[i] <- (aa_abiotic$Prcp[i]-mean(aa_abiotic$Prcp[]))/sd(aa_abiotic$Prcp[])
    }

    #amb$mass <- as.numeric(aa_$mass)
    aa_ch.pa$Meta.Mass[is.na(aa_ch.pa$Meta.Mass)]<-mean(aa_ch.pa$Meta.Mass,na.rm=T)
    stdmass_aa<-rep(NA,length(aa_ch.pa$Meta.Mass))
    
    #Scale mass covariate
    for (i in 1:length(aa_ch.pa$Meta.Mass)) {
      stdmass_aa[i] <- (aa_ch.pa$Meta.Mass[i]-mean(aa_ch.pa$Meta.Mass[]))/sd(aa_ch.pa$Meta.Mass[])
    }
    
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
                             #alpha = runif(dim(aa_CH)[2]-1, 0, 1),
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
    parameters <- c("mu.phi", "mean.phi", "beta.b", "mu.p",#"alpha", 
                    "mean.p", "beta.m", "sigma2.phi", "sigma2.p", 
                    "beta.f","beta.g", "beta.a", "sigma2.beta.c", 
                    "sigma2.beta.d", "sigma2.beta.h", "sigma2.beta.j", 
                    "beta.c", "beta.d", "beta.e", "beta.h", "beta.j", 
                    "epsilon.phi", "epsilon.p", "phi", "p") 
    
    # MCMC settings
    ni <- 15000
    nt <- 5
    nb <- 7000
    nc <- 4
    
    # Call JAGS from R (JRT 55 min)
    aa.cjs.trt.mass.cov.rand <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-rand.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
    print(aa.cjs.trt.mass.cov.rand)
    
    #treatment contrasts
    trtvals<-combn(1:4,m=2,simplify=F)
    trt.cont<-matrix(NA,dim(aa.cjs.trt.mass.cov.rand$sims.list$beta.a)[1],length(trtvals))
    for(i in 1:length(trtvals)){
      trt.cont[,i]<-(aa.cjs.trt.mass.cov.rand$sims.list$beta.a[,trtvals[[i]][[1]]]-aa.cjs.trt.mass.cov.rand$sims.list$beta.a[,trtvals[[i]][[2]]])
    }

    
###################################################################################################
#Best supported model structure based on DIC
#6. Phi(t*g+mass+cov+block+pen)P(.*g+block+pen): 
# FULL MODEL
# Treatment effect
# Survival by mass
# Temperature and precipitation covariates
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
                                #alpha = runif(dim(aa_CH)[2]-1, 0, 1),
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
    parameters <- c("mu.phi", "mean.phi", "beta.b", "mu.p",#"alpha", 
                    "mean.p", "beta.m", "sigma2.phi", "sigma2.p", 
                    "beta.f","beta.g", "beta.a", "sigma2.beta.c", 
                    "sigma2.beta.d", "sigma2.beta.h", "sigma2.beta.j", 
                    "beta.c", "beta.d", "beta.e", "beta.h", "beta.j", 
                    "epsilon.phi", "epsilon.p", "phi", "p") 
    
    # MCMC settings
    ni <- 15000
    nt <- 5
    nb <- 7000
    nc <- 4
    
    # Call JAGS from R (JRT 55 min)
    aa.cjs.trt.mass.cov.rand1 <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-rand1.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
    print(aa.cjs.trt.mass.cov.rand1)
    
    #treatment contrasts
    trtvals<-combn(1:4,m=2,simplify=F)
    trt.cont1<-matrix(NA,dim(aa.cjs.trt.mass.cov.rand1$sims.list$beta.e)[1],length(trtvals))
    for(i in 1:length(trtvals)){
      trt.cont[,i]<-(aa.cjs.trt.mass.cov.rand1$sims.list$beta.e[,trtvals[[i]][[1]]]-aa.cjs.trt.mass.cov.rand1$sims.list$beta.e[,trtvals[[i]][[2]]])
    }
###################################################################################################
#Fully time-dependent model
#7. Phi(t*g+mass+cov+block+pen)P(t*g+cov+block+pen): 
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
    phi[i,t] <- (1/(1+exp(-(mean.phi + beta.a[group[i],t] + beta.b*mass[i] + beta.f*temp[t] + beta.g*precip[t] + beta.c[block[i]] + beta.d[pen[i]] + epsilon.phi[t])))) ^int[t]
    p[i,t] <- 1/(1+exp(-(mean.p + beta.m[m[i,t]] + beta.e[group[i],t] + beta.k*temp[t] + beta.l*precip[t] + beta.h[block[i],t] + beta.j[pen[i],t] + epsilon.p[t])))
    } #t
    } #i
    
    ## For survival
    
    mean.phi~dnorm(0,0.001)
    mu.phi<-1/(1+exp(-mean.phi))               # Logit transformed survival grand mean/intercept
    
    for (t in 1:(n.occasions-1)){
    epsilon.phi[t] ~ dnorm(0, tau.phi)         # Prior for survival temporal residuals
    }
    sigma.phi ~ dunif(0,5)                     # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)            # Residual temporal variance
    
    for (u in 1:g){
    beta.a[u] ~ dunif(0, 1)                    # Priors for treatment-specific survival
    }
    
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
    

    ##For overall recapture
    
    mean.p~dnorm(0,0.001)
    mu.p<-1/(1+exp(-mean.p))                # Logit transformed Recapture grand mean/intercept
    
    for (t in 1:(n.occasions-1)){
    epsilon.p[t] ~ dnorm(0, tau.p)          # Prior for recapture residuals
    }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)             # Residual temporal variance
    
    for (u in 1:2){
    beta.m[u] ~ dunif(0, 1)                 # Prior for effect of trapping history
    }
    
    for (u in 1:g){
    for (t in 1:(n.occasions-1)){
    beta.e[u,t] ~ dunif(0, 1)               # Prior for time and group-spec. recapture
    } #t
    } #g
    
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
    
    # Initial values (probably need to adjust thse to match dimensions of certain parameters)
    aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                                sigma.phi = runif(1, 0, 2), 
                                sigma.p = runif(1, 0, 2), 
                                mean.phi = runif(1, 0, 1),
                                mean.p = runif(1, 0, 1), 
                                beta.f = runif(1, -5, 5), 
                                beta.g = runif(1, -5, 5),
                                beta.k = runif(1, -5, 5), 
                                beta.l = runif(1, -5, 5),
                                beta.e = array(runif(68, 0, 1),dim=c(4,16)), 
                                beta.a = array(runif(68, 0, 1),dim=c(4,16)),
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
    parameters <- c("mu.phi", "mean.phi", "beta.b", "beta.f","beta.g", 
                    "mu.p", "mean.p", "beta.m", "sigma2.phi", "sigma2.p", 
                    "beta.k","beta.l", "beta.a", "sigma2.beta.c", 
                    "sigma2.beta.d", "sigma2.beta.h", "sigma2.beta.j", 
                    "beta.c", "beta.d", "beta.e", "beta.h", "beta.j", 
                    "epsilon.phi", "epsilon.p", "phi", "p") 
    
    # MCMC settings
    ni <- 15000
    nt <- 5
    nb <- 7000
    nc <- 4
    
    # Call JAGS from R (JRT 55 min)
    aa.cjs.trt.mass.cov.rand.tt <- jags(aa.data, parallel=TRUE, aa.inits, parameters, "aa-cjs-trt-mass-cov-rand-tt.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
    print(aa.cjs.trt.mass.cov.rand)
    
    #treatment contrasts
    trtvals<-combn(1:4,m=2,simplify=F)
    trt.cont<-matrix(NA,dim(aa.cjs.trt.mass.cov.rand.tt$sims.list$beta.a)[1],length(trtvals))
    for(i in 1:length(trtvals)){
      trt.cont[,i]<-(aa.cjs.trt.mass.cov.rand.tt$sims.list$beta.a[,trtvals[[i]][[1]]]-aa.cjs.trt.mass.cov.rand.tt$sims.list$beta.a[,trtvals[[i]][[2]]])
    }
    