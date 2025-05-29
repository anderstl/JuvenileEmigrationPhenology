#questions: 
#include known state? 
#how to include size/mass/growth? 
#keep block/pen effects? 
#model convergence
#predictors on both phi and p (e.g. treatment effects)
#model fit? 
#re-do in NIMBLE?
#time-varying vs not

#Structure: time-varying phi and p (with time treated as random effect), with block and pen effects

#Steps to limit model complexity
#1: test for precip and temp effects on p; keep only if significant
#2: test for mass, precip and temp effects on phi; keep only if significant
#3: add in treatment effects on both phi and p. 

#run data generating code
source('Code/CJS_data_generation.R')

# Bundle A. annulatum data
aa.data <- list(y = aa_CH, int=interval_aa$int, 
                f = f_aa, nind = dim(aa_CH)[1], 
                n.occasions = dim(aa_CH)[2], 
                z = known.state.cjs(aa_CH), 
                nblock = length(unique(block_aa)), 
                block = as.numeric(block_aa), 
                npen = length(unique(pen_aa)), 
                pen = as.numeric(pen_aa),
                temp = aa_stdtempc, precip = aa_stdprecip,
                mass=stdmass_aa, 
                daysheld=stddaysheld_aa, 
                g = length(unique(group2_aa)), 
                group=group2_aa, 
                m=m_aa)

# Bundle A. opacum data
ao.data <- list(y = ao_CH, int=interval_ao$int, 
                f = f_ao, nind = dim(ao_CH)[1], 
                n.occasions = dim(ao_CH)[2], 
                z = known.state.cjs(ao_CH), 
                nblock = length(unique(block_ao)), 
                block = as.numeric(block_ao), 
                npen = length(unique(pen_ao)), 
                pen = as.numeric(pen_ao),
                temp = ao_stdtempc, precip = ao_stdprecip,
                mass=stdmass_ao, 
                daysheld=stddaysheld_ao, 
                g = length(unique(group2_ao)), 
                group=group2_ao, 
                m=m_ao)

# MCMC settings
ni <- 50000
nt <- 10
nb <- 5000
nc <- 3

library(jagsUI)
#covariate effects on p

# Specify model in BUGS language
sink("coveffsp.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(phi.bl[block[i]] + phi.pen[pen[i]] + epsilon.phi[t]))))^int[t] #phi.group[group[i]]  + 
        p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + p.temp*temp[t] + p.pcp*precip[t] + p.bl[block[i]] + p.pen[pen[i]] + epsilon.p[t]))) #p.group[group[i]] + 
      } #t
    } #i
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    p.temp ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter
    p.pcp ~ dnorm(0, 0.01)I(-10, 10)      # Prior for precip slope parameter
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    for (b in 1:nblock){
        #for (t in 1:(n.occasions-1)){
            p.bl[b] ~ dnorm(0,tau.p.bl)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
        #}
    }
    
    sigma.p.bl~dunif(0,5)
    tau.p.bl<-pow(sigma.p.bl,-2)
    sigma2.p.bl <- pow(sigma.p.bl, 2)
    
    for (p in 1:npen){
          #for (t in 1:(n.occasions-1)){
            p.pen[p] ~ dnorm(0,tau.p.pen)    #Prior for logit of mean recapture with random effect of pen given block
          #}
    }
    sigma.p.pen~dunif(0,5)
    tau.p.pen<-pow(sigma.p.pen,-2)
    sigma2.p.pen <- pow(sigma.p.pen, 2)
    
     for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
     }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)           # Residual temporal variance
    
    ##For survival
    
    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
        phi.bl[b] ~ dnorm(0,tau.phi.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      #}
    }
    sigma.phi.bl~dunif(0,5)
    tau.phi.bl<-pow(sigma.phi.bl,-2)
    sigma2.phi.bl <- pow(sigma.phi.bl, 2)
    
    for (p in 1:npen){
      #for (t in 1:(n.occasions-1)){
        phi.pen[p] ~ dnorm(0,tau.phi.pen)    #Prior for logit of mean survival with random effect of pen given block
      #}
    }
    sigma.phi.pen~dunif(0,10)
    tau.phi.pen<-pow(sigma.phi.pen,-2)
    sigma2.phi.pen <- pow(sigma.phi.pen, 2)

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

# Initial values for A. annulatum 
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            phi.bl = runif(length(unique(block_aa)), 0, 1), 
                            p.bl = runif(length(unique(block_aa)), 0, 1), 
                            p.temp = runif(1, -5,5),
                            p.pcp = runif(1, -5,5),
                            phi.pen = runif(length(unique(pen_aa)), 0, 1), 
                            p.pen = runif(length(unique(pen_aa)), 0,1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.p.bl= runif(1, 0, 2), 
                            sigma.phi.bl= runif(1, 0, 2), 
                            sigma.p.pen= runif(1, 0, 2), 
                            sigma.phi.pen= runif(1, 0, 2))}

ao.inits <- function(){list(z = cjs.init.z(ao_CH,f_ao), 
                              phi.bl = runif(length(unique(block_ao)), 0, 1), 
                              p.bl = runif(length(unique(block_ao)), 0, 1), 
                              p.temp = runif(1, -5,5),
                              p.pcp = runif(1, -5,5),
                              phi.pen = runif(length(unique(pen_ao)), 0, 1), 
                              p.pen = runif(length(unique(pen_ao)), 0,1), 
                              beta.m = runif (2, 0, 1),
                              sigma.phi = runif(1, 0, 2), 
                              sigma.p = runif(1, 0, 2), 
                              sigma.p.bl= runif(1, 0, 2), 
                              sigma.phi.bl= runif(1, 0, 2), 
                              sigma.p.pen= runif(1, 0, 2), 
                              sigma.phi.pen= runif(1, 0, 2))}
  
# Parameters monitored
parameters.p <- c( "p.pcp","p.temp","beta.m",
                   "p.bl","p.pen","phi.bl","phi.pen") 

# Call JAGS from R (JRT 36 min)
aa_covp_fit <- jags(aa.data, parallel=TRUE, 
                                 aa.inits, 
                                 parameters.p, 
                                 "coveffsp.jags", 
                                 n.chains = nc, 
                                 n.thin = nt, 
                                 n.iter = ni, 
                                 n.burnin = nb)
print(aa_covp_fit)

ao_covp_fit <- jags(ao.data, parallel=TRUE, 
                   ao.inits, 
                   parameters.p, 
                   "coveffsp.jags", 
                   n.chains = nc, 
                   n.thin = nt, 
                   n.iter = ni, 
                   n.burnin = nb)
print(ao_covp_fit)

#covariate effects on phi
sink("coveffsphi.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(phi.mass*mass[i]+ phi.temp*temp[t] + phi.pcp*precip[t] + phi.days*daysheld[t] + phi.bl[block[i]] + phi.pen[pen[i]] + epsilon.phi[t]))))^int[t] #phi.group[group[i]]  + 
        p[i,t] <- 1/(1+exp(-(beta.m[m[i,t]] + p.bl[block[i]] + p.pen[pen[i]] + epsilon.p[t]))) #p.group[group[i]] + 
      } #t
    } #i
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    p.temp ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter
   
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    for (b in 1:nblock){
        #for (t in 1:(n.occasions-1)){
            p.bl[b] ~ dnorm(0,tau.p.bl)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
        #}
    }
    
    sigma.p.bl~dunif(0,5)
    tau.p.bl<-pow(sigma.p.bl,-2)
    sigma2.p.bl <- pow(sigma.p.bl, 2)
    
    for (p in 1:npen){
          #for (t in 1:(n.occasions-1)){
            p.pen[p] ~ dnorm(0,tau.p.pen)    #Prior for logit of mean recapture with random effect of pen given block
          #}
    }
    sigma.p.pen~dunif(0,5)
    tau.p.pen<-pow(sigma.p.pen,-2)
    sigma2.p.pen <- pow(sigma.p.pen, 2)
    
     for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
     }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)           # Residual temporal variance
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))             # Logit transformed survival grand mean/intercept
  
    phi.temp ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter
    phi.pcp ~ dnorm(0, 0.01)I(-10, 10)      # Prior for precip slope parameter
    phi.mass ~dnorm(0,0.01)I(-10, 10)
    phi.days ~dnorm(0,0.01)I(-10, 10)

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
        phi.bl[b] ~ dnorm(0,tau.phi.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      #}
    }
    sigma.phi.bl~dunif(0,5)
    tau.phi.bl<-pow(sigma.phi.bl,-2)
    sigma2.phi.bl <- pow(sigma.phi.bl, 2)
    
    for (p in 1:npen){
      #for (t in 1:(n.occasions-1)){
        phi.pen[p] ~ dnorm(0,tau.phi.pen)    #Prior for logit of mean survival with random effect of pen given block
      #}
    }
    sigma.phi.pen~dunif(0,10)
    tau.phi.pen<-pow(sigma.phi.pen,-2)
    sigma2.phi.pen <- pow(sigma.phi.pen, 2)

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

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            phi.bl = runif(length(unique(block_aa)), 0, 1), 
                            p.bl = runif(length(unique(block_aa)), 0, 1), 
                            phi.mass = runif(1, -5, 5),
                            phi.temp = runif(1, -5,5),
                            phi.days = runif(1, -5,5),
                            phi.pcp = runif(1, -5,5),
                            phi.pen = runif(length(unique(pen_aa)), 0, 1), 
                            p.pen = runif(length(unique(pen_aa)), 0,1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.p.bl= runif(1, 0, 2), 
                            sigma.phi.bl= runif(1, 0, 2), 
                            sigma.p.pen= runif(1, 0, 2), 
                            sigma.phi.pen= runif(1, 0, 2))}

ao.inits <- function(){list(z = cjs.init.z(ao_CH,f_ao), 
                            phi.bl = runif(length(unique(block_ao)), 0, 1), 
                            p.bl = runif(length(unique(block_ao)), 0, 1), 
                            phi.mass = runif(1, -5, 5),
                            phi.temp = runif(1, -5,5),
                            p.temp = runif(1, -5,5),
                            phi.days = runif(1, -5,5),
                            phi.pcp = runif(1, -5,5),
                            phi.pen = runif(length(unique(pen_ao)), 0, 1), 
                            p.pen = runif(length(unique(pen_ao)), 0,1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.p.bl= runif(1, 0, 2), 
                            sigma.phi.bl= runif(1, 0, 2), 
                            sigma.p.pen= runif(1, 0, 2), 
                            sigma.phi.pen= runif(1, 0, 2))}

# Parameters monitored
parameters.phi <- c( "phi.pcp","phi.temp","phi.mass","phi.days","p.temp","beta.m",
                   "p.bl","p.pen","phi.bl","phi.pen") 

aa_covphi_fit <- jags(aa.data, parallel=TRUE, 
                    aa.inits, 
                    parameters.phi, 
                    "coveffsphi.jags", 
                    n.chains = nc, 
                    n.thin = nt, 
                    n.iter = ni, 
                    n.burnin = nb)
print(aa_covphi_fit)

ao_covphi_fit <- jags(ao.data, parallel=TRUE, 
                    ao.inits, 
                    parameters.phi, 
                    "coveffsphi.jags", 
                    n.chains = nc, 
                    n.thin = nt, 
                    n.iter = ni, 
                    n.burnin = nb)
print(ao_covphi_fit)

#final models with group effects
sink("aman_final.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(phi.group[group[i]] + phi.mass*mass[i]+ phi.temp*temp[t] + phi.bl[block[i]] + phi.pen[pen[i]] + epsilon.phi[t]))))^int[t]  
        p[i,t] <- 1/(1+exp(-(p.group[group[i]] + beta.m[m[i,t]] + p.bl[block[i]] + p.pen[pen[i]] + epsilon.p[t]))) 
      } #t
    } #i
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept

    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    #p.group[1] ~ dnorm(0, 0.01)I(-10,10)                   
    #p.group[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    #p.group[3] ~ dnorm(0, 0.01)I(-10,10)
    #p.group[4] ~ dnorm(0, 0.01)I(-10,10)
    for(i in 1:g){
      p.group[i]~dnorm(0, 0.01)I(-10,10)
    }
    for (b in 1:nblock){
        #for (t in 1:(n.occasions-1)){
            p.bl[b] ~ dnorm(0,tau.p.bl)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
        #}
    }
    
    sigma.p.bl~dunif(0,5)
    tau.p.bl<-pow(sigma.p.bl,-2)
    sigma2.p.bl <- pow(sigma.p.bl, 2)
    
    for (p in 1:npen){
          #for (t in 1:(n.occasions-1)){
            p.pen[p] ~ dnorm(0,tau.p.pen)    #Prior for logit of mean recapture with random effect of pen given block
          #}
    }
    sigma.p.pen~dunif(0,5)
    tau.p.pen<-pow(sigma.p.pen,-2)
    sigma2.p.pen <- pow(sigma.p.pen, 2)
    
     for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
     }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)           # Residual temporal variance
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))             # Logit transformed survival grand mean/intercept
    
    #phi.group[1] ~ dnorm(0, 0.01)I(-10,10)
    #phi.group[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    #phi.group[3] ~ dnorm(0, 0.01)I(-10,10)
    #phi.group[4] ~ dnorm(0, 0.01)I(-10,10)
    for(i in 1:g){
      phi.group[i]~dnorm(0, 0.01)I(-10,10)
    }
    
    phi.temp ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter
    phi.mass ~ dnorm(0,0.01)I(-10, 10)  # Prior for mass slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
        phi.bl[b] ~ dnorm(0,tau.phi.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      #}
    }
    sigma.phi.bl~dunif(0,5)
    tau.phi.bl<-pow(sigma.phi.bl,-2)
    sigma2.phi.bl <- pow(sigma.phi.bl, 2)
    
    for (p in 1:npen){
      #for (t in 1:(n.occasions-1)){
        phi.pen[p] ~ dnorm(0,tau.phi.pen)    #Prior for logit of mean survival with random effect of pen given block
      #}
    }
    sigma.phi.pen~dunif(0,10)
    tau.phi.pen<-pow(sigma.phi.pen,-2)
    sigma2.phi.pen <- pow(sigma.phi.pen, 2)

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

# Parameters monitored
parameters.aa <- c("phi.group","p.group","phi.temp","phi.mass","beta.m",
                   "phi.bl","p.bl",
                   "phi","p") 

# Initial values (probably need to adjust thse to match dimensions of certain parameters)
aa.inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
                            phi.group = rnorm(2), 
                            p.group = rnorm(2), 
                            #p.group = runif(4,0,1),
                            phi.bl = runif(length(unique(block_aa)), 0, 1), 
                            #p.bl = array(runif(64, 0, 1),dim=c(4,16)), 
                            p.bl = runif(length(unique(block_aa)), 0, 1), 
                            phi.mass = runif(1, -5, 5),
                            phi.temp = runif(1, -5,5),
                            #p.temp = runif(1, -5,5),
                            #phi.pcp = runif(1, -5,5),
                            #p.pcp = runif(1, -5,5),
                            phi.pen = runif(length(unique(pen_aa)), 0, 1), 
                            #p.pen = array(runif(length(unique(pen_aa))*16, 0,1),dim=c(24,16)), 
                            p.pen = runif(length(unique(pen_aa)), 0,1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.p.bl= runif(1, 0, 2), 
                            sigma.phi.bl= runif(1, 0, 2), 
                            sigma.p.pen= runif(1, 0, 2), 
                            sigma.phi.pen= runif(1, 0, 2))}

aa_final_fit <- jags(aa.data, parallel=TRUE, 
                      aa.inits, 
                      parameters.aa, 
                      "aman_final.jags", 
                      n.chains = nc, 
                      n.thin = nt, 
                      n.iter = ni, 
                      n.adapt = nb)
print(aa_final_fit)
saveRDS(aa_final_fit,'Results/aa_finalmod_2g.rds')
aa_final_fit<-readRDS("Results/aa_finalmod_2g.rds")
plot(aa_final_fit)

#run 2group_graphs.R to get 2-group plots

# Old A. annulatum graphs with 4 groups -----------------------------------


#median phi across all groups
# aa_phi_med<-median(aa_final_fit$sims.list$phi.group)
# aa_phi_ci<-quantile(aa_final_fit$sims.list$phi.group,probs = c(0.025,0.975))
# exp(aa_phi_med)/(1+exp(aa_phi_med)) #median
# exp(aa_phi_ci[1])/(1+exp(aa_phi_ci[1])) #lower
# exp(aa_phi_ci[2])/(1+exp(aa_phi_ci[2])) #upper
# 
# #median p across all groups
# aa_p_med<-median(aa_final_fit$sims.list$p.group)
# aa_p_ci<-quantile(aa_final_fit$sims.list$p.group,probs = c(0.025,0.975))
# exp(aa_p_med)/(1+exp(aa_p_med)) #median
# exp(aa_p_ci[1])/(1+exp(aa_p_ci[1])) #lower
# exp(aa_p_ci[2])/(1+exp(aa_p_ci[2])) #upper

#Calculate phi distributions (need to track phi and p first)
phi.list<-as.data.frame(aa_final_fit$mean$phi)
phi.l<-as.data.frame(aa_final_fit$q2.5$phi)
phi.h<-as.data.frame(aa_final_fit$q97.5$phi)
phi.listv<-as.matrix(aa_final_fit$mean$phi)
phi.lv<-as.matrix(aa_final_fit$q2.5$phi)
phi.hv<-as.matrix(aa_final_fit$q97.5$phi)
phi.mean <- phi.list %>% summarise_all(mean, na.rm=TRUE) 
phi.med <- phi.list %>% summarise_all(median, na.rm=TRUE) 
phi.lower <- as.numeric(phi.l %>% summarise_all(mean, na.rm=TRUE))
phi.higher <- as.numeric(phi.h %>% summarise_all(mean, na.rm=TRUE))
mean(phi.listv, na.rm = TRUE) #mean survival = 0.87
median(phi.listv, na.rm = TRUE) #median survival= 0.95
sd(phi.listv, na.rm = TRUE)#0.20
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.55
phi.ci.high<-mean(phi.hv, na.rm = TRUE)#0.97

#changed how group membership works- matches data so we don't have re-order everything. 
g1.phi<-as.matrix(subset(phi.list[aa.data$group==1,]))
g2.phi<-as.matrix(subset(phi.list[aa.data$group==2,]))
#g3.phi<-as.matrix(subset(phi.list[aa.data$group==3,]))
#g4.phi<-as.matrix(subset(phi.list[aa.data$group==4,]))
g1.phi.dat<-as.data.frame(subset(phi.list[aa.data$group==1,]))
g2.phi.dat<-as.data.frame(subset(phi.list[aa.data$group==2,]))
#g3.phi.dat<-as.data.frame(subset(phi.list[aa.data$group==3,]))
#g4.phi.dat<-as.data.frame(subset(phi.list[aa.data$group==4,]))
g1.phil<-as.data.frame(subset(phi.l[aa.data$group==1,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[aa.data$group==2,]))
#g3.phil<-as.data.frame(subset(phi.l[aa.data$group==3,]))
#g4.phil<-as.data.frame(subset(phi.l[aa.data$group==4,]))
g1.phih<-as.data.frame(subset(phi.h[aa.data$group==1,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[aa.data$group==2,]))
#g3.phih<-as.data.frame(subset(phi.h[aa.data$group==3,]))
#g4.phih<-as.data.frame(subset(phi.h[aa.data$group==4,]))
phi.g1 <- g1.phi.dat %>% summarise_all(mean)
phi.g2 <- g2.phi.dat %>% summarise_all(mean)
#phi.g3 <- g3.phi.dat %>% summarise_all(mean)
#phi.g4 <- g4.phi.dat %>% summarise_all(mean)
phi.g1.med <- g1.phi.dat %>% summarise_all(median)
phi.g2.med <- g2.phi.dat %>% summarise_all(median)
#phi.g3.med <- g3.phi.dat %>% summarise_all(median)
#phi.g4.med <- g4.phi.dat %>% summarise_all(median)
g1.low<-as.numeric(g1.phil %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.phih %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.phil %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.phih %>% summarise_all(mean, na.rm=TRUE))
#g3.low<-as.numeric(g3.phil %>% summarise_all(mean, na.rm=TRUE))
#g3.high<-as.numeric(g3.phih %>% summarise_all(mean, na.rm=TRUE))
#g4.low<-as.numeric(g4.phil %>% summarise_all(mean, na.rm=TRUE))
#g4.high<-as.numeric(g4.phih %>% summarise_all(mean, na.rm=TRUE))
x.g1.phi<-mean(g1.phi)#Overall spp. means
x.g2.phi<-mean(g2.phi)
#x.g3.phi<-mean(g3.phi)
#x.g4.phi<-mean(g4.phi)
med.g1.phi<-median(g1.phi)#Overall treatment medians
med.g2.phi<-median(g2.phi)
#med.g3.phi<-median(g3.phi)
#med.g4.phi<-median(g4.phi)
means.phi<-c(x.g1.phi, x.g2.phi)#, x.g3.phi, x.g4.phi)#0.8708020 0.8556910 0.8570171 0.8678066
meds.phi<-c(med.g1.phi, med.g2.phi)#, med.g3.phi, med.g4.phi) #0.9541836 0.9441955 0.9469313 0.9532164
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
#sd.g3.phi<-sd(g3.phi)
#sd.g4.phi<-sd(g4.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi)#, sd.g3.phi, sd.g4.phi)#0.2001507 0.2071274 0.2087593 0.2034356

#Figure of treatment-specific temporal survival
#pdf("~/GitHub/JuvenileEmigrationPhenology/Fig1.pdf", width = 10, height = 8)
png("Results/Fig1.png", width = 5, height = 7,res=500,units="in")
recapdates<-c("8/1/18","9/12/18","11/7/18","3/12/19","5/7/19")
par(mar=c(3.5,4.5,1,2.5), mgp=c(2.5,1,0), oma=c(0,0,0,2), mfrow=c(2,1))
plot(x=(1:16),y= phi.g1.med, type="b", pch=1, col="salmon1",bty='l',las=T,xaxt="n",
     ylim=c(0,1), ylab=expression("Survival probability ("~italic(phi)~")"), xlab="",xaxt="n")
axis(1,at = c(1,4,8,12,16),labels=F)
text(labels=recapdates,y=-.1,srt=45,adj=1,x=c(1,4,8,12,16),xpd=T,cex=0.9)
segments((1:16), g1.low, (1:16), g1.high, col="salmon1")
points(x=(1:16)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2)
segments((1:16)+.1, g2.low, (1:16)+.1, g2.high, col="deepskyblue3")
#points(x=(1:16)+.2,phi.g3.med, type="b", pch=0, col="midnightblue", lty=1)
#segments((1:16)+.2, g3.low, (1:16)+.2, g3.high, col="midnightblue")
#points(x=(1:16)+.3,phi.g4.med, type="b", pch=5, col="orangered4", lty=4)
#segments((1:16)+.3, g4.low, (1:16)+.3, g4.high, col="orangered4")
#mtext(line=2,side=1,at=c(1,4,8,12,16),recapdates,cex.axis=0.75)
par(new = TRUE)
plot(x=(1:16), aa_stdtempc[1:16], type="b", lty=5, lwd=2, col=1, axes = FALSE, bty = "n", xlab = "", ylab = "", 
     ylim=c(-3,3), pch=2)
segments((1:16), aa_stdtempc[1:16]-aa_stdtempsd[1:16], (1:16), aa_stdtempc[1:16]+aa_stdtempsd[1:16], col=1)
axis(side=4, at = pretty(c(-3.2,3)))
mtext(expression(Scaled~Mean~Air~Temp~(C)), side=4, las=0, line=2)
mtext("A)", side=3, at=1, las=1, line=0)

#Calculate recapture distributions
p.list<-as.data.frame(aa_final_fit$mean$p)
p.l<-as.data.frame(aa_final_fit$q2.5$p)
p.h<-as.data.frame(aa_final_fit$q97.5$p)
p.listv<-as.matrix(aa_final_fit$mean$p)
p.lv<-as.matrix(aa_final_fit$q2.5$p)
p.hv<-as.matrix(aa_final_fit$q97.5$p)
p.mean <- p.list %>% summarise_all(mean, na.rm=TRUE) 
p.med <- p.list %>% summarise_all(median, na.rm=TRUE) 
p.lower <- as.numeric(p.l %>% summarise_all(mean, na.rm=TRUE))
p.higher <- as.numeric(p.h %>% summarise_all(mean, na.rm=TRUE))
mean(p.listv, na.rm = TRUE) #mean survival = 0.25
median(p.listv, na.rm = TRUE) #median survival= 0.27
sd(p.listv, na.rm = TRUE)#0.09
p.ci.low<-mean(p.lv, na.rm = TRUE)#0.07
p.ci.high<-mean(p.hv, na.rm = TRUE)#0.53

g1.p<-as.matrix(subset(p.list[aa.data$group==1,]))
g2.p<-as.matrix(subset(p.list[aa.data$group==2,]))
#g3.p<-as.matrix(subset(p.list[aa.data$group==3,]))
#g4.p<-as.matrix(subset(p.list[aa.data$group==4,]))
g1.p.dat<-as.data.frame(subset(p.list[aa.data$group==1,]))
g2.p.dat<-as.data.frame(subset(p.list[aa.data$group==2,]))
#g3.p.dat<-as.data.frame(subset(p.list[aa.data$group==3,]))
#g4.p.dat<-as.data.frame(subset(p.list[aa.data$group==4,]))
g1.pl<-as.data.frame(subset(p.l[aa.data$group==1,])) #Spp.-specific lower CI
g2.pl<-as.data.frame(subset(p.l[aa.data$group==2,]))
#g3.pl<-as.data.frame(subset(p.l[aa.data$group==3,]))
#g4.pl<-as.data.frame(subset(p.l[aa.data$group==4,]))
g1.ph<-as.data.frame(subset(p.h[aa.data$group==1,])) #spp.-specific upper CI
g2.ph<-as.data.frame(subset(p.h[aa.data$group==2,]))
#g3.ph<-as.data.frame(subset(p.h[aa.data$group==3,]))
#g4.ph<-as.data.frame(subset(p.h[aa.data$group==4,]))
p.g1 <- g1.p.dat %>% summarise_all(mean)
p.g2 <- g2.p.dat %>% summarise_all(mean)
#p.g3 <- g3.p.dat %>% summarise_all(mean)
#p.g4 <- g4.p.dat %>% summarise_all(mean)
p.g1.med <- g1.p.dat %>% summarise_all(median)
p.g2.med <- g2.p.dat %>% summarise_all(median)
#p.g3.med <- g3.p.dat %>% summarise_all(median)
#p.g4.med <- g4.p.dat %>% summarise_all(median)
g1.low<-as.numeric(g1.pl %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.ph %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.pl %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.ph %>% summarise_all(mean, na.rm=TRUE))
#g3.low<-as.numeric(g3.pl %>% summarise_all(mean, na.rm=TRUE))
#g3.high<-as.numeric(g3.ph %>% summarise_all(mean, na.rm=TRUE))
#g4.low<-as.numeric(g4.pl %>% summarise_all(mean, na.rm=TRUE))
#g4.high<-as.numeric(g4.ph %>% summarise_all(mean, na.rm=TRUE))
x.g1.p<-mean(g1.p)#Overall spp. means
x.g2.p<-mean(g2.p)
#x.g3.p<-mean(g3.p)
#x.g4.p<-mean(g4.p)
med.g1.p<-median(g1.p)#Overall treatment medians
med.g2.p<-median(g2.p)
#med.g3.p<-median(g3.p)
#med.g4.p<-median(g4.p)
means.p<-c(x.g1.p, x.g2.p)#, x.g3.p, x.g4.p)#0.2829697 0.2859278 0.2968621 0.3280439
meds.p<-c(med.g1.p, med.g2.p)#, med.g3.p, med.g4.p) #0.2510888 0.2510146 0.2787384 0.2855799
sd.g1.p<-sd(g1.p)
sd.g2.p<-sd(g2.p)
#sd.g3.p<-sd(g3.p)
#sd.g4.p<-sd(g4.p)
sd.p<-c(sd.g1.p, sd.g2.p)#, sd.g3.p, sd.g4.p)#0.07919079 0.09192346 0.08523798 0.10025805

#Add panel of treatment-specific temporal recapture
plot(x=(1:16),y= p.g1.med, type="b", pch=1, col="salmon1",lty=3, bty='l',las=1,
     ylim=c(0,1), ylab=expression("Recapture Probability ("~italic(rho)~")"), xlab="Recapture Date",xaxt="n")
axis(1,at = c(1,4,8,12,16),labels=F)
text(labels=recapdates,y=-.1,srt=45,adj=1,x=c(1,4,8,12,16),xpd=T,cex=0.9)
segments((1:16), g1.low, (1:16), g1.high, col="salmon1")
points(x=(1:16)+.1,p.g2.med, type="b", pch=6, col="deepskyblue3", lty=2)
segments((1:16)+.1, g2.low, (1:16)+.1, g2.high, col="deepskyblue3")
#points(x=(1:16)+.2,p.g3.med, type="b", pch=0, col="midnightblue", lty=1)
#segments((1:16)+.2, g3.low, (1:16)+.2, g3.high, col="midnightblue")
#points(x=(1:16)+.3,p.g4.med, type="b", pch=5, col="orangered4", lty=4)
#segments((1:16)+.3, g4.low, (1:16)+.3, g4.high, col="orangered4")
mtext("B)", side=3, at=1, las=0, line=1)
legend(x = 4, y=1, bty = 'n',
       legend=c("J1", "J3"),
       pch=c(1,6,0,5), lty=c(3,2), col=c("salmon1", "deepskyblue3"))
       #legend=c("L1J1", "L1J3", "L3J1", "L3J3"),
       #pch=c(1,6,0,5), lty=c(3,2,1,4), col=c("salmon1", "deepskyblue3", "midnightblue", "orangered4"),ncol=2)
dev.off()

#Group effect on survival
plot(density(aa_final_fit$sims.list$phi.group[,1]), xlim=c(-3,8))#L3J3
lines(density(aa_final_fit$sims.list$phi.group[,2]), col=2)#L3J1
lines(density(aa_final_fit$sims.list$phi.group[,3]), col=3)#L1J3
lines(density(aa_final_fit$sims.list$phi.group[,4]), col=4)#L1J1

#Calculate % of posterior of differences overlapping zero
phiL1J1.L1J3_f<-round(sum(ifelse((aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,2])<=0,1,0))/
    #sum(aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,2])) * 100
    length(aa_final_fit$sims.list$phi.group[,1]),2)
phiL1J1.L3J1_f<-round(sum(ifelse((aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,3])<=0,1,0))/
    #sum(aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,3])) * 100
    length(aa_final_fit$sims.list$phi.group[,1]),2)
phiL1J1.L3J3_f<-round(sum(ifelse((aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,4])<=0,1,0))/
    #sum(aa_final_fit$sims.list$phi.group[,2]-aa_final_fit$sims.list$phi.group[,4])) * 100
    length(aa_final_fit$sims.list$phi.group[,1]),2)
phiL1J3.L3J1_f<-round(sum(ifelse((aa_final_fit$sims.list$phi.group[,2]-aa_final_fit$sims.list$phi.group[,3])<=0,1,0))/
    #sum(aa_final_fit$sims.list$phi.group[,2]-aa_final_fit$sims.list$phi.group[,3])) * 100
    length(aa_final_fit$sims.list$phi.group[,1]),2)
phiL1J3.L3J3_f<-round(sum(ifelse((aa_final_fit$sims.list$phi.group[,2]-aa_final_fit$sims.list$phi.group[,4])<=0,1,0))/
    #sum(aa_final_fit$sims.list$phi.group[,4]-aa_final_fit$sims.list$phi.group[,3])) * 100
    length(aa_final_fit$sims.list$phi.group[,1]),2)
phiL3J1.L3J3_f<-round(sum(ifelse((aa_final_fit$sims.list$phi.group[,3]-aa_final_fit$sims.list$phi.group[,4])<=0,1,0))/
    #sum(aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,4])) * 100
    length(aa_final_fit$sims.list$phi.group[,1]),2)

#If difference of posteriors overlaps zero, no significant difference
png("Results/FigS2.png",width = 7, height = 9,units="in",res=500)
par(mfrow=c(3,2),mar=c(3,4,2,1))
plot(density(aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,2]),main="L1J1-L1J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J1.L1J3_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,3]),main="L1J1-L3J1",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J1.L3J1_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,4]),main="L1J1-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J1.L3J3_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$phi.group[,2]-aa_final_fit$sims.list$phi.group[,3]),main="L1J3-L3J1",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J3.L3J1_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$phi.group[,2]-aa_final_fit$sims.list$phi.group[,4]),main="L1J3-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J3.L3J3_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$phi.group[,3]-aa_final_fit$sims.list$phi.group[,4]),main="L3J1-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL3J1.L3J3_f,sep=" "),adj=0.9)
dev.off()

#Group effect on recapture
plot(density(aa_final_fit$sims.list$p.group[,1]), xlim=c(-5,5),main="")#L1J1
lines(density(aa_final_fit$sims.list$p.group[,2]), col=2)#L1J3
lines(density(aa_final_fit$sims.list$p.group[,3]), col=3)#L3J1
lines(density(aa_final_fit$sims.list$p.group[,4]), col=4)#L3J3

#Calculate % of posterior of differences overlapping zero- is this right? shouldn't it just be divided by 18k
pL1J1.L1J3_f<-round(sum(ifelse((aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,2])<=0,1,0))/
                #sum(aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,2])) * 100
                length(aa_final_fit$sims.list$p.group[,1]),2) 

pL1J1.L3J1_f<-round(sum(ifelse((aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,3])<=0,1,0))/
                #sum(aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,3])) * 100
                length(aa_final_fit$sims.list$p.group[,1]),2) 

pL1J1.L3J3_f<-round(sum(ifelse((aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,4])<=0,1,0))/
                #sum(aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,4])) * 100
                length(aa_final_fit$sims.list$p.group[,1]),2) 

pL1J3.L3J1_f<-round(sum(ifelse((aa_final_fit$sims.list$p.group[,2]-aa_final_fit$sims.list$p.group[,3])<=0,1,0))/
                #sum(aa_final_fit$sims.list$p.group[,2]-aa_final_fit$sims.list$p.group[,3])) * 100
                length(aa_final_fit$sims.list$p.group[,1]),2) 

pL1J3.L3J3_f<-round(sum(ifelse((aa_final_fit$sims.list$p.group[,2]-aa_final_fit$sims.list$p.group[,4])<=0,1,0))/
                #sum(aa_final_fit$sims.list$p.group[,2]-aa_final_fit$sims.list$p.group[,4])) * 100
                length(aa_final_fit$sims.list$p.group[,1]),2) 

pL3J1.L3J3_f<-round(sum(ifelse((aa_final_fit$sims.list$p.group[,3]-aa_final_fit$sims.list$p.group[,4])<=0,1,0))/
                #sum(aa_final_fit$sims.list$p.group[,3]-aa_final_fit$sims.list$p.group[,4])) * 100
                length(aa_final_fit$sims.list$p.group[,1]),2) 

#If difference of posteriors for recapture overlaps zero, no significant difference
png("Results/FigS3.png",width = 7, height = 9,units="in",res=500)
par(mfrow=c(3,2),mar=c(3,4,2,1))
plot(density(aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,2]),main="L1J1-L1J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J1.L1J3_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,3]),main="L1J1-L3J1",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J1.L3J1_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,4]),main="L1J1-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J1.L3J3_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$p.group[,2]-aa_final_fit$sims.list$p.group[,3]),main="L1J3-L3J1",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J3.L3J1_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$p.group[,2]-aa_final_fit$sims.list$p.group[,4]),main="L1J3-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J3.L3J3_f,sep=" "),adj=0.9)

plot(density(aa_final_fit$sims.list$p.group[,3]-aa_final_fit$sims.list$p.group[,4]),main="L3J1-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL3J1.L3J3_f,sep=" "),adj=0.9)
dev.off()


# A. opacum final model ---------------------------------------------------

sink("amop_final.jags")
cat("
  model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(phi.group[group[i]] + phi.temp*temp[t]  + phi.bl[block[i]] + phi.pen[pen[i]] + epsilon.phi[t]))))^int[t]  
        p[i,t] <- 1/(1+exp(-(p.group[group[i]] + beta.m[m[i,t]] + p.temp*temp[t]+ p.bl[block[i]] + p.pen[pen[i]] + epsilon.p[t]))) 
      } #t
    } #i
    
    ## For recapture
  
    #mean.p~dnorm(0,0.001)
    #mu.p<-1/(1+exp(-mean.p))           # Logit transformed recapture grand mean/intercept
    p.temp ~ dnorm(0, 1)I(-15, 15)     # Prior for temp slope parameter
    
    for (u in 1:2){
      beta.m[u] ~ dunif(0, 1)        # Priors for previous recapture effect
    }
    
    # p.group[1] ~ dnorm(0, 0.01)I(-5,5)                   
    # p.group[2] ~ dnorm(0, 0.01)I(-5,5)      # Priors for difference in treatment-spec. recapture compared to treatment 1
    # p.group[3] ~ dnorm(0, 0.01)I(-5,5)
    # p.group[4] ~ dnorm(0, 0.01)I(-5,5)
    for(i in 1:g){
      p.group[i]~dnorm(0, 0.01)I(-5,5)
    }
    
    for (b in 1:nblock){
        #for (t in 1:(n.occasions-1)){
            p.bl[b] ~ dnorm(0,tau.p.bl)      #Prior for logit of mean recapture with random effect of block (random effect of block on p)
        #}
    }
    
    sigma.p.bl~dunif(0,5)
    tau.p.bl<-pow(sigma.p.bl,-2)
    sigma2.p.bl <- pow(sigma.p.bl, 2)
    
    for (p in 1:npen){
          #for (t in 1:(n.occasions-1)){
            p.pen[p] ~ dnorm(0,tau.p.pen)    #Prior for logit of mean recapture with random effect of pen given block
          #}
    }
    sigma.p.pen~dunif(0,5)
    tau.p.pen<-pow(sigma.p.pen,-2)
    sigma2.p.pen <- pow(sigma.p.pen, 2)
    
     for (t in 1:(n.occasions-1)){
      epsilon.p[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
     }
    sigma.p ~ dunif(0,5)                    # Prior on standard deviation
    tau.p <- pow(sigma.p, -2)
    sigma2.p <- pow(sigma.p, 2)           # Residual temporal variance
    
    ##For survival
    
    #mean.phi~dnorm(0,0.001)
    #mu.phi<-1/(1+exp(-mean.phi))             # Logit transformed survival grand mean/intercept
    
    # phi.group[1] ~ dnorm(0, 0.01)I(-10,10)
    # phi.group[2] ~ dnorm(0, 0.01)I(-10,10)      # Priors for difference in treatment-spec. survival compared to treatment 1
    # phi.group[3] ~ dnorm(0, 0.01)I(-10,10)
    # phi.group[4] ~ dnorm(0, 0.01)I(-10,10)
    for(i in 1:g){
      phi.group[i]~dnorm(0, 0.01)I(-10,10)
    }
    
    phi.temp ~ dnorm(0, 1)I(-10, 10)     # Prior for temp slope parameter

    for (t in 1:(n.occasions-1)){
      epsilon.phi[t] ~ dnorm(0, tau.phi)      # Prior for survival residuals
    }
    sigma.phi ~ dunif(0,5)                    # Prior on standard deviation
    tau.phi <- pow(sigma.phi, -2)
    sigma2.phi <- pow(sigma.phi, 2)           # Residual temporal variance
    
    for (b in 1:nblock){
        phi.bl[b] ~ dnorm(0,tau.phi.bl)   #Prior for logit of mean survival with random effect of block (random effect of block on phi)
      #}
    }
    sigma.phi.bl~dunif(0,5)
    tau.phi.bl<-pow(sigma.phi.bl,-2)
    sigma2.phi.bl <- pow(sigma.phi.bl, 2)
    
    for (p in 1:npen){
      #for (t in 1:(n.occasions-1)){
        phi.pen[p] ~ dnorm(0,tau.phi.pen)    #Prior for logit of mean survival with random effect of pen given block
      #}
    }
    sigma.phi.pen~dunif(0,10)
    tau.phi.pen<-pow(sigma.phi.pen,-2)
    sigma2.phi.pen <- pow(sigma.phi.pen, 2)

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

# Parameters monitored
parameters.ao <- c("phi.group","p.group","phi.temp","p.temp","beta.m",
                   "phi.bl","p.bl","phi","p") 

ao.inits <- function(){list(z = cjs.init.z(ao_CH,f_ao), 
                            phi.group = rnorm(2), 
                            p.group = rnorm(2), 
                            #p.group = runif(4,0,1), 
                            phi.bl = runif(length(unique(block_ao)), 0, 1), 
                            #p.bl = array(runif(64, 0, 1),dim=c(4,16)), 
                            p.bl = runif(length(unique(block_ao)), 0, 1), 
                            #phi.mass = runif(1, -5, 5),
                            phi.temp = runif(1, -5,5),
                            p.temp = runif(1, -5,5),
                            #phi.pcp = runif(1, -5,5),
                            #p.pcp = runif(1, -5,5),
                            phi.pen = runif(length(unique(pen_ao)), 0, 1), 
                            #p.pen = array(runif(length(unique(pen_ao))*16, 0,1),dim=c(24,16)), 
                            p.pen = runif(length(unique(pen_ao)), 0,1), 
                            beta.m = runif (2, 0, 1),
                            sigma.phi = runif(1, 0, 2), 
                            sigma.p = runif(1, 0, 2), 
                            sigma.p.bl= runif(1, 0, 2), 
                            sigma.phi.bl= runif(1, 0, 2), 
                            sigma.p.pen= runif(1, 0, 2), 
                            sigma.phi.pen= runif(1, 0, 2))}

ao_final_fit <- jags(ao.data, parallel=TRUE, 
                     ao.inits, 
                     parameters.ao, 
                     "amop_final.jags", 
                     n.chains = nc, 
                     n.thin = 25, 
                     n.iter = 150000, 
                     n.adapt = 100000)

saveRDS(ao_final_fit,"Results/ao_finalmod_2g.rds")
ao_final_fit<-readRDS("Results/ao_final_fit.rds")
print(ao_final_fit)
plot(ao_final_fit)


#run 2group_graphs.R to get 2-group plots
# Old A. opacum 4-group plots ---------------------------------------------------------


# #median phi across all groups
# ao_phi_med<-median(ao_final_fit$sims.list$phi.group)
# ao_phi_ci<-quantile(ao_final_fit$sims.list$phi.group,probs = c(0.025,0.975))
# exp(ao_phi_med)/(1+exp(ao_phi_med)) #median
# exp(ao_phi_ci[1])/(1+exp(ao_phi_ci[1])) #lower
# exp(ao_phi_ci[2])/(1+exp(ao_phi_ci[2])) #upper
# 
# #median p across all groups
# ao_p_med<-median(ao_final_fit$sims.list$p.group)
# ao_p_ci<-quantile(ao_final_fit$sims.list$p.group,probs = c(0.025,0.975))
# exp(ao_p_med)/(1+exp(ao_p_med)) #median
# exp(ao_p_ci[1])/(1+exp(ao_p_ci[1])) #lower
# exp(ao_p_ci[2])/(1+exp(ao_p_ci[2])) #upper

#Calculate phi distributions
phi.list<-as.data.frame(ao_final_fit$mean$phi)
phi.l<-as.data.frame(ao_final_fit$q2.5$phi)
phi.h<-as.data.frame(ao_final_fit$q97.5$phi)
phi.listv<-as.matrix(ao_final_fit$mean$phi)
phi.lv<-as.matrix(ao_final_fit$q2.5$phi)
phi.hv<-as.matrix(ao_final_fit$q97.5$phi)
phi.mean <- phi.list %>% summarise_all(mean, na.rm=TRUE) 
phi.med <- phi.list %>% summarise_all(median, na.rm=TRUE) 
phi.lower <- as.numeric(phi.l %>% summarise_all(mean, na.rm=TRUE))
phi.higher <- as.numeric(phi.h %>% summarise_all(mean, na.rm=TRUE))
mean(phi.listv, na.rm = TRUE) #mean survival = 0.93
median(phi.listv, na.rm = TRUE) #median survival= 0.96
sd(phi.listv, na.rm = TRUE)#0.09
phi.ci.low<-mean(phi.lv, na.rm = TRUE)#0.84
phi.ci.high<-mean(phi.hv, na.rm = TRUE)#0.98

g1.phi<-as.matrix(subset(phi.list[ao.data$group==1,]))
g2.phi<-as.matrix(subset(phi.list[ao.data$group==2,]))
g3.phi<-as.matrix(subset(phi.list[ao.data$group==3,]))
g4.phi<-as.matrix(subset(phi.list[ao.data$group==4,]))
g1.phi.dat<-as.data.frame(subset(phi.list[ao.data$group==1,]))
g2.phi.dat<-as.data.frame(subset(phi.list[ao.data$group==2,]))
g3.phi.dat<-as.data.frame(subset(phi.list[ao.data$group==3,]))
g4.phi.dat<-as.data.frame(subset(phi.list[ao.data$group==4,]))
g1.phil<-as.data.frame(subset(phi.l[ao.data$group==1,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[ao.data$group==2,]))
g3.phil<-as.data.frame(subset(phi.l[ao.data$group==3,]))
g4.phil<-as.data.frame(subset(phi.l[ao.data$group==4,]))
g1.phih<-as.data.frame(subset(phi.h[ao.data$group==1,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[ao.data$group==2,]))
g3.phih<-as.data.frame(subset(phi.h[ao.data$group==3,]))
g4.phih<-as.data.frame(subset(phi.h[ao.data$group==4,]))
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
means.phi<-c(x.g1.phi, x.g2.phi, x.g3.phi, x.g4.phi)#0.8943092 0.9342185 0.9154442 0.9110601
meds.phi<-c(med.g1.phi, med.g2.phi, med.g3.phi, med.g4.phi) #0.9534554 0.9621434 0.9560036 0.9548538
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.g3.phi<-sd(g3.phi)
sd.g4.phi<-sd(g4.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi, sd.g3.phi, sd.g4.phi)#0.10384241 0.07389021 0.09097088 0.09336241


#Panel of treatment-specific temporal survival
#pdf("~/GitHub/JuvenileEmigrationPhenology/Fig2.pdf", width = 15, height = 20)
png("Results/Fig2.png", width = 5, height = 7,units="in",res=500)
recapdates1<-c("7/6/19","8/21/19","10/2/19","11/9/19","1/14/20","3/3/20","5/4/20")
#par(mai=c(2,2,1,2), mgp=c(5,2,0), oma=c(0,0,0,2), mfrow=c(2,1))
par(mar=c(3.5,4,1,2.5), mgp=c(2.5,1,0), oma=c(0,0,0,2), mfrow=c(2,1))
plot(x=(1:14),y= phi.g1.med, type="b", pch=1, col="salmon1",lty=3, bty='l',
     ylim=c(0,1), ylab=expression("Survival probability ("~italic(phi)~")"), xlab="",las=1,xaxt="n")
axis(1,at=seq(2,14,2),labels=F)
text(labels=recapdates1,y=-.1,srt=45,adj=1,x=seq(2,14,2),xpd=T,cex=0.9)
segments((1:14), g1.low, (1:14), g1.high, col="salmon1")
points(x=(1:14)+.1,phi.g2.med, type="b", pch=6, col="deepskyblue3", lty=2)
segments((1:14)+.1, g2.low, (1:14)+.1, g2.high, col="deepskyblue3")
points(x=(1:14)+.2,phi.g3.med, type="b", pch=0, col="midnightblue", lty=1)
segments((1:14)+.2, g3.low, (1:14)+.2, g3.high, col="midnightblue")
points(x=(1:14)+.3,phi.g4.med, type="b", pch=5, col="orangered4", lty=4)
segments((1:14)+.3, g4.low, (1:14)+.3, g4.high, col="orangered4")
#mtext(at=c(2,4,6,8,10,12,14),text=recapdates1,line=2,side=1)

##Add temperature data on z-axis
# par(new = TRUE)
# plot(x=(1:14), ao_stdtempc[1:14], type="b", lty=5, lwd=2, col=1, axes = FALSE, bty = "n", xlab = "", ylab = "", 
#      ylim=c(-3,3), pch=2,xaxt="n")
# segments((1:14), ao_stdtempc[1:14]-ao_stdtempsd[1:14], (1:14), ao_stdtempc[1:14]+ao_stdtempsd[1:14], col=1)
# axis(side=4, at = pretty(c(-3.2,3)))
# mtext(expression(Scaled~Mean~Air~Temp~(C)), side=4, las=0, line=2)
# mtext("A)", side=3, at=1, las=1, line=0)
legend(x = 7, y = 0.2, bty = 'n', 
       legend=c("L1J1", "L1J3", "L3J1", "L3J3"),
       lwd=c(2,2,2,2,2), pch=c(1,6,0,5,2), lty=c(3,2,1,4,5), cex=1,  
       col=c("salmon1", "deepskyblue3", "midnightblue", "orangered4"),ncol=2)

#Calculate recapture distributions
p.list<-as.data.frame(ao_final_fit$mean$p)
p.l<-as.data.frame(ao_final_fit$q2.5$p)
p.h<-as.data.frame(ao_final_fit$q97.5$p)
p.listv<-as.matrix(ao_final_fit$mean$p)
p.lv<-as.matrix(ao_final_fit$q2.5$p)
p.hv<-as.matrix(ao_final_fit$q97.5$p)
p.mean <- p.list %>% summarise_all(mean, na.rm=TRUE) 
p.med <- p.list %>% summarise_all(median, na.rm=TRUE) 
p.lower <- as.numeric(p.l %>% summarise_all(mean, na.rm=TRUE))
p.higher <- as.numeric(p.h %>% summarise_all(mean, na.rm=TRUE))
mean(p.listv, na.rm = TRUE) #mean recapture = 0.44
median(p.listv, na.rm = TRUE) #median recapture= 0.37
sd(p.listv, na.rm = TRUE)#0.24
p.ci.low<-mean(p.lv, na.rm = TRUE)#0.30
p.ci.high<-mean(p.hv, na.rm = TRUE)#0.59
p.ci.high;p.ci.low

g1.p<-as.matrix(subset(p.list[ao.data$group==1,]))
g2.p<-as.matrix(subset(p.list[ao.data$group==2,]))
g3.p<-as.matrix(subset(p.list[ao.data$group==3,]))
g4.p<-as.matrix(subset(p.list[ao.data$group==4,]))
g1.p.dat<-as.data.frame(subset(p.list[ao.data$group==1,]))
g2.p.dat<-as.data.frame(subset(p.list[ao.data$group==2,]))
g3.p.dat<-as.data.frame(subset(p.list[ao.data$group==3,]))
g4.p.dat<-as.data.frame(subset(p.list[ao.data$group==4,]))
g1.pl<-as.data.frame(subset(p.l[ao.data$group==1,])) #Spp.-specific lower CI
g2.pl<-as.data.frame(subset(p.l[ao.data$group==2,]))
g3.pl<-as.data.frame(subset(p.l[ao.data$group==3,]))
g4.pl<-as.data.frame(subset(p.l[ao.data$group==4,]))
g1.ph<-as.data.frame(subset(p.h[ao.data$group==1,])) #spp.-specific upper CI
g2.ph<-as.data.frame(subset(p.h[ao.data$group==2,]))
g3.ph<-as.data.frame(subset(p.h[ao.data$group==3,]))
g4.ph<-as.data.frame(subset(p.h[ao.data$group==4,]))
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
means.p<-c(x.g1.p, x.g2.p, x.g3.p, x.g4.p)#0.4674974 0.4357866 0.4668106 0.4585805
meds.p<-c(med.g1.p, med.g2.p, med.g3.p, med.g4.p) #0.4235455 0.4031004 0.4301593 0.4297407
sd.g1.p<-sd(g1.p)
sd.g2.p<-sd(g2.p)
sd.g3.p<-sd(g3.p)
sd.g4.p<-sd(g4.p)
sd.p<-c(sd.g1.p, sd.g2.p, sd.g3.p, sd.g4.p)#0.2373022 0.2419632 0.2362928 0.2341313

#Add panel of treatment-specific temporal recapture
plot(x=(1:14),y= p.g1.med, type="b", pch=1, col="salmon1",lty=3, bty='l',las=1,
     ylim=c(0,1), ylab=expression("Recapture probability ("~italic(rho)~")"), xlab="Recapture Date",xaxt="n")
axis(1,at=seq(2,14,2),labels=F)
text(labels=recapdates1,y=-.1,srt=45,adj=1,x=seq(2,14,2),xpd=T,cex = 0.9)
segments((1:14), g1.low, (1:14), g1.high, col="salmon1")
points(x=(1:14)+.1,p.g2.med, type="b", pch=6, col="deepskyblue3", lty=2)
segments((1:14)+.1, g2.low, (1:14)+.1, g2.high, col="deepskyblue3")
points(x=(1:14)+.2,p.g3.med, type="b", pch=0, col="midnightblue", lty=1)
segments((1:14)+.2, g3.low, (1:14)+.2, g3.high, col="midnightblue")
points(x=(1:14)+.3,p.g4.med, type="b", pch=5, col="orangered4", lty=4)
segments((1:14)+.3, g4.low, (1:14)+.3, g4.high, col="orangered4")
mtext("B)", side=3, at=1, las=0, line=0)
# par(new = TRUE)
# plot(x=(1:14), ao_stdtempc[1:14], type="b", lty=5, lwd=2, col=1, axes = FALSE, bty = "n", xlab = "", ylab = "", 
#      ylim=c(-3,3), pch=2)
# segments((1:14), ao_stdtempc[1:14]-ao_stdtempsd[1:14], (1:14), ao_stdtempc[1:14]+ao_stdtempsd[1:14], col=1)
# axis(side=4, at = pretty(c(-3.2,3)))
# mtext(expression(Scaled~Mean~Air~Temp~(C)), side=4, las=0, line=2)
dev.off()

#Group effect on recapture
plot(density(ao_final_fit$sims.list$p.group[,1]), xlim=c(-3,3))#L1J1
lines(density(ao_final_fit$sims.list$p.group[,2]), col=2)#L1J3
lines(density(ao_final_fit$sims.list$p.group[,3]), col=3)#L3J1
lines(density(ao_final_fit$sims.list$p.group[,4]), col=4)#L3J3

#Calculate % of posterior of differences overlapping zero for recapture
pL1J1.L1J3_f<-round(sum(ifelse((ao_final_fit$sims.list$p.group[,1]-ao_final_fit$sims.list$p.group[,2])<=0,1,0))/
    #sum(ao_final_fit$sims.list$p.group[,3]-ao_final_fit$sims.list$p.group[,1])) * 100
    length(ao_final_fit$sims.list$p.group[,3]),2)

pL1J1.L3J1_f<-round(sum(ifelse((ao_final_fit$sims.list$p.group[,1]-ao_final_fit$sims.list$p.group[,3])<=0,1,0))/
    #sum(ao_final_fit$sims.list$p.group[,2]-ao_final_fit$sims.list$p.group[,3])) * 100
    length(ao_final_fit$sims.list$p.group[,3]),2)

pL1J1.L3J3_f<-round(sum(ifelse((ao_final_fit$sims.list$p.group[,1]-ao_final_fit$sims.list$p.group[,4])<=0,1,0))/
    #sum(ao_final_fit$sims.list$p.group[,4]-ao_final_fit$sims.list$p.group[,3])) * 100
    length(ao_final_fit$sims.list$p.group[,3]),2)

pL1J3.L3J1_f<-round(sum(ifelse((ao_final_fit$sims.list$p.group[,2]-ao_final_fit$sims.list$p.group[,3])<=0,1,0))/
    #sum(ao_final_fit$sims.list$p.group[,4]-ao_final_fit$sims.list$p.group[,1])) * 100
    length(ao_final_fit$sims.list$p.group[,3]),2)

pL1J3.L3J3_f<-round(sum(ifelse((ao_final_fit$sims.list$p.group[,2]-ao_final_fit$sims.list$p.group[,4])<=0,1,0))/
    #sum(ao_final_fit$sims.list$p.group[,4]-ao_final_fit$sims.list$p.group[,2])) * 100
    length(ao_final_fit$sims.list$p.group[,3]),2)

pL3J1.L3J3_f<-round(sum(ifelse((ao_final_fit$sims.list$p.group[,3]-ao_final_fit$sims.list$p.group[,4])<=0,1,0))/
    #sum(ao_final_fit$sims.list$p.group[,2]-ao_final_fit$sims.list$p.group[,1])) * 100
    length(ao_final_fit$sims.list$p.group[,3]),2)

#If difference of posteriors overlaps zero, no significant difference
png("Results/FigS5.png",width = 7, height = 9,units="in",res=500)
par(mfrow=c(3,2),mar=c(3,4,2,1))
plot(density(ao_final_fit$sims.list$p.group[,1]-ao_final_fit$sims.list$p.group[,2]),main="L1J1-L1J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J1.L1J3_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$p.group[,1]-ao_final_fit$sims.list$p.group[,3]),main="L1J1-L3J1",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J1.L3J1_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$p.group[,1]-ao_final_fit$sims.list$p.group[,4]),main="L1J1-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J1.L3J3_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$p.group[,2]-ao_final_fit$sims.list$p.group[,3]),main="L1J3-L3J1",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J3.L3J1_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$p.group[,2]-ao_final_fit$sims.list$p.group[,4]),main="L1J3-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL1J3.L3J3_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$p.group[,3]-ao_final_fit$sims.list$p.group[,4]),main="L3J1-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pL3J1.L3J3_f,sep=" "),adj=0.9)
dev.off()

#Calculate % of posterior of differences overlapping zero
phiL1J1.L1J3_f<-round(sum(ifelse((ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,2])<=0,1,0))/
                        #sum(ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,3])) * 100
                        length(ao_final_fit$sims.list$phi.group[,3]),2)

phiL1J1.L3J1_f<-round(sum(ifelse((ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,3])<=0,1,0))/
                        #sum(ao_final_fit$sims.list$phi.group[,2]-ao_final_fit$sims.list$phi.group[,3])) * 100
                        length(ao_final_fit$sims.list$phi.group[,3]),2)

phiL1J1.L3J3_f<-round(sum(ifelse((ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,4])<=0,1,0))/
                        #sum(ao_final_fit$sims.list$phi.group[,4]-ao_final_fit$sims.list$phi.group[,3])) * 100
                        length(ao_final_fit$sims.list$phi.group[,3]),2)

phiL1J3.L3J1_f<-round(sum(ifelse((ao_final_fit$sims.list$phi.group[,2]-ao_final_fit$sims.list$phi.group[,3])<=0,1,0))/
                       #sum(ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,4])) * 100
                       length(ao_final_fit$sims.list$phi.group[,3]),2)

phiL1J3.L3J3_f<-round(sum(ifelse((ao_final_fit$sims.list$phi.group[,2]-ao_final_fit$sims.list$phi.group[,4])<=0,1,0))/
                        #sum(ao_final_fit$sims.list$phi.group[,2]-ao_final_fit$sims.list$phi.group[,4])) * 100
                        length(ao_final_fit$sims.list$phi.group[,3]),2)

phiL3J1.L3J3_f<-round(sum(ifelse((ao_final_fit$sims.list$phi.group[,3]-ao_final_fit$sims.list$phi.group[,4])<=0,1,0))/
                        #sum(ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,2])) * 100
                        length(ao_final_fit$sims.list$phi.group[,3]),2)

#Group effect on survival
plot(density(ao_final_fit$sims.list$phi.group[,1]), xlim=c(-3,8))#L1J1
lines(density(ao_final_fit$sims.list$phi.group[,2]), col=2)#L1J3
lines(density(ao_final_fit$sims.list$phi.group[,3]), col=3)#L3J1
lines(density(ao_final_fit$sims.list$phi.group[,4]), col=4)#L3J3

#If difference of posteriors overlaps zero, no significant difference for survival
png("Results/FigS4.png",width = 7, height = 9,units="in",res=500)
par(mfrow=c(3,2),mar=c(3,4,2,1))
plot(density(ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,2]),main="L1J1-L1J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J1.L1J3_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,3]),main="L1J1-L3J1",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J1.L3J1_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,4]),main="L1J1-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J1.L3J3_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$phi.group[,2]-ao_final_fit$sims.list$phi.group[,3]),main="L1J3-L3J1",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J3.L3J1_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$phi.group[,2]-ao_final_fit$sims.list$phi.group[,4]),main="L1J3-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL1J3.L3J3_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$phi.group[,3]-ao_final_fit$sims.list$phi.group[,4]),main="L3J1-L3J3",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiL3J1.L3J3_f,sep=" "),adj=0.9)
dev.off()


