# Bundle data
ao.data <- list(y = ao_CH, 
                int=interval_ao$int, 
                f = f_ao, 
                nind = dim(ao_CH)[1], 
                m=m_ao,
                n.occasions = dim(ao_CH)[2], 
                z = known.state.cjs(ao_CH))

# Bundle data
aa.data <- list(y = aa_CH, 
                int=interval_aa$int, 
                f = f_aa, 
                nind = dim(aa_CH)[1], 
                m=m_aa,
                n.occasions = dim(aa_CH)[2], 
                z = known.state.cjs(aa_CH))

#nimble parameters
ni=15000
nc=3
nb=7000
nt=10

library(nimble)
#time constant p and phi
mod1 <- nimbleCode({
    
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
  })

# Initial values
ao.inits <- function(){list(mean.phi = runif(1, 0, 1), 
                            beta = runif(2, 0, 1), 
                            z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("mean.phi", "beta", "phi", "p")

samples_ao1 <- nimbleMCMC(
  code = mod1,
  constants = ao.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = ao.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt,
  WAIC=T)

library(MCMCvis)
MCMCsummary(object = samples_ao1, params="mean.phi",round = 3)

# Initial values
aa.inits <- function(){list(mean.phi = runif(1, 0, 1), 
                            beta = runif(2, 0, 1), 
                            z = cjs.init.z(aa_CH,f_aa))}

samples_aa1 <- nimbleMCMC(
  code = mod1,
  constants = aa.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = aa.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt,
  WAIC=T)


#time dependent p and phi
mod2 <- nimbleCode({
    
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
      epsilon[t] ~ dnorm(0, sd = 100)
    }
    
    #sigma ~ dunif(0, 10)                     # Prior for standard deviation
    #tau <- pow(sigma, -2)
    #sigma2 <- pow(sigma, 2)                  # Residual temporal variance
    
    
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
    })

# Initial values
ao.inits <- function(){list(alpha = runif(dim(ao_CH)[2]-1, 0, 1), 
                            beta = runif(2, 0, 1), 
                            z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha","beta", "phi", "p")

samples_ao2 <- nimbleMCMC(
  code = mod2,
  constants = ao.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = ao.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt,
  WAIC=T)

# Initial values
aa.inits <- function(){list(alpha = runif(dim(aa_CH)[2]-1, 0, 1), 
                            beta = runif(2, 0, 1), 
                            z = cjs.init.z(aa_CH,f_aa))}
samples_aa2 <- nimbleMCMC(
  code = mod2,
  constants = aa.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = aa.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt,
  WAIC=T)

#time dependent phi and constant p
mod3 <- nimbleCode({
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
})

# Initial values
ao.inits <- function(){list(alpha = runif(dim(ao_CH)[2]-1, 0, 1), 
                            beta = runif(2, 0, 1), 
                            z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("alpha","beta", "phi", "p")

samples_ao3 <- nimbleMCMC(
  code = mod3,
  constants = ao.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = ao.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt,
  WAIC=T)

# Initial values
aa.inits <- function(){list(alpha = runif(dim(aa_CH)[2]-1, 0, 1), 
                            beta = runif(2, 0, 1), 
                            z = cjs.init.z(aa_CH,f_aa))}

samples_aa3 <- nimbleMCMC(
  code = mod3,
  constants = aa.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = aa.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt,
  WAIC=T)

#time dependent p and constant phi
mod4 <- nimbleCode({
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
    epsilon[t] ~ dnorm(0, sd = 100)
  }
  
  #sigma ~ dunif(0, 10)                     # Prior for standard deviation
  #tau <- pow(sigma, -2)
  #sigma2 <- pow(sigma, 2)                  # Residual temporal variance
  
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
})

# Initial values
ao.inits <- function(){list(mean.phi = runif(1, 0, 1), 
                            beta = runif(2, 0, 1), 
                            z = cjs.init.z(ao_CH,f_ao))}

# Parameters monitored
parameters <- c("mean.phi","beta", "phi", "p")

samples_ao4 <- nimbleMCMC(
  code = mod4,
  constants = ao.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = ao.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt,
  WAIC=T)

aa.inits <- function(){list(mean.phi = runif(1, 0, 1), 
                            beta = runif(2, 0, 1), 
                            z = cjs.init.z(aa_CH,f_aa))}

samples_aa4 <- nimbleMCMC(
  code = mod4,
  constants = aa.data, ## provide the combined data & constants as constants
  #data = ao.data,
  inits = aa.inits,
  monitors = parameters,
  nchains = nc,
  niter = ni,
  nburnin = nb,
  thin = nt,
  WAIC=T)

#model comparison for amop
samples_ao1$WAIC
samples_ao2$WAIC
samples_ao3$WAIC
samples_ao4$WAIC

#model comparison for aman
samples_aa1$WAIC
samples_aa2$WAIC
samples_aa3$WAIC
samples_aa4$WAIC


