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
  
  aa_recaps <- read.csv("Data/AMAN/AMAN_phenology_recap_data_master.csv", header=T)           ## recapture data
  aa_weather <- read_excel("Data/AMAN/AaCOE_CapenPark_Daily-Weather_20180601-20190531.xlsx")  ## weather data
  
  aa_assign <- read_excel("Data/AMAN/Aa_COEffects_Pen_Assignments.xlsx")             ## initial pen assignments
  aa_treats <- read.csv("Data/AMAN/Aa_COEffects_Treatments.csv", header=T)           ## treatment data
  aa_tagged <- read_excel("Data/AMAN/Aa_COEffects_Tagged_Animals.xlsx")              ## all tagged animals and metamorphosis sizes
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
  
    aa_weather$DOY <- as.numeric(format(as.Date(as.character(aa_weather$DATE), "%m/%d/%Y"), "%j"))
    aa_weather$DOY_adj <- ifelse(aa_weather$DOY >= doy.adj, yes=aa_weather$DOY-(doy.adj-1), no=ifelse(aa_weather$DOY < 152, yes=aa_weather$DOY+(365-doy.adj+1), no=aa_weather$DOY-(doy.adj-1)))
    
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
    
    # Create group variable
    group_aa <- as.factor(aa_ch.pa$Treatment)
    
    group_aa.L <- character(length = dim(aa_ch.pa)[1])      ## breeding phenology group
    group_aa.J <- character(length = dim(aa_ch.pa)[1])      ## emigration phenology group
    
    for(i in 1:dim(aa_ch.pa)[1]){
      group_aa.L[i] <- unlist(strsplit(aa_ch.pa$Treatment[i], "(?<=.{2})", perl=T))[[1]]
      group_aa.J[i] <- unlist(strsplit(aa_ch.pa$Treatment[i], "(?<=.{2})", perl=T))[[2]]
    }
    
    group_aa.L <- as.factor(group_aa.L)
    group_aa.J <- as.factor(group_aa.J)
    
    # Create vector with occasion of marking
    get.first <- function(x) min(which(x!=0))
    f_aa <- apply(aa_CH, 1, get.first)
    
    # Create matrix m to indicate when an individual was captured, to address test 2.ct trap effect
    m_aa<-aa_CH[,1:(dim(aa_CH)[2]-1)]
    u_aa<-which (m_aa==0)
    m_aa[u_aa]<-2
    
    
    ## Fixed group effects
    ## -------------------
    
    # Specify model in JAGS language
    ## constant parameters by group
    sink("cjs-group.jags")
    cat("
        model {
        
        # Priors and constraints
        for (i in 1:nind){
          for (t in f[i]:(n.occasions-1)){
            phi[i,t] <- phi.g[group[i]]
            p[i,t] <- p.g[group[i]]
          } #t
        } #i
        
        for (u in 1:g){
          phi.g[u] ~ dunif(0, 1)              # Priors for group-specific survival
          p.g[u] ~ dunif(0, 1)                # Priors for group-specific recapture
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
    aa_jags.data <- list(y = aa_CH, f = f, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), g = length(unique(group)), group = as.numeric(group))
    
    # Initial values
    inits <- function(){list(z = cjs.init.z(aa_CH, f), phi.g = runif(length(unique(group)), 0, 1), p.g = runif(length(unique(group)), 0, 1))}  
    
    # Parameters monitored
    parameters <- c("phi.g", "p.g")
    
    ## MCMC settings
    ni <- 10000
    nt <- 5
    nb <- 2000
    nc <- 3
    
    ## Call JAGS from R (BRT 2 min)
    cjs.group <- jagsUI(aa_jags.data, inits, parameters, "cjs-group.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
    
    ## Summarize posteriors
    print(cjs.group, digits = 3)
    
    par(mfrow=c(1,2), las=1)
    hist(cjs.group$BUGSoutput$sims.list$phi.g)
    ## -------------------
    
    ## Fixed Group and Time Effects: 
    ## -----------------------------
    # Specify model in BUGS language
    ## Two Groups - Larvae or Juveniles:
    ## --------------------
    sink("cjs-2gps.jags")
    cat("
        model {
        # Priors and constraints
        for (i in 1:nind){
          for (t in f[i]:(n.occasions-1)){
            logit(phi[i,t]) <- beta[group[i]] + gamma[t]
            p[i,t] <- p.g[group[i]] 
          } #t
        } #i
        
        # for survival parameters
        for (t in 1:(n.occasions-1)){
          gamma[t] ~ dnorm(0, 0.01)I(-10, 10)                       # Priors for time effects
          # epsilon[t] ~ dnorm(0, tau)
          # phi.g1[t] <- 1 / (1+exp(-gamma[t]-beta[1]*x-epsilon[t]))  # Back-transformed survival of L1J1
          # phi.g2[t] <- 1 / (1+exp(-gamma[t]-beta[2]*x-epsilon[t]))  # Back-transformed survival of L1J3 
        
          phi.g1[t] <- 1 / (1+exp(-gamma[t]))  # Back-transformed survival of L1
          phi.g2[t] <- 1 / (1+exp(-gamma[t]-beta[2]))  # Back-transformed survival of L3
        }
        
        beta[1] <- 0                            # Corner constraint
        beta[2] ~ dnorm(0, 0.01)I(-10, 10)      # Prior for difference in L3 and L1 survival
        
        # for recapture parameters
        for (u in 1:g){
          p.g[u] ~ dunif(0, 1)                 # Priors for group-spec. recapture
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
    
    # Bundle data -- Larvae
    jags.data.L <- list(y=aa_CH, f=f, nind=dim(aa_CH)[1], n.occasions=dim(aa_CH)[2], z=known.state.cjs(aa_CH),
                        g=length(unique(group.L)), group=as.numeric(group.L))
    
    # Initial values -- Larvae
    inits.L <- function(){list(z=cjs.init.z(aa_CH, f), gamma=rnorm(dim(aa_CH)[2]-1), beta=c(NA, rnorm(1)),
                               p.g=runif(length(unique(group.L)), 0, 1))}
    
    # Bundle data -- Juveniles
    jags.data.J <- list(y=aa_CH, f=f, nind=dim(aa_CH)[1], n.occasions=dim(aa_CH)[2], z=known.state.cjs(aa_CH), 
                        g=length(unique(group.J)), group=as.numeric(group.J))
    
    # Initial values -- Juveniles
    inits.J <- function(){list(z=cjs.init.z(aa_CH, f), gamma=rnorm(dim(aa_CH)[2]-1), beta=c(NA, rnorm(1)),
                               p.g=runif(length(unique(group.J)), 0, 1))}  
    
    
    # Parameters monitored
    parameters <- c("phi.g1", "phi.g2", "p.g", "beta")
    
    
    ## MCMC settings
    ni <- 5000
    nt <- 10
    nb <- 2500
    nc <- 3
    
    # Call JAGS from R (BRT 7 min)
    cjs.add.J <- jags(jags.data.J, inits.J, parameters, "cjs-2gps.jags", n.chains=nc, n.thin=nt, n.iter=ni, n.burnin=nb)
    cjs.add.L <- jags(jags.data.L, inits.L, parameters, "cjs-2gps.jags", n.chains=nc, n.thin=nt, n.iter=ni, n.burnin=nb)
    
    
    # Summarize posteriors
    print(cjs.add.J, digits=3)   
    print(cjs.add.L, digist=3)
    
    #create confidence bands for survival?
    lphi.g1 <- uphi.g1 <- lphi.g2 <- uphi.g2 <- lphi.g3 <- uphi.g3 <- lphi.g4 <- uphi.g4 <- numeric()
    for (t in 1:(dim(CH)[2]-1)){
      ## Juvenile Treats
      lphi.g1[t] <- quantile(cjs.add.J$sims.list$phi.g1[,t], 0.025)
      uphi.g1[t] <- quantile(cjs.add.J$sims.list$phi.g1[,t], 0.975)
      lphi.g2[t] <- quantile(cjs.add.J$sims.list$phi.g2[,t], 0.025)
      uphi.g2[t] <- quantile(cjs.add.J$sims.list$phi.g2[,t], 0.975)
      ## Larval Treats
      lphi.g3[t] <- quantile(cjs.add.L$sims.list$phi.g1[,t], 0.025)
      uphi.g3[t] <- quantile(cjs.add.L$sims.list$phi.g1[,t], 0.975)
      lphi.g4[t] <- quantile(cjs.add.L$sims.list$phi.g2[,t], 0.025)
      uphi.g4[t] <- quantile(cjs.add.L$sims.list$phi.g2[,t], 0.975)
    }
    
    #create confidence bands for detection?
    ## Juvenile Treats
    lp.g1 <- quantile(cjs.add.J$sims.list$p.g[,1], 0.025)
    up.g1 <- quantile(cjs.add.J$sims.list$p.g[,1], 0.975)
    lp.g2 <- quantile(cjs.add.J$sims.list$p.g[,2], 0.025)
    up.g2 <- quantile(cjs.add.J$sims.list$p.g[,2], 0.975)
    ## Larval Treats
    lp.g3 <- quantile(cjs.add.L$sims.list$p.g[,1], 0.025)
    up.g3 <- quantile(cjs.add.L$sims.list$p.g[,1], 0.975)
    lp.g4 <- quantile(cjs.add.L$sims.list$p.g[,2], 0.025)
    up.g4 <- quantile(cjs.add.L$sims.list$p.g[,2], 0.975)
    
    #plot detection probability by larval and juvenile groups
    png("AaCOE_Detection-Prob_2-group.png", width3.5, height3.5, units='in', res=600)       ## export plot to file
    
    plot(1:4, c(cjs.add.J$mean$p.g, cjs.add.L$mean$p.g), ylim=c(0.1,0.8), xlim=c(0.8,4.2), 
         pch=c(16,17,1,2), axes=F, xlab="", ylab="Detection Probability",
         col=c("#e08214", "#fdb863", "#542788", "#8073ac"), cex=2)
    axis(1, at= 1:4, labels= rep(NA,2), tcl= -0.25)
    axis(1, at= seq(1,4,1), labels= c("1 Emigration\n Date","3 Emigration\n Dates",
                                      "1 Breeding\n Date","3 Breeding\n Dates"), 
         mgp=c(0,1.8,0))
    axis(2, at=seq(0.1, 0.8, 0.1), labels=c(NA,"0.2",NA,"0.4",NA,"0.6",NA,"0.8"), las=1)
    segments(1, lp.g1, 1, up.g1, col="#e08214", lty=1, lwd=2)
    segments(2, lp.g2, 2, up.g2, col="#fdb863", lty=1, lwd=2)
    segments(3, lp.g3, 3, up.g3, col="#542788", lty=2, lwd=2)
    segments(4, lp.g4, 4, up.g4, col="#8073ac", lty=2, lwd=2)
    points(1:4, c(cjs.add.J$mean$p.g, cjs.add.L$mean$p.g), pch=c(16,17,1,2),col=c("#e08214", "#fdb863", "#542788", "#8073ac"), cex=2)
    dev.off()
    
    png("AaCOE_Apparent-Survival-Prob_2-group-Juv.png", width=5, height=5, units='in', res=600)         
    #png("AaCOE_Apparent-Survival-Prob_2-group.png", width=5, height=5, units='in', res=600)         
    
    plot(lty=2, xlim=c(1,16), bty="n", cex=1.5, axes=FALSE, 
         x=(1:(dim(aa_CH)[2]-1))-0.1, y=cjs.add.J$mean$phi.g1, type="b", col="#e08214", pch=16,
         #x=(1:(dim(CH)[2]-1))-0.1, y=cjs.add.L$mean$phi.g1, type="b", col="#542788", pch=16,
         ylim=c(0,1.0), ylab="Apparent Survival (%)", xlab = "Recapture Event")
    axis(1, at=1:16, labels=rep(NA,17), tcl= -0.25)
    axis(1, at=seq(2,16,2), labels=c("2","4","6","8","10","12","14","16"))
    axis(2, at=seq(0,1,0.1), labels=c("0.0",NA,"0.2",NA,"0.4",NA,"0.6",NA,"0.8",NA,"1.0"), las=1)
    points(x=(1:(dim(aa_CH)[2]-1))+0.1, y=cjs.add.J$mean$phi.g2, type="b", pch=17, lty=1, lwd=1.5, col="#fdb863", cex=1.5)
    points(x=(1:(dim(aa_CH)[2]-1))+0.1, y=cjs.add.L$mean$phi.g2, type="b", pch=17, lty=1, lwd=1.5, col="#8073ac", cex=1.5)
    
    legend(12, 0.25, lty=c(1,2), box.lty=0, title="Phenology Treatment",
           #pch=c(16, 17), col=c("#e08214","#fdb863"), legend=c("1 Breeding Date", "3 Breeding Dates"))
           pch=c(16, 17), col=c("#542788","#8073ac"), legend=c("1 Emigration Date", "3 Emigration Dates"))
    
    # points(as.numeric(as.factor(w.df$Period))[1:16]-0.5, w.df$Precp.mm[1:16]/100, pch=8, cex=1.5, lwd=1.5, col="blue") #, type="b", lty=3)              ## rainfall accumulation in decimeter 
    # axis(4, at=seq(0, 1.4, 0.1), labels=c("0", NA, "2", NA, "4", NA, "6", NA, "8", NA, "10", NA, "12", NA, "14"), las=1)      ## add axis for rainfall
    
    segments((1:(dim(aa_CH)[2]-1))-0.1, lphi.g1, (1:(dim(aa_CH)[2]-1))-0.1, uphi.g1, col="#e08214")
    segments((1:(dim(aa_CH)[2]-1))+0.1, lphi.g2, (1:(dim(aa_CH)[2]-1))+0.1, uphi.g2, col="#fdb863")
    #segments((1:(dim(CH)[2]-1))-0.1, lphi.g3, (1:(dim(CH)[2]-1))-0.1, uphi.g3, col="#542788")
    #segments((1:(dim(CH)[2]-1))+0.1, lphi.g4, (1:(dim(CH)[2]-1))+0.1, uphi.g4, col="#8073ac")
    dev.off()  
    
    L1.pred <- numeric(length(cjs.add.L$mean$phi.g1)+1)
    L1.pred[1] <- 96
    L3.pred <- numeric(length(cjs.add.L$mean$phi.g2)+1)
    L3.pred[1] <- 96
    J1.pred <- numeric(length(cjs.add.J$mean$phi.g1)+1)
    J1.pred[1] <- 96
    J3.pred <- numeric(length(cjs.add.J$mean$phi.g2)+1)
    J3.pred[1] <- 96
    
    for(it in 2:length(J1.pred)){
      #  L1.pred[it] <- L1.pred[it-1] * cjs.add.L$mean$phi.g1[it-1]
      # L3.pred[it] <- L3.pred[it-1] * cjs.add.L$mean$phi.g2[it-1]
      J1.pred[it] <- J1.pred[it-1] * cjs.add.J$mean$phi.g1[it-1]
      J3.pred[it] <- J3.pred[it-1] * cjs.add.J$mean$phi.g2[it-1]
    }
    
    L1.pred <- round(L1.pred, 0)
    L3.pred <- round(L3.pred, 0)
    J1.pred <- round(J1.pred, 0)
    J3.pred <- round(J3.pred, 0)
    
    
    png("AaCOE_Estimated-Survival_2g-Juv.png", width=5, height=5, units='in', res=600)        
    #png("AaCOE_Estimated-Survival_2g-Larv.png", width=5, height=5, units='in', res=600)
    
    plot(type="b", pch=16, lty=2, ylim=c(0,100), ylab="Estimated Number Surviving", xlab="Recapture Event", bty="n", cex=1.5, axes=FALSE,
         # x=(0:(dim(CH)[2]-1))-0.1, y=J1.pred, col="#e08214")   ## 1 date juveniles
         x=(0:(dim(aa_CH)[2]-1))-0.1, y=J1.pred, col="#542788")   ## 1 date larvae
    axis(1, at=0:16, labels=rep(NA,17), tcl= -0.25)
    axis(1, at=seq(0,16,2), labels=c("0","2","4","6","8","10","12","14","16"))
    axis(2, at=seq(0,100,10), labels=c("0",NA,"20",NA,"40",NA,"60",NA,"80",NA,"100"), las=1)
    # points(x=(0:(dim(CH)[2]-1))+0.1, y=L3.pred, type="b", pch=17, lty=1, col="#fdb863", cex=1.5)
    points(x=(0:(dim(aa_CH)[2]-1))+0.1, y=J3.pred, type="b", pch=17, lty=1, col="#8073ac", cex=1.5)
    
    legend(12, 95, lty=c(1,2), box.lty=0, title="Phenology Treatment",
           # pch=c(16, 17), col=c("#e08214","#fdb863"), legend=c("1 Breeding Date", "3 Breeding Dates"))
           pch=c(16, 17), col=c("#542788","#8073ac"), legend=c("1 Emigration Date", "3 Emigration Dates"))
    # 
    
    # points(as.numeric(as.factor(w.df$Period))[1:16]-0.5, w.df$Precp.mm[1:16]/10, pch=8, cex=1.5, col="blue") #, type="b", lty=3)              ## rainfall accumulation in centimeter 
    # lines(as.numeric(as.factor(w.df$Period))[1:16]-0.4, w.df$Temp.Avg[1:16], pch=42, cex=2, col="red", type="b", lty=3)                                  ## average temp in degrees C
    # axis(4, at=seq(0, 20, 2), labels=c("0", NA, "4", NA, "8", NA, "12", NA, "16", NA, "20"), las=1)      ## add axis for rainfall
    dev.off()
    
    
    ## --------------------
    
    ## Four Groups:
    ## ------------
    sink("cjs-4groups.jags")
    cat("
        model {
        
        # Priors and constraints
        for (i in 1:nind){
          for (t in f[i]:(n.occasions-1)){
            logit(phi[i,t]) <- beta[group[i]] + gamma[t]
            p[i,t] <- p.g[group[i]] 
          } #t
        } #i
        
        # for survival parameters
        for (t in 1:(n.occasions-1)){
          gamma[t] ~ dnorm(0, 0.01)I(-10, 10)                       # Priors for time effects
          # epsilon[t] ~ dnorm(0, tau)
          # phi.g1[t] <- 1 / (1+exp(-gamma[t]-beta[1]*x-epsilon[t]))  # Back-transformed survival of L1J1
          # phi.g2[t] <- 1 / (1+exp(-gamma[t]-beta[2]*x-epsilon[t]))  # Back-transformed survival of L1J3 
          # phi.g3[t] <- 1 / (1+exp(-gamma[t]-beta[3]*x-epsilon[t]))  # Back-transformed survival of L3J1 
          # phi.g4[t] <- 1 / (1+exp(-gamma[t]-beta[4]*x-epsilon[t]))  # Back-transformed survival of L3J3
        
          phi.g1[t] <- 1 / (1+exp(-gamma[t]))  # Back-transformed survival of L1J1
          phi.g2[t] <- 1 / (1+exp(-gamma[t]-beta[2]))  # Back-transformed survival of L1J3
          phi.g3[t] <- 1 / (1+exp(-gamma[t]-beta[3]))  # Back-transformed survival of L3J1
          phi.g4[t] <- 1 / (1+exp(-gamma[t]-beta[4]))  # Back-transformed survival of L3J3
        }
        
        beta[1] <- 0                            # Corner constraint
        beta[2] ~ dnorm(0, 0.01)I(-10, 10)      # Prior for difference in J1L3 and J1L1 survival
        beta[3] ~ dnorm(0, 0.01)I(-10, 10)      # Prior for difference in J3L1 and J1L1 survival
        beta[4] ~ dnorm(0, 0.01)I(-10, 10)      # Prior for difference in J3L3 and J1L1 survival
        
        # for recapture parameters
        for (u in 1:g){
          p.g[u] ~ dunif(0, 1)                 # Priors for group-spec. recapture
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
    jags.data <- list(y=CH, f=f, nind=dim(CH)[1], n.occasions=dim(CH)[2], z=known.state.cjs(CH), 
                      g=length(unique(group)), group=as.numeric(group))
    
    # Initial values
    inits <- function(){list(z=cjs.init.z(CH, f), gamma=rnorm(dim(CH)[2]-1), beta=c(NA, rnorm(1), rnorm(1), rnorm(1)),
                             p.g=runif(length(unique(group)), 0, 1))}  
    
    # Parameters monitored
    parameters <- c("phi.g1", "phi.g2", "phi.g3", "phi.g4", "p.g", "beta")
    ## ------------
    
    # MCMC settings
    ni <- 100000
    nt <- 25
    nb <- 50000
    nc <- 3
    
    # Call JAGS from R (BRT 7 min)
    cjs.add <- jags(jags.data, inits, parameters, "cjs-4groups.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
    
    # Summarize posteriors
    print(cjs.add, digits = 3)   
    
    # Figure of survival by treatment
    lower.g1 <- upper.g1 <- lower.g2 <- upper.g2 <- lower.g3 <- upper.g3 <- lower.g4 <- upper.g4 <- numeric()
    for (t in 1:(dim(CH)[2]-1)){
      lower.g1[t] <- quantile(cjs.add$sims.list$phi.g1[,t], 0.025)
      upper.g1[t] <- quantile(cjs.add$sims.list$phi.g1[,t], 0.975)
      lower.g2[t] <- quantile(cjs.add$sims.list$phi.g2[,t], 0.025)
      upper.g2[t] <- quantile(cjs.add$sims.list$phi.g2[,t], 0.975)
      lower.g3[t] <- quantile(cjs.add$sims.list$phi.g3[,t], 0.025)
      upper.g3[t] <- quantile(cjs.add$sims.list$phi.g3[,t], 0.975)
      lower.g4[t] <- quantile(cjs.add$sims.list$phi.g4[,t], 0.025)
      upper.g4[t] <- quantile(cjs.add$sims.list$phi.g4[,t], 0.975)
    }
    
    
    lp.g1 <- quantile(cjs.add$sims.list$p.g[,1], 0.025)
    up.g1 <- quantile(cjs.add$sims.list$p.g[,1], 0.975)
    lp.g2 <- quantile(cjs.add$sims.list$p.g[,2], 0.025)
    up.g2 <- quantile(cjs.add$sims.list$p.g[,2], 0.975)
    lp.g3 <- quantile(cjs.add$sims.list$p.g[,3], 0.025)
    up.g3 <- quantile(cjs.add$sims.list$p.g[,3], 0.975)
    lp.g4 <- quantile(cjs.add$sims.list$p.g[,4], 0.025)
    up.g4 <- quantile(cjs.add$sims.list$p.g[,4], 0.975)
    
    
    png("AaCOE_Detection-Probability-Pilot.png", width=3500, height=2500, units='px', res=400)       ## export plot to file
    
    plot(1:4, cjs.add$mean$p.g, ylim=c(0.1,0.8), xlim=c(0.8, 4.2), pch=c(16,17,1,2), axes=F, 
         xlab="", ylab="Detection Probability",
         col=c("#8c510a", "#d8b365", "#01665e", "#5ab4ac"), cex=1.25)
    axis(1, at = 1:4, labels = rep(NA,4), tcl = -0.25)
    axis(1, at = seq(1,4,1), labels = c("1 Breeding, \n 1 Emigration \n Dates","1 Breeding, \n 3 Emigration \n Dates",
                                        "3 Breeding, \n 1 Emigration \n Dates","3 Breeding, \n 3 Emigration \n Dates"), 
         mgp=c(0,2.5,0))
    axis(2, at = seq(0.1, 0.8, 0.1), labels = c(NA, "0.2", NA, "0.4", NA, "0.6", NA, "0.8"), las = 1)
    segments(1, lp.g1, 1, up.g1, col= "#8c510a", lty=1, lwd=1.5)
    segments(2, lp.g2, 2, up.g2, col= "#d8b365", lty=2, lwd=1.5)
    segments(3, lp.g3, 3, up.g3, col= "#01665e", lty=1, lwd=1.5)
    segments(4, lp.g4, 4, up.g4, col= "#5ab4ac", lty=2, lwd=1.5)
    dev.off()
    
    surv.plotdat<-data.frame(Phi=c(cjs.add$mean$phi.g1,
                                            cjs.add$mean$phi.g2,
                                            cjs.add$mean$phi.g3,
                                            cjs.add$mean$phi.g4),
          Trts=rep(c("1 Breeding, 1 Emigration", 
                     "1 Breeding, 3 Emigration", 
                     "3 Breeding, 1 Emigration",
                     "3 Breeding, 3 Emigration"),each=length(cjs.add$mean$phi.g1)),
          Days=rep(c(1:(dim(CH)[2]-1)-0.05),4))
    cbbPalette <- c("#D55E00","#E69F00", "#009E73", "#0072B2")
    png("SurvivalProbability.png",res=600,width=5,height=5,units="in")
    ggplot(surv.plotdat,aes(Days,Phi,color=Trts,shape=Trts,linetype=Trts))+
      geom_point(size=3)+
      geom_line(aes(group=Trts),size=1.5)+
      scale_linetype_manual(values=1:4,name="Treatment")+
      scale_shape_manual(name = "Treatment",values=c(16,17,1,2))+
      scale_color_manual(name = "Treatment",
                         values = cbbPalette)+      
      scale_x_continuous(breaks=1:length(period_dates$dayMonth),labels=period_dates$dayMonth)+
      labs(y = "Survival probability", x = "Recapture Date",color="Legend")+
      my_theme2()+
      lims(y=c(0,1))+
      theme(axis.text.x=element_text(angle=45,hjust=1),legend.position=c(0.4,0.2))
    dev.off()
    
    plot(x=(1:(dim(CH)[2]-1))-0.05, y = cjs.add$mean$phi.g1, type = "b", lty=2, pch = 16, xlim=c(0,16), ylim = c(0, 1.4), col="#8c510a",
         ylab = "Survival probability", xlab = "Recapture Event", bty = "n", cex = 1.5, axes = FALSE)
    axis(1, at = 0:16, labels = rep(NA,17), tcl = -0.25)
    axis(1, at = seq(2,16,2), labels = c("2","4","6","8","10","12","14","16"))
    axis(2, at = seq(0, 1, 0.1), labels = c("0.0", NA, "0.2", NA, "0.4", NA, "0.6", NA, "0.8", NA, "1.0"), las = 1)
    points(x = (1:(dim(CH)[2]-1))-0.2, y = cjs.add$mean$phi.g2, type = "b", pch = 17, lty = 1, lwd=1.5, col="#d8b365", cex = 1.5)
    points(x = (1:(dim(CH)[2]-1))+0.05, y = cjs.add$mean$phi.g3, type = "b", pch = 1, lty = 2, lwd=1.5, col="#01665e", cex = 1.5)
    points(x = (1:(dim(CH)[2]-1))+0.2, y = cjs.add$mean$phi.g4, type = "b", pch = 2, lty = 1, lwd=1.5, col="#5ab4ac", cex = 1.5)
    points(as.numeric(as.factor(wet.df$Period))[1:16]-0.5, wet.df$Precp.mm[1:16]/100, pch=8, cex=1.5, lwd=1.5, col="blue") #, type="b", lty=3)              ## rainfall accumulation in decimeter 
    
    axis(4, at=seq(0, 1.4, 0.1), labels=c("0", NA, "2", NA, "4", NA, "6", NA, "8", NA, "10", NA, "12", NA, "14"), las=1)      ## add axis for rainfall
    # segments((1:(dim(CH)[2]-1))-0.1, lower.g1, (1:(dim(CH)[2]-1))-0.1, upper.g1)
    # segments((1:(dim(CH)[2]-1))+0.1, lower.g2, (1:(dim(CH)[2]-1))+0.1, upper.g2)
    # segments((1:(dim(CH)[2]-1))-0.2, lower.g3, (1:(dim(CH)[2]-1))-0.2, upper.g3)
    # segments((1:(dim(CH)[2]-1))+0.2, lower.g4, (1:(dim(CH)[2]-1))+0.2, upper.g4)
    
    
    J1.pred <- numeric(length(cjs.add$mean$phi.g1)+1)
    J1.pred[1] <- 96
    J3.pred <- numeric(length(cjs.add$mean$phi.g2)+1)
    J3.pred[1] <- 96
    
    for(it in 2:length(J1.pred)){
      J1.pred[it] <- J1.pred[it-1] * cjs.add$mean$phi.g1[it-1]
      J3.pred[it] <- J3.pred[it-1] * cjs.add$mean$phi.g2[it-1]
    }
    
    J1.pred <- round(J1.pred, 0)
    J3.pred <- round(J3.pred, 0)
    
    
    L1J1.pred <- numeric(length(cjs.add$mean$phi.g1)+1); L1J1.pred[1] <- 48
    L1J3.pred <- numeric(length(cjs.add$mean$phi.g2)+1); L1J3.pred[1] <- 48
    L3J1.pred <- numeric(length(cjs.add$mean$phi.g3)+1); L3J1.pred[1] <- 48
    L3J3.pred <- numeric(length(cjs.add$mean$phi.g4)+1); L3J3.pred[1] <- 48
    
    for(it in 2:length(L1J1.pred)){
      L1J1.pred[it] <- L1J1.pred[it-1] * cjs.add$mean$phi.g1[it-1]
      L1J3.pred[it] <- L1J3.pred[it-1] * cjs.add$mean$phi.g2[it-1]
      L3J1.pred[it] <- L3J1.pred[it-1] * cjs.add$mean$phi.g3[it-1]
      L3J3.pred[it] <- L3J3.pred[it-1] * cjs.add$mean$phi.g4[it-1]
    }
    
    L1J1.pred <- round(L1J1.pred, 0)
    L1J3.pred <- round(L1J3.pred, 0)
    L3J1.pred <- round(L3J1.pred, 0)
    L3J3.pred <- round(L3J3.pred, 0)
    
    
    
    plot(x=(0:(dim(CH)[2]-1))-0.07, y = L1J1.pred, type = "b", pch = 16, lty=2, ylim = c(0, 50), col="#8c510a",
         ylab = "Estimated Number Surviving", xlab = "Recapture Event", bty = "n", cex = 1.5, axes = FALSE)
    axis(1, at = 0:16, labels = rep(NA,17), tcl = -0.25)
    axis(1, at = seq(0,16,2), labels = c("0", "2","4","6","8","10","12","14","16"))
    axis(2, at = seq(0, 50, 5), labels = c("0", NA, "10", NA, "20", NA, "30", NA, "40", NA, "50"), las = 1)
    points(x = (0:(dim(CH)[2]-1))-0.2, y = L1J3.pred, type = "b", pch = 17, lty = 1, col="#d8b365", cex = 1.5)
    points(x = (0:(dim(CH)[2]-1))+0.07, y = L3J1.pred, type = "b", pch = 1, lty = 2, col="#01665e", cex = 1.5)             
    points(x = (0:(dim(CH)[2]-1))+0.2, y = L3J3.pred, type = "b", pch = 2, lty = 1, col="#5ab4ac", cex = 1.5)
    
    points(as.numeric(as.factor(wet.df$Period))[1:16]-0.5, wet.df$Precp.mm[1:16]/10, pch=8, cex=1.5, col="blue") #, type="b", lty=3)              ## rainfall accumulation in centimeter 
    lines(as.numeric(as.factor(wet.df$Period))[1:16]-0.4, wet.df$Temp.Avg[1:16], pch=42, cex=2, col="red", type="b", lty=3)                                  ## average temp in degrees C
    axis(4, at=seq(0, 20, 2), labels=c("0", NA, "4", NA, "8", NA, "12", NA, "16", NA, "20"), las=1)      ## add axis for rainfall
    
    
    ## Export Plot:
    ## ------------
    # png("C:/Users/Jacob/Box Sync/FLW_Phenology/Terrestrial_Pens/AaCOE_Survival-Estimates-Pilot.png", width=2500, height=3500, units='px', res=400)       ## export plot to file
    #   par(mfrow=c(2,1))
    #   ## Survival Probability
    #     plot(x=(1:(dim(CH)[2]-1))-0.1, y = cjs.add$mean$phi.g1, type = "b", pch = 16, xlim=c(0,16), ylim = c(0.15, 1),
    #          ylab = "Survival Probability", xlab = "", bty = "n", cex = 1.5, axes = FALSE)
    #     axis(1, at = 0:16, labels = rep(NA,17), tcl = -0.25)
    #     axis(1, at = seq(2,16,2), labels = c("2","4","6","8","10","12","14","16"))
    #     axis(2, at = seq(0, 1, 0.1), labels = c("0.0", NA, "0.2", NA, "0.4", NA, "0.6", NA, "0.8", NA, "1.0"), las = 1)
    #     segments((1:(dim(CH)[2]-1))-0.1, lower.f, (1:(dim(CH)[2]-1))-0.1, upper.f)
    #     points(x = (1:(dim(CH)[2]-1))+0.1, y = cjs.add$mean$phi.g2, type = "b", pch = 1, lty = 2, cex = 1.5)
    #     segments((1:(dim(CH)[2]-1))+0.1, lower.m, (1:(dim(CH)[2]-1))+0.1, upper.m)
    #   
    #   ## Estimated # Surviving
    #     plot(x=(0:(dim(CH)[2]-1))-0.1, y = J1.pred, type = "b", pch = 16, ylim = c(0, 100), ylab = "Estimated Number Surviving", xlab = "Recapture Event", bty = "n", cex = 1.5, axes = FALSE)
    #       axis(1, at = 0:16, labels = rep(NA,17), tcl = -0.25)
    #       axis(1, at = seq(2,16,2), labels = c("2","4","6","8","10","12","14","16"))
    #       axis(2, at = seq(0, 100, 10), labels = c("0", NA, "20", NA, "40", NA, "60", NA, "80", NA, "100"), las = 1)
    #       # segments((1:(dim(CH)[2]-1))-0.1, lower.f, (1:(dim(CH)[2]-1))-0.1, upper.f)
    #       points(x = (0:(dim(CH)[2]-1))+0.1, y = J3.pred, type = "b", pch = 1, lty = 2, cex = 1.5)
    #       # segments((1:(dim(CH)[2]-1))+0.1, lower.m, (1:(dim(CH)[2]-1))+0.1, upper.m)
    # dev.off() 
    ## ------------
    
    ## -----------------------------
    
    
    ## Fixed Groups and Random Time:
    ## -----------------------------
    # Specify model in BUGS language
    sink("cjs-FGRT.jags")
    cat("
        model {
        # Priors and constraints
        for (i in 1:nind){
          for (t in f[i]:(n.occasions-1)){
            logit(phi[i,t]) <- eta.phi[t,group[i]]
            p[i,t] <- p.g[group[i]]
          } #t
        } #i
        
        # for survival parameters
        for (t in 1:(n.occasions-1)){
          eta.phi[t,1:g] ~ dmnorm(mu.phi[], Omega[,])
        } #t
        
        for (u in 1:g){      
          mean.phi[u] ~ dunif(0, 1)    # Priors on mean group-spec. survival
          mu.phi[u] <- log(mean.phi[u] / (1-mean.phi[u]))
        } #g
        
        Omega[1:g, 1:g] ~ dwish(R[,], df)  # Priors for variance-covariance matrix
        Sigma[1:g, 1:g] <- inverse(Omega[,])
        
        # for recapture parameters
        for (u in 1:g){
          p.g[u] ~ dunif(0, 1)            # Priors for group-spec. recapture
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
    # jags.data <- list(y = CH, f = f, nind = dim(CH)[1], n.occasions = dim(CH)[2], z = known.state.cjs(CH), g = length(unique(group)), group = as.numeric(group), R=)
    # 
    jags.data <- list(y = CH, f = f, nind = dim(CH)[1], n.occasions = dim(CH)[2], z = known.state.cjs(CH), g = length(unique(group)), group = as.numeric(group), 
                      R = matrix(c(2, 0, 0, 0, 0, 3, 0, 0, 0, 0, 3, 0, 0, 0, 0, 2), ncol = length(unique(group))), df = length(unique(group))+1)
    
    # Initial values
    inits <- function(){list(z = cjs.init.z(CH, f), p.g = runif(length(unique(group)), 0, 1), Omega = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), ncol = length(unique(group))))}  
    
    # Parameters monitored
    parameters <- c("eta.phi", "p.g", "Sigma", "mean.phi")
    
    
    # MCMC settings
    ni <- 10000
    nt <- 5
    nb <- 1000
    nc <- 3
    
    # Call JAGS from R (BRT 7 min)
    cjs.fgrt <- jags(jags.data, inits, parameters, "cjs-FGRT.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
    
    # Summarize posteriors
    print(cjs.fgrt, digits = 3) 
    
    
## -----------------
    #ARIANNE'S Full Model
    #5. Phi(.*g+mass+block+pen)P(t*g+cov+block+pen): 
      # FULL MODEL
      # Treatment effect
      # Survival by mass
      # Temperature and precipitation covariates (not yet)
      # Time-dependent recapture and time-constant survival 
      # Grand means
      # With immediate trap response 
    
    group_aa<-as.numeric(as.factor(aa_ch.pa$Treatment))
    block_aa<-as.numeric(aa_ch.pa$Block)
    pen_aa<-as.numeric(as.factor(paste(aa_ch.pa$Block,aa_ch.pa$Pen,sep="")))
    
    
    # abiotic <- read.table("Abiotic.txt",header=T,colClasses="character")
    # str(abiotic)
    # cor.test(as.numeric(abiotic$tempc), as.numeric(abiotic$precip)) #Not autocorrelated
    # 
    # abiotic$tempc <- as.numeric(abiotic$tempc)
    # stdtempc<-rep(NA,length(abiotic$tempc))
    # abiotic$precip <- as.numeric(abiotic$precip)
    # stdprecip<-rep(NA,length(abiotic$precip))
    # abiotic$temp.sd <- as.numeric(abiotic$temp.sd)
    # stdtempsd<-rep(NA,length(abiotic$temp.sd))
    # 
    # #Scale temp. and precip. covariates
    # for (i in 1:length(abiotic$tempc)) {
    #   stdtempc[i] <- (abiotic$tempc[i]-mean(abiotic$tempc[]))/sd(abiotic$tempc[])
    #   stdtempsd[i] <- (abiotic$temp.sd[i]-mean(abiotic$temp.sd[]))/sd(abiotic$temp.sd[])
    #   stdprecip[i] <- (abiotic$precip[i]-mean(abiotic$precip[]))/sd(abiotic$precip[])
    # }
    # 
    #amb$mass <- as.numeric(aa_$mass)
    aa_ch.pa$Meta.Mass[is.na(aa_ch.pa$Meta.Mass)]<-mean(aa_ch.pa$Meta.Mass,na.rm=T)
    stdmass_aa<-rep(NA,length(aa_ch.pa$Meta.Mass))
    
    #Scale mass covariate
    for (i in 1:length(aa_ch.pa$Meta.Mass)) {
      stdmass_aa[i] <- (aa_ch.pa$Meta.Mass[i]-mean(aa_ch.pa$Meta.Mass[]))/sd(aa_ch.pa$Meta.Mass[])
    }
    
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
    
    # Specify model in BUGS language
    sink("amb-cjs-trt-mass-cov-rand.jags")
    cat("
    model {
    
    ## Priors and constraints
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        phi[i,t] <- (1/(1+exp(-(mean.phi + beta.a[group[i]] + beta.b*mass[i] + beta.c[block[i]]  + beta.d[pen[i]] + epsilon.phi[i])))) ^int[t] 
        p[i,t] <- 1/(1+exp(-(mean.p + beta.m[m[i,t]] + beta.e[group[i],t] + beta.h[block[i],t] + beta.j[pen[i],t] +  epsilon.p[t]))) #+ beta.f*temp[t] + beta.g*precip[t] 
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
    #beta.f ~ dnorm(0, 0.001)I(-10, 10)         # Prior for temp slope parameter
    #beta.g ~ dnorm(0, 0.001)I(-10, 10)         # Prior for precip slope parameter

    
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
    jags.data <- list(y = aa_CH, int=interval_aa$int, f = f_aa, nind = dim(aa_CH)[1], n.occasions = dim(aa_CH)[2], z = known.state.cjs(aa_CH), 
                      nblock = length(unique(block_aa)), block = as.numeric(block_aa), npen = length(unique(pen_aa)), pen = as.numeric(pen_aa),
                      mass=stdmass_aa, g = length(unique(group_aa)), group=group_aa, m=m_aa)
                      #temp = stdtempc, precip = stdprecip, )
    
    # Initial values (probably need to adjust thse to match dimensions of certain parameters)
    inits <- function(){list(z = cjs.init.z(aa_CH,f_aa), 
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
    parameters <- c("mu.phi", "mean.phi", "beta.b", "mu.p", "mean.p", "beta.m", "sigma2.phi", "sigma2.p", "beta.f","beta.g", "beta.a", "sigma2.beta.c", "sigma2.beta.d", "sigma2.beta.h", "sigma2.beta.j", "beta.c", "beta.d", "beta.e", "beta.h", "epsilon.phi", "epsilon.p", "phi", "p") #"beta.j", 
    
    # MCMC settings
    ni <- 500
    nt <- 10
    nb <- 300
    nc <- 3
    
    # Call JAGS from R (JRT 55 min)
    amb.cjs.trt.mass.cov.rand <- jags(jags.data, parallel=TRUE, inits, parameters, "amb-cjs-trt-mass-cov-rand.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
    

