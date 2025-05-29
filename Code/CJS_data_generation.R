#Data Generation for CJS Models

if(!require(tidyr)) install.packages('tidyr'); library("tidyr")
if(!require(readxl)) install.packages('readxl'); library("readxl")
if(!require(ggplot2)) install.packages('ggplot2'); library("ggplot2")
if(!require(dplyr)) install.packages('dplyr'); library("dplyr")
if(!require(viridis)) install.packages('viridis'); library("viridis")
library(lattice)
library(coda)
library(jagsUI)
library(mcmcplots)
library(lubridate)

## A. annulatum --------------------------------

##Load Data
aa_recaps <- read.csv("~/GitRepos/JuvenileEmigrationPhenology/Data/AMAN/AMAN_phenology_recap_data_master.csv", header=T)           ## recapture data
aa_assign <- read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Pen_Assignments.xlsx")             ## initial pen assignments
aa_treats <- read.csv("~/GitRepos/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Treatments.csv", header=T)           ## treatment data
aa_tagged <- read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMAN/Aa_COEffects_Tagged_Animals.xlsx")              ## all tagged animals and metamorphosis sizes

## Format/Merge Data:
## ------------------
doy.adj <- 172 ## DOY value to standardize values to June 21, 2018

aa_recaps <- subset(aa_recaps, aa_recaps$Notes != "Arianne Salamander") #remove capture of arianne's salamanders

#separate out block, pen and microhabitat
aa_recaps$Re.Block <- NULL
aa_recaps$Re.Pen <- NULL
aa_recaps$Re.Micro <- NULL

for(i in 1:dim(aa_recaps)[1]) {
  aa_recaps$Re.Block[i] <- unlist(strsplit(aa_recaps$Recap_Loc[i], ","))[1]
  aa_recaps$Re.Pen[i] <- unlist(strsplit(aa_recaps$Recap_Loc[i], ","))[2]
  aa_recaps$Re.Micro[i] <- unlist(strsplit(aa_recaps$Recap_Loc[i], ","))[3]
}

#format dates
aa_recaps$DOY <- as.numeric(format(as.Date(as.character(aa_recaps$Recap_Date),"%m/%d/%Y"),"%j"))
aa_recaps$DOY_adj <- ifelse(aa_recaps$DOY >= doy.adj, yes=aa_recaps$DOY-(doy.adj-1), no=aa_recaps$DOY+(365-doy.adj+1))
aa_recaps$DOY_adj[aa_recaps$Recap_Date=="7/6/2019"]<-365+aa_recaps$DOY_adj[aa_recaps$Recap_Date=="7/6/2019"] #adjust one value that spanned years
aa_recaps$Rel.DOY <- as.numeric(format(as.Date(as.character(aa_recaps$Release),"%m/%d/%Y"),"%j"))
aa_recaps$Rel.DOY_adj <- ifelse(aa_recaps$Rel.DOY >= doy.adj, yes=aa_recaps$Rel.DOY-(doy.adj-1), no=recaps$Rel.DOY+(365-doy.adj+1))
aa_recaps$Recap_Loc <- as.character(aa_recaps$Recap_Loc)

aa_assign$Release_DOY <- as.numeric(format(as.Date(as.character(aa_assign$Release_Date), "%Y-%m-%d"),"%j"))
aa_assign$DOY_adj <- ifelse(aa_assign$Release_DOY >= doy.adj, yes=aa_assign$Release_DOY-(doy.adj-1), no=aa_assign$Release_DOY+(365-doy.adj+1))

aa_tagged$Meta.DOY <- as.numeric(format(as.Date(as.character(aa_tagged$Meta.Date), "%Y-%m-%d"),"%j"))
aa_tagged$Meta.DOY_adj <- ifelse(aa_tagged$Meta.DOY >= doy.adj, yes=aa_tagged$Meta.DOY-(doy.adj-1), no=aa_tagged$Meta.DOY+(365-doy.adj+1))
aa_tagged$Tag.DOY <- as.numeric(format(as.Date(as.character(aa_tagged$Tag.Date), "%Y-%m-%d"),"%j"))
aa_tagged$Tag.DOY_adj <- ifelse(aa_tagged$Tag.DOY >= doy.adj, yes=aa_tagged$Tag.DOY-(doy.adj-1), no=aa_tagged$Tag.DOY+(365-doy.adj+1))

#merge all data sets
df.a <- merge(aa_assign, aa_tagged, by="PIT_Tag", type="left")
df.a$Days.Held<-df.a$Release_DOY-df.a$Meta.DOY
df.a$Days.Held<-ifelse(is.na(df.a$Days.Held),mean(df.a$Days.Held,na.rm=T),df.a$Days.Held)
aa_df <- merge(aa_recaps, df.a, by="PIT_Tag", type="left")

aa_df$Moved <- ifelse(is.na(aa_df$Moved)==T, yes=0, no=aa_df$Moved)
aa_df$Pre.Alive <- aa_df$Moved + aa_df$Pre_Alive
aa_df$Pre.Alive <- ifelse(aa_df$Pre.Alive >= 1, yes=1, no=0)

## create a matrix of zeros with # inds X # periods dimensions
## loop over individuals, populate the cells with data
## concatenate data on the back end to create the capture history cells

## Create Capture History Matrix (messy but works)
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
aa_ch.pa<-merge(aa_ch.pa,df.a[,c("PIT_Tag","Meta.Mass","Meta.Date","Days.Held")],by=c("PIT_Tag"))
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

# Format AA predictor data ---------------------------------------------------

#define groups, blocks and pens
aa_ch.pa$Treatment2<-ifelse(aa_ch.pa$Treatment=="L1J1", "J1", aa_ch.pa$Treatment) #Create juvenile-only treatment factor
aa_ch.pa$Treatment2<-ifelse(aa_ch.pa$Treatment=="L3J1", "J1", aa_ch.pa$Treatment2)
aa_ch.pa$Treatment2<-ifelse(aa_ch.pa$Treatment=="L3J3", "J3", aa_ch.pa$Treatment2)
aa_ch.pa$Treatment2<-ifelse(aa_ch.pa$Treatment=="L1J3", "J3", aa_ch.pa$Treatment2)
group2_aa<-as.numeric(as.factor(aa_ch.pa$Treatment2))
#group_aa<-as.numeric(as.factor(aa_ch.pa$Treatment)) # 4 group predictor variable

#get pen, block and release date as vectors
block_aa<-as.numeric(aa_ch.pa$Block)
pen_aa<-as.numeric(as.factor(paste(aa_ch.pa$Block,aa_ch.pa$Pen,sep="")))
rel_aa<-as.numeric(as.factor(aa_ch.pa$Release))

#define abiotic covariates
aa_abiotic <- readRDS("~/GitRepos/JuvenileEmigrationPhenology/aa_abiotic.rds")
#aa_abiotic$propMax <- c(0.14, 0.27, 0.33, rep(0,13)) #add row of proportion of previous interval days with max temp. above 35C
str(aa_abiotic)
cor.test(as.numeric(aa_abiotic$Tmin), as.numeric(aa_abiotic$Prcp)) #Not autocorrelated

aa_abiotic$Tmin <- as.numeric(aa_abiotic$Tmin)
aa_stdtempc<-rep(NA,length(aa_abiotic$Tmin))
aa_abiotic$Prcp <- as.numeric(aa_abiotic$Prcp)
aa_stdprecip<-rep(NA,length(aa_abiotic$Prcp))
aa_stdprecip<-rep(NA,length(aa_abiotic$Prcp))
#aa_stdpropMax<-rep(NA,length(aa_abiotic$propMax))
#aa_reachMax<-as.numeric(factor(c(1, 1, 1, rep(0,13)))) #factor indicating whether CTmax (max temp. above 35C) reached in previous interval days
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
  #aa_stdtempsd[i] <- (aa_abiotic$temp.sd[i]-mean(aa_abiotic$temp.sd[]))/sd(aa_abiotic$temp.sd[])
  aa_stdprecip[i] <- (aa_abiotic$log.Prcp[i]-mean(aa_abiotic$log.Prcp[]))/sd(aa_abiotic$log.Prcp[])
}

#define body mass covariate
#amb$mass <- as.numeric(aa_$mass)
aa_ch.pa$Meta.Mass[is.na(aa_ch.pa$Meta.Mass)]<-mean(aa_ch.pa$Meta.Mass,na.rm=T)
stdmass_aa<-rep(NA,length(aa_ch.pa$Meta.Mass))

#Scale mass covariate
for (i in 1:length(aa_ch.pa$Meta.Mass)) {
  stdmass_aa[i] <- (aa_ch.pa$Meta.Mass[i]-mean(aa_ch.pa$Meta.Mass[]))/sd(aa_ch.pa$Meta.Mass[])
}

#define days held covariate
aa_ch.pa$Days.Held[is.na(aa_ch.pa$Days.Held)]<-mean(aa_ch.pa$Days.Held,na.rm=T)
stddaysheld_aa<-rep(NA,length(aa_ch.pa$Days.Held))

#Scale days held covariate
for (i in 1:length(aa_ch.pa$Days.Held)) {
  stddaysheld_aa[i] <- (aa_ch.pa$Days.Held[i]-mean(aa_ch.pa$Days.Held[]))/sd(aa_ch.pa$Days.Held[])
}


# A. opacum data -----------------------------------------------------------

#Load data
ao_recaps <- read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMOP/Recap_Database_2019-2020-Experiments_Master_20200125.xlsx", sheet=2,na=c("NA", ""))## recapture data
ao_penID <- read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMOP/Pens_Assignments_2019-2020-Experiments.xlsx", na=c("NA", ""))
ao_endfates<-read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMOP/BreakdownFates_2019-2020-Experiments.xlsx",na=c("NA",""))
ao_sex<-read_excel("~/GitRepos/JuvenileEmigrationPhenology/Data/AMOP/Siegel Salamander data.xlsx",na=c("NA",""))
ao_endfates<-merge(ao_endfates,ao_sex,by="PIT_Tag")
ao_endfates<-merge(ao_endfates,ao_penID,by=c("PIT_Tag","Juv.Treat","Juv.Pen","Treatment","Species"))

## Format/Merge Data:
## ------------------
DOY.adj <- 157                           ## Standardize DOY to date of first release- 07 June 2019

ao_recaps<-ao_recaps%>%
  #mutate(mutate(across(where(is.character), as.factor)))%>%
  filter(!grepl("^UNK.",PIT_Tag))%>% #untagged animals we caught
  filter(!grepl("Arianne",Notes))%>% #arianne's animals
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
ao_recaps<-ao_recaps%>%
  mutate(DOY_adj1=if_else(Recap_Date>"2020-06-06",DOY_adj+365,DOY_adj))

ao_penID<-ao_penID%>%
  mutate(Days.Held=yday(ao_penID$Release.Date)-yday(ao_penID$Meta.Date))%>%
  select("PIT_Tag","Species","Juv.Pen","Juv.Treat","Meta.Mass.g", "Tag.Mass","Rel.Cohort","Days.Held")%>%
  mutate(Rel.Block = sapply(strsplit(Juv.Pen, ","), function(x) x[1]),
         Rel.Pen = sapply(strsplit(Juv.Pen, ","), function(x) x[2]))%>%
  mutate(across(where(is.character), as.factor))

#define treatments
ao_penID$Treatment2<-ifelse(ao_penID$Juv.Treat=="L1-J1", "J1", ao_penID$Juv.Treat) #Create juvenile-only treatment factor
ao_penID$Treatment2<-ifelse(ao_penID$Juv.Treat=="L3-J1", "J1", ao_penID$Treatment2)
ao_penID$Treatment2<-ifelse(ao_penID$Juv.Treat=="L3-J3", "J3", ao_penID$Treatment2)
ao_penID$Treatment2<-ifelse(ao_penID$Juv.Treat=="L1-J3", "J3", ao_penID$Treatment2)

ao_df <- merge(ao_recaps, ao_penID, by=c("PIT_Tag"),all.x=T)
# df<-df%>%
#   mutate(Species=Species.y)%>%
#   select(!c(Species.x,Species.y))

ao_df<- ao_df%>%
  filter(as.factor(Juv.Treat)%in%c("L3-J1","L3-J3","L1-J1","L1-J3"))

#metamorph sizes used in terrestrial pens
ao_mass<-ao_penID%>%filter(as.factor(Juv.Treat)%in%c("L3-J1","L3-J3","L1-J1","L1-J3"))%>%
  group_by(Juv.Treat)%>%
  summarise(meanMass=mean(Meta.Mass.g,na.rm=T),
            sdMass=sd(Meta.Mass.g,na.rm=T),
            minMass=min(Meta.Mass.g),
            maxMass=max(Meta.Mass.g))
ao.mass.pl<-ao_penID%>%
  filter(as.factor(Treatment2)%in%c("J1","J3"))%>%
  ggplot(aes(Treatment2,Meta.Mass.g))+
  geom_boxplot()+
  geom_jitter(color="gray",width = 0.2,shape=1)+
  theme_classic()+
  labs(y="Metamorph Mass (g)",x="Phenology Treatment")

ao.daysheld.pl<-ao_penID%>%
  filter(as.factor(Treatment2)%in%c("J1","J3"))%>%
  ggplot(aes(Treatment2,Days.Held))+
  geom_boxplot()+
  geom_jitter(color="gray",width = 0.2,shape=1)+
  theme_classic()+
  labs(y="Days Held",x="Phenology Treatment")

#Build capture history matrix
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

#recombine with release info
ao_penID<-filter(ao_penID,as.factor(Juv.Treat)%in%c("L3-J1","L3-J3","L1-J1","L1-J3"))
ao_wide<-merge(ao_penID[,c("PIT_Tag", "Juv.Treat", "Rel.Block", "Rel.Pen","Meta.Mass.g", "Rel.Cohort","Days.Held")],ao_ch.pa,all=T)

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


# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f_ao <- apply(ao_CH, 1, get.first)

# Create matrix m to indicate when an individual was captured, to address test 2.ct trap effect
m_ao<-ao_CH[,1:(dim(ao_CH)[2]-1)]
u_ao<-which (m_ao==0)
m_ao[u_ao]<-2

#load weather data
ao_abiotic <- readRDS("~/GitRepos/JuvenileEmigrationPhenology/ao_abiotic.rds")
cor.test(as.numeric(ao_abiotic$Tmin), as.numeric(ao_abiotic$Prcp)) #Not autocorrelated

ao_stdtempc<-rep(NA,length(ao_abiotic$Tavg))
ao_stdprecip<-rep(NA,length(ao_abiotic$Prcp))
#ao_stdtempsd <- as.numeric(ao_abiotic$temp.sd)
# stdtempsd<-rep(NA,length(abiotic$temp.sd))

#Scale temp. and precip. covariates
for (i in 1:length(ao_abiotic$Tmin)) {
  ao_stdtempc[i] <- (ao_abiotic$Tmin[i]-mean(ao_abiotic$Tmin[]))/sd(ao_abiotic$Tmin[])
  #ao_stdtempsd[i] <- (ao_abiotic$temp.sd[i]-mean(ao_abiotic$temp.sd[]))/sd(ao_abiotic$temp.sd[])
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
interval_ao<-ao_recaps%>%
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

# Format AO predictor data ---------------------------------------------------
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L1-J1", "J1", ao_wide$Juv.Treat) #Create juvenile-only treatment factor
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L3-J1", "J1", ao_wide$Treatment2)
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L3-J3", "J3", ao_wide$Treatment2)
ao_wide$Treatment2<-ifelse(ao_wide$Juv.Treat=="L1-J3", "J3", ao_wide$Treatment2)

#group_ao<-as.numeric(ao_wide$Juv.Treat) #for 4-group analysis
group2_ao<-as.numeric(as.factor(ao_wide$Treatment2))
block_ao<-as.numeric(ao_wide$Rel.Block)
pen_ao<-as.numeric(as.factor(paste(ao_wide$Rel.Block,ao_wide$Rel.Pen,sep="")))
rel_ao<-as.numeric(ao_wide$Rel.Cohort)

#define days held covariate
ao_wide$Days.Held[is.na(ao_wide$Days.Held)]<-mean(ao_wide$Days.Held,na.rm=T)
stddaysheld_ao<-rep(NA,length(ao_wide$Days.Held))

#Scale days held covariate
for (i in 1:length(ao_wide$Days.Held)) {
  stddaysheld_ao[i] <- (ao_wide$Days.Held[i]-mean(ao_wide$Days.Held[]))/sd(ao_wide$Days.Held[])
}

