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

setwd("C:/Users/Tom/DropBox/SERDP_Project/Pens/2018-2019_Experiment/")
recaps <- read.csv("AMAN_phenology_recap_data_master.csv", header=T)           ## recapture data
weather <- read_excel("AaCOE_CapenPark_Daily-Weather_20180601-20190531.xlsx")  ## weather data

assign <- read_excel("Aa_COEffects_Pen_Assignments.xlsx")             ## initial pen assignments
treats <- read.csv("Aa_COEffects_Treatments.csv", header=T)           ## treatment data
tagged <- read_excel("Aa_COEffects_Tagged_Animals.xlsx")              ## all tagged animals and metamorphosis sizes
## --------------------------------

## Format/Merge Data:
## ------------------
doy.adj <- 172                           ## DOY value to standardize values to

recaps <- subset(recaps, recaps$Notes != "Arianne Salamander")
recaps$DOY <- as.numeric(format(as.Date(as.character(recaps$Recap_Date),"%m/%d/%Y"),"%j"))
recaps$DOY_adj <- ifelse(recaps$DOY >= doy.adj, yes=recaps$DOY-(doy.adj-1), no=recaps$DOY+(365-doy.adj+1))
recaps$Rel.DOY <- as.numeric(format(as.Date(as.character(recaps$Release),"%m/%d/%Y"),"%j"))
recaps$Rel.DOY_adj <- ifelse(recaps$Rel.DOY >= doy.adj, yes=recaps$Rel.DOY-(doy.adj-1), no=recaps$Rel.DOY+(365-doy.adj+1))

recaps$Recap_Loc <- as.character(recaps$Recap_Loc)

recaps$Re.Block <- NULL
recaps$Re.Pen <- NULL
recaps$Re.Micro <- NULL

for(i in 1:dim(recaps)[1]) {
  recaps$Re.Block[i] <- unlist(strsplit(recaps$Recap_Loc[i], ","))[1]
  recaps$Re.Pen[i] <- unlist(strsplit(recaps$Recap_Loc[i], ","))[2]
  recaps$Re.Micro[i] <- unlist(strsplit(recaps$Recap_Loc[i], ","))[3]
}

assign$Release_DOY <- as.numeric(format(as.Date(as.character(assign$Release_Date), "%Y-%m-%d"),"%j"))
assign$DOY_adj <- ifelse(assign$Release_DOY >= doy.adj, yes=assign$Release_DOY-(doy.adj-1), no=assign$Release_DOY+(365-doy.adj+1))

tagged$Meta.DOY <- as.numeric(format(as.Date(as.character(tagged$Meta.Date), "%Y-%m-%d"),"%j"))
tagged$Meta.DOY_adj <- ifelse(tagged$Meta.DOY >= doy.adj, yes=tagged$Meta.DOY-(doy.adj-1), no=tagged$Meta.DOY+(365-doy.adj+1))
tagged$Tag.DOY <- as.numeric(format(as.Date(as.character(tagged$Tag.Date), "%Y-%m-%d"),"%j"))
tagged$Tag.DOY_adj <- ifelse(tagged$Tag.DOY >= doy.adj, yes=tagged$Tag.DOY-(doy.adj-1), no=tagged$Tag.DOY+(365-doy.adj+1))

df.a <- merge(assign, tagged, by="PIT_Tag", type="left")
df <- merge(recaps, df.a, by="PIT_Tag", type="left")

df$Moved <- ifelse(is.na(df$Moved)==T, yes=0, no=df$Moved)
df$Pre.Alive <- df$Moved + df$Pre_Alive
df$Pre.Alive <- ifelse(df$Pre.Alive >= 1, yes=1, no=0)

## create a matrix of zeros with # inds X # periods dimensions
## loop over individuals, populate the cells with data
## concatenate data on the back end to create the capture history cells

## Create Capture History Matrix (messy... needs cleaned later)
ch.pa <- matrix(0, nrow=dim(assign)[1], ncol=max(as.numeric(recaps$Period), na.rm=T)+5)
ch.pa <- as.data.frame(ch.pa)
colnames(ch.pa)[1:4] <- c("PIT_Tag", "Treatment", "Block", "Pen")
ch.pa$PIT_Tag <- assign$PIT_Tag
ch.pa$Treatment <- assign$Treatment
ch.pa$Block <- assign$Block
ch.pa$Pen <- assign$Pen
ch.pa[,5] <- 1

recaps$Moved <- ifelse(is.na(recaps$Moved)==T, yes=0, no=recaps$Moved)

# recaps$Visual <- ifelse(recaps$Visual == 2, yes=0, no=recaps$Visual)          
recaps$Pre.Alive <- recaps$Moved + recaps$Pre_Alive
recaps$Pre.Alive <- ifelse(recaps$Pre.Alive >= 1, yes=1, no=0)

ag.recaps <- aggregate(recaps$Pre.Alive, by=list(recaps$PIT_Tag, recaps$Period), FUN='sum')
colnames(ag.recaps) <- c("PIT_Tag", "Period", "Nobs")
ag.recaps$Pre.Alive <- ag.recaps$Nobs
ag.recaps$Pre.Alive <- ifelse(ag.recaps$Pre.Alive >= 1, yes=1, no=ag.recaps$Pre.Alive)

for(i in 1:dim(ch.pa)[1]){
  ind <- ch.pa$PIT_Tag[i]
  sdf <- subset(ag.recaps, ag.recaps$PIT_Tag == ind)
  
  if(dim(sdf)[1]>0){
    for(c in 6:dim(ch.pa)[2]){
      for(s in 1:dim(sdf)[1]){
        p <- sdf$Period[s]
        if((c-5) == p){
          ch.pa[i,c] <- sdf$Pre.Alive[s]
        }
      }
    }
  }
}

CH <- as.matrix(ch.pa[,5:dim(ch.pa)[2]])            ## isolate capture history matrix data only. 

## Plot Recap Data by Treatment

#determine when last capture occurred for each individual
last_capt<-recaps %>% 
  group_by(PIT_Tag) %>% 
  arrange(DOY_adj) %>%  
  slice(n())

temp <- subset(recaps, (Visual==1 & is.na(Mass_g)==F) | Moved == 1)
temp <- subset(temp, Species == "AMAN")
r.df <- merge(temp, treats, by.x=c("Re.Block", "Re.Pen"), by.y=c("Block", "Pen"), all.x = T)
r.df$Treatment <- as.factor(r.df$Treat.abrv)

r.plot <- as.data.frame(table(r.df$Period, r.df$Treatment))
colnames(r.plot) <- c("Period", "Treatment", "Freq")  
r.plot$RGB <- ifelse(r.plot$Treatment == "L1J1", yes="#e66101", ##light orange
                     no=ifelse(r.plot$Treatment == "L1J3", yes="#fdb863",  ## dark orange
                               no=ifelse(r.plot$Treatment == "L3J1", yes="#5e3c99",  ##lavendar 
                                         no="#b2abd2")))  ##dark purple

r.plot$Phenology.Treatments <- ifelse(r.plot$Treatment == 1.1, yes="1 Breeding, 1 Emigration Date", ##light orange
                                      no=ifelse(r.plot$Treatment == 1.3, yes="1 Breeding, 3 Emigration Dates",  ## dark orange
                                                no=ifelse(r.plot$Treatment == 3.1, yes="3 Breeding, 1 Emigration Dates",  ##lavendar 
                                                          no="3 Breeding, 3 Emigration Dates")))  ##dark purple

par(mfrow=c(1,1))
plot(as.numeric(as.character(r.plot$Period[which(r.plot$Treatment=="1.1")]))-0.20, r.plot$Freq[which(r.plot$Treatment=="1.1")], xlim=c(1, 16), ylim=c(0,41), type="b", lwd=1, col="#8c510a", pch=16, 
     xlab="Recapture Period", ylab="Number of Recaps (Brown & Teal Lines) \n Total Precip (cm) Between Surveys (Blue Line)", mgp=c(1.8,0.8,0))
lines(as.numeric(as.character(r.plot$Period[which(r.plot$Treatment=="1.3")]))-0.10, r.plot$Freq[which(r.plot$Treatment=="1.3")], col="#d8b365", type="b", lwd=1, pch=16)
lines(as.numeric(as.character(r.plot$Period[which(r.plot$Treatment=="3.1")]))+0.10, r.plot$Freq[which(r.plot$Treatment=="3.1")], col="#01665e", type="b", lwd=1, pch=17, lty=2)
lines(as.numeric(as.character(r.plot$Period[which(r.plot$Treatment=="3.3")]))+0.20, r.plot$Freq[which(r.plot$Treatment=="3.3")], col="#5ab4ac", type="b", lwd=1, pch=17, lty=2)


## Analyze Growth:
## ---------------
## Plot Recaps
temp <- subset(recaps, (Visual==1 & is.na(Mass_g)==F) | Moved == 1)
temp <- subset(temp, Species == "AMAN")
r.df <- merge(temp, treats, by.x=c("Re.Block", "Re.Pen"), by.y=c("Block", "Pen"), all.x = T)
r.df$Treatment <- as.factor(r.df$Treat.abrv)

r.plot <- as.data.frame(table(r.df$Period, r.df$Treatment))
colnames(r.plot) <- c("Period", "Treatment", "Freq")  
r.plot$RGB <- ifelse(r.plot$Treatment == "L1J1", yes="#e66101", ##light orange
                     no=ifelse(r.plot$Treatment == "L1J3", yes="#fdb863",  ## dark orange
                               no=ifelse(r.plot$Treatment == "L3J1", yes="#5e3c99",  ##lavendar 
                                         no="#b2abd2")))  ##dark purple

r.plot$Phenology.Treatments <- ifelse(r.plot$Treatment == 1.1, yes="1 Breeding, 1 Emigration", ##light orange
                                      no=ifelse(r.plot$Treatment == 1.3, yes="1 Breeding, 3 Emigration",  ## dark orange
                                                no=ifelse(r.plot$Treatment == 3.1, yes="3 Breeding, 1 Emigration",  ##lavendar 
                                                          no="3 Breeding, 3 Emigration")))  ##dark purple
period_dates<-recaps%>%
  group_by(as.numeric(as.character(Period)))%>%
  summarise(FirstDate=min(as.Date(Recap_Date,format="%m/%d/%Y")))
period_dates<-na.omit(period_dates) #why is there an NA for June 7?
period_dates$dayMonth<-format(as.Date(as.character(period_dates$FirstDate),"%Y-%m-%d"),"%d-%b")#calculate Julian Date

p <- ggplot(data=r.plot, aes(x=Period, y=Freq, color=Phenology.Treatments, group=Phenology.Treatments,linetype=Phenology.Treatments,shape=Phenology.Treatments))+
  geom_line(size=1.5) + 
  my_theme2() +
  scale_shape_manual(values=c(16,17,1,2), name="Phenology Treatment") +
  scale_linetype_manual(values=1:4, name="Phenology Treatment") +
  scale_x_discrete(breaks=seq(1,length(period_dates$dayMonth),1),labels=(period_dates$dayMonth))+
  scale_color_manual(values=cbbPalette, name="Phenology Treatment") +
  geom_point(size=3)+
  ylab("Number of Salamanders Detected Alive") + xlab("Recapture Date") +
  theme(legend.position=c(0.4 ,0.75),axis.text.x = element_text(angle=45,hjust=1)) 
p

## Summarize Recaps:
df <- subset(df, Species == "AMAN")

df2 <- merge(df, treats, by.x=c("Re.Block", "Re.Pen"), by.y=c("Block", "Pen"), all.x = T)
# df2$Treatment <- as.factor(df2$Treatment)


length(unique(subset(df2, Visual==1 & is.na(df2$Mass_g)==F)$PIT_ID))
length(unique(subset(df2, (Visual==1 & is.na(Mass_g)==F) | Moved == 1)$PIT_ID))

table(df2$Treatment.x, df2$Visual, df2$Period)

df2 <- subset(df2, is.na(df2$Treatment.x)==F)

df2$RGB <- ifelse(df2$Treatment.x == "L1J1", yes="#e66101", ##light orange
                  no=ifelse(df2$Treatment.x == "L1J3", yes="#fdb863",  ## dark orange
                            no=ifelse(df2$Treatment.x == "L3J1", yes="#5e3c99",  ##lavendar 
                                      no="#b2abd2")))  ##dark purple

df2$Phenology.Treatments <- ifelse(df2$Treatment.x == "L1J1", yes="1 Breeding, 1 Emigration", ##light orange
                                   no=ifelse(df2$Treatment.x == "L1J3", yes="1 Breeding, 3 Emigration",  ## dark orange
                                             no=ifelse(df2$Treatment.x == "L3J1", yes="3 Breeding, 1 Emigration",  ##lavendar 
                                                       no="3 Breeding, 3 Emigration")))  ##dark purple

m <- ggplot(df2, aes(jitter(Period,amount = 0.2), Mass_g, color=Phenology.Treatments)) +
  geom_point(size=2) +  
  #theme_classic() +
  my_theme2() + 
  scale_x_continuous(breaks=seq(1,length(period_dates$dayMonth),1),labels=(period_dates$dayMonth))+
  scale_color_manual(values=c("#a6611a", "#dfc27d", "#018571", "#80cdc1"), 
                     guide=guide_legend(title="Phenology Treatment")) +
  ylab("Mass (grams)") + xlab("Recapture Event") +
  theme(legend.position=c(0.25, 0.85),axis.text.x=element_text(angle=45,hjust=1)) 
m