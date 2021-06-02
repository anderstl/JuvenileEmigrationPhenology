## Load Packages:
## --------------
if(!require(tidyr)) install.packages('tidyr'); library("tidyr")
if(!require(readxl)) install.packages('readxl'); library("readxl")
if(!require(ggplot2)) install.packages('ggplot2'); library("ggplot2")
if(!require(dplyr)) install.packages('dplyr'); library("dplyr")
if(!require(viridis)) install.packages('viridis'); library("viridis")

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

ao_recaps <- read_excel("Data/AMOP/Recap_Database_2019-2020-Experiments_Master_20200125.xlsx", sheet=2,na=c("NA", ""))## recapture data
ao_penID <- read_excel("Data/AMOP/Pens_Assignments_2019-2020-Experiments.xlsx", na=c("NA", ""))
ao_endfates<-read_excel("Data/AMOP/BreakdownFates_2019-2020-Experiments.xlsx",na=c("NA",""))
ao_sex<-read_excel("Data/AMOP/Siegel Salamander data.xlsx",na=c("NA",""))
ao_endfates<-merge(ao_endfates,ao_sex,by="PIT_Tag")
ao_endfates<-merge(ao_endfates,ao_penID,by=c("PIT_Tag","Juv.Treat","Juv.Pen","Treatment","Species"))

## initial pen assignments

## --------------------------------

## Format/Merge Data:
## ------------------
DOY.adj <- 157                           ## Standardize DOY to date of first release- 07 June 2019

ao_recaps<-ao_recaps%>%
  filter(!grepl("Arianne",Notes) | is.na(Notes)== T)%>%
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
         mutate_if(sapply(ao_recaps, is.character), as.factor)

ao_penID1<-ao_penID%>%
  select("PIT_Tag","Species","Juv.Pen","Juv.Treat")%>%
  mutate(Rel.Block = sapply(strsplit(Juv.Pen, ","), function(x) x[1]),
  Rel.Pen = sapply(strsplit(Juv.Pen, ","), function(x) x[2]))
ao_penID1<-ao_penID1%>%
  mutate_if(sapply(ao_penID1, is.character), as.factor)

ao_df <- merge(ao_recaps, ao_penID1, by=c("PIT_Tag"),all=T)

## Summarize/Visualize Data:
## -------------------------
head(df)


## Plot Recap Data by Opacum Variation Treatment
ao_alive <- ao_df%>%
  filter(!(Period%in%c("R1","R2","R3")) & as.factor(Juv.Treat)%in%c("L3-J1","L3-J3","L1-J1","L1-J3"))%>%
  filter(Visual==1 | Moved == 1 | Burr_Vis == 1 )%>%
  distinct(PIT_Tag,Period,.keep_all = TRUE)%>%
  droplevels()

ao_plotdat<-ao_alive%>%
  group_by(Period,Juv.Treat,Juv.Pen)%>%
  summarise(Count=n_distinct(PIT_Tag))

ao_dat <- as.data.frame(table(ao_alive$Period, ao_alive$Juv.Treat))
colnames(ao_dat) <- c("Period", "Treatment","Freq")  

period_dates<-ao_recaps%>%
  group_by(as.numeric(as.character(Period)))%>%
  summarise(FirstDate=min(Recap_Date))
period_dates<-na.omit(period_dates) #why is there an NA for June 7?
period_dates$dayMonth<-format(as.Date(as.character(period_dates$FirstDate),"%Y-%m-%d"),"%d-%b")#calculate Julian Date
cbbPalette <- c("#D55E00","#E69F00", "#009E73", "#0072B2")

#png("opacum_var.png",res=600,height=5,width=5,units="in")
ggplot(ao_dat,aes(as.numeric(as.character(Period)),Freq,shape=Treatment,linetype=Treatment,color=Treatment,group=Treatment))+
  geom_point(size=3)+
  geom_line(size=1.5)+
  scale_shape_manual(values=c(16,17,1,2),labels=c("1 Breeding, 1 Emigration", 
                                                  "1 Breeding, 3 Emigration", 
                                                  "3 Breeding, 1 Emigration",
                                                  "3 Breeding, 3 Emigration"))+
  scale_linetype_manual(values=1:4,labels=c("1 Breeding, 1 Emigration", 
                                             "1 Breeding, 3 Emigration", 
                                             "3 Breeding, 1 Emigration",
                                             "3 Breeding, 3 Emigration"))+
  my_theme2()+
  theme(legend.position = c(0.4,0.8),axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x="Date",y="Number of Salamanders Detected Alive")+
  scale_color_manual(values=cbbPalette,labels=c("1 Breeding, 1 Emigration", 
                                                         "1 Breeding, 3 Emigration", 
                                                         "3 Breeding, 1 Emigration",
                                                         "3 Breeding, 3 Emigration"))+
  scale_x_continuous(breaks=seq(1,length(period_dates$dayMonth),1),labels=(period_dates$dayMonth))
#dev.off()

#final fate plots
ao_endfates$Alive<-ifelse(grepl("alive",ao_endfates$Fate_notes),1,
                         ifelse(grepl("Dead",ao_endfates$Fate_notes),0,NA))
ao_totals<-ao_endfates%>%
  filter(Juv.Treat%in%c("L1-J1","L3-J1","L1-J3","L3-J3"))%>%
  group_by(Juv.Treat,Juv.Pen,Species)%>%
  dplyr::summarise(Total=sum(Alive,na.rm=T))
  
png("juv_opacvar_survival.png",res=600,height=5,width=5,units="in")
ggplot(ao_totals,aes(Juv.Treat,Total/9))+
  geom_boxplot(fill="darkorchid2")+
  my_theme2()+
  labs(y="Percent Recovered Alive",x="Treatment")+
  lims(y=c(0,1))
dev.off()

car::Anova(glm(cbind(Total,9-Total)~Juv.Treat,data=ao_totals,binomial))


#plot sex by size at metamorphosis
plot(as.numeric(as.factor(Sex))~Meta.Mass.g,ao_endfates,subset=c(Species=="AMOP"))
summary(glm(as.factor(Sex)~Meta.Mass.g,ao_endfates,family=binomial,subset=c(Species=="AMOP")))
