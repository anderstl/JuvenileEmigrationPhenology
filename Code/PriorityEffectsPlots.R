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

pe_recaps <- read_excel("Data/AMOP/Recap_Database_2019-2020-Experiments_Master_20200125.xlsx", sheet=2,na=c("NA", ""))## recapture data
pe_penID <- read_excel("Data/AMOP/Pens_Assignments_2019-2020-Experiments.xlsx", na=c("NA", ""))
pe_endfates<-read_excel("Data/AMOP/BreakdownFates_2019-2020-Experiments.xlsx",na=c("NA",""))
pe_sex<-read_excel("Data/AMOP/Siegel Salamander data.xlsx",na=c("NA",""))
endfates<-merge(pe_endfates,pe_sex,by="PIT_Tag")
endfates<-merge(pe_endfates,pe_penID,by="PIT_Tag")

## initial pen assignments

## --------------------------------

## Format/Merge Data:
## ------------------
DOY.adj <- 157                           ## Standardize DOY to date of first release- 07 June 2019

pe_recaps<-pe_recaps%>%
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
pe_recaps<-pe_recaps%>%
  mutate_if(sapply(pe_recaps, is.character), as.factor)

pe_penID1<-pe_penID%>%
  select("PIT_Tag","Species","Juv.Pen","Juv.Treat")%>%
  mutate(Rel.Block = sapply(strsplit(Juv.Pen, ","), function(x) x[1]),
         Rel.Pen = sapply(strsplit(Juv.Pen, ","), function(x) x[2]))
pe_penID1<-pe_penID1%>%
  mutate_if(sapply(pe_penID1, is.character), as.factor)

pe_df <- merge(pe_recaps, pe_penID1, by=c("PIT_Tag"),all=T)

## Summarize/Visualize Data:
## -------------------------
head(df)

## Plot Recap Data by Priority Effects Treatment
pe_alive <- pe_df%>%
  filter(!(Period%in%c("R1","R2","R3")) & Juv.Treat%in%c("AMAN-First","AMOP-First","Same-Time"))%>%
  filter(Visual==1 | Moved == 1 | Burr_Vis == 1)%>%
  distinct(PIT_Tag,Period,.keep_all = TRUE)

r.plot1<-pe_alive%>%
  group_by(Period,Juv.Treat,Juv.Pen,Species.x)%>%
  summarise(Count=n_distinct(PIT_Tag))

pe_dat <- as.data.frame(table(pe_alive$Period, pe_alive$Juv.Treat,pe_alive$Species.x))
colnames(r.plot) <- c("Period", "Treatment", "Species","Freq")  
period_dates<-recaps%>%group_by(as.numeric(as.character(Period)))%>%summarise(FirstDate=min(Recap_Date))
period_dates<-na.omit(period_dates) #why is there an NA for June 7?
period_dates$dayMonth<-format(as.Date(as.character(period_dates$FirstDate),"%Y-%m-%d"),"%d-%b")#calculate Julian Date

ggplot(pe_dat,aes(as.numeric(as.character(Period)),Freq,color=Treatment,group=Treatment))+
  geom_point()+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=viridis(3,end=0.9))+
  labs(x="Date",y="Number of Individuals Detected Alive")+
  facet_wrap(~Species,ncol=1)+
  scale_x_continuous(breaks=seq(1,length(period_dates$dayMonth),1),labels=period_dates$dayMonth)

#final fate plots
final_fate$Alive<-ifelse(grepl("alive",final_fate$Fate_notes),1,
                         ifelse(grepl("Dead",final_fate$Fate_notes),0,NA))
final_totals<-final_fate%>%
  group_by(Juv.Treat,Juv.Pen,Species)%>%
  dplyr::summarise(Total=sum(Alive,na.rm=T))

pe_var<-final_totals%>%
  filter(Juv.Treat%in%c("Same-Time","AMAN-First","AMOP-First"))

png("juv_pe_survival.png",res=600,height=5,width=5,units="in")
ggplot(pe_var,aes(Juv.Treat,Total/4,fill=Species))+
  geom_boxplot()+
  my_theme2()+
  scale_fill_manual(values=c("goldenrod","darkorchid2"),labels=c("Ringed","Marbled"))+
  labs(y="Percent Recovered Alive",x="Treatment")+
  lims(y=c(0,1))+
  scale_x_discrete(labels=c("Ringed First","Marbled First","Same Time"))+
  theme(legend.position = "top",legend.direction = "horizontal")
dev.off()

Anova(glm(cbind(Total,4-Total)~Juv.Treat+Species,data=pe_var,binomial))
