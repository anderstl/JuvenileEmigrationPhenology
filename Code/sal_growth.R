#To Incorporate yet
#1. mass change since last capture (or mass at metamorphosis)
#2. add mass at metamorphosis (or tagging mass) as additional growth point

#annulatum growth
interval_aa$Period<-as.numeric(interval_aa$`as.factor(Period)`)
aa_growth<-merge(aa_recaps,interval_aa,by="Period")

aa_assign<-transform(aa_assign, LarvalTrt = substr(Treatment, 1, 2), JuvTrt = substr(Treatment, 3, 4))
aa_growth<-merge(aa_growth,aa_assign[,c('PIT_Tag','JuvTrt')],by="PIT_Tag")

aa_growth<-aa_growth%>%
  filter(Notes != "Arianne Salamander")%>%
  select(Period,Species,PIT_Tag,Pen_ID,JuvTrt,SVL_mm,TL_mm,Mass_g,Recap_DOY,Recap_DOY_adj,days)%>%
  filter(!(is.na(SVL_mm) & is.na(Mass_g)))
aa_growth<-aa_growth%>%
  mutate(Growth_g=Mass_g/Recap_DOY_adj,
         Growth_mm=SVL_mm/Recap_DOY_adj) #calculates growth as mass or svl divided by elapsed days since start of experiment but doesn't include size at metamorphosis so not quite right

  
aamass.trt.pl<-ggplot(aa_growth,aes(as.factor(Period),Mass_g,fill=JuvTrt))+
  geom_boxplot()+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))+
  theme_classic()+
  labs(x= 'Recapture Period',y="Mass (g)")

aasvl_trt.pl<-ggplot(aa_growth,aes(as.factor(Period),SVL_mm,fill=JuvTrt))+
  geom_boxplot()+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))+
  theme_classic()+
  labs(x= 'Recapture Period',y="SVL (mm)")

aa_mass.pl<-ggplot(aa_growth,aes(Recap_DOY_adj,Mass_g,group=PIT_Tag,color=JuvTrt))+
  geom_point()+
  geom_line()+
  theme_classic()+
  labs(x= 'Day of Experiment',y="Mass (g)")

aa_svl.pl<-ggplot(aa_growth,aes(Recap_DOY_adj,SVL_mm,group=PIT_Tag,color=JuvTrt))+
  geom_point()+
  geom_line()+
  theme_classic()+
  #geom_text()+
  labs(x= 'Day of Experiment',y="SVL (mm)")

png('growth_aa.png',res=600,height=7,width=9,units="in")
plot_grid(aasvl_trt.pl,aamass.trt.pl,aa_svl.pl,aa_mass.pl,ncol=2)
dev.off()

#Opacum growth
#make data frame containing only captures with known size/mass values
#need to add in size at tagging/metamorphosis
ao_growth<-merge(ao_df,interval_ao,by="Period")
ao_penID<-ao_penID%>%
  mutate(Period=as.character(rep(0,nrow(ao_penID))))%>%
  mutate(Mass_g=Tag.Mass,
         Recap_DOY=157,
         Recap_DOY_Adj=0)

ao_growth<-bind_rows(ao_growth,ao_penID[,c('JuvTrt','PIT_Tag',"Period","Mass_g")])

ao_growth<-ao_growth%>%
  select(Period,PIT_Tag,Pen_ID,JuvTrt,SVL_mm,TL_mm,Mass_g,Rel.DOY,Recap_DOY,Recap_DOY_adj,Rel.DOY_adj,days)
ao_growth<-ao_growth%>%
  mutate(Growth_g=Mass_g/Recap_DOY_adj,
         Growth_mm=SVL_mm/Recap_DOY_adj)

ggplot(ao_growth,aes(Growth_g,fill=JuvTrt))+
  geom_histogram(position="dodge")+
  facet_wrap(~Period,scales="free")+
  theme_bw()

aomass.trt.pl<-ggplot(ao_growth,aes(Period,Mass_g,fill=JuvTrt))+
  geom_boxplot()+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))+
  theme_classic()+
  labs(x= 'Recapture Period',y="Mass (g)")

aosvl_trt.pl<-ggplot(ao_growth,aes(Period,SVL_mm,fill=JuvTrt))+
  geom_boxplot()+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))+
  theme_classic()+
  labs(x= 'Recapture Period',y="SVL (mm)")

ao_growg.pl<-ggplot(ao_growth,aes(Period,Growth_g,group=PIT_Tag,color=JuvTrt))+
  geom_boxplot()+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))+
  theme_classic()+
  labs(x= 'Day of Experiment',y="Growth (g/d)")

ao_growmm.pl<-ggplot(ao_growth,aes(Period,Growth_mm,group=PIT_Tag,color=JuvTrt))+
  geom_boxplot()+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))+
  theme_classic()+
  labs(x= 'Day of Experiment',y="Growth (mm/d)")

ao_mass.pl<-ggplot(ao_growth,aes(Recap_DOY_adj,Mass_g,group=PIT_Tag,color=JuvTrt))+
  geom_point()+
  geom_line()+
  theme_classic()+
  labs(x= 'Day of Experiment',y="Mass (g)")

ao_svl.pl<-ggplot(ao_growth,aes(Recap_DOY_adj,SVL_mm,group=PIT_Tag,color=JuvTrt))+
  geom_point()+
  geom_line()+
  theme_classic()+
  #geom_text()+
  labs(x= 'Day of Experiment',y="SVL (mm)")

library(cowplot)
png('growth_ao.png',res=600,height=7,width=9,units="in")
plot_grid(aosvl_trt.pl,aomass.trt.pl,ao_svl.pl,ao_mass.pl,ncol=2)
dev.off()
