#Jake's plotting code- doesn't work anymore

## Summarize/Visualize Data:
## -------------------------
head(df)

j.int <- aggregate(recaps$DOY_adj, by=list(recaps$Period), FUN="max", na.rm=T)[2]
weather$Period <- ifelse(weather$DOY_adj <= j.int[1,], yes="I01", 
                         no=ifelse(weather$DOY_adj > j.int[1,] & weather$DOY_adj <= j.int[2,], yes="I02", 
                                   no=ifelse(weather$DOY_adj > j.int[2,] & weather$DOY_adj <= j.int[3,], yes="I03", 
                                             no=ifelse(weather$DOY_adj > j.int[3,] & weather$DOY_adj <= j.int[4,], yes="I04", 
                                                       no=(ifelse(weather$DOY_adj > j.int[4,] & weather$DOY_adj <= j.int[5,], yes="I05", 
                                                                  no=ifelse(weather$DOY_adj > j.int[5,] & weather$DOY_adj <= j.int[6,], yes="I06", 
                                                                            no=ifelse(weather$DOY_adj > j.int[6,] & weather$DOY_adj <= j.int[7,], yes="I07", 
                                                                                      no=ifelse(weather$DOY_adj > j.int[7,] & weather$DOY_adj <= j.int[8,], yes="I08", 
                                                                                                no=ifelse(weather$DOY_adj > j.int[8,] & weather$DOY_adj <= j.int[9,], yes="I09", 
                                                                                                          no=ifelse(weather$DOY_adj > j.int[9,] & weather$DOY_adj <= j.int[10,], yes="I10", 
                                                                                                                    no=ifelse(weather$DOY_adj > j.int[10,] & weather$DOY_adj <= j.int[11,], yes="I11", 
                                                                                                                              no=ifelse(weather$DOY_adj > j.int[11,] & weather$DOY_adj <= j.int[12,], yes="I12", 
                                                                                                                                        no=ifelse(weather$DOY_adj > j.int[12,] & weather$DOY_adj <= j.int[13,], yes="I13", 
                                                                                                                                                  no=ifelse(weather$DOY_adj > j.int[13,] & weather$DOY_adj <= j.int[14,], yes="I14", 
                                                                                                                                                            no=ifelse(weather$DOY_adj > j.int[14,] & weather$DOY_adj <= j.int[15,], yes="I15", 
                                                                                                                                                                      no=ifelse(weather$DOY_adj > j.int[15,] & weather$DOY_adj <= j.int[16,], yes="I16", 
                                                                                                                                                                                no=ifelse(weather$DOY_adj > j.int[16,] & weather$DOY_adj <= j.int[17,], yes="I17",no=NA
                                                                                                                                                                                ))))))))))))))))))

wet.df <- group_by(weather, Period) %>%
  summarise(
    N.days = n(),
    Temp.Avg = mean(AVG_ATEMP_C, na.rm=T),
    Temp.Min = min(MIN_ATEMP_C, na.rm=T),
    Temp.Max = max(MAX_ATEMP_C, na.rm=T),
    Precp.mm = sum(TOT_PREC_MM, na.rm=T), 
    SMois.2in = mean(SOIL_MOIS_2in_VWC, na.rm=T), 
    SMois.4in = mean(SOIL_MOIS_4in_VWC, na.rm=T),
    SMois.8in = mean(SOIL_MOIS_8in_VWC, na.rm=T),
    SMois.20in = mean(SOIL_MOIS_20in_VWC, na.rm=T),
    SMois.40in = mean(SOIL_MOIS_40in_VWC, na.rm=T), 
    Solar.Rad = mean(TOT_SOL_RAD_MJpM2, na.rm=T)
  )

## Visualize some data
par(mfrow=c(2,1))

## Plot Temperature Data
plot(weather$DOY_adj[21:365], weather$AVG_ATEMP_C[21:365], type="b", xlab="", ylab="Temperature (C)", ylim=c(round(min(weather$MIN_ATEMP_C), 0)-1, round(max(weather$MAX_ATEMP_C), 0)+1))
points(weather$DOY_adj[21:365], weather$AVG_ATEMP_C[21:365], col=as.factor(weather$Period[21:365]), pch=16)
lines(weather$DOY_adj[21:365], weather$MIN_ATEMP_C[21:365], lty=2, col="dark grey")
lines(weather$DOY_adj[21:365], weather$MAX_ATEMP_C[21:365], lty=2, col="dark grey")

## Plot Daily Precipitation Data
plot(weather$DOY_adj[21:365], weather$TOT_PREC_MM[21:365], type="b", xlab="", ylab="Total Daily Precipitation (mm)")
points(weather$DOY_adj[21:365], weather$TOT_PREC_MM[21:365], col=as.factor(weather$Period[21:365]), pch=16)

## Plot Recap Data by Treatment
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
lines(as.numeric(as.factor(wet.df$Period))[1:16], wet.df$Precp.mm[1:16]/10, pch=8, cex=1.5, lwd=1.5, col="blue", type="b", lty=3)                          ## rainfall accumulation in cm 
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

png("IPR_AA_recaps.png", width=5, height=5, units='in', res=600)
p  ##insert plot code here
dev.off()


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

png("IPR_AA_mass.png", width=5, height=5, units='in', res=600)
m  ##insert plot code here
dev.off()


## ---------------