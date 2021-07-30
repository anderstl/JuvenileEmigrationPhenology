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

