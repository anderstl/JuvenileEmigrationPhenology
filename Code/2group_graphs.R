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

g1.phi.dat<-as.data.frame(subset(phi.list[aa.data$group==1,]))
g2.phi.dat<-as.data.frame(subset(phi.list[aa.data$group==2,]))

g1.phil<-as.data.frame(subset(phi.l[aa.data$group==1,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[aa.data$group==2,]))

g1.phih<-as.data.frame(subset(phi.h[aa.data$group==1,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[aa.data$group==2,]))

phi.g1 <- g1.phi.dat %>% summarise_all(mean)
phi.g2 <- g2.phi.dat %>% summarise_all(mean)

phi.g1.med <- g1.phi.dat %>% summarise_all(median)
phi.g2.med <- g2.phi.dat %>% summarise_all(median)

g1.low<-as.numeric(g1.phil %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.phih %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.phil %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.phih %>% summarise_all(mean, na.rm=TRUE))

x.g1.phi<-mean(g1.phi)#Overall spp. means
x.g2.phi<-mean(g2.phi)

med.g1.phi<-median(g1.phi)#Overall treatment medians
med.g2.phi<-median(g2.phi)

means.phi<-c(x.g1.phi, x.g2.phi)
meds.phi<-c(med.g1.phi, med.g2.phi)
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)
sd.phi<-c(sd.g1.phi, sd.g2.phi)

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
legend(x = 12, y=0.2, bty = 'n',
       legend=c("J1", "J3"),
       pch=c(1,6), lty=c(1,2), col=c("salmon1", "deepskyblue3"),ncol=1)

#mtext(line=2,side=1,at=c(1,4,8,12,16),recapdates,cex.axis=0.75)

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
mean(p.listv, na.rm = TRUE) #mean survival = 0.31
median(p.listv, na.rm = TRUE) #median survival= 0.27
sd(p.listv, na.rm = TRUE)#0.09
p.ci.low<-mean(p.lv, na.rm = TRUE)#0.19
p.ci.high<-mean(p.hv, na.rm = TRUE)#0.44

g1.p<-as.matrix(subset(p.list[aa.data$group==1,]))
g2.p<-as.matrix(subset(p.list[aa.data$group==2,]))

g1.p.dat<-as.data.frame(subset(p.list[aa.data$group==1,]))
g2.p.dat<-as.data.frame(subset(p.list[aa.data$group==2,]))

g1.pl<-as.data.frame(subset(p.l[aa.data$group==1,])) #Spp.-specific lower CI
g2.pl<-as.data.frame(subset(p.l[aa.data$group==2,]))

g1.ph<-as.data.frame(subset(p.h[aa.data$group==1,])) #spp.-specific upper CI
g2.ph<-as.data.frame(subset(p.h[aa.data$group==2,]))

p.g1 <- g1.p.dat %>% summarise_all(mean)
p.g2 <- g2.p.dat %>% summarise_all(mean)

p.g1.med <- g1.p.dat %>% summarise_all(median)
p.g2.med <- g2.p.dat %>% summarise_all(median)

g1.low<-as.numeric(g1.pl %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.ph %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.pl %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.ph %>% summarise_all(mean, na.rm=TRUE))

x.g1.p<-mean(g1.p)#Overall spp. means
x.g2.p<-mean(g2.p)

med.g1.p<-median(g1.p)#Overall treatment medians
med.g2.p<-median(g2.p)

means.p<-c(x.g1.p, x.g2.p)
meds.p<-c(med.g1.p, med.g2.p)
sd.g1.p<-sd(g1.p)
sd.g2.p<-sd(g2.p)

sd.p<-c(sd.g1.p, sd.g2.p)

#Add panel of treatment-specific temporal recapture
plot(x=(1:16),y= p.g1.med, type="b", pch=1, col="salmon1",lty=1, bty='l',las=1,
     ylim=c(0,1), ylab=expression("Recapture Probability ("~italic(rho)~")"), xlab="Recapture Date",xaxt="n")
axis(1,at = c(1,4,8,12,16),labels=F)
text(labels=recapdates,y=-.1,srt=45,adj=1,x=c(1,4,8,12,16),xpd=T,cex=0.9)
segments((1:16), g1.low, (1:16), g1.high, col="salmon1")
points(x=(1:16)+.1,p.g2.med, type="b", pch=6, col="deepskyblue3", lty=2)
segments((1:16)+.1, g2.low, (1:16)+.1, g2.high, col="deepskyblue3")
mtext("B)", side=3, at=1, las=0, line=1)

dev.off()

#Group effect on survival
plot(density(aa_final_fit$sims.list$phi.group[,1]), xlim=c(-3,8))#J1
lines(density(aa_final_fit$sims.list$phi.group[,2]), col=2)#J3

#Group effect on recapture
plot(density(aa_final_fit$sims.list$p.group[,1]), xlim=c(-5,5),main="")#J1
lines(density(aa_final_fit$sims.list$p.group[,2]), col=2)#J3

#Calculate % of posterior of differences overlapping zero
phiJ1.J3_f<-round(sum(ifelse((aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,2])<=0,1,0))/
                        #sum(aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,2])) * 100
                        length(aa_final_fit$sims.list$phi.group[,1]),2)

#Calculate % of posterior of differences overlapping zero
pJ1.J3_f<-round(sum(ifelse((aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,2])<=0,1,0))/
                  #sum(aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,2])) * 100
                  length(aa_final_fit$sims.list$p.group[,1]),2) 

#If difference of posteriors overlaps zero, no significant difference
png("Results/FigS2.png",width = 7, height = 3.5,units="in",res=500)
par(mfrow=c(1,2),mar=c(3,4,2,1))
plot(density(aa_final_fit$sims.list$phi.group[,1]-aa_final_fit$sims.list$phi.group[,2]),main=expression(italic(phi)),xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiJ1.J3_f,sep=" "),adj=0.9)

#If difference of posteriors for recapture overlaps zero, no significant difference
plot(density(aa_final_fit$sims.list$p.group[,1]-aa_final_fit$sims.list$p.group[,2]),main=expression(italic(rho)),xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pJ1.J3_f,sep=" "),adj=0.9)

dev.off()

#AMOP graphs
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

g1.phi.dat<-as.data.frame(subset(phi.list[ao.data$group==1,]))
g2.phi.dat<-as.data.frame(subset(phi.list[ao.data$group==2,]))

g1.phil<-as.data.frame(subset(phi.l[ao.data$group==1,])) #Spp.-specific lower CI
g2.phil<-as.data.frame(subset(phi.l[ao.data$group==2,]))

g1.phih<-as.data.frame(subset(phi.h[ao.data$group==1,])) #spp.-specific upper CI
g2.phih<-as.data.frame(subset(phi.h[ao.data$group==2,]))

phi.g1 <- g1.phi.dat %>% summarise_all(mean)
phi.g2 <- g2.phi.dat %>% summarise_all(mean)

phi.g1.med <- g1.phi.dat %>% summarise_all(median)
phi.g2.med <- g2.phi.dat %>% summarise_all(median)

g1.low<-as.numeric(g1.phil %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.phih %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.phil %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.phih %>% summarise_all(mean, na.rm=TRUE))
x.g1.phi<-mean(g1.phi)#Overall spp. means
x.g2.phi<-mean(g2.phi)

med.g1.phi<-median(g1.phi)#Overall treatment medians
med.g2.phi<-median(g2.phi)

means.phi<-c(x.g1.phi, x.g2.phi)
meds.phi<-c(med.g1.phi, med.g2.phi)
sd.g1.phi<-sd(g1.phi)
sd.g2.phi<-sd(g2.phi)

sd.phi<-c(sd.g1.phi, sd.g2.phi)


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
#mtext(at=c(2,4,6,8,10,12,14),text=recapdates1,line=2,side=1)
mtext("A)", side=3, at=1, las=1, line=0)
legend(x = 1, y = 0.2, bty = 'n', 
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

g1.p.dat<-as.data.frame(subset(p.list[ao.data$group==1,]))
g2.p.dat<-as.data.frame(subset(p.list[ao.data$group==2,]))

g1.pl<-as.data.frame(subset(p.l[ao.data$group==1,])) #Spp.-specific lower CI
g2.pl<-as.data.frame(subset(p.l[ao.data$group==2,]))

g1.ph<-as.data.frame(subset(p.h[ao.data$group==1,])) #spp.-specific upper CI
g2.ph<-as.data.frame(subset(p.h[ao.data$group==2,]))

p.g1 <- g1.p.dat %>% summarise_all(mean)
p.g2 <- g2.p.dat %>% summarise_all(mean)

p.g1.med <- g1.p.dat %>% summarise_all(median)
p.g2.med <- g2.p.dat %>% summarise_all(median)

g1.low<-as.numeric(g1.pl %>% summarise_all(mean, na.rm=TRUE))
g1.high<-as.numeric(g1.ph %>% summarise_all(mean, na.rm=TRUE))
g2.low<-as.numeric(g2.pl %>% summarise_all(mean, na.rm=TRUE))
g2.high<-as.numeric(g2.ph %>% summarise_all(mean, na.rm=TRUE))
x.g1.p<-mean(g1.p)#Overall spp. means
x.g2.p<-mean(g2.p)

med.g1.p<-median(g1.p)#Overall treatment medians
med.g2.p<-median(g2.p)

means.p<-c(x.g1.p, x.g2.p)
meds.p<-c(med.g1.p, med.g2.p)
sd.g1.p<-sd(g1.p)
sd.g2.p<-sd(g2.p)

sd.p<-c(sd.g1.p, sd.g2.p)

#Add panel of treatment-specific temporal recapture
plot(x=(1:14),y= p.g1.med, type="b", pch=1, col="salmon1",lty=3, bty='l',las=1,
     ylim=c(0,1), ylab=expression("Recapture probability ("~italic(rho)~")"), xlab="Recapture Date",xaxt="n")
axis(1,at=seq(2,14,2),labels=F)
text(labels=recapdates1,y=-.1,srt=45,adj=1,x=seq(2,14,2),xpd=T,cex = 0.9)
segments((1:14), g1.low, (1:14), g1.high, col="salmon1")
points(x=(1:14)+.1,p.g2.med, type="b", pch=6, col="deepskyblue3", lty=2)
segments((1:14)+.1, g2.low, (1:14)+.1, g2.high, col="deepskyblue3")
mtext("B)", side=3, at=1, las=0, line=0)
par(new = TRUE)
plot(x=(1:14), ao_stdtempc[1:14], type="b", lty=5, lwd=2, col=1, axes = FALSE, bty = "n", xlab = "", ylab = "", 
     ylim=c(-3,3), pch=2)
segments((1:14), ao_stdtempc[1:14]-ao_stdtempsd[1:14], (1:14), ao_stdtempc[1:14]+ao_stdtempsd[1:14], col=1)
axis(side=4, at = pretty(c(-3.2,3)))
mtext(expression(Scaled~Mean~Air~Temp~(C)), side=4, las=0, line=2)
dev.off()

#Group effect on recapture
plot(density(ao_final_fit$sims.list$p.group[,1]))#L1J1
lines(density(ao_final_fit$sims.list$p.group[,2]), col=2)#L1J3

#Group effect on survival
plot(density(ao_final_fit$sims.list$phi.group[,1]))#L1J1
lines(density(ao_final_fit$sims.list$phi.group[,2]), col=2)#L1J3

#Calculate % of posterior of differences overlapping zero for recapture
pJ1.J3_f<-round(sum(ifelse((ao_final_fit$sims.list$p.group[,1]-ao_final_fit$sims.list$p.group[,2])<=0,1,0))/
                      #sum(ao_final_fit$sims.list$p.group[,3]-ao_final_fit$sims.list$p.group[,1])) * 100
                      length(ao_final_fit$sims.list$p.group[,2]),2)

#Calculate % of posterior of differences overlapping zero
phiJ1.J3_f<-round(sum(ifelse((ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,2])<=0,1,0))/
                        #sum(ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,3])) * 100
                        length(ao_final_fit$sims.list$phi.group[,2]),2)


#If difference of posteriors overlaps zero, no significant difference for survival
png("Results/FigS4.png",width = 7, height = 9,units="in",res=500)
par(mfrow=c(1,2),mar=c(3,4,2,1))
plot(density(ao_final_fit$sims.list$p.group[,1]-ao_final_fit$sims.list$p.group[,2]),main="p",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",pJ1.J3_f,sep=" "),adj=0.9)

plot(density(ao_final_fit$sims.list$phi.group[,1]-ao_final_fit$sims.list$phi.group[,2]),main="phi",xlab="")
abline(v=0,col="red")
mtext(line=-2,side=3,text = paste("f =",phiJ1.J3_f,sep=" "),adj=0.9)
