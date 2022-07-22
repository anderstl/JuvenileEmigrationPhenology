#Violin plot of treatment posteriors
library(vioplot)
logit<-function(x){y=exp(x)/(1+exp(x));return(y)}
pdf("Treatment-Effects.pdf", width = 9, height = 9)
#AMAN
par(mfrow=c(2,1), mai=c(0.8,2,0.5,0.5), mgp=c(7,2.5,0), bty='l')
vioplot(aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,1], 
        aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,2], 
        aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,3], 
        aa.cjs.trt.mass.cov.add5$sims.list$beta.g1[,4], 
        names=c("", "", "", ""),  
        col=c("plum4", "lightskyblue3", "peachpuff", "thistle1"),
        cex.lab=3, cex.axis=2.5)
mtext("Survival", side=2, las=0, line=6, cex=2.5)
mtext("a", side=3, at=0.6, las=0, line=1, cex=2.5)
mtext(c("A", "A", "A", "A"), side=3, at=c(1:4), las=0, line=0, cex=1.5)

par(mfrow=c(2,1), mai=c(0.8,2,0.5,0.5), mgp=c(7,2.5,0), bty='l')
vioplot(aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,1], 
        aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,2], 
        aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,3], 
        aa.cjs.trt.mass.cov.add5$sims.list$beta.g2[,4], 
        names=c("", "", "", ""),  
        col=c("plum4", "lightskyblue3", "peachpuff", "thistle1"),
        cex.lab=3, cex.axis=2.5)
mtext("Effect size- Recapture", side=2, las=0, line=6, cex=2.5)
mtext("a", side=3, at=0.6, las=0, line=1, cex=2.5)
mtext(c("A", "B", "B", "B"), side=3, at=c(1:4), las=0, line=0, cex=1.5)


#AMOP
vioplot(ao.cjs.trt.mass.cov.fixed3$sims.list$beta.a[,1], 
        ao.cjs.trt.mass.cov.fixed3$sims.list$beta.a[,2], 
        ao.cjs.trt.mass.cov.fixed3$sims.list$beta.a[,3], 
        ao.cjs.trt.mass.cov.fixed3$sims.list$beta.a[,4], 
        names=c("L1J1", "L1J3", "L3J1", "L3J3"),  
        ylim=c(-4, 2.7), col=c("plum4", "lightskyblue3", "peachpuff", "thistle1"),
        cex.lab=3, cex.axis=2.5)
mtext("Effect size- Recapture", side=2, las=0, line=6, cex=2.5)
mtext("b", side=3, at=0.6, las=0, line=1, cex=2.5)
mtext(c("A", "AB", "AB", "B"), side=3, at=c(1:4), las=0, line=0, cex=1.5)

vioplot(ao.cjs.trt.mass.cov.fixed3$sims.list$beta.e[,1], 
        ao.cjs.trt.mass.cov.fixed3$sims.list$beta.e[,2], 
        ao.cjs.trt.mass.cov.fixed3$sims.list$beta.e[,3], 
        ao.cjs.trt.mass.cov.fixed3$sims.list$beta.e[,4], 
        names=c("L1J1", "L1J3", "L3J1", "L3J3"),  
        ylim=c(-4, 2.7), col=c("plum4", "lightskyblue3", "peachpuff", "thistle1"),
        cex.lab=3, cex.axis=2.5)
mtext("Effect size- Recapture", side=2, las=0, line=6, cex=2.5)
mtext("b", side=3, at=0.6, las=0, line=1, cex=2.5)
mtext(c("A", "AB", "AB", "B"), side=3, at=c(1:4), las=0, line=0, cex=1.5)

dev.off()
