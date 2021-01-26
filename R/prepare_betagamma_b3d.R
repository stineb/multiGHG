## output is a list of data frames
sens_b3d <- list()

setup <- "_NrP"

## read beta sensitivities (only one climate pattern: no change in climate)
beta <- read.table( paste("/alphadata01/bstocker/output_multiGHG/beta_OO8_",setup,".dat",sep=""),
                    col.names=c("year","dT","dCO2","dC","dcN2O","dcCH4")
                   )
en2o <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8_C_",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
beta$deN2O <- en2o$tot - ctrl$tot

ech4 <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8_C_",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
beta$deCH4 <- ech4$tot - ctrl$tot

## read gamma sensitivities as data frame for each climate pattern
## Hadley
HA <- read.table( paste("/alphadata01/bstocker/output_multiGHG/gamma_HA8_",setup,".dat",sep=""),
                    col.names=c("year","dT","dCO2","dC","dcN2O","dcCH4")
                   )
en2o <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_HA8__T",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
HA$deN2O <- en2o$tot - ctrl$tot
ech4 <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_HA8__T",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
HA$deCH4 <- ech4$tot - ctrl$tot

## MPI
MP <- read.table( paste("/alphadata01/bstocker/output_multiGHG/gamma_MP8_",setup,".dat",sep=""),
                    col.names=c("year","dT","dCO2","dC","dcN2O","dcCH4")
                   )
en2o <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_MP8__T",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
MP$deN2O <- en2o$tot - ctrl$tot
ech4 <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_MP8__T",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
MP$deCH4 <- ech4$tot - ctrl$tot

## IPSL
IP <- read.table( paste("/alphadata01/bstocker/output_multiGHG/gamma_IP8_",setup,".dat",sep=""),
                    col.names=c("year","dT","dCO2","dC","dcN2O","dcCH4")
                   )
en2o <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_IP8__T",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
IP$deN2O <- en2o$tot - ctrl$tot
ech4 <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_IP8__T",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
IP$deCH4 <- ech4$tot - ctrl$tot

## MIROC
MI <- read.table( paste("/alphadata01/bstocker/output_multiGHG/gamma_MI8_",setup,".dat",sep=""),
                    col.names=c("year","dT","dCO2","dC","dcN2O","dcCH4")
                   )
en2o <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_MI8__T",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
MI$deN2O <- en2o$tot - ctrl$tot
ech4 <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_MI8__T",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
MI$deCH4 <- ech4$tot - ctrl$tot

## CCSM
CC <- read.table( paste("/alphadata01/bstocker/output_multiGHG/gamma_CC8_",setup,".dat",sep=""),
                    col.names=c("year","dT","dCO2","dC","dcN2O","dcCH4")
                   )
en2o <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_CC8__T",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".n2o.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
CC$deN2O <- en2o$tot - ctrl$tot
ech4 <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_CC8__T",setup,".ch4_em.out",sep=""),
                   col.names=c("year","tot","nat","peat","crop","past","built") )
ctrl <- read.table( paste("/alphadata03/roth/results/multiGHG/lpj_ascii/trans_OO8___",setup,".ch4_em.out",sep=""), 
                   col.names=c("year","tot","nat","peat","crop","past","built") )
CC$deCH4 <- ech4$tot - ctrl$tot

gamma <- list( HA=HA, MP=MP, IP=IP, MI=MI, CC=CC )


## //////////////////////////////////////////////////////////////////////
## Calculate gamma values
## ----------------------------------------------------------------------
## based on cumulative signal
HA <- gamma$HA$dC[which(gamma$HA$year==2100.5)]/gamma$HA$dT[which(gamma$HA$year==2100.5)]
MP <- gamma$MP$dC[which(gamma$MP$year==2100.5)]/gamma$MP$dT[which(gamma$MP$year==2100.5)]
IP <- gamma$IP$dC[which(gamma$IP$year==2100.5)]/gamma$IP$dT[which(gamma$IP$year==2100.5)]
MI <- gamma$MI$dC[which(gamma$MI$year==2100.5)]/gamma$MI$dT[which(gamma$MI$year==2100.5)]
CC <- gamma$CC$dC[which(gamma$CC$year==2100.5)]/gamma$CC$dT[which(gamma$CC$year==2100.5)]
dC <- list( HA=HA, MP=MP, IP=IP, MI=MI, CC=CC )

HA <- gamma$HA$deN2O[which(gamma$HA$year==2100.5)]/gamma$HA$dT[which(gamma$HA$year==2100.5)]
MP <- gamma$MP$deN2O[which(gamma$MP$year==2100.5)]/gamma$MP$dT[which(gamma$MP$year==2100.5)]
IP <- gamma$IP$deN2O[which(gamma$IP$year==2100.5)]/gamma$IP$dT[which(gamma$IP$year==2100.5)]
MI <- gamma$MI$deN2O[which(gamma$MI$year==2100.5)]/gamma$MI$dT[which(gamma$MI$year==2100.5)]
CC <- gamma$CC$deN2O[which(gamma$CC$year==2100.5)]/gamma$CC$dT[which(gamma$CC$year==2100.5)]
deN2O <- list( HA=HA, MP=MP, IP=IP, MI=MI, CC=CC )

HA <- gamma$HA$deCH4[which(gamma$HA$year==2100.5)]/gamma$HA$dT[which(gamma$HA$year==2100.5)]
MP <- gamma$MP$deCH4[which(gamma$MP$year==2100.5)]/gamma$MP$dT[which(gamma$MP$year==2100.5)]
IP <- gamma$IP$deCH4[which(gamma$IP$year==2100.5)]/gamma$IP$dT[which(gamma$IP$year==2100.5)]
MI <- gamma$MI$deCH4[which(gamma$MI$year==2100.5)]/gamma$MI$dT[which(gamma$MI$year==2100.5)]
CC <- gamma$CC$deCH4[which(gamma$CC$year==2100.5)]/gamma$CC$dT[which(gamma$CC$year==2100.5)]
deCH4 <- list( HA=HA, MP=MP, IP=IP, MI=MI, CC=CC )

cum <- list( dC=dC, deN2O=deN2O, deCH4=deCH4 )

## based on linear regression
HA <- lm( gamma$HA$dC ~ gamma$HA$dT )
MP <- lm( gamma$MP$dC ~ gamma$MP$dT )
IP <- lm( gamma$IP$dC ~ gamma$IP$dT )
MI <- lm( gamma$MI$dC ~ gamma$MI$dT )
CC <- lm( gamma$CC$dC ~ gamma$CC$dT )
dC <- list( HA=HA, MP=MP, IP=IP, MI=MI, CC=CC )

HA <- lm( gamma$HA$deN2O ~ gamma$HA$dT )
MP <- lm( gamma$MP$deN2O ~ gamma$MP$dT )
IP <- lm( gamma$IP$deN2O ~ gamma$IP$dT )
MI <- lm( gamma$MI$deN2O ~ gamma$MI$dT )
CC <- lm( gamma$CC$deN2O ~ gamma$CC$dT )
deN2O <- list( HA=HA, MP=MP, IP=IP, MI=MI, CC=CC )

HA <- lm( gamma$HA$deCH4 ~ gamma$HA$dT )
MP <- lm( gamma$MP$deCH4 ~ gamma$MP$dT )
IP <- lm( gamma$IP$deCH4 ~ gamma$IP$dT )
MI <- lm( gamma$MI$deCH4 ~ gamma$MI$dT )
CC <- lm( gamma$CC$deCH4 ~ gamma$CC$dT )
deCH4 <- list( HA=HA, MP=MP, IP=IP, MI=MI, CC=CC )

reg <- list( dC=dC, deN2O=deN2O, deCH4=deCH4 )

gamma.vals <- list( cum=cum, reg=reg )


## //////////////////////////////////////////////////////////////////////
## Calculate beta values
## ----------------------------------------------------------------------
## based on cumulative signal
dC    <- beta$dC[which(beta$year==2100.5)] / beta$dCO2[which(beta$year==2100.5)]
deN2O <- beta$deN2O[which(beta$year==2100.5)] / beta$dCO2[which(beta$year==2100.5)]
deCH4 <- beta$deCH4[which(beta$year==2100.5)] / beta$dCO2[which(beta$year==2100.5)]
CO2pT <- beta$dCO2[which(beta$year==2100.5)] / beta$dT[which(beta$year==2100.5)]
cum <- list( dC=dC, deN2O=deN2O, deCH4=deCH4, CO2pT=CO2pT )

## based on linear regression
dC    <- lm( beta$dC ~ beta$dCO2 )
deN2O <- lm( beta$deN2O ~ beta$dCO2 )
deCH4 <- lm( beta$deCH4 ~ beta$dCO2 )

reg <- list( dC=dC, deN2O=deN2O, deCH4=deCH4 )

beta.vals <- list( cum=cum, reg=reg )



## //////////////////////////////////////////////////////////////////////
## Save the whole thing
## ----------------------------------------------------------------------
## attach all to the main list and save
sens_b3d$beta.data <- beta
sens_b3d$gamma.data <- gamma
sens_b3d$gamma.vals <- gamma.vals
sens_b3d$beta.vals <- beta.vals

save( sens_b3d, file="sens_b3d.Rdata" )


## //////////////////////////////////////////////////////////////////////
## PLOT dC VS T
## ----------------------------------------------------------------------
pdf( paste('/alphadata01/bstocker/multiGHG_analysis/gamma_dC_',setup,'.pdf',sep=""), width=6, height=6 )
xlim <- c(0,10)
ylim <- c(-1000,0)
par( xaxs="i", yaxs="i", las=1  )

plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"T ["^o,"C]")), ylab=expression(paste(Delta,"C [PgC]")) )
axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=40),labels=F,tck=-0.01)
axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=40),labels=F,tck=-0.01)

cols <- c(rgb(0,1,0,0.5),NA,NA,NA,rgb(0,0,1,0.5))

lines( gamma$HA$dT, gamma$HA$dC, col=cols[5], lwd=2 )
lines( gamma$MP$dT, gamma$MP$dC, col=cols[5], lwd=2 )
lines( gamma$IP$dT, gamma$IP$dC, col=cols[5], lwd=2 )
lines( gamma$MI$dT, gamma$MI$dC, col=cols[5], lwd=2 )
lines( gamma$CC$dT, gamma$CC$dC, col=cols[5], lwd=2 )

points( gamma$HA$dT[which(gamma$HA$year==2100.5)], gamma$HA$dC[which(gamma$HA$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$MP$dT[which(gamma$MP$year==2100.5)], gamma$MP$dC[which(gamma$MP$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$IP$dT[which(gamma$IP$year==2100.5)], gamma$IP$dC[which(gamma$IP$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$MI$dT[which(gamma$MI$year==2100.5)], gamma$MI$dC[which(gamma$MI$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$CC$dT[which(gamma$CC$year==2100.5)], gamma$CC$dC[which(gamma$CC$year==2100.5)], col="red", lwd=2, pch=4 )

abline( 0, gamma.vals$cum$dC$HA, col="red", lty=2 )
abline( 0, gamma.vals$cum$dC$MP, col="red", lty=2 )
abline( 0, gamma.vals$cum$dC$IP, col="red", lty=2 )
abline( 0, gamma.vals$cum$dC$MI, col="red", lty=2 )
abline( 0, gamma.vals$cum$dC$CC, col="red", lty=2 )

abline( gamma.vals$reg$dC$HA, col="red", lty=3 )
abline( gamma.vals$reg$dC$MP, col="red", lty=3 )
abline( gamma.vals$reg$dC$IP, col="red", lty=3 )
abline( gamma.vals$reg$dC$MI, col="red", lty=3 )
abline( gamma.vals$reg$dC$CC, col="red", lty=3 )

dev.off()

## Result for 2100 to standard output:
print("GAMMA dC at 2100 AD (GtC/K)")
print("------CUMULATIVE----------")
print(paste("HA: ", gamma.vals$cum$dC$HA ))
print(paste("MP: ", gamma.vals$cum$dC$MP ))
print(paste("IP: ", gamma.vals$cum$dC$IP ))
print(paste("MI: ", gamma.vals$cum$dC$MI ))
print(paste("CC: ", gamma.vals$cum$dC$CC ))
print(paste("MEAN: ", mean( c(gamma.vals$cum$dC$MP, gamma.vals$cum$dC$HA, gamma.vals$cum$dC$IP, gamma.vals$cum$dC$MI, gamma.vals$cum$dC$CC )) ))
print("------LIN. REG.----------")
print(paste("HA: ", gamma.vals$reg$dC$HA$coefficients[2] ))
print(paste("MP: ", gamma.vals$reg$dC$MP$coefficients[2] ))
print(paste("IP: ", gamma.vals$reg$dC$IP$coefficients[2] ))
print(paste("MI: ", gamma.vals$reg$dC$MI$coefficients[2] ))
print(paste("CC: ", gamma.vals$reg$dC$CC$coefficients[2]))
print(paste("MEAN: ", mean( c(gamma.vals$reg$dC$MP, gamma.vals$reg$dC$HA, gamma.vals$reg$dC$IP, gamma.vals$reg$dC$MI, gamma.vals$reg$dC$CC )) ))


## //////////////////////////////////////////////////////////////////////
## PLOT dC VS CO2
## ----------------------------------------------------------------------
pdf( paste('/alphadata01/bstocker/multiGHG_analysis/beta_dC_',setup,'.pdf',sep=""), width=6, height=6 )
xlim <- c(-200,1800)
ylim <- c(0,650)
par( xaxs="i", yaxs="i", las=1  )

plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"CO"[2]," [ppm]")), ylab=expression(paste(Delta,"C [PgC]")) )
axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=100),labels=F,tck=-0.01)
axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=100),labels=F,tck=-0.01)
axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
box(lwd=2)

cols <- c("green",NA,NA,NA,"blue")

lines( beta$dCO2, beta$dC, col=cols[5], lwd=2 )

points( beta$dCO2[which(beta$year==2100.5)], beta$dC[which(beta$year==2100.5)], col="red", lwd=2, pch=4 )

abline( 0, beta.vals$cum$dC, col="red", lty=2 )
abline( beta.vals$reg$dC, col="red", lty=3 )

text( 500, 50,  paste("beta from regression (PgC/ppm):", as.character(format(sens_b3d$beta.vals$reg$dC$coefficients[2], digits=2))), adj=c(0,0) )
text( 500, 100, paste("beta cum. by 2100 AD (PgC/ppm):", as.character(format(sens_b3d$beta.vals$cum$dC, digits=2))), adj=c(0,0) )

dev.off()

## Result for 2100 to standard output:
print("BETA dC at 2100 (GtC/ppm)")
print("-------CUMULATIVE---------")
print( beta.vals$cum$dC )
print("-------LIN. REG.---------")
print( beta.vals$reg$dC$coefficients[2] )

## //////////////////////////////////////////////////////////////////////
## PLOT deN2O VS T
## ----------------------------------------------------------------------
pdf( paste('/alphadata01/bstocker/multiGHG_analysis/gamma_deN2O_',setup,'.pdf',sep=""), width=6, height=6 )
xlim <- c(0,10)
ylim <- c(0,10)
par( xaxs="i", yaxs="i", las=1  )

plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"T ["^o,"C]")), ylab=expression(paste(Delta,"eN"[2],"O [TgNyr"^-1,"]")) )
axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)

cols <- c(rgb(0,1,0,0.5),NA,NA,NA,rgb(0,0,1,0.5))

points( gamma$HA$dT, gamma$HA$deN2O, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )
points( gamma$MP$dT, gamma$MP$deN2O, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )
points( gamma$IP$dT, gamma$IP$deN2O, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )
points( gamma$MI$dT, gamma$MI$deN2O, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )
points( gamma$CC$dT, gamma$CC$deN2O, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )

points( gamma$HA$dT[which(gamma$HA$year==2100.5)], gamma$HA$deN2O[which(gamma$HA$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$MP$dT[which(gamma$MP$year==2100.5)], gamma$MP$deN2O[which(gamma$MP$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$IP$dT[which(gamma$IP$year==2100.5)], gamma$IP$deN2O[which(gamma$IP$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$MI$dT[which(gamma$MI$year==2100.5)], gamma$MI$deN2O[which(gamma$MI$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$CC$dT[which(gamma$CC$year==2100.5)], gamma$CC$deN2O[which(gamma$CC$year==2100.5)], col="red", lwd=2, pch=4 )

abline( 0, gamma.vals$cum$deN2O$HA, col="red", lty=2 )
abline( 0, gamma.vals$cum$deN2O$MP, col="red", lty=2 )
abline( 0, gamma.vals$cum$deN2O$IP, col="red", lty=2 )
abline( 0, gamma.vals$cum$deN2O$MI, col="red", lty=2 )
abline( 0, gamma.vals$cum$deN2O$CC, col="red", lty=2 )

abline( gamma.vals$reg$deN2O$HA, col="red", lty=3 )
abline( gamma.vals$reg$deN2O$MP, col="red", lty=3 )
abline( gamma.vals$reg$deN2O$IP, col="red", lty=3 )
abline( gamma.vals$reg$deN2O$MI, col="red", lty=3 )
abline( gamma.vals$reg$deN2O$CC, col="red", lty=3 )

dev.off()

## Result for 2100 to standard output:
print("GAMMA deN2O at 2100 AD (GtC/K)")
print("------CUMULATIVE----------")
print(paste("HA: ", gamma.vals$cum$deN2O$HA ))
print(paste("MP: ", gamma.vals$cum$deN2O$MP))
print(paste("IP: ", gamma.vals$cum$deN2O$IP))
print(paste("MI: ", gamma.vals$cum$deN2O$MI))
print(paste("CC: ", gamma.vals$cum$deN2O$CC))
print(paste("MEAN: ", mean( c(gamma.vals$cum$deN2O$MP, gamma.vals$cum$deN2O$HA, gamma.vals$cum$deN2O$IP, gamma.vals$cum$deN2O$MI, gamma.vals$cum$deN2O$CC )) ))
print("------LIN. REG.----------")
print(paste("HA: ", gamma.vals$reg$deN2O$HA$coefficients[2] ))
print(paste("MP: ", gamma.vals$reg$deN2O$MP$coefficients[2] ))
print(paste("IP: ", gamma.vals$reg$deN2O$IP$coefficients[2] ))
print(paste("MI: ", gamma.vals$reg$deN2O$MI$coefficients[2] ))
print(paste("CC: ", gamma.vals$reg$deN2O$CC$coefficients[2]))
print(paste("MEAN: ", mean( c(gamma.vals$reg$deN2O$MP, gamma.vals$reg$deN2O$HA, gamma.vals$reg$deN2O$IP, gamma.vals$reg$deN2O$MI, gamma.vals$reg$deN2O$CC )) ))

## //////////////////////////////////////////////////////////////////////
## PLOT deN2O VS CO2
## ----------------------------------------------------------------------
pdf( paste('/alphadata01/bstocker/multiGHG_analysis/beta_deN2O_',setup,'.pdf',sep=""), width=6, height=6 )
xlim <- c(-200,1800)
ylim <- c(-0.5,3.5)
par( xaxs="i", yaxs="i", las=1  )

plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"CO"[2]," [ppm]")), ylab=expression(paste(Delta,"eN"[2],"O [TgNyr"^-1,"]")) )
axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=100),labels=F,tck=-0.01)
axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=100),labels=F,tck=-0.01)
axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
box(lwd=2)

cols <- c("green",NA,NA,NA,"blue")

points( beta$dCO2, beta$deN2O, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )

points( beta$dCO2[which(beta$year==2100.5)], beta$deN2O[which(beta$year==2100.5)], col="red", lwd=2, pch=4 )

abline( 0, beta.vals$cum$deN2O, col="red", lty=2 )
abline( beta.vals$reg$deN2O, col="red", lty=3 )

text( 0.25, 500, "LPX  C-N version", col="blue", adj=c(0,0) )

dev.off()

## Result for 2100 to standard output:
print("BETA deN2O at 2100 (GtC/ppm)")
print("-------CUMULATIVE---------")
print( beta.vals$cum$deN2O )
print("-------LIN. REG.---------")
print( beta.vals$reg$deN2O$coefficients[2] )



## //////////////////////////////////////////////////////////////////////
## PLOT deCH4 VS T
## ----------------------------------------------------------------------
pdf( paste('/alphadata01/bstocker/multiGHG_analysis/gamma_deCH4_',setup,'.pdf',sep=""), width=6, height=6 )
xlim <- c(0,10)
ylim <- c(0,100)
par( xaxs="i", yaxs="i", las=1  )

plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"T ["^o,"C]")), ylab=expression(paste(Delta,"eCH"[4]," [TgNyr"^-1,"]")) )
axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=4),labels=F,tck=-0.01)
axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=4),labels=F,tck=-0.01)

cols <- c(rgb(0,1,0,0.5),NA,NA,NA,rgb(0,0,1,0.5))

points( gamma$HA$dT, gamma$HA$deCH4, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )
points( gamma$MP$dT, gamma$MP$deCH4, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )
points( gamma$IP$dT, gamma$IP$deCH4, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )
points( gamma$MI$dT, gamma$MI$deCH4, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )
points( gamma$CC$dT, gamma$CC$deCH4, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )

points( gamma$HA$dT[which(gamma$HA$year==2100.5)], gamma$HA$deCH4[which(gamma$HA$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$MP$dT[which(gamma$MP$year==2100.5)], gamma$MP$deCH4[which(gamma$MP$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$IP$dT[which(gamma$IP$year==2100.5)], gamma$IP$deCH4[which(gamma$IP$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$MI$dT[which(gamma$MI$year==2100.5)], gamma$MI$deCH4[which(gamma$MI$year==2100.5)], col="red", lwd=2, pch=4 )
points( gamma$CC$dT[which(gamma$CC$year==2100.5)], gamma$CC$deCH4[which(gamma$CC$year==2100.5)], col="red", lwd=2, pch=4 )

abline( 0, gamma.vals$cum$deCH4$HA, col="red", lty=2 )
abline( 0, gamma.vals$cum$deCH4$MP, col="red", lty=2 )
abline( 0, gamma.vals$cum$deCH4$IP, col="red", lty=2 )
abline( 0, gamma.vals$cum$deCH4$MI, col="red", lty=2 )
abline( 0, gamma.vals$cum$deCH4$CC, col="red", lty=2 )

abline( gamma.vals$reg$deCH4$HA, col="red", lty=3 )
abline( gamma.vals$reg$deCH4$MP, col="red", lty=3 )
abline( gamma.vals$reg$deCH4$IP, col="red", lty=3 )
abline( gamma.vals$reg$deCH4$MI, col="red", lty=3 )
abline( gamma.vals$reg$deCH4$CC, col="red", lty=3 )

dev.off()

## Result for 2100 to standard output:
print("GAMMA deCH4 at 2100 AD (GtC/K)")
print("------CUMULATIVE----------")
print(paste("HA: ", gamma.vals$cum$deCH4$HA ))
print(paste("MP: ", gamma.vals$cum$deCH4$MP))
print(paste("IP: ", gamma.vals$cum$deCH4$IP))
print(paste("MI: ", gamma.vals$cum$deCH4$MI))
print(paste("CC: ", gamma.vals$cum$deCH4$CC))
print(paste("MEAN: ", mean( c(gamma.vals$cum$deCH4$MP, gamma.vals$cum$deCH4$HA, gamma.vals$cum$deCH4$IP, gamma.vals$cum$deCH4$MI, gamma.vals$cum$deCH4$CC )) ))
print("------LIN. REG.----------")
print(paste("HA: ", gamma.vals$reg$deCH4$HA$coefficients[2] ))
print(paste("MP: ", gamma.vals$reg$deCH4$MP$coefficients[2] ))
print(paste("IP: ", gamma.vals$reg$deCH4$IP$coefficients[2] ))
print(paste("MI: ", gamma.vals$reg$deCH4$MI$coefficients[2] ))
print(paste("CC: ", gamma.vals$reg$deCH4$CC$coefficients[2]))
print(paste("MEAN: ", mean( c(gamma.vals$reg$deCH4$MP, gamma.vals$reg$deCH4$HA, gamma.vals$reg$deCH4$IP, gamma.vals$reg$deCH4$MI, gamma.vals$reg$deCH4$CC )) ))

## //////////////////////////////////////////////////////////////////////
## PLOT deCH4 VS CO2
## ----------------------------------------------------------------------
pdf( paste( '/alphadata01/bstocker/multiGHG_analysis/beta_deCH4_',setup,'.pdf',sep=""), width=6, height=6 )
xlim <- c(-200,1800)
ylim <- c(0,200)
par( xaxs="i", yaxs="i", las=1  )

plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"CO"[2]," [ppm]")), ylab=expression(paste(Delta,"eCH"[4]," [TgNyr"^-1,"]")) )
axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=100),labels=F,tck=-0.01)
axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=100),labels=F,tck=-0.01)
axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=10),labels=F,tck=-0.01)
box(lwd=2)

cols <- c("green",NA,NA,NA,"blue")

points( beta$dCO2, beta$deCH4, col=rgb(0,0,1,0.2), lty=1, lwd=1, pch=21, bg=rgb(0,0,1,0.3) )

points( beta$dCO2[which(beta$year==2100.5)], beta$deCH4[which(beta$year==2100.5)], col="red", lwd=2, pch=4 )

abline( 0, beta.vals$cum$deCH4, col="red", lty=2 )
abline( beta.vals$reg$deCH4, col="red", lty=3 )

text( 0.25, 500, "LPX  C-N version", col="blue", adj=c(0,0) )

dev.off()

## Result for 2100 to standard output:
print("BETA deCH4 at 2100 (GtC/ppm)")
print("-------CUMULATIVE---------")
print( beta.vals$cum$deCH4 )
print("-------LIN. REG.---------")
print( beta.vals$reg$deCH4$coefficients[2] )





#setups <- c('L__P','_NrP','LNr_','LN_P','LNrP')
#nsetups <- length(setups)
#nmodls <- 5
#ltime <- 535

## coupling <- "T" # select 'C', 'T', or 'CT'

## if (coupling=="T") {
## ## //////////////////////////////////////////
## ## GAMMA
## ## ------------------------------------------
  
##   dC <- array(NA,dim=c(ltime,nmodls,nsetups))
##   dT <- array(NA,dim=c(ltime,nmodls,nsetups))
  
##   for (setup in seq(nsetups)){
##     tmp <- read.table(paste(
##                             "/alphadata01/bstocker/output_multiGHG/gamma_dC_",
##                             setups[setup],
##                             ".dat",sep=""
##                             ))
##     for (modl in seq(nmodls)){
##       dC[,modl,setup] <- tmp[,modl]
##     }
    
##     tmp <- read.table(paste(
##                             "/alphadata01/bstocker/output_multiGHG/gamma_dT_",
##                             setups[setup],
##                             ".dat",sep=""
##                             ))
##     for (modl in seq(nmodls)){
##       dT[,modl,setup] <- tmp[,modl]
##     }
##   }
  
## } else if (coupling=="C"){
## ## //////////////////////////////////////////
## ## BETA
## ## ------------------------------------------

##   dC   <- array(NA,dim=c(ltime,nsetups))
##   dCO2 <- array(NA,dim=c(ltime,nsetups))
  
##   for (setup in seq(nsetups)){
##     tmp <- read.table(paste(
##                             "/alphadata01/bstocker/output_multiGHG/beta_",
##                             setups[setup],
##                             ".dat",sep=""
##                             ),
##                       col.names=c("year","dCO2","dC","beta")
##                       )
##     dC[,setup] <- tmp$dC
##     dCO2[,setup] <- tmp$dCO2
##   }
## }

## if (coupling=="T") {
##   ## //////////////////////////////////////////////////////////////////////////////////////
##   ## PLOT dC VS T
##   ## --------------------------------------------------------------------------------------
##   pdf( '/alphadata01/bstocker/multiGHG_analysis/gamma_dC.pdf', width=6, height=6 )
##   xlim <- c(0,10)
##   ylim <- c(-1000,0)
##   par( xaxs="i", yaxs="i", las=1  )
  
##   plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"T ["^o,"C]")), ylab=expression(paste(Delta,"C [PgC]")) )
##   axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
##   axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=40),labels=F,tck=-0.01)
##   axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
##   axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=40),labels=F,tck=-0.01)
  
##   cols <- c(rgb(0,1,0,0.5),NA,NA,NA,rgb(0,0,1,0.5))
  
##   ## polygon( c( dT[,3,1], rev(dT[,1,1]) ), c( dC[,3,1], rev(dC[,1,1])), col=cols[1], border=NA )
##   ## polygon( c( dT[,2,5], rev(dT[,5,5]) ), c( dC[,2,5], rev(dC[,5,5])), col=cols[5], border=NA )
  
##   for (modl in seq(nmodls)){
##     for (setup in c(1,5)){
##       lines( dT[,modl,setup], dC[,modl,setup],col=cols[setup],lwd=2 )
##     }
##   }
  
##   text( 0.25, -400, "LPX  C-N version", col="blue", adj=c(0,0) )
##   text( 0.25, -450, "LPX  C-only version", col="green", adj=c(0,0) )
  
##   dev.off()
  
## } else if (coupling=="C"){
##   ## //////////////////////////////////////////////////////////////////////////////////////
##   ## PLOT dC VS CO2
##   ## --------------------------------------------------------------------------------------
##   pdf( '/alphadata01/bstocker/multiGHG_analysis/beta_dC.pdf', width=6, height=6 )
##   xlim <- c(-200,1800)
##   ylim <- c(0,650)
##   par( xaxs="i", yaxs="i", las=1  )
  
##   plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"CO"[2]," [ppm]")), ylab=expression(paste(Delta,"C [PgC]")) )
##   axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=100),labels=F,tck=-0.01)
##   axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
##   axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=100),labels=F,tck=-0.01)
##   axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
##   box(lwd=2)
  
##   cols <- c("green",NA,NA,NA,"blue")
##   for (setup in c(1,5)){
##     lines( dCO2[,setup], dC[,setup],col=cols[setup],lwd=2 )
##   }
  
##   text( 0.25, 500, "LPX  C-N version", col="blue", adj=c(0,0) )
##   text( 0.25, 470, "LPX  C-only version", col="green", adj=c(0,0) )
  
##   dev.off()
## }
