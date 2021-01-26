ndep.rcp26  <- read.table('ndep_rcp26.dat', header=FALSE, col.names=c("year","input") )
ndep.rcp85  <- read.table('ndep_rcp85.dat', header=FALSE, col.names=c("year","input") )
nfert.rcp26 <- read.table('nfert_rcp26.dat', header=FALSE, col.names=c("year","input") )
nfert.rcp85 <- read.table('nfert_rcp85.dat', header=FALSE, col.names=c("year","input") )


## PLOTTING

## N-dep
pdf( "/alphadata01/bstocker/multiGHG_analysis/fig/ndep.pdf", width=7, height=6 )

par( xaxs="i", yaxs="i", las=1, cex.lab=1.5, cex.axis=1.3)

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- 0
ylim2 <- 80

plot( ndep.rcp26$year[ndep.rcp26$year<=2006], ndep.rcp26$input[ndep.rcp26$year<=2006]*1e-12,
     type="l", xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2), axes=FALSE, lwd=2,
     xlab="year AD", ylab="TgN/yr"
     )
lines( ndep.rcp26$year[ndep.rcp26$year>=2006], ndep.rcp26$input[ndep.rcp26$year>=2006]*1e-12,
     col="blue", lwd=2
     )
lines( ndep.rcp85$year[ndep.rcp85$year>=2006], ndep.rcp85$input[ndep.rcp85$year>=2006]*1e-12,
     col="red", lwd=2
     )

axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=5),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=5),labels=F,tck=-0.01)

text( 1910, 70, "N-deposition", cex=1.5, adj=c(0,0), font=1 )

legend( "bottomright", c("historical","RCP 2.6","RCP 8.5"), lty=1, bty="n", lwd=2, col=c("black","blue","red"), cex=1.5 )

dev.off()


## N-fert
pdf( "/alphadata01/bstocker/multiGHG_analysis/fig/nfert.pdf", width=7, height=6 )

par( xaxs="i", yaxs="i", las=1, cex.lab=1.5, cex.axis=1.3 )

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- 0
ylim2 <- 200

plot( nfert.rcp26$year[nfert.rcp26$year<=2006], nfert.rcp26$input[nfert.rcp26$year<=2006]*1e-12,
     type="l", xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2), axes=FALSE, lwd=2,
     xlab="year AD", ylab="TgN/yr"
     )
lines( nfert.rcp26$year[nfert.rcp26$year>=2006], nfert.rcp26$input[nfert.rcp26$year>=2006]*1e-12,
     col="blue", lwd=2
     )
lines( nfert.rcp85$year[nfert.rcp85$year>=2006], nfert.rcp85$input[nfert.rcp85$year>=2006]*1e-12,
     col="red", lwd=2
     )

axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=-0.01)

text( 1910, 180, "N-fertiliser input", cex=1.5, adj=c(0,0), font=1 )

legend( "bottomright", c("historical","RCP 2.6","RCP 8.5"), lty=1, bty="n", lwd=2, col=c("black","blue","red"), cex=1.5 )

dev.off()
