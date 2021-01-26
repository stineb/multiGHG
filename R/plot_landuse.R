landuse.rcp26  <- read.table('landuse_rcp26.dat', header=FALSE, col.names=c("year","total","crop","past") )
landuse.rcp85  <- read.table('landuse_rcp85.dat', header=FALSE, col.names=c("year","total","crop","past") )

landuse.rcp26[2:4] <- landuse.rcp26[2:4]*1e-12
landuse.rcp85[2:4] <- landuse.rcp85[2:4]*1e-12

## PLOTTING

## N-dep
pdf( "/alphadata01/bstocker/multiGHG_analysis/fig/landuse.pdf", width=7, height=5 )

par( xaxs="i", yaxs="i", las=1, cex.lab=1.5, cex.axis=1.3, mar=c(4,5,1,2) )

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- 0
ylim2 <- 60

## total
plot( landuse.rcp26$year[landuse.rcp26$year<=2006], landuse.rcp26$total[landuse.rcp26$year<=2006],
     type="l",
     xlim=c(xlim1,xlim2),
     ylim=c(ylim1,ylim2),
     axes=FALSE,
     lwd=2,
     xlab="year AD", ylab=expression(paste("10"^6,"km"^2))
     )
lines( landuse.rcp26$year[landuse.rcp26$year>=2006], landuse.rcp26$total[landuse.rcp26$year>=2006],
     col="blue", lwd=2
     )
lines( landuse.rcp85$year[landuse.rcp85$year>=2006], landuse.rcp85$total[landuse.rcp85$year>=2006],
     col="red", lwd=2
     )

## crop
lines( landuse.rcp26$year[landuse.rcp26$year<=2006], landuse.rcp26$crop[landuse.rcp26$year<=2006],
     col="black", lwd=2, lty=3
     )
lines( landuse.rcp26$year[landuse.rcp26$year>=2006], landuse.rcp26$crop[landuse.rcp26$year>=2006],
     col="blue", lwd=2, lty=3
     )
lines( landuse.rcp85$year[landuse.rcp85$year>=2006], landuse.rcp85$crop[landuse.rcp85$year>=2006],
     col="red", lwd=2, lty=3
     )

## past
lines( landuse.rcp26$year[landuse.rcp26$year<=2006], landuse.rcp26$past[landuse.rcp26$year<=2006],
     col="black", lwd=2, lty=5
     )
lines( landuse.rcp26$year[landuse.rcp26$year>=2006], landuse.rcp26$past[landuse.rcp26$year>=2006],
     col="blue", lwd=2, lty=5
     )
lines( landuse.rcp85$year[landuse.rcp85$year>=2006], landuse.rcp85$past[landuse.rcp85$year>=2006],
     col="red", lwd=2, lty=5
     )



axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=200),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=200),labels=F,tck=-0.01)

text( 1910, 50, "land use", cex=1.5, adj=c(0,0), font=1 )

legend( "bottomright", c("historical","RCP 2.6","RCP 8.5"), lty=1, bty="n", lwd=2, col=c("black","blue","red"), cex=1.5 )
legend( "bottom", c("total","crop","pasture"), lty=c(1,3,5), bty="n", lwd=2, cex=1.5 )

dev.off()

