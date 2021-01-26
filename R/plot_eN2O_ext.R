library(Hmisc)

e.n2o.rcp26 <- read.table('/alphadata01/bstocker/input_data/ghg_data/n2o_data/eN2Oext_bysources_rcp26_HARMONIZED.dat', header=TRUE )
e.n2o.rcp85 <- read.table('/alphadata01/bstocker/input_data/ghg_data/n2o_data/eN2Oext_bysources_rcp85_HARMONIZED.dat', header=TRUE )

e.n2o.oc.hist <- read.table('/alphadata01/bstocker/multiGHG_analysis/eN2O_oc_historical.dat', col.names=c("year","emission"))
e.n2o.oc.rcp26 <- read.table('/alphadata01/bstocker/multiGHG_analysis/eN2O_oc_rcp26.dat', col.names=c("year","emission"))
e.n2o.oc.rcp85 <- read.table('/alphadata01/bstocker/multiGHG_analysis/eN2O_oc_rcp85.dat', col.names=c("year","emission"))


## PLOTTING

## N-dep
pdf( "/alphadata01/bstocker/multiGHG_analysis/fig/eN2Oext.pdf", width=12, height=5.5 )

panel <- layout(matrix(c(1:2),1,2,byrow=TRUE), widths=c(12,12), heights=c(6))
##layout.show(panel)

par( xaxs="i", yaxs="i", las=1, cex.lab=1.5, cex.axis=1.3)

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- 0
ylim2 <- 10


plot( e.n2o.rcp26$year[e.n2o.rcp26$year<=2006], e.n2o.rcp26$total[e.n2o.rcp26$year<=2006],
     type="l",
     xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
     axes=FALSE, lwd=2,
     xlab="year AD", ylab="TgN/yr"
     )
lines( e.n2o.rcp26$year[e.n2o.rcp26$year>=2006], e.n2o.rcp26$total[e.n2o.rcp26$year>=2006],
     col="blue", lwd=2
     )
lines( e.n2o.rcp85$year[e.n2o.rcp85$year>=2006], e.n2o.rcp85$total[e.n2o.rcp85$year>=2006],
     col="red", lwd=2
     )
lines( e.n2o.oc.hist$year, e.n2o.oc.hist$emission, col="green", lwd=2 )
lines( e.n2o.rcp85$year[e.n2o.rcp85$year>=2006],
      e.n2o.rcp85$year[e.n2o.rcp85$year>=2006]*0+e.n2o.oc.hist$emission[length(e.n2o.oc.hist$emission)],
      col="green", lwd=2
      )

axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=0.5),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=0.5),labels=F,tck=-0.01)

#text( 1910, 8.5, expression(paste(bold("eN")[bold(2)],bold("O")^EXT)), cex=1.5, adj=c(0,0), font=1 )

legend( "topleft", c("Zaehle et al., 2011", "RCP 2.6","RCP 8.5","ocean"), lty=1, bty="n", lwd=2, col=c("black","blue","red","green"), cex=1.5)

## subplot(
##         plot(e.n2o.rcp26$year[e.n2o.rcp26$year<=2006], e.n2o.rcp26$fossil[e.n2o.rcp26$year<=2006],
##              type="l"
##              ),
##         1970, 7, size=c(1,1)
##         )
xlim2 <- 2005
ylim2 <- 3

plot(e.n2o.rcp26$year[e.n2o.rcp26$year<=2006], e.n2o.rcp26$fossil[e.n2o.rcp26$year<=2006],
     type="l", xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2), col=6,xlab="year AD", ylab="TgN/yr",
     lwd=2, axes=FALSE
     )
lines(e.n2o.rcp26$year[e.n2o.rcp26$year<=2006], e.n2o.rcp26$fire[e.n2o.rcp26$year<=2006], col=8, lwd=2 )
lines(e.n2o.rcp26$year[e.n2o.rcp26$year<=2006], e.n2o.rcp26$manure[e.n2o.rcp26$year<=2006], col=5, lwd=2 )

axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=0.1),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=0.1),labels=F,tck=-0.01)
box(lwd=2)

legend( "topleft", c("domestic/industrial","fire","manure"), lty=1, bty="n", lwd=2, col=c(6,8,5), cex=1.5 )

dev.off()
