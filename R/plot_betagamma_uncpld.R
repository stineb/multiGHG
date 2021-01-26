
load("multiGHG_historical_timeseries_global.Rdata")
load("multiGHG_rcp26_timeseries_global.Rdata")
load("multiGHG_rcp85_timeseries_global.Rdata")

  ## ///////////////////////////////////////////////////////////////
  ## PLOT dC VS CO2
  ## ---------------------------------------------------------------
  pdf( '/alphadata01/bstocker/multiGHG_analysis/beta_dC_uncpld.pdf', width=6, height=6 )
  xlim <- c(380,940)
  ylim <- c(6,11)
  par( xaxs="i", yaxs="i", las=1  )
  
  plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste("CO"[2]," [ppm]")), ylab=expression(paste(Delta,"C [PgC]")) )
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=20),labels=F,tck=-0.01)
  axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=20),labels=F,tck=-0.01)
  axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
  box(lwd=2)
  par(new=T)
xxx
  points(
         dCO2.8[ which(dCO2.8$year==2006):which(dCO2.8$year==2100),2],
         e.n2o.fut[,1,withNr,1],
         col=rgb(0,0,1),
         lty=1,
         lwd=1,
         pch=21,
         bg=rgb(0,0,1,0.5)
         )
  par(new=T)
  points(
       dCO2.8[ which(dCO2.8$year==2006):which(dCO2.8$year==2100),2],
       e.n2o.fut[,1,noNr,1],
       col=rgb(0,0,1),
       lty=1,
       lwd=1,
       pch=21
       )
  abline(lreg.Nr,lwd=3,col=rgb(0,0,1,1))
  abline(lreg.noNr,lwd=3,col=rgb(0,0,1,0.5))
  
  
##  text(400,10.5,expression(paste("CO"[2]," sensitivity of N"[2],"O emissions")),adj=c(0,0),cex=1.5)
  
  text(565,6.8,paste("slope with Nr: ",as.character(format(lreg.Nr$coefficients[2],digits=3))),col="blue",adj=c(0,0))
  text(770,6.75,expression(paste("Tg N/yr/ppm")),adj=c(0,0),col="blue")
  text(565,6.5,paste("slope without Nr: ",as.character(format(lreg.noNr$coefficients[2],digits=2))),col="blue",adj=c(0,0))
  text(790,6.45,expression(paste("Tg N/yr/ppm")),adj=c(0,0),col="blue")
  
  dev.off()


xxx
