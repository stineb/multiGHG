
setups <- c('L__P','_NrP','LNr_','LN_P','LNrP')
nsetups <- length(setups)
nmodls <- 5
ltime <- 535

coupling <- "T" # select 'C', 'T', or 'CT'

if (coupling=="T") {
## //////////////////////////////////////////
## GAMMA
## ------------------------------------------
  
  dC <- array(NA,dim=c(ltime,nmodls,nsetups))
  dT <- array(NA,dim=c(ltime,nmodls,nsetups))
  
  for (setup in seq(nsetups)){
    tmp <- read.table(paste(
                            "/alphadata01/bstocker/output_multiGHG/gamma_dC_",
                            setups[setup],
                            ".dat",sep=""
                            ))
    for (modl in seq(nmodls)){
      dC[,modl,setup] <- tmp[,modl]
    }
    
    tmp <- read.table(paste(
                            "/alphadata01/bstocker/output_multiGHG/gamma_dT_",
                            setups[setup],
                            ".dat",sep=""
                            ))
    for (modl in seq(nmodls)){
      dT[,modl,setup] <- tmp[,modl]
    }
  }
  
} else if (coupling=="C"){
## //////////////////////////////////////////
## BETA
## ------------------------------------------

  dC   <- array(NA,dim=c(ltime,nsetups))
  dCO2 <- array(NA,dim=c(ltime,nsetups))
  
  for (setup in seq(nsetups)){
    tmp <- read.table(paste(
                            "/alphadata01/bstocker/output_multiGHG/beta_",
                            setups[setup],
                            ".dat",sep=""
                            ),
                      col.names=c("year","dCO2","dC","beta")
                      )
    dC[,setup] <- tmp$dC
    dCO2[,setup] <- tmp$dCO2
  }
}

if (coupling=="T") {
  ## //////////////////////////////////////////////////////////////////////////////////////
  ## PLOT dC VS T
  ## --------------------------------------------------------------------------------------
  pdf( '/alphadata01/bstocker/multiGHG_analysis/gamma_dC.pdf', width=6, height=6 )
  xlim <- c(0,10)
  ylim <- c(-1000,0)
  par( xaxs="i", yaxs="i", las=1  )
  
  plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"T ["^o,"C]")), ylab=expression(paste(Delta,"C [PgC]")) )
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
  axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=40),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
  axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=40),labels=F,tck=-0.01)
  
  cols <- c(rgb(0,1,0,0.5),NA,NA,NA,rgb(0,0,1,0.5))
  
  ## polygon( c( dT[,3,1], rev(dT[,1,1]) ), c( dC[,3,1], rev(dC[,1,1])), col=cols[1], border=NA )
  ## polygon( c( dT[,2,5], rev(dT[,5,5]) ), c( dC[,2,5], rev(dC[,5,5])), col=cols[5], border=NA )
  
  for (modl in seq(nmodls)){
    for (setup in c(1,5)){
      lines( dT[,modl,setup], dC[,modl,setup],col=cols[setup],lwd=2 )
    }
  }
  
  text( 0.25, -400, "LPX  C-N version", col="blue", adj=c(0,0) )
  text( 0.25, -450, "LPX  C-only version", col="green", adj=c(0,0) )
  
  dev.off()
  
} else if (coupling=="C"){
  ## //////////////////////////////////////////////////////////////////////////////////////
  ## PLOT dC VS CO2
  ## --------------------------------------------------------------------------------------
  pdf( '/alphadata01/bstocker/multiGHG_analysis/beta_dC.pdf', width=6, height=6 )
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
  for (setup in c(1,5)){
    lines( dCO2[,setup], dC[,setup],col=cols[setup],lwd=2 )
  }
  
  text( 0.25, 500, "LPX  C-N version", col="blue", adj=c(0,0) )
  text( 0.25, 470, "LPX  C-only version", col="green", adj=c(0,0) )
  
  dev.off()
}
