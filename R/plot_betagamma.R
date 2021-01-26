## ///////////////////////////////////////////////////////////////
## READ N2O DATA
## ---------------------------------------------------------------
## Define scenarios
scen <- c("rcp85","rcp26")
nscen <- length(scen)

setup <- "C" # select 'C', 'T', or 'CT'

if (setup == "CT") {
  withNr <- 1
  noNr   <- 2
} else if (setup == "T" ){
  withNr <- 3
  noNr <- 4
} else if (setup == "C"){
  withNr <- 5
  noNr <- 6
}

## Define setups
sims <- c("r1","r2","r3","r4","r5","r6","r7","r8")
nsims <- length(sims)

## Read list of file names
names.26 <- read.table('/alphadata01/bstocker/multiGHG_analysis/runnames_rcp26.txt', header=F )$V1
n.26 <- length(names.26)
names.85 <- read.table('/alphadata01/bstocker/multiGHG_analysis/runnames_rcp85.txt', header=F )$V1
n.85 <- length(names.85)

nmods <- c( n.85, n.26 )

## Read data, historical period
tmp <- read.table('/alphadata01/bstocker/output_multiGHG/eN2O_r1_historical.dat', header=F)
lhist <- length(tmp[,1])
yrs.hist <- tmp[,1]
e.n2o.hist <- array( NA, dim=c(lhist,nsims) )
e.ch4.hist <- array( NA, dim=c(lhist,nsims) )
dC.hist    <- array( NA, dim=c(lhist,nsims) )
for (i in seq(nsims)){
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/eN2O_',sims[i],'_historical.dat', sep="" )
  e.n2o.hist[,i] <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/eCH4_',sims[i],'_historical.dat', sep="" )
  e.ch4.hist[,i] <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/dCtot_',sims[i],'_historical.dat', sep="" )
  tmp1 <- read.table( filn, header=F )$V2
  dC.hist[,i] <- -(tmp1-tmp1[1])*1e-15
}

## Read data, future
tmp <- read.table(paste('/alphadata01/bstocker/output_multiGHG/eN2O_r1_rcp26_',names.26[5],'.dat', sep="" ))
lfut <- length(tmp[,1])
yrs.fut <- tmp[,1]
e.n2o.fut <- array( NA, dim=c(lfut,nscen,nsims,max(n.26,n.85)))
e.ch4.fut <- array( NA, dim=c(lfut,nscen,nsims,max(n.26,n.85)))
dC.fut    <- array( NA, dim=c(lfut,nscen,nsims,max(n.26,n.85)))
for (i in seq(nscen)){
  for (j in seq(nsims)){
    if (j > 4) {
      k <- 1
      filn <- paste('/alphadata01/bstocker/output_multiGHG/eN2O_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
      tmp <- read.table(filn,header=F)$V2
      e.n2o.fut[1:length(tmp),i,j,k] <- tmp       
      filn <- paste('/alphadata01/bstocker/output_multiGHG/eCH4_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
      tmp <- read.table(filn,header=F)$V2
      e.ch4.fut[1:length(tmp),i,j,k] <- tmp       
      filn <- paste('/alphadata01/bstocker/output_multiGHG/dCtot_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
      tmp <- read.table(filn,header=F)$V2
      dC.fut[1:length(tmp),i,j,k] <- -(tmp-tmp[1])*1e-15 + dC.hist[length(dC.hist[,j]),j]       
    } else {
      if (scen[i]=="rcp26") {
        for (k in seq(n.26)){
          filn <- paste('/alphadata01/bstocker/output_multiGHG/eN2O_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
          tmp <- read.table(filn,header=F)$V2
          e.n2o.fut[1:length(tmp),i,j,k] <- tmp 
          filn <- paste('/alphadata01/bstocker/output_multiGHG/eCH4_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
          tmp <- read.table(filn,header=F)$V2
          e.ch4.fut[1:length(tmp),i,j,k] <- tmp 
          filn <- paste('/alphadata01/bstocker/output_multiGHG/dCtot_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
          tmp <- read.table(filn,header=F)$V2
          dC.fut[1:length(tmp),i,j,k] <- -(tmp-tmp[1])*1e-15 + dC.hist[length(dC.hist[,j]),j] 
        }
      } else if (scen[i]=="rcp85") {
        for (k in seq(n.85)){
          filn <- paste('/alphadata01/bstocker/output_multiGHG/eN2O_',sims[j],'_',scen[i],'_',names.85[k],'.dat', sep="" )
          tmp <- read.table(filn,header=F)$V2
          e.n2o.fut[1:length(tmp),i,j,k] <- tmp
          filn <- paste('/alphadata01/bstocker/output_multiGHG/eCH4_',sims[j],'_',scen[i],'_',names.85[k],'.dat', sep="" )
          tmp <- read.table(filn,header=F)$V2
          e.ch4.fut[1:length(tmp),i,j,k] <- tmp
          filn <- paste('/alphadata01/bstocker/output_multiGHG/dCtot_',sims[j],'_',scen[i],'_',names.85[k],'.dat', sep="" )
          tmp <- read.table(filn,header=F)$V2
          dC.fut[1:length(tmp),i,j,k] <- -(tmp-tmp[1])*1e-15 + dC.hist[length(dC.hist[,j]),j] 
        }
      }
    }
  }
}



## ///////////////////////////////////////////////////////////////
## READ TEMPERATURE DATA
## ---------------------------------------------------------------
## Read CMIP data, RCP 8.5
tmp <- read.table(
                      '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp85.dat',
                     col.names = c("year","ips.1","ips.2","ips.3","ips.4","mpi.1","mpi.2","mpi.3","mir.1","ccs.1","ccs.2","ccs.3","ccs.4","ccs.5")
                      )
## Subset of years > 1859 AD
tmp <- tmp[tmp$year>1859,]
## Read Hadley CMIP data and attach to data frame
tmp2 <- read.table( '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp85_had.dat', col.names = c("year","had.1","had.2","had.3") )
tmp$had.1 <- tmp2$had.1
tmp$had.2 <- tmp2$had.2
tmp$had.3 <- tmp2$had.3
## Rearrange in right order to plot with 'mathlines' against n2o emissions
dT.cmip.8 <- data.frame(year=tmp$year,
                        had.1=tmp$had.1,
                        had.2=tmp$had.2,
                        had.3=tmp$had.3,
                        had.4=tmp$had.3,
                        mpi.1=tmp$mpi.1,
                        mpi.2=tmp$mpi.2,
                        mpi.3=tmp$mpi.3,
                        ips.1=tmp$ips.1,
                        ips.2=tmp$ips.2,
                        ips.3=tmp$ips.3,
                        ips.4=tmp$ips.4,
                        mir.1=tmp$mir.1,
                        ccs.1=tmp$ccs.1,
                        ccs.2=tmp$ccs.2,
                        ccs.3=tmp$ccs.3,
                        ccs.4=tmp$ccs.4,
                        ccs.5=tmp$ccs.5
                        )



## Read CMIP data, RCP 2.6
tmp <- read.table(
                      '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp26.dat',
                     col.names = c("year","ips.1","ips.2","ips.3","mpi.1","mpi.2","mpi.3","mir.1","ccs.1","ccs.2","ccs.3","ccs.4","ccs.5")
                      )
## Subset of years > 1859 AD
tmp <- tmp[tmp$year>1859 & tmp$year<2100,]
## Read Hadley CMIP data and attach to data frame
tmp2 <- read.table( '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp26_had.dat',
                  col.names = c("year","had.1","had.2","had.3","had.4") )
tmp$had.1 <- tmp2$had.1
tmp$had.2 <- tmp2$had.2
tmp$had.3 <- tmp2$had.3
tmp$had.4 <- tmp2$had.4
## Rearrange in right order to plot with 'mathlines' against n2o emissions
dT.cmip.2 <- data.frame(year=tmp$year,
                        had.1=tmp$had.1,
                        had.2=tmp$had.2,
                        had.3=tmp$had.3,
                        mpi.1=tmp$mpi.1,
                        mpi.2=tmp$mpi.2,
                        mpi.3=tmp$mpi.3,
                        ips.1=tmp$ips.1,
                        ips.2=tmp$ips.2,
                        ips.3=tmp$ips.3,
                        mir.1=tmp$mir.1,
                        ccs.1=tmp$ccs.1,
                        ccs.2=tmp$ccs.2,
                        ccs.3=tmp$ccs.3,
                        ccs.4=tmp$ccs.4,
                        ccs.5=tmp$ccs.5
                        )


## ///////////////////////////////////////////////////////////////
## READ CO2 DATA
## ---------------------------------------------------------------
dCO2.8 <- read.table('/alphadata01/bstocker/lpx/multiGHG/start/input/cCO2_rcp85.dat', col.names=c("year","co2"))
  

## ///////////////////////////////////////////////////////////////
## CALCULATE SLOPE
## ---------------------------------------------------------------
## convert matrix to vector
dT.matrix <- as.matrix(dT.cmip.8[dT.cmip.8$year>2005,2:18])
dT.vec    <- c(dT.matrix)
dC.vec    <- c(dCO2.8[ which(dCO2.8$year==2006):which(dCO2.8$year==2100),2])
n2o.vec.Nr <- c(e.n2o.fut[,1,withNr,])
n2o.vec.noNr <- c(e.n2o.fut[,1,noNr,])
ch4.vec.Nr <- c(e.ch4.fut[,1,withNr,])

## use linear model function for N2O
if (setup=="T") {
  lreg.Nr <- lm( n2o.vec.Nr ~ dT.vec  )
  lreg.noNr <- lm( n2o.vec.noNr ~ dT.vec  )
} else if (setup=="C"){
  lreg.Nr <- lm( n2o.vec.Nr[!is.na(n2o.vec.Nr)] ~ dC.vec  )
  lreg.noNr <- lm( n2o.vec.noNr[!is.na(n2o.vec.noNr)] ~ dC.vec  )
}

## use linear model function for CH4
if (setup=="T") {
  lreg.Nr.ch4 <- lm( ch4.vec.Nr ~ dT.vec  )
} else if (setup=="C"){
}

if (setup=="T"){
  ## ///////////////////////////////////////////////////////////////
  ## PLOT N2O VS T
  ## ---------------------------------------------------------------
  pdf( '/alphadata01/bstocker/multiGHG_analysis/gamma_n2o.pdf', width=6, height=6 )
  
  xlim <- c(0,7)
  ylim <- c(6,18)
  par( xaxs="i", yaxs="i", las=1  )
  
  plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"T ["^o,"C]")), ylab=expression(paste("eN"[2],"O [TgNyr"^-1,"]")) )
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
  axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=0.5),labels=F,tck=-0.01)
  axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
  matpoints(dT.cmip.8[dT.cmip.8$year>2005,2:18],
            e.n2o.fut[,1,withNr,],
            col=rgb(1,0,0,0.2),
            lty=1,
            lwd=1,
            pch=21,
            bg=rgb(1,0,0,0.3)
            )
  matpoints(dT.cmip.8[dT.cmip.8$year>2005,2:18],
            e.n2o.fut[,1,noNr,],
            col=rgb(1,0,0,0.2),
            lty=1,
            lwd=1,
            pch=21
            )
  abline(lreg.Nr,lwd=3,col=rgb(1,0,0,1))
  abline(lreg.noNr,lwd=3,col=rgb(1,0,0,0.5))
  
  
##  text(0.2,16.5,expression(paste("temperature sensitivity of N"[2],"O emissions")),adj=c(0,0),cex=1.5)
   
  text(3.7,8,paste("slope with Nr: ",as.character(format(lreg.Nr$coefficients[2],digits=3))),col="red",adj=c(0,0))
  text(5.75,7.92,expression(paste("Tg N/yr/"^o,"C")),adj=c(0,0),col="red")
  text(3.7,7.5,paste("slope without Nr: ",as.character(format(lreg.noNr$coefficients[2],digits=2))),col="red",adj=c(0,0))
  text(6.05,7.4,expression(paste("Tg N/yr/"^o,"C")),adj=c(0,0),col="red")
 
  dev.off()

} else if (setup=="C") {
  ## ///////////////////////////////////////////////////////////////
  ## PLOT N2O VS CO2
  ## ---------------------------------------------------------------
  pdf( '/alphadata01/bstocker/multiGHG_analysis/beta_n2o.pdf', width=6, height=6 )
  xlim <- c(380,940)
  ylim <- c(6,11)
  par( xaxs="i", yaxs="i", las=1  )
  
  plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste("CO"[2]," [ppm]")), ylab=expression(paste("eN"[2],"O [TgNyr"^-1,"]")) )
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=20),labels=F,tck=-0.01)
  axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=20),labels=F,tck=-0.01)
  axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
  box(lwd=2)
  par(new=T)
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

}

if (setup=="T"){
  ## ///////////////////////////////////////////////////////////////
  ## PLOT CH4 VS T
  ## ---------------------------------------------------------------
  pdf( '/alphadata01/bstocker/multiGHG_analysis/gamma_ch4.pdf', width=7, height=6 )
  
  xlim <- c(0,7)
  ylim <- c(160,300)
  par( xaxs="i", yaxs="i", las=1 )
  
  plot( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste(Delta,"T ["^o,"C]")), ylab=expression(paste("eCH"[4]," [TgNyr"^-1,"]")) )
  axis(1,lwd=1.75);  axis(1,at=seq(xlim[1],xlim[2],by=0.2),labels=F,tck=-0.01)
  axis(2,lwd=1.75);  axis(2,at=seq(ylim[1],ylim[2],by=4),labels=F,tck=-0.01)
  axis(3,lwd=1.75,labels=F);  axis(3,at=seq(xlim[1],xlim[2],by=0.2),labels=F,tck=-0.01)
  axis(4,lwd=1.75,labels=F);  axis(4,at=seq(ylim[1],ylim[2],by=4),labels=F,tck=-0.01)
  matpoints(dT.cmip.8[dT.cmip.8$year>2005,2:18],
            e.ch4.fut[,1,withNr,],
            col=rgb(1,0,0,0.2),
            lty=1,
            lwd=2,
            pch=19
            )
  abline(lreg.Nr.ch4,lwd=3,col=rgb(1,0,0,1))
  
  
  text(0.2,280,expression(paste("temperature sensitivity of CH"[4]," emissions")),adj=c(0,0),cex=1.5)
  
  text(4,170,paste("slope: ",as.character(format(lreg.Nr.ch4$coefficients[2],digits=3))),col="red",adj=c(0,0))
  text(5.05,169,expression(paste("TgCH"[4],"/yr/"^o,"C")),adj=c(0,0),col="red")
  
  dev.off()
}
