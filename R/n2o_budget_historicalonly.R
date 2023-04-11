## ////////////////////////////////////////////////////////////////////////////////////////////////
## This is the pendant to the matlab routine with the same name
## Calculates concentrations resulting from different setups
## beni@climate.unibe.ch
## ------------------------------------------------------------------------------------------------

## Chose whether to diagnose flux or use explicit ocean source
diag <- TRUE

source(paste0(here::here(), '/R/calcConc.R'))
source(paste0(here::here(), '/R/myspline.R'))

## ////////////////////////////////////////////////////////////////////////////////////////////////
## READ FILES
## ------------------------------------------------------------------------------------------------

## Read list of file names
names <- read.table(paste0(here::here(), '/data/runnames_historical.txt'), header=F )$V1
nsims <- length(names)

## Read N2O concentration data to diagnose flux
c.n2o.data <- read.table(paste0(here::here(), '/data/cN2O_rcp85.dat'), col.names=c("year","concentration"))  # xxx missing from repo

## Read N2O emissions ANTH (=fossil+fire+manure) from Zaehle
# e.n2o.ext <- read.table('/alphadata01/bstocker/input_data/ghg_data/n2o_data/eN2Oext_bysources_rcp26_HARMONIZED.dat', head=TRUE )
e.n2o.ext <- read.table(paste0(here::here(), '/data/eN2Oext_bysources_rcp26_HARMONIZED.dat'), head=TRUE )  # xxx missing from repo

## Read oceanic N2O source
# e.n2o.oc <- read.table('/alphadata01/bstocker/multiGHG_analysis/eN2O_oc_historical.dat', col.names=c("year","emission"))
e.n2o.oc <- read.table(paste0(here::here(), '/data/eN2O_oc_historical.dat'), col.names=c("year","emission"))
tmp <- data.frame( year=1765:(e.n2o.oc$year[1]-1), emission=rep(e.n2o.oc$emission[1],length(1765:(e.n2o.oc$year[1]-1))) )
e.n2o.oc <- rbind( tmp, e.n2o.oc)

## Read N2O emissions from LPX simulations
# tmp <- read.table('/alphadata01/bstocker/output_multiGHG/eN2O_r1_historical.dat', header=F)
tmp <- read.table(paste0(here::here(), '/data/eN2O_r1_historical.dat'), header=F)  # missing from repo
lhist <- length(tmp[,1])
yrs.hist <- tmp[,1]
e.n2o <- array( NA, dim=c(lhist,nsims) )
for (i in seq(nsims)){
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/eN2O_',names[i],'.dat', sep="" )
  e.n2o[,i] <- read.table( filn, header=F )$V2
}


## ////////////////////////////////////////////////////////////////////////////////////////////////
## CORRECT EMISSIONS OF NON-STANDARD RUNS TO MATCH IN PERIOD 1765-1796
## ------------------------------------------------------------------------------------------------
target <- mean(e.n2o[,1][yrs.hist>=1765 & yrs.hist<=1796])
offset <- array(NA,dim=c(nsims))
for (i in seq(nsims)){
  offset[i] <- mean(e.n2o[,i][yrs.hist>=1765 & yrs.hist<=1796]) - target
  e.n2o[,i] <- e.n2o[,i] - offset[i] 
}


## ////////////////////////////////////////////////////////////////////////////////////////////////
## CALCULATE CONCENTRATIONS
## ------------------------------------------------------------------------------------------------

## Define emissions (in TgN/yr) for year 2000
e2000 <- 15.815

## Call concentration function with full vector for each sim
c.n2o <- array( NA, dim=c(lhist,nsims) )
for (i in seq(nsims)){
  c.n2o[,i] <- calc_cN2O( e.n2o[,i]
                        + e.n2o.oc$emission[e.n2o.oc$year < 2007]
                        + e.n2o.ext$total[e.n2o.ext$year > 1764 & e.n2o.ext$year <2007],
                          e2000
                        )
}

## ////////////////////////////////////////////////////////////////////////////////////////////////
## DIAGNOSE EMISSIONS
## ------------------------------------------------------------------------------------------------

## Call concentration function with full vector for each sim
e.n2o.diag <- array( NA, dim=c(lhist,nsims) )
e.n2o.diag.spl <- array( NA, dim=c(lhist,nsims) )
for (i in seq(nsims)){
  e.n2o.diag[,i] <- diag_n2o( e.n2o[,i]
                          + e.n2o.ext$total[e.n2o.ext$year > 1764 & e.n2o.ext$year < 2007],
                          c.n2o.data$concentration[c.n2o.data$year > 1764 & c.n2o.data$year < 2007],
                          e2000
                          )
}


## ////////////////////////////////////////////////////////////////////////////////////////////////
## PLOT
## ------------------------------------------------------------------------------------------------
cols=c("black","red","green","blue")
xlim <- c(1800,2005)

if (diag){

  e.n2o.diag.spl <- myspline( yrs.hist[2:241], e.n2o.diag[2:241,1], 30 )

  ylim <- c(2,6)
  for (i in c(1,2,3,5)){
    plot( yrs.hist, e.n2o.diag[,i], 
          type="l", col=cols[min(i,length(cols))], 
          xlim=c(1765,2005), #ylim=c(260,270),
          axes=F,
          #xlim=c(1765,2005), ylim=c(260,310) 
          )
    par(new=T)
  }
  axis(1); axis(2); axis(3); axis(4)

  pdf( '/fig/n2o_diag.pdf', width=7, height=6 )
  par(new=F,xaxs="i",yaxs="i",las=1)
  
  plot(yrs.hist, e.n2o.diag[,1],
       type="l", axes=FALSE, lwd=1,
       xlim=xlim, ylim=ylim,
       xlab="year AD", ylab=expression(paste("TgNyr"^-1))
       )
  lines(yrs.hist[2:241], e.n2o.diag.spl, lwd=2, col="red" )
  
  axis(1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=10),labels=F,tck=-0.01 )
  axis(2, lwd=2 );  axis(2, at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
  axis(3, labels=F, lwd=2); axis(3, at=seq(xlim[1],xlim[2],by=10),labels=F,tck=-0.01 )
  axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
  box(lwd=2)

  text( 1810, 5.3, expression(paste("eN"[2],"O"^"diag")), cex=2, adj=c(0,0), font=2 )
  
  dev.off()
  
  print(      "----------------------------------------------")
  print(paste("MEAN MISSING FLUX FOR r1_historical, 1870:1900"))
  print(mean(e.n2o.diag[,1][yrs.hist>=1870 & yrs.hist<=1901]))
  print(      "----------------------------------------------")
  
} else {
  
  for (i in c(1,2,3,5)){
    plot( yrs.hist, c.n2o[,i], 
          type="l", col=cols[min(i,length(cols))], 
          #xlim=c(1765,1910), ylim=c(260,310),
          axes=F,
          xlim=c(1765,2005), ylim=c(280,325) 
          )
    par(new=T)
  }
  axis(1); axis(2); axis(3); axis(4)
  box()
#   par(new=T)
#   plot( c.n2o.data,
#         lty=2, 
#         xlim=c(1765,1910), ylim=c(260,270)
#         )
}


## ////////////////////////////////////////////////////////////////////////////////////////////////
## WRITE TO FILE
## ------------------------------------------------------------------------------------------------
out <- data.frame( c.n2o )
colnames(out) <- names
year <- data.frame( year=yrs.hist )
out <- cbind(year,out)
write.csv( out, file=paste0(here::here(), "/data/cN2O_hist.dat"), row.names=F, quote=FALSE )

out <- data.frame( e.n2o.diag )
colnames(out) <- names
year <- data.frame( year=yrs.hist )
out <- cbind(year,out)
write.csv( out, file=paste0(here::here(), "/data/eN2O_diag.dat"), row.names=F, quote=FALSE )


