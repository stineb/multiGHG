## ////////////////////////////////////////////////////////////////////////////////////////////////
## This is the pendant to the matlab routine with the same name
## Calculates concentrations resulting from different setups
## beni@climate.unibe.ch
## ------------------------------------------------------------------------------------------------

## Chose whether to diagnose flux or use explicit ocean source
diag <- FALSE

source('/alphadata01/bstocker/multiGHG_analysis/calcConc.R')
source('/alphadata01/bstocker/multiGHG_analysis/myspline.R')

## ////////////////////////////////////////////////////////////////////////////////////////////////
## READ FILES
## ------------------------------------------------------------------------------------------------

## Read list of file names
names <- read.table('/alphadata01/bstocker/multiGHG_analysis/runnames_historical.txt', header=F )$V1
nsims <- length(names)

## Read CH4 concentration data to diagnose flux
c.ch4.data <- read.table('/alphadata01/bstocker/multiGHG_analysis/cCH4_rcp85.dat', col.names=c("year","concentration"))

## Read CH4 emissions EXT (=total emissions from RCP database: all categories, non-natural)
e.ch4.ext <- read.table('/alphadata01/bstocker/multiGHG_analysis/eCH4_rcp85_anth.dat', col.names=c("year","emissions") )

## Read NOx emissions
e.nox <- read.table( '/alphadata01/bstocker/multiGHG_analysis/eNOx_rcp85.dat', col.names=c("year","emissions") )

## Read CO emissions
e.co  <- read.table( '/alphadata01/bstocker/multiGHG_analysis/eCO_rcp85.dat', col.names=c("year","emissions") )

## Read VOC emissions
e.voc <- read.table( '/alphadata01/bstocker/multiGHG_analysis/eVOC_rcp85.dat', col.names=c("year","emissions") )

## Read CH4 emissions from LPX simulations
tmp <- read.table('/alphadata01/bstocker/output_multiGHG/eCH4_r1_historical.dat', header=F)
lhist <- length(tmp[,1])
yrs.hist <- tmp[,1]
e.ch4 <- array( NA, dim=c(lhist,nsims) )
for (i in seq(nsims)){
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/eCH4_',names[i],'.dat', sep="" )
  e.ch4[,i] <- read.table( filn, header=F )$V2
}


## ////////////////////////////////////////////////////////////////////////////////////////////////
## CORRECT EMISSIONS OF NON-STANDARD RUNS TO MATCH IN PERIOD 1765-1796
## ------------------------------------------------------------------------------------------------

if (!(diag)){

  print("test")
  target <- mean(e.ch4[,1][yrs.hist>=1765 & yrs.hist<=1796])
  offset <- array(NA,dim=c(nsims))
  for (i in seq(nsims)){
    offset[i] <- mean(e.ch4[,i][yrs.hist>=1765 & yrs.hist<=1796]) - target
    e.ch4[,i] <- e.ch4[,i] - offset[i]
  }

  
  ## ////////////////////////////////////////////////////////////////////////////////////////////////
  ## CONSTANT EXTRA SOURCE OF 38 TgCH4/YR TO CLOSE BUDGET IN 1900 (BY EYE)
  ## ////////////////////////////////////////////////////////////////////////////////////////////////
  e.ch4.xtra <- e.ch4[,1]*0 + 38
  ## print(head(e.ch4.xtra))
}

## ////////////////////////////////////////////////////////////////////////////////////////////////
## CALCULATE CONCENTRATIONS
## ------------------------------------------------------------------------------------------------

## Call concentration function with full vector for each sim
c.ch4 <- array( NA, dim=c(lhist,nsims) )
for (i in seq(nsims)){
  c.ch4[,i] <- calc.cCH4(
                         e.ch4[,i],
                         e.ch4.ext$emissions[e.ch4.ext$year>=1765&e.ch4.ext$year<=2006],
                         e.ch4.xtra,
                         c.ch4.data$concentration[c.ch4.data$year>=1765&c.ch4.data$year<=2007],
                         e.nox,
                         e.co,
                         e.voc,
                         yrs.hist
                        )
}

## ////////////////////////////////////////////////////////////////////////////////////////////////
## DIAGNOSE EMISSIONS
## ------------------------------------------------------------------------------------------------

## Call concentration function with full vector for each sim
e.ch4.diag <- array( NA, dim=c(lhist,nsims) )
for (i in seq(nsims)){
  e.ch4.diag[,i] <- diag.ch4(
                             e.ch4[,i],
                             e.ch4.ext$emissions[e.ch4.ext$year>=1765&e.ch4.ext$year<=2006],
                             c.ch4.data$concentration[c.ch4.data$year>=1765&c.ch4.data$year<=2007],
                             e.nox,
                             e.co,
                             e.voc,
                             yrs.hist
                             )
}


## ////////////////////////////////////////////////////////////////////////////////////////////////
## PLOT
## ------------------------------------------------------------------------------------------------
cols=c("black","red","green","blue")
xlim <- c(1800,2005)

if (diag){

#  pdf( '/alphadata01/bstocker/multiGHG_analysis/ch4_diag.pdf', width=7, height=6 )
  ylim <- c(0,50)
  for (i in c(1,2,3,5)){
    plot( yrs.hist, e.ch4.diag[,i], 
          type="l", col=cols[min(i,length(cols))], 
          xlim=xlim, ylim=ylim,
          axes=F,
          )
    par(new=T)
  }
  axis(1); axis(2); axis(3); axis(4)
#  dev.off()
  
  ## dev.off()


  print(      "----------------------------------------------")
  print(paste("MEAN MISSING FLUX FOR r1_historical, 1870:1900"))
  print(mean(e.ch4.diag[,1][yrs.hist>=1870 & yrs.hist<=1901]))
  print(      "----------------------------------------------")
  
} else {
  
  ## for (i in c(1,2,3,5)){
  ##   plot( yrs.hist, c.ch4[,i], 
  ##         type="l", col=cols[min(i,length(cols))], 
  ##         #xlim=c(1765,1910), ylim=c(400,800),
  ##         axes=F,
  ##         xlim=c(1765,2005), ylim=c(450,1800) 
  ##         )
  ##   par(new=T)
  ## }
  ## axis(1); axis(2); axis(3); axis(4)
  ## box()
#   par(new=T)
#   plot( c.ch4.data,
#         lty=2, 
#         xlim=c(1765,1910), ylim=c(260,270)
#         )
}


## ////////////////////////////////////////////////////////////////////////////////////////////////
## WRITE TO FILE
## ------------------------------------------------------------------------------------------------
out <- data.frame( c.ch4 )
colnames(out) <- names
year <- data.frame( year=yrs.hist )
out <- cbind(year,out)
write.csv( out, file="/alphadata01/bstocker/output_multiGHG/cCH4_hist.dat", row.names=F, quote=FALSE )

out <- data.frame( e.ch4.diag )
colnames(out) <- names
year <- data.frame( year=yrs.hist )
out <- cbind(year,out)
write.csv( out, file="/alphadata01/bstocker/output_multiGHG/eCH4_diag.dat", row.names=F, quote=FALSE )





## something wrong with my R script to diagnose, matlab does ok
e.ch4.diag <- read.table( '/alphadata01/bstocker/multiGHG_analysis/eCH4_diag_r1.dat', col.names=c("year","ech4") )
xlim <- c(1800,2004)
ylim <- c(0,120)

pdf( '/alphadata01/bstocker/multiGHG_analysis/ch4_diag.pdf', width=7, height=6 )
par(new=F,xaxs="i",yaxs="i",las=1)

e.ch4.diag.spl <- myspline( e.ch4.diag$year[2:240], e.ch4.diag$ech4[2:240], 30 )

plot( e.ch4.diag$year, e.ch4.diag$ech4,
     type="l", axes=FALSE, lwd=2,
     xlim=xlim, ylim=ylim,
     xlab="year AD", ylab=expression(paste("TgCH"[4],"yr"^-1))
     )
lines( e.ch4.diag$year[2:240], e.ch4.diag.spl, lwd=2, col="red" )

axis(1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim[1],xlim[2],by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=5),labels=F,tck=-0.01)
box(lwd=2)

text( 1810, 5.3, expression(paste("eCH"[4],""^"diag")), cex=2, adj=c(0,0), font=2 )

dev.off()
