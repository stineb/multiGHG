## ///////////////////////////////////////////////////////////////
## LOAD DATA
## ---------------------------------------------------------------
load('b3d.Rdata')


## ///////////////////////////////////////////////////////////////
## CALCULATE SLOPE
## ---------------------------------------------------------------
# CO2
lreg.co2.had <- lm( co2.f.8$had[co2.f.8$year>2000&co2.f.8$year<2011] ~
               dT.f.8$had[co2.f.8$year>2000&co2.f.8$year<2011]
               )
lreg.co2.mpi <- lm( co2.f.8$mpi[co2.f.8$year>2000&co2.f.8$year<2011] ~
               dT.f.8$mpi[co2.f.8$year>2000&co2.f.8$year<2011]
               )
lreg.co2.ips <- lm( co2.f.8$ips[co2.f.8$year>2000&co2.f.8$year<2011] ~
               dT.f.8$ips[co2.f.8$year>2000&co2.f.8$year<2011]
               )
lreg.co2.mir <- lm( co2.f.8$mir[co2.f.8$year>2000&co2.f.8$year<2011] ~
               dT.f.8$mir[co2.f.8$year>2000&co2.f.8$year<2011]
               )
lreg.co2.ccs <- lm( co2.f.8$ccs[co2.f.8$year>2000&co2.f.8$year<2011] ~
               dT.f.8$ccs[co2.f.8$year>2000&co2.f.8$year<2011]
               )
# N2O
lreg.n2o.had <- lm( n2o.f.8$had[n2o.f.8$year>2000&n2o.f.8$year<2011] ~
               dT.f.8$had[n2o.f.8$year>2000&n2o.f.8$year<2011]
               )
lreg.n2o.mpi <- lm( n2o.f.8$mpi[n2o.f.8$year>2000&n2o.f.8$year<2011] ~
               dT.f.8$mpi[n2o.f.8$year>2000&n2o.f.8$year<2011]
               )
lreg.n2o.ips <- lm( n2o.f.8$ips[n2o.f.8$year>2000&n2o.f.8$year<2011] ~
               dT.f.8$ips[n2o.f.8$year>2000&n2o.f.8$year<2011]
               )
lreg.n2o.mir <- lm( n2o.f.8$mir[n2o.f.8$year>2000&n2o.f.8$year<2011] ~
               dT.f.8$mir[n2o.f.8$year>2000&n2o.f.8$year<2011]
               )
lreg.n2o.ccs <- lm( n2o.f.8$ccs[n2o.f.8$year>2000&n2o.f.8$year<2011] ~
               dT.f.8$ccs[n2o.f.8$year>2000&n2o.f.8$year<2011]
               )

# CH4
lreg.ch4.had <- lm( ch4.f.8$had[ch4.f.8$year>2000&ch4.f.8$year<2011] ~
               dT.f.8$had[ch4.f.8$year>2000&ch4.f.8$year<2011]
               )
lreg.ch4.mpi <- lm( ch4.f.8$mpi[ch4.f.8$year>2000&ch4.f.8$year<2011] ~
               dT.f.8$mpi[ch4.f.8$year>2000&ch4.f.8$year<2011]
               )
lreg.ch4.ips <- lm( ch4.f.8$ips[ch4.f.8$year>2000&ch4.f.8$year<2011] ~
               dT.f.8$ips[ch4.f.8$year>2000&ch4.f.8$year<2011]
               )
lreg.ch4.mir <- lm( ch4.f.8$mir[ch4.f.8$year>2000&ch4.f.8$year<2011] ~
               dT.f.8$mir[ch4.f.8$year>2000&ch4.f.8$year<2011]
               )
lreg.ch4.ccs <- lm( ch4.f.8$ccs[ch4.f.8$year>2000&ch4.f.8$year<2011] ~
               dT.f.8$ccs[ch4.f.8$year>2000&ch4.f.8$year<2011]
               )


## ///////////////////////////////////////////////////////////////
## PLOT CO2 VS T
## ---------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/cCO2vsTemp.pdf' )

xlim <- c(-1,10)
ylim <- c(0,2500)
par( xaxs="i", yaxs="i", las=1  )

# Had
plot( dT.f.8$had, co2.f.8$had, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     ylab=expression(paste("cCO"[2]," [ppm]")), xlab=expression(paste(Delta,"T [K]"))
     )
par( new=TRUE )
plot( dT.f.8$had[dT.f.8$year>2000&dT.f.8$year<2011], co2.f.8$had[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.co2.had, col=rgb( 1,0,0,1) )

# MPI
par( new=TRUE )
plot( dT.f.8$mpi, co2.f.8$mpi, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$mpi[dT.f.8$year>2000&dT.f.8$year<2011], co2.f.8$mpi[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.co2.mpi, col=rgb( 1,0,0,1) )

# IPS
par( new=TRUE )
plot( dT.f.8$ips, co2.f.8$ips, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$ips[dT.f.8$year>2000&dT.f.8$year<2011], co2.f.8$ips[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.co2.ips, col=rgb( 1,0,0,1) )


# MIR
par( new=TRUE )
plot( dT.f.8$mir, co2.f.8$mir, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$mir[dT.f.8$year>2000&dT.f.8$year<2011], co2.f.8$mir[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.co2.mir, col=rgb( 1,0,0,1) )

# CCS
par( new=TRUE )
plot( dT.f.8$ccs, co2.f.8$ccs, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$ccs[dT.f.8$year>2000&dT.f.8$year<2011], co2.f.8$ccs[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.co2.ccs, col=rgb( 1,0,0,1) )

# ANNOTATIONS
text(6,600,expression(paste("sensitivity in ppm/K")),adj=c(0,0),col="red")
text(6,500,
     paste("HAD: ",as.character(format(lreg.co2.had$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,400,
     paste("MPI: ",as.character(format(lreg.co2.mpi$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,300,
     paste("IPS: ",as.character(format(lreg.co2.ips$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,200,
     paste("MIR: ",as.character(format(lreg.co2.mir$coefficients[2],digits=2))),
     col="red",adj=c(0,0)
     )
text(6,100,
     paste("CCS: ",as.character(format(lreg.co2.ccs$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )

dev.off()


## ///////////////////////////////////////////////////////////////
## PLOT N2O VS T
## ---------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/cN2OvsTemp.pdf' )

xlim <- c(-1,10)
ylim <- c(250,730)
par( xaxs="i", yaxs="i", las=1  )

# Had
plot( dT.f.8$had, n2o.f.8$had, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     ylab=expression(paste("cN2O"[2]," [ppb]")), xlab=expression(paste(Delta,"T [K]"))
     )
par( new=TRUE )
plot( dT.f.8$had[dT.f.8$year>2000&dT.f.8$year<2011], n2o.f.8$had[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.n2o.had, col=rgb( 1,0,0,1) )

# MPI
par( new=TRUE )
plot( dT.f.8$mpi, n2o.f.8$mpi, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$mpi[dT.f.8$year>2000&dT.f.8$year<2011], n2o.f.8$mpi[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.n2o.mpi, col=rgb( 1,0,0,1) )

# IPS
par( new=TRUE )
plot( dT.f.8$ips, n2o.f.8$ips, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$ips[dT.f.8$year>2000&dT.f.8$year<2011], n2o.f.8$ips[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.n2o.ips, col=rgb( 1,0,0,1) )


# MIR
par( new=TRUE )
plot( dT.f.8$mir, n2o.f.8$mir, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$mir[dT.f.8$year>2000&dT.f.8$year<2011], n2o.f.8$mir[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.n2o.mir, col=rgb( 1,0,0,1) )

# CCS
par( new=TRUE )
plot( dT.f.8$ccs, n2o.f.8$ccs, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$ccs[dT.f.8$year>2000&dT.f.8$year<2011], n2o.f.8$ccs[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.n2o.ccs, col=rgb( 1,0,0,1) )

# ANNOTATIONS
text(6,420,expression(paste("sensitivity in ppb/K")),adj=c(0,0),col="red")
text(6,400,
     paste("HAD: ",as.character(format(lreg.n2o.had$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,380,
     paste("MPI: ",as.character(format(lreg.n2o.mpi$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,360,
     paste("IPS: ",as.character(format(lreg.n2o.ips$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,340,
     paste("MIR: ",as.character(format(lreg.n2o.mir$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,320,
     paste("CCS: ",as.character(format(lreg.n2o.ccs$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )

dev.off()


## ///////////////////////////////////////////////////////////////
## PLOT CH4 VS T
## ---------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/cCH4vsTemp.pdf' )

xlim <- c(-1,10)
ylim <- c(0,6000)
par( xaxs="i", yaxs="i", las=1  )

# Had
plot( dT.f.8$had, ch4.f.8$had, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     ylab=expression(paste("cCH4"[2]," [ppb]")), xlab=expression(paste(Delta,"T [K]"))
     )
par( new=TRUE )
plot( dT.f.8$had[dT.f.8$year>2000&dT.f.8$year<2011], ch4.f.8$had[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.ch4.had, col=rgb( 1,0,0,1) )

# MPI
par( new=TRUE )
plot( dT.f.8$mpi, ch4.f.8$mpi, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$mpi[dT.f.8$year>2000&dT.f.8$year<2011], ch4.f.8$mpi[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.ch4.mpi, col=rgb( 1,0,0,1) )

# IPS
par( new=TRUE )
plot( dT.f.8$ips, ch4.f.8$ips, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$ips[dT.f.8$year>2000&dT.f.8$year<2011], ch4.f.8$ips[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.ch4.ips, col=rgb( 1,0,0,1) )


# MIR
par( new=TRUE )
plot( dT.f.8$mir, ch4.f.8$mir, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$mir[dT.f.8$year>2000&dT.f.8$year<2011], ch4.f.8$mir[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.ch4.mir, col=rgb( 1,0,0,1) )

# CCS
par( new=TRUE )
plot( dT.f.8$ccs, ch4.f.8$ccs, pch=4, col=rgb( 0,0,0,0.3), xlim=xlim, ylim=ylim,
     xlab="", ylab="", axes=FALSE
     )
par( new=TRUE )
plot( dT.f.8$ccs[dT.f.8$year>2000&dT.f.8$year<2011], ch4.f.8$ccs[dT.f.8$year>2000&dT.f.8$year<2011],
     pch=4, col=rgb( 1,0,0,1), xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE
     )
abline( lreg.ch4.ccs, col=rgb( 1,0,0,1) )

# ANNOTATIONS
text(6,2000,expression(paste("sensitivity in ppb/K")),adj=c(0,0),col="red")
text(6,1700,
     paste("HAD: ",as.character(format(lreg.ch4.had$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,1400,
     paste("MPI: ",as.character(format(lreg.ch4.mpi$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,1100,
     paste("IPS: ",as.character(format(lreg.ch4.ips$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,800,
     paste("MIR: ",as.character(format(lreg.ch4.mir$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )
text(6,500,
     paste("CCS: ",as.character(format(lreg.ch4.ccs$coefficients[2],digits=3))),
     col="red",adj=c(0,0)
     )

dev.off()
