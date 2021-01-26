## ///////////////////////////////////////////////////
## This script produces Fig.4 of multiGHG paper.
## First, run write_ascii_b3d.jnl 2 (8)
## beni@climate.unibe.ch
## 27.12.2012
## ---------------------------------------------------

## RCP 8.5 data
co2.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_8_b3d_RFco2.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
co2.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_8_b3d_RFall.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
co2.fnn.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_8_b3d_RFall_noNr.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )

ch4.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/ch4_8_b3d_RFco2.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
ch4.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/ch4_8_b3d_RFall.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
ch4.fnn.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/ch4_8_b3d_RFall_noNr.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )

n2o.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/n2o_8_b3d_RFco2.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
n2o.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/n2o_8_b3d_RFall.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
n2o.fnn.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/n2o_8_b3d_RFall_noNr.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )

rf.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RF_8_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
rf.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RF_8_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
rf.fnn.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RF_8_b3d_RFall_noNr.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

fn2o.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFn2o_8_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
fn2o.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFn2o_8_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

fch4.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFch4_8_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
fch4.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFch4_8_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

fco2.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFco2_8_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
fco2.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFco2_8_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

falb.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFalb_8_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
falb.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFalb_8_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

dT.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/dT_8_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
dT.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/dT_8_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
dT.fnn.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/dT_8_b3d_RFall_noNr.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

## RCP 2.6 data
co2.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_2_b3d_RFco2.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
co2.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_2_b3d_RFall.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )

ch4.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/ch4_2_b3d_RFco2.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
ch4.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/ch4_2_b3d_RFall.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )

n2o.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/n2o_2_b3d_RFco2.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
n2o.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/n2o_2_b3d_RFall.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )

rf.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RF_2_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
rf.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RF_2_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

fn2o.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFn2o_2_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
fn2o.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFn2o_2_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

fch4.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFch4_2_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
fch4.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFch4_2_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

fco2.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFco2_2_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
fco2.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFco2_2_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

falb.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFalb_2_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
falb.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/RFalb_2_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

dT.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/dT_2_b3d_RFco2.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )
dT.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/dT_2_b3d_RFall.dat',
                   col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                   )

nyrs <- length(co2.0.8$year)

## Add columns for min and max
co2.f.8$min <- apply(co2.f.8[,3:7],1,min)
co2.f.8$max <- apply(co2.f.8[,3:7],1,max)
co2.fnn.8$min <- apply(co2.fnn.8[,3:7],1,min)
co2.fnn.8$max <- apply(co2.fnn.8[,3:7],1,max)

ch4.f.8$min <- apply(ch4.f.8[,3:7],1,min)
ch4.f.8$max <- apply(ch4.f.8[,3:7],1,max)
ch4.fnn.8$min <- apply(ch4.fnn.8[,3:7],1,min)
ch4.fnn.8$max <- apply(ch4.fnn.8[,3:7],1,max)

n2o.f.8$min <- apply(n2o.f.8[,3:7],1,min)
n2o.f.8$max <- apply(n2o.f.8[,3:7],1,max)
n2o.fnn.8$min <- apply(n2o.fnn.8[,3:7],1,min)
n2o.fnn.8$max <- apply(n2o.fnn.8[,3:7],1,max)

rf.f.8$min <- apply(rf.f.8[,3:7],1,min)
rf.f.8$max <- apply(rf.f.8[,3:7],1,max)

dT.f.8$min <- apply(dT.f.8[,3:7],1,min)
dT.f.8$max <- apply(dT.f.8[,3:7],1,max)
dT.fnn.8$min <- apply(dT.fnn.8[,3:7],1,min)
dT.fnn.8$max <- apply(dT.fnn.8[,3:7],1,max)

dT.0.8$min <- apply(dT.0.8[,3:7],1,min)
dT.0.8$max <- apply(dT.0.8[,3:7],1,max)

fn2o.f.8$min <- apply(fn2o.f.8[,3:7],1,min)
fn2o.f.8$max <- apply(fn2o.f.8[,3:7],1,max)

fch4.f.8$min <- apply(fch4.f.8[,3:7],1,min)
fch4.f.8$max <- apply(fch4.f.8[,3:7],1,max)

fco2.f.8$min <- apply(fco2.f.8[,3:7],1,min)
fco2.f.8$max <- apply(fco2.f.8[,3:7],1,max)

falb.f.8$min <- apply(falb.f.8[,3:7],1,min)
falb.f.8$max <- apply(falb.f.8[,3:7],1,max)

co2.f.2$min <- apply(co2.f.2[,3:7],1,min)
co2.f.2$max <- apply(co2.f.2[,3:7],1,max)

ch4.f.2$min <- apply(ch4.f.2[,3:7],1,min)
ch4.f.2$max <- apply(ch4.f.2[,3:7],1,max)

n2o.f.2$min <- apply(n2o.f.2[,3:7],1,min)
n2o.f.2$max <- apply(n2o.f.2[,3:7],1,max)

rf.f.2$min <- apply(rf.f.2[,3:7],1,min)
rf.f.2$max <- apply(rf.f.2[,3:7],1,max)

dT.f.2$min <- apply(dT.f.2[,3:7],1,min)
dT.f.2$max <- apply(dT.f.2[,3:7],1,max)

dT.0.2$min <- apply(dT.0.2[,3:7],1,min)
dT.0.2$max <- apply(dT.0.2[,3:7],1,max)

fn2o.f.2$min <- apply(fn2o.f.2[,3:7],1,min)
fn2o.f.2$max <- apply(fn2o.f.2[,3:7],1,max)

fch4.f.2$min <- apply(fch4.f.2[,3:7],1,min)
fch4.f.2$max <- apply(fch4.f.2[,3:7],1,max)

fco2.f.2$min <- apply(fco2.f.2[,3:7],1,min)
fco2.f.2$max <- apply(fco2.f.2[,3:7],1,max)

falb.f.2$min <- apply(falb.f.2[,3:7],1,min)
falb.f.2$max <- apply(falb.f.2[,3:7],1,max)

## Read CMIP data, RCP 8.5
dT.cmip.8 <- read.table(
                      '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp85.dat',
                     col.names = c("year","ips.1","ips.2","ips.3","ips.4","mpi.1","mpi.2","mpi.3","mir.1","ccs.1","ccs.2","ccs.3","ccs.4","ccs.5")
                      )
## Subset of years > 1859 AD
dT.cmip.8 <- dT.cmip.8[dT.cmip.8$year>1859,]
## Read Hadley CMIP data and attach to data frame
tmp <- read.table( '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp85_had.dat', col.names = c("year","had.1","had.2","had.3") )
dT.cmip.8$had.1 <- tmp$had.1
dT.cmip.8$had.2 <- tmp$had.2
dT.cmip.8$had.3 <- tmp$had.3

                     
## Read CMIP-EXT RCP 8.5 data
dT.cmip.8.ext <- read.table(
                          '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp85_ext.dat',
                          col.names=c("year","ips","mpi","ccs")
                          )
## Subset of years > 1859 AD
dT.cmip.8.ext <- dT.cmip.8.ext[dT.cmip.8.ext$year>1859 & dT.cmip.8.ext$year<2300,]
## Read Hadley CMIP-EXT data and attach to data frame
tmp <- read.table( '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp85_ext_had.dat', col.names = c("year","had") )
dT.cmip.8.ext$had <- tmp$had



## Read CMIP data, RCP 2.6
dT.cmip.2 <- read.table(
                      '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp26.dat',
                     col.names = c("year","ips.1","ips.2","ips.3","mpi.1","mpi.2","mpi.3","mir.1","ccs.1","ccs.2","ccs.3","ccs.4","ccs.5")
                      )
## Subset of years > 1859 AD
dT.cmip.2 <- dT.cmip.2[dT.cmip.2$year>1859 & dT.cmip.2$year<2100,]

## Read Hadley CMIP data and attach to data frame
tmp <- read.table( '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp26_had.dat',
                  col.names = c("year","had.1","had.2","had.3","had.4") )
dT.cmip.2$had.1 <- tmp$had.1
dT.cmip.2$had.2 <- tmp$had.2
dT.cmip.2$had.3 <- tmp$had.3
dT.cmip.2$had.4 <- tmp$had.4

## Read CMIP-EXT RCP 2.6 data
dT.cmip.2.ext <- read.table(
                          '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp26_ext.dat',
                          col.names=c("year","ips","mpi","ccs")
                          )
## Subset of years > 1859 AD
dT.cmip.2.ext <- dT.cmip.2.ext[dT.cmip.2.ext$year>1859 & dT.cmip.2.ext$year<2300,]
## Read Hadley CMIP-EXT data and attach to data frame
tmp <- read.table( '/alphadata01/bstocker/output_multiGHG/tas_cmip_rcp26_ext_had.dat', col.names = c("year","had") )
dT.cmip.2.ext$had <- tmp$had


## ///////////////////////////////////////////////////
## PLOTTING
## ---------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/cALL_b3d.pdf', width=10, height=8 )

panel <- layout(
  matrix( c(1:4), 2, 2, byrow=TRUE ),
  widths=c(8,8),
  heights=c(5.2,6),
  TRUE
  )
#layout.show(panel)

par( xaxs="i", yaxs="i", las=1  )

## ----------------------------------------------------------------
## N2O
## ----------------------------------------------------------------
xlim <- c(1900,2300)
ylim <- c(240,800)
thick <- 1  ## needed so lines do not disappear!

par( mar=c(1,4.2,2,1) )

plot(n2o.0.8$year, n2o.0.8$ctrl,
     type="l",lty=5,
     xlim=xlim, ylim=ylim,
     col="red",
     axes=FALSE, xlab="", ylab=expression(paste("cN"[2],"O [ppb]"))
     )
lines(n2o.0.2$year, n2o.0.2$ctrl,
      lty=5, col="blue"
      )

axis( 1, lwd=2, labels=F ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
box( lwd=2 )

## lines( n2o.f.8$year, n2o.f.8$max, col=rgb(1,0,0,0.6) )
## lines( n2o.f.8$year, n2o.f.8$min, col=rgb(1,0,0,0.6) )

polygon( c(n2o.f.8$year,rev(n2o.f.8$year)), c(n2o.f.8$min-thick,rev(n2o.f.8$max+thick)), col=rgb(1,0,0,0.6), border=NA  )
polygon( c(n2o.f.2$year,rev(n2o.f.2$year)), c(n2o.f.2$min-thick,rev(n2o.f.2$max+thick)), col=rgb(0,0,1,0.6), border=NA  )

text( 1920, 720, expression(paste(bold("a ")," cN"[2],"O")), cex=1.5, adj=c(0,0) )

## ----------------------------------------------------------------
## CH4
## ----------------------------------------------------------------
ylim <- c(800,6000)
thick <- 10  ## needed so lines do not disappear!

plot(ch4.0.8$year, ch4.0.8$ctrl,
     type="l",lty=5,
     xlim=xlim, ylim=ylim,
     col="red",
     axes=FALSE, xlab="year AD", ylab=expression(paste("cCH"[4]," [ppb]"))
     )
lines(ch4.0.2$year, ch4.0.2$ctrl,
      lty=5, col="blue"
      )

axis( 1, lwd=2, labels=F ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=200), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=200),labels=F,tck=-0.01)
box( lwd=2 )

#lines( ch4.f.8$year, ch4.f.8$max, col=rgb(1,0,0,0.6) )
#lines( ch4.f.8$year, ch4.f.8$min, col=rgb(1,0,0,0.6) )

polygon( c(ch4.f.8$year,rev(ch4.f.8$year)), c(ch4.f.8$min-thick,rev(ch4.f.8$max+thick)), col=rgb(1,0,0,0.6), border=NA  )
polygon( c(ch4.f.2$year,rev(ch4.f.2$year)), c(ch4.f.2$min-thick,rev(ch4.f.2$max+thick)), col=rgb(0,0,1,0.6), border=NA  )

text( 1920, 5300, expression(paste(bold("b ")," cCH"[4])), cex=1.5, adj=c(0,0) )

par( mar=c(5,4.2,1,1) )

## ----------------------------------------------------------------
## radiative forcing
## ----------------------------------------------------------------
ylim <- c(-0.2,1)
thick <- 0.003

plot(rf.0.8$year, rf.0.8$had,
     type="n",
     xlim=xlim, ylim=ylim,
     axes=FALSE, xlab="year AD", ylab=expression(paste(Delta,"RF [Wm"^-2,"]"))
     )
axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.1), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.1),labels=F,tck=-0.01)
box( lwd=2 )

## polygon( c(rf.f.8$year,rev(rf.f.8$year)), c(rf.f.8$min-rf.f.8$ctrl,rev(rf.f.8$max+thick-rf.f.8$ctrl)),
##         col=rgb(1,0,0,0.6), border=NA  )
## polygon( c(rf.f.2$year,rev(rf.f.2$year)), c(rf.f.2$min-rf.f.2$ctrl,rev(rf.f.2$max+thick-rf.f.2$ctrl)),
##         col=rgb(0,0,1,0.6), border=NA  )

polygon(c(rf.f.8$year,rev(rf.f.8$year)),
        c((fn2o.f.8$min-thick-fn2o.f.8$ctrl)+(fch4.f.8$min-thick-fch4.f.8$ctrl),rev((fn2o.f.8$max+thick-fn2o.f.8$ctrl)+(fch4.f.8$max+thick-fch4.f.8$ctrl))),
        col=rgb(1,0,0,0.6), border=NA
        )
polygon(c(rf.f.2$year,rev(rf.f.2$year)),
        c((fn2o.f.2$min-thick-fn2o.f.2$ctrl)+(fch4.f.2$min-thick-fch4.f.2$ctrl),rev((fn2o.f.2$max+thick-fn2o.f.2$ctrl)+(fch4.f.2$max+thick-fch4.f.2$ctrl))),
        col=rgb(0,0,1,0.6), border=NA
        )

legend( "bottomright", c("RCP 2.6", "RCP 8.5"), lty=c(1,1), bty="n", lwd=5, col=c(rgb(0,0,1,0.6),rgb(1,0,0,0.6)), cex=1 )

text( 1920, 0.8, expression(paste(bold("c  "), Delta,"RF(N"[2],"O+CH"[4],")")), cex=1.5, adj=c(0,0) )

## ----------------------------------------------------------------
## TEMPERATURE
## ----------------------------------------------------------------
ylim <- c(-1,14)
mygrey <- rgb(0,0,0,0.25)
thick <- 0.03

par( mgp=c(3,1,0) )
plot( dT.cmip.8$year, dT.cmip.8$ips.1,
     xlim=xlim, ylim=ylim,
     type="l", col=mygrey,
     axes=FALSE, xlab="year AD", ylab=expression(paste(Delta,"T ["^o,"C]"))
     )
axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.5), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
box( lwd=2 )

par( mgp=c(3,1,0) )
matlines(dT.cmip.8$year, dT.cmip.8[,2:17],
      col=mygrey, lty=1
      )
matlines(dT.cmip.8.ext$year, dT.cmip.8.ext[,2:5],
      col=mygrey, lty=1
      )
matlines(dT.cmip.2$year, dT.cmip.2[,2:17],
      col=mygrey, lty=1
      )
matlines(dT.cmip.2.ext$year, dT.cmip.2.ext[,2:5],
      col=mygrey, lty=1
      )

polygon( c(dT.f.8$year,rev(dT.f.8$year)), c(dT.f.8$min-thick,rev(dT.f.8$max+thick)),
        col=rgb(1,0,0,0.6), border=NA  )
polygon( c(dT.0.8$year,rev(dT.0.8$year)), c(dT.0.8$min-thick,rev(dT.0.8$max+thick)),
        col=rgb(1,0,0,0.3), border=NA  )

polygon( c(dT.f.2$year,rev(dT.f.2$year)), c(dT.f.2$min-thick,rev(dT.f.2$max+thick)),
        col=rgb(0,0,1,0.6), border=NA  )
polygon( c(dT.0.2$year,rev(dT.0.2$year)), c(dT.0.2$min-thick,rev(dT.0.2$max+thick)),
        col=rgb(0,0,1,0.3), border=NA  )

lines( dT.0.8$year, dT.0.8$ctrl,
      lty=5, col="red"
      )
lines( dT.0.2$year, dT.0.2$ctrl,
      lty=5, col="blue"
      )


text( 1920, 12, expression(paste(bold("d  "), Delta, "T")), cex=1.5, adj=c(0,0) )

dev.off()


## ///////////////////////////////////////////////////
## 2nd PLOT: RADIATIVE FORCINGS ARISING FROM FEEDBACKS BY AGENT
## ---------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/fagents.pdf', width=10, heigh=8 )

panel <- layout(
  matrix( c(1:4), 2, 2, byrow=TRUE ),
  widths=c(8,8),
  heights=c(5.2,6),
  TRUE
  )
#layout.show(panel)

par( xaxs="i", yaxs="i", las=1,
    mar=c(1,4,2,1)
    )

ylim <- c(-0.5,1)
thick <- 0.005  ## needed so lines do not disappear!

## n2o
par( mar=c(1,5,2,1) )
plot( fn2o.f.8$year, fn2o.f.8$min, type="n", xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab=expression(paste("RF [Wm"^-2,"]")) )
polygon( c(fn2o.f.8$year,rev(fn2o.f.8$year)), c(fn2o.f.8$min-thick-fn2o.f.8$ctrl,rev(fn2o.f.8$max+thick-fn2o.f.8$ctrl)),
        col=rgb(1,0,0,0.6), border=NA  )
polygon( c(fn2o.f.2$year,rev(fn2o.f.2$year)), c(fn2o.f.2$min-thick-fn2o.f.2$ctrl,rev(fn2o.f.2$max+thick-fn2o.f.2$ctrl)),
        col=rgb(0,0,1,0.6), border=NA  )
axis( 1, lwd=2, labels=F ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.1), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.1),labels=F,tck=-0.01)
box( lwd=2 )
text( 1920, 0.8, expression(paste(bold("a  "),Delta,"RF(N"[2],"O)")), cex=1.5, adj=c(0,0) )


## ch4
par( mar=c(1,1,2,5) )
plot( fn2o.f.8$year, fn2o.f.8$min, type="n", xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab="" )
polygon( c(fch4.f.8$year,rev(fch4.f.8$year)), c(fch4.f.8$min-thick-fch4.f.8$ctrl,rev(fch4.f.8$max+thick-fch4.f.8$ctrl)),
        col=rgb(1,0,0,0.6), border=NA  )
polygon( c(fch4.f.2$year,rev(fch4.f.2$year)), c(fch4.f.2$min-thick-fch4.f.2$ctrl,rev(fch4.f.2$max+thick-fch4.f.2$ctrl)),
        col=rgb(0,0,1,0.6), border=NA  )
axis( 1, lwd=2, labels=F ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2, labels=F);  axis(2, at=seq(ylim[1],ylim[2],by=0.1), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.1),labels=F,tck=-0.01)
box( lwd=2 )
mtext(expression(paste("RF [Wm"^-2,"]")), 4, line=3, las=3, cex=0.75 )
text( 1920, 0.8, expression(paste(bold("b  "),Delta,"RF(CH"[4],")")), cex=1.5, adj=c(0,0) )


## co2
par( mar=c(5,5,1,1) )
plot( fn2o.f.8$year, fn2o.f.8$min, type="n", xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab=expression(paste("RF [Wm"^-2,"]")) )
polygon( c(fco2.f.8$year,rev(fco2.f.8$year)), c(fco2.f.8$min-thick-fco2.f.8$ctrl,rev(fco2.f.8$max+thick-fco2.f.8$ctrl)),
        col=rgb(1,0,0,0.6), border=NA  )
polygon( c(fco2.f.2$year,rev(fco2.f.2$year)), c(fco2.f.2$min-thick-fco2.f.2$ctrl,rev(fco2.f.2$max+thick-fco2.f.2$ctrl)),
        col=rgb(0,0,1,0.6), border=NA  )
axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.1), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.1),labels=F,tck=-0.01)
box( lwd=2 )
text( 1920, 0.8, expression(paste(bold("c  "),Delta,"RF(CO"[2],")")), cex=1.5, adj=c(0,0) )


## albedo
par( mar=c(5,1,1,5) )
plot( fn2o.f.8$year, fn2o.f.8$min, type="n", xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab="" )
polygon( c(falb.f.8$year,rev(falb.f.8$year)), c(falb.f.8$min-thick-falb.f.8$ctrl,rev(falb.f.8$max+thick-falb.f.8$ctrl)),
        col=rgb(1,0,0,0.6), border=NA  )
polygon( c(falb.f.2$year,rev(falb.f.2$year)), c(falb.f.2$min-thick-falb.f.2$ctrl,rev(falb.f.2$max+thick-falb.f.2$ctrl)),
        col=rgb(0,0,1,0.6), border=NA  )
axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2, labels=F);  axis(2, at=seq(ylim[1],ylim[2],by=0.1), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.1),labels=F,tck=-0.01)
box( lwd=2 )
mtext(expression(paste("RF [Wm"^-2,"]")), 4, line=3, las=3, cex=0.75 )
text( 1920, 0.8, expression(paste(bold("d  "),Delta,"RF(albedo)")), cex=1.5, adj=c(0,0) )

legend( "bottomright", c("RCP 2.6", "RCP 8.5"), lty=c(1,1), bty="n", lwd=5, col=c(rgb(0,0,1,0.6),rgb(1,0,0,0.6)), cex=1 )


dev.off()

## ///////////////////////////////////////////////////
## 3rd PLOT: cCO2 
## ---------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/cCO2_b3d.pdf', width=7, heigh=5 )

xlim <- c(1900,2300)
ylim <- c(0,2500)
thick <- 5  ## needed so lines do not disappear!

par( xaxs="i", yaxs="i", las=1, mar=c(4,5,1,1) )#, cex.lab=1.5, cex.axis=1.3)

plot(co2.0.8$year, co2.0.8$ctrl,
     type="l",lty=5,
     xlim=xlim, ylim=ylim,
     col="red",
     axes=FALSE, xlab="year AD", ylab=expression(paste("cCO"[2]," [ppm]"))
     )
lines(co2.0.2$year, co2.0.2$ctrl,
      lty=5, col="blue"
      )

axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=100), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=100),labels=F,tck=-0.01)
box( lwd=2 )

## lines( co2.f.8$year, co2.f.8$max, col=rgb(1,0,0,0.6) )
## lines( co2.f.8$year, co2.f.8$min, col=rgb(1,0,0,0.6) )

polygon( c(co2.f.8$year,rev(co2.f.8$year)), c(co2.f.8$min-thick,rev(co2.f.8$max+thick)), col=rgb(1,0,0,0.6), border=NA  )
polygon( c(co2.f.2$year,rev(co2.f.2$year)), c(co2.f.2$min-thick,rev(co2.f.2$max+thick)), col=rgb(0,0,1,0.6), border=NA  )
polygon( c(co2.fnn.8$year,rev(co2.fnn.8$year)), c(co2.fnn.8$min-thick,rev(co2.fnn.8$max+thick)), col=rgb(0,1,0,0.6), border=NA  )

text( 1920, 2200, expression(paste(" cCO"[2])), cex=1.5, adj=c(0,0) )

#legend( "left", c("RCP 2.6", "RCP 8.5"), lty=c(1,1), bty="n", lwd=5, col=c(rgb(0,0,1,0.6),rgb(1,0,0,0.6)), cex=1 )

text( 1920, 2000, "RCP 2.6", adj=c(0,0), col=rgb(0,0,1,0.6) )
text( 1920, 1900, "RCP 8.5", adj=c(0,0), col=rgb(1,0,0,0.6) )
text( 1920, 1800, "RCP 8.5 no Nr", adj=c(0,0), col=rgb(0,1,0,0.6) )

dev.off()


## ///////////////////////////////////////////////////
## VALUES FOR PAPER:
## ---------------------------------------------------

print(paste("Additional warming, CT-all - ctrl, by 2300: ",
            dT.f.8$min[dT.f.8$year==2299.5]-dT.f.8$ctrl[dT.f.8$year==2299.5],"-",
            dT.f.8$max[dT.f.8$year==2299.5]-dT.f.8$ctrl[dT.f.8$year==2299.5] ))

print(paste("Additional warming, CT-(co2/albedo-only) - ctrl, by 2300: ",
            dT.0.8$min[dT.0.8$year==2299.5]-dT.0.8$ctrl[dT.0.8$year==2299.5],"-",
            dT.0.8$max[dT.0.8$year==2299.5]-dT.0.8$ctrl[dT.0.8$year==2299.5] ))

print(paste("Difference due to N2O and CH4 : ",
            (dT.f.8$min[dT.f.8$year==2299.5]-dT.f.8$ctrl[dT.f.8$year==2299.5])
            -(dT.0.8$min[dT.0.8$year==2299.5]-dT.0.8$ctrl[dT.0.8$year==2299.5]),
            (dT.f.8$max[dT.f.8$year==2299.5]-dT.f.8$ctrl[dT.f.8$year==2299.5])
            -(dT.0.8$max[dT.0.8$year==2299.5]-dT.0.8$ctrl[dT.0.8$year==2299.5])
            ))


print(paste("percentage due to N2O and CH4 : ",
            1-((dT.0.8$min[dT.0.8$year==2299.5]-dT.0.8$ctrl[dT.0.8$year==2299.5])
            /(dT.f.8$min[dT.f.8$year==2299.5]-dT.f.8$ctrl[dT.f.8$year==2299.5])),"-",
            1-((dT.0.8$max[dT.0.8$year==2299.5]-dT.0.8$ctrl[dT.0.8$year==2299.5])
            /(dT.f.8$max[dT.f.8$year==2299.5]-dT.f.8$ctrl[dT.f.8$year==2299.5]))
            ))



## ///////////////////////////////////////////////////
## PLOTTING single gas for presentations
## ---------------------------------------------------

## ----------------------------------------------------------------
## N2O
## ----------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/cN2O_b3d.pdf', width=7, height=5.5 )
par( mar=c(4,4,5,1), bg=NA, xaxs="i", yaxs="i", las=1  )
xlim <- c(1900,2300)
ylim <- c(240,800)
thick <- 1  ## needed so lines do not disappear!

plot(n2o.0.8$year, n2o.0.8$ctrl,
     type="l",lty=5,
     xlim=xlim, ylim=ylim,
     col="red",
     axes=FALSE, xlab="year AD", ylab=expression(paste("cN"[2],"O [ppb]"))
     )
lines(n2o.0.2$year, n2o.0.2$ctrl,
      lty=5, col="blue"
      )

axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=20), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=20),labels=F,tck=-0.01)
box( lwd=2 )


polygon( c(n2o.f.8$year,rev(n2o.f.8$year)), c(n2o.f.8$min-thick,rev(n2o.f.8$max+thick)), col=rgb(1,0,0,0.6), border=NA  )
polygon( c(n2o.f.2$year,rev(n2o.f.2$year)), c(n2o.f.2$min-thick,rev(n2o.f.2$max+thick)), col=rgb(0,0,1,0.6), border=NA  )
polygon( c(n2o.fnn.8$year,rev(n2o.fnn.8$year)), c(n2o.fnn.8$min-thick,rev(n2o.fnn.8$max+thick)), col=rgb(0,1,0,0.6), border=NA  )

text( 1920, 720, expression(paste("cN"[2],"O")), cex=1.5, adj=c(0,0) )
text( 1920, 660, "RCP 2.6", adj=c(0,0), col=rgb(0,0,1,0.6) )
text( 1920, 630, "RCP 8.5", adj=c(0,0), col=rgb(1,0,0,0.6) )
text( 1920, 600, "RCP 8.5 no Nr", adj=c(0,0), col=rgb(0,1,0,0.6) )

dev.off()

## ----------------------------------------------------------------
## CH4
## ----------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/cCH4_b3d.pdf', width=7, height=5.5 )
par( mar=c(4,4,5,1), bg=NA, xaxs="i", yaxs="i", las=1  )
ylim <- c(800,6000)
thick <- 10  ## needed so lines do not disappear!

plot(ch4.0.8$year, ch4.0.8$ctrl,
     type="l",lty=5,
     xlim=xlim, ylim=ylim,
     col="red",
     axes=FALSE, xlab="year AD", ylab=expression(paste("cCH"[4]," [ppb]"))
     )
lines(ch4.0.2$year, ch4.0.2$ctrl,
      lty=5, col="blue"
      )

axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=200), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=200),labels=F,tck=-0.01)
box( lwd=2 )

polygon( c(ch4.f.8$year,rev(ch4.f.8$year)), c(ch4.f.8$min-thick,rev(ch4.f.8$max+thick)), col=rgb(1,0,0,0.6), border=NA  )
polygon( c(ch4.f.2$year,rev(ch4.f.2$year)), c(ch4.f.2$min-thick,rev(ch4.f.2$max+thick)), col=rgb(0,0,1,0.6), border=NA  )

text( 1920, 5300, expression(paste("cCH"[4])), cex=1.5, adj=c(0,0) )
text( 1920, 4800, "RCP 2.6", adj=c(0,0), col=rgb(0,0,1,0.6) )
text( 1920, 4500, "RCP 8.5", adj=c(0,0), col=rgb(1,0,0,0.6) )

dev.off()


## ----------------------------------------------------------------
## radiative forcing
## ----------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/RF_b3d.pdf', width=7, height=5.5 )
par( mar=c(4,4,5,1), bg=NA, xaxs="i", yaxs="i", las=1  )
ylim <- c(-0.2,1)
thick <- 0.003

plot(rf.0.8$year, rf.0.8$had,
     type="n",
     xlim=xlim, ylim=ylim,
     axes=FALSE, xlab="year AD", ylab=expression(paste(Delta,"RF [Wm"^-2,"]"))
     )
axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.05), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.05),labels=F,tck=-0.01)
box( lwd=2 )

polygon(c(rf.f.8$year,rev(rf.f.8$year)),
        c((fn2o.f.8$min-thick-fn2o.f.8$ctrl)+(fch4.f.8$min-thick-fch4.f.8$ctrl),rev((fn2o.f.8$max+thick-fn2o.f.8$ctrl)+(fch4.f.8$max+thick-fch4.f.8$ctrl))),
        col=rgb(1,0,0,0.6), border=NA
        )
polygon(c(rf.f.2$year,rev(rf.f.2$year)),
        c((fn2o.f.2$min-thick-fn2o.f.2$ctrl)+(fch4.f.2$min-thick-fch4.f.2$ctrl),rev((fn2o.f.2$max+thick-fn2o.f.2$ctrl)+(fch4.f.2$max+thick-fch4.f.2$ctrl))),
        col=rgb(0,0,1,0.6), border=NA
        )

text( 1920, 0.8, expression(paste(Delta,"RF(N"[2],"O+CH"[4],")")), cex=1.5, adj=c(0,0) )
text( 1920, 0.7, "RCP 2.6", adj=c(0,0), col=rgb(0,0,1,0.6) )
text( 1920, 0.65, "RCP 8.5", adj=c(0,0), col=rgb(1,0,0,0.6) )

dev.off()


## ----------------------------------------------------------------
## TEMPERATURE
## ----------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/dT_b3d.pdf', width=7, height=5.5 )
par( mar=c(4,4,5,1), bg=NA, xaxs="i", yaxs="i", las=1  )
ylim <- c(-1,14)
mygrey <- rgb(0,0,0,0.25)
thick <- 0.03

par( mgp=c(3,1,0) )
plot( dT.cmip.8$year, dT.cmip.8$ips.1,
     xlim=xlim, ylim=ylim,
     type="l", col=mygrey,
     axes=FALSE, xlab="year AD", ylab=expression(paste(Delta,"T ["^o,"C]"))
     )
axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.5), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=20), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.5),labels=F,tck=-0.01)
box( lwd=2 )

par( mgp=c(3,1,0) )
matlines(dT.cmip.8$year, dT.cmip.8[,2:17],
      col=mygrey, lty=1
      )
matlines(dT.cmip.8.ext$year, dT.cmip.8.ext[,2:5],
      col=mygrey, lty=1
      )
matlines(dT.cmip.2$year, dT.cmip.2[,2:17],
      col=mygrey, lty=1
      )
matlines(dT.cmip.2.ext$year, dT.cmip.2.ext[,2:5],
      col=mygrey, lty=1
      )

polygon( c(dT.f.8$year,rev(dT.f.8$year)), c(dT.f.8$min-thick,rev(dT.f.8$max+thick)),
        col=rgb(1,0,0,0.6), border=NA  )
polygon( c(dT.0.8$year,rev(dT.0.8$year)), c(dT.0.8$min-thick,rev(dT.0.8$max+thick)),
        col=rgb(1,0,0,0.3), border=NA  )
polygon( c(dT.fnn.8$year,rev(dT.fnn.8$year)), c(dT.fnn.8$min-thick,rev(dT.fnn.8$max+thick)),
        col=rgb(0,1,0,0.6), border=NA  )

polygon( c(dT.f.2$year,rev(dT.f.2$year)), c(dT.f.2$min-thick,rev(dT.f.2$max+thick)),
        col=rgb(0,0,1,0.6), border=NA  )
polygon( c(dT.0.2$year,rev(dT.0.2$year)), c(dT.0.2$min-thick,rev(dT.0.2$max+thick)),
        col=rgb(0,0,1,0.3), border=NA  )

lines( dT.0.8$year, dT.0.8$ctrl,
      lty=5, col="red"
      )
lines( dT.0.2$year, dT.0.2$ctrl,
      lty=5, col="blue"
      )


text( 1920, 12, expression(paste(Delta, "T")), cex=1.5, adj=c(0,0) )
text( 1920, 10.5, "RCP 2.6", adj=c(0,0), col=rgb(0,0,1,0.6) )
text( 1920, 9.8, "RCP 8.5", adj=c(0,0), col=rgb(1,0,0,0.6) )
text( 1920, 9.1, "RCP 8.5 no Nr", adj=c(0,0), col=rgb(0,1,0,0.6) )
text( 1920, 8.4, "CMIP5 models", adj=c(0,0), col=rgb(0,0,0,0.5) )

dev.off()


## Save variables into workspace file
save( co2.f.8, ch4.f.8, n2o.f.8, dT.f.8, file="b3d.Rdata" )
