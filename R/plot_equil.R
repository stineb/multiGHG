## ///////////////////////////////////////////////////
## This script produces equil. climate sensitivity Fig.
## First, run write_ascii_equil.jnl
## beni@climate.unibe.ch
## 03.01.2013
## ---------------------------------------------------

dT.0 <- read.table('/alphadata01/bstocker/output_multiGHG/dT_equil_rfCO2.dat',
                   col.names=c("year","ctrl0.0","ctrl3.7","had","mpi","ips","mir","ccs")
                   )
dT.f <- read.table('/alphadata01/bstocker/output_multiGHG/dT_equil_rfALL.dat',
                   col.names=c("year","ctrl0.0","ctrl3.7","had","mpi","ips","mir","ccs")
                   )
dT.2x <- read.table('/alphadata01/bstocker/output_multiGHG/dT_equil_2xCO2.dat',
                    col.names=c("year","ips")
                    )

fb.0 <- read.table('/alphadata01/bstocker/output_multiGHG/FB_equil_rfCO2.dat',
                   col.names=c("year","ctrl0.0","ctrl3.7","had","mpi","ips","mir","ccs")
                   )
fb.f <- read.table('/alphadata01/bstocker/output_multiGHG/FB_equil_rfALL.dat',
                   col.names=c("year","ctrl0.0","ctrl3.7","had","mpi","ips","mir","ccs")
                   )

## Add columns for min and max
dT.0$min <- apply(dT.0[,4:8],1,min)
dT.0$max <- apply(dT.0[,4:8],1,max)

dT.f$min <- apply(dT.f[,4:8],1,min)
dT.f$max <- apply(dT.f[,4:8],1,max)

fb.0$min <- apply(fb.0[,4:8],1,min)
fb.0$max <- apply(fb.0[,4:8],1,max)

fb.f$min <- apply(fb.f[,4:8],1,min)
fb.f$max <- apply(fb.f[,4:8],1,max)

## ///////////////////////////////////////////////////
## PLOTTING
## ---------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/equil.pdf', width=7, height=7 )

panel <- layout(
  matrix( c(1:2), 2, 1, byrow=TRUE ),
  widths=c(7),
  heights=c(3.7,4.3),
  TRUE
  )
#layout.show(panel)

par( xaxs="i", yaxs="i", las=1,
    mar=c(1,4,1,1)
    #mgp=c(2,1,0)
    )

mygrey <- rgb(0,0,0,0.25)


## ----------------------------------------------------------------
## temperature change
## ----------------------------------------------------------------
xlim <- c(0,2035)
ylim <- c(-0.2,4)

plot( 1:length(dT.0$ctrl0.0), dT.0$ctrl0.0,
     type="n",
     xlim=xlim, ylim=ylim,
     col=mygrey,
     axes=FALSE, xlab="", ylab=expression(paste(Delta,"T ["^o,"C]"))
     )
lines( 1:length(dT.0$ctrl3.7), dT.0$ctrl3.7-dT.0$ctrl0.0 )

axis( 1, lwd=2, labels=F); axis(1, at=seq(xlim[1],xlim[2],by=100), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.2), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=100), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
box( lwd=2 )

polygon( c(1:length(dT.0$ctrl3.7),rev(1:length(dT.0$ctrl3.7))),
        c(dT.0$min-dT.0$ctrl0.0,rev(dT.0$max-dT.0$ctrl0.0)),
        col=rgb(0,0,1,0.6), border=NA
        )
polygon( c(1:length(dT.f$ctrl3.7),rev(1:length(dT.f$ctrl3.7))),
        c(dT.f$min-dT.f$ctrl0.0,rev(dT.f$max-dT.f$ctrl0.0)),
        col=rgb(1,0,0,0.6), border=NA
        )
lines( 1:length(dT.2x$ips), dT.2x$ips, lty=2 )

text( 50,3.5, expression(paste(bold("a  "), Delta,"T (3.7 Wm"^-2,")")), cex=1.2, adj=c(0,0) )

legend( "bottomright",
       c(expression(paste("ctrl")),
         expression(paste("2"%*%"cCO"[2])),
         expression(paste("CT-",Delta,"C-",Delta, alpha)),
         expression(paste("CT-all agents"))),
         lty=c(1,3,1,1), bty="n", lwd=c(2,2,5,5),
         col=c("black","black",rgb(0,0,1,0.6),rgb(1,0,0,0.6)),
         cex=1
         )


## ----------------------------------------------------------------
## feedback parameter
## ----------------------------------------------------------------
xlim <- c(0,2035)
ylim <- c(0,0.5)

par(mar=c(4,4,1,1))

plot( 1:length(fb.0$ctrl0.0), fb.0$ctrl0.0,
     type="n",
     xlim=xlim, ylim=ylim,
     axes=FALSE, xlab="simulation year", ylab=expression(paste("Wm"^-2,"K"^-1))
     )

axis( 1, lwd=2, ); axis(1, at=seq(xlim[1],xlim[2],by=100), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.02), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=100), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.02),labels=F,tck=-0.01)
box( lwd=2 )

polygon( c(36:length(fb.0$ctrl3.7),rev(36:length(fb.0$ctrl3.7))),
        c(-fb.0$max[36:length(fb.0$ctrl3.7)],rev(-fb.0$min[36:length(fb.0$ctrl3.7)])),
        col=rgb(0,0,1,0.6), border=NA
        )
polygon( c(36:length(fb.f$ctrl3.7),rev(36:length(fb.f$ctrl3.7))),
        c(-fb.f$max[36:length(fb.0$ctrl3.7)],rev
          (-fb.f$min[36:length(fb.0$ctrl3.7)])),
        col=rgb(1,0,0,0.6), border=NA
        )

text( 50,0.45, expression(paste(bold("b  "),"feedback factor ",italic("r")^"CT")), cex=1.2, adj=c(0,0) )

dev.off()


## VALUES FOR PAPER:
s.f.ctrl <-  dT.f$ctrl3.7[length(dT.f$min)]-dT.f$ctrl0.0[length(dT.0$ctrl0.0)]
s.0.min  <-  dT.0$min[length(dT.f$min)]-dT.0$ctrl0.0[length(dT.0$ctrl0.0)]
s.0.max  <-  dT.0$max[length(dT.f$max)]-dT.0$ctrl0.0[length(dT.0$ctrl0.0)]
s.f.min  <-  dT.f$min[length(dT.f$min)]-dT.f$ctrl0.0[length(dT.f$ctrl0.0)]
s.f.max  <-  dT.f$max[length(dT.f$max)]-dT.f$ctrl0.0[length(dT.f$ctrl0.0)]
s.2x     <-  dT.2x$ips[length(dT.2x$ips)]

print(paste("climate sensitivity, no land : ",
            s.f.ctrl
            ))
print(paste("climate sensitivity, co2/albedo only : ",
            s.0.min,
            "-",
            s.0.max
            ))
print(paste("climate sensitivity, all interactions : ",
            s.f.min,
            "-",
            s.f.max
            ))
print(paste("land-dC-albedo amplifies climate sens. by : ",
            s.0.min/s.f.ctrl,
            "-",
            s.0.max/s.f.ctrl
            ))
print(paste("total land amplifies climate sens. by : ",
            s.f.min/s.f.ctrl,
            "-",
            s.f.max/s.f.ctrl
            ))
print(paste("climate sensitivity, conventionally defined : ",
            s.2x
            ))
print(paste("total land amplifies CONVENTIONAL climate sens. by : ",
            s.f.min/s.2x,
            "-",
            s.f.max/s.2x
            ))
