
## RCP 2.6 data
rf.2 <- read.table('/alphadata01/bstocker/output_multiGHG/RF_2_b3d_RFall.dat',
               col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
               )

dt.2 <- read.table('/alphadata01/bstocker/output_multiGHG/dT_2_b3d_RFall.dat',
               col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
               )

## RCP 8.5 data
rf.8 <- read.table('/alphadata01/bstocker/output_multiGHG/RF_8_b3d_RFall.dat',
               col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
               )

dt.8 <- read.table('/alphadata01/bstocker/output_multiGHG/dT_8_b3d_RFall.dat',
               col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
               )


## define data frame for rho
rho.2 <- data.frame(
                    year=rf.2$year,
                    had=rf.2$had/dt.2$ctrl,
                    mpi=rf.2$mpi/dt.2$ctrl,
                    ips=rf.2$ips/dt.2$ctrl,
                    mir=rf.2$mir/dt.2$ctrl,
                    ccs=rf.2$ccs/dt.2$ctrl
                    )
rho.8 <- data.frame(
                    year=rf.8$year,
                    had=rf.8$had/dt.8$ctrl,
                    mpi=rf.8$mpi/dt.8$ctrl,
                    ips=rf.8$ips/dt.8$ctrl,
                    mir=rf.8$mir/dt.8$ctrl,
                    ccs=rf.8$ccs/dt.8$ctrl
                    )

## define data frame for feedback factor
fb.2 <- data.frame(
                    year=rf.2$year,
                    had=-(rf.2$had/dt.2$had-rho.2$had),
                    mpi=-(rf.2$mpi/dt.2$mpi-rho.2$mpi),
                    ips=-(rf.2$ips/dt.2$ips-rho.2$ips),
                    mir=-(rf.2$mir/dt.2$mir-rho.2$mir),
                    ccs=-(rf.2$ccs/dt.2$ccs-rho.2$ccs)
                    )
fb.8 <- data.frame(
                    year=rf.8$year,
                    had=-(rf.8$had/dt.8$had-rho.8$had),
                    mpi=-(rf.8$mpi/dt.8$mpi-rho.8$mpi),
                    ips=-(rf.8$ips/dt.8$ips-rho.8$ips),
                    mir=-(rf.8$mir/dt.8$mir-rho.8$mir),
                    ccs=-(rf.8$ccs/dt.8$ccs-rho.8$ccs)
                    )

## Add columns for min and max
fb.2$min <- apply(fb.2[2:6],1,min)
fb.2$max <- apply(fb.2[2:6],1,max)

fb.8$min <- apply(fb.8[2:6],1,min)
fb.8$max <- apply(fb.8[2:6],1,max)

## ///////////////////////////////////////////////////
## PLOTTING
## ---------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_vs_time.pdf', width=7, height=5 )

par( xaxs="i", yaxs="i", las=1, mar=c(4,5,1,1) )#, cex.lab=1.5, cex.axis=1.3)

xlim <- c(1990,2300)
ylim <- c(-0.1,0.3)
thick <- 0.005  ## needed so lines do not disappear!

plot( fb.2$year, fb.2$had, type="n",
     xlim=xlim, ylim=ylim,
     axes=FALSE,
     xlab="year AD", ylab=expression(paste("feedback parameter ",italic("r ")," [Wm"^-2,"K"^-1,"]"))
     )
axis( 1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=10), labels=F, tck=-0.01)
axis( 2, lwd=2);  axis(2, at=seq(ylim[1],ylim[2],by=0.02), labels=F, tck=-0.01)
axis( 3, lwd=2, labels=F ); axis(3, at=seq(xlim[1],xlim[2],by=10), labels=F, tck=-0.01)
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.02),labels=F,tck=-0.01)
box( lwd=2 )


polygon( c(fb.2$year,rev(fb.2$year)),
        c(fb.2$min-thick,rev(fb.2$max+thick)),
        col=rgb(0,0,1,0.6), border=NA
        )
polygon( c(fb.8$year,rev(fb.8$year)),
        c(fb.8$min-thick,rev(fb.8$max+thick)),
        col=rgb(1,0,0,0.6), border=NA
        )

legend( "bottomright", c("RCP 2.6", "RCP 8.5"),
       lty=c(1,1), bty="n", lwd=5,
       col=c(rgb(0,0,1,0.6),rgb(1,0,0,0.6)),
       cex=1
       )

dev.off()
