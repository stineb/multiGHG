## RCP data
co2.rcp26 <- read.table('/alphadata01/bstocker/input_data/ghg_data/rcp26/cCO2_rcp26.dat',
                        header=FALSE, col.names=c("year","conc")
                        )
co2.rcp85 <- read.table('/alphadata01/bstocker/input_data/ghg_data/rcp85/cCO2_rcp85.dat',
                        header=FALSE, col.names=c("year","conc")
                        )
## Bern3D results
co2.0.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_8_b3d_RFco2.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
co2.f.8 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_8_b3d_RFall.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
co2.0.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_2_b3d_RFco2.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
co2.f.2 <- read.table( '/alphadata01/bstocker/output_multiGHG/co2_2_b3d_RFall.dat',
                    col.names=c("year","ctrl","had","mpi","ips","mir","ccs")
                    )
## Add columns for min and max
co2.f.8$min <- apply(co2.f.8[,3:7],1,min)
co2.f.8$max <- apply(co2.f.8[,3:7],1,max)
co2.f.2$min <- apply(co2.f.2[,3:7],1,min)
co2.f.2$max <- apply(co2.f.2[,3:7],1,max)


pdf( "/alphadata01/bstocker/multiGHG_analysis/fig/co2_rcp.pdf", width=7, height=5 )

par( xaxs="i", yaxs="i", las=1, cex.lab=1.5, cex.axis=1.3, mar=c(4,5,1,2) )

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- 150
ylim2 <- 1000
thick <- 5  ## needed so lines do not disappear!

plot( co2.rcp26$year[co2.rcp26$year<=2006], co2.rcp26$conc[co2.rcp26$year<=2006],
     type="l",
     xlim=c(xlim1,xlim2),
     ylim=c(ylim1,ylim2),
     axes=FALSE,
     lwd=2,
     xlab="year AD", ylab="ppm"
     )
lines( co2.rcp26$year[co2.rcp26$year>=2006], co2.rcp26$conc[co2.rcp26$year>=2006],
     col="blue", lwd=2
     )
lines( co2.rcp85$year[co2.rcp85$year>=2006], co2.rcp85$conc[co2.rcp85$year>=2006],
     col="red", lwd=2
     )

axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=50),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=50),labels=F,tck=-0.01)
box(lwd=2)

#polygon( c(co2.f.8$year,rev(co2.f.8$year)), c(co2.f.8$min-thick,rev(co2.f.8$max+thick)), col=rgb(1,0,0,0.4), border=NA  )
#polygon( c(co2.f.2$year,rev(co2.f.2$year)), c(co2.f.2$min-thick,rev(co2.f.2$max+thick)), col=rgb(0,0,1,0.4), border=NA  )


text( 1910, 850, expression(paste("cCO"[2], "  for offline simulations")), cex=1.5, adj=c(0,0), font=1 )

legend( "bottomright", c("historical","RCP 2.6","RCP 8.5"), lty=1, bty="n", lwd=2, col=c("black","blue","red"), cex=1.5 )

dev.off()
