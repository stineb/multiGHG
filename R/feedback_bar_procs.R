data <- read.table( '/alphadata03/roth/projects/multiGHG/feedbacks/procs.dat',
                      col.names=c("ycord","min","mean","max") )

pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_procs.pdf', width=5, height=10 )

panel <- layout(
                matrix( c(1:3), ncol=1, nrow=3 ),
                widths=7,
                heights=c(6.1,5.8,6.4)
                )

transp <- 0.6
colors.rect <- c( rgb(1,0,0,0.6), rgb(1,0,0,0.6), rgb(1,0,0,0.6),
                 rgb(0,0,1,0.6), rgb(0,0,1,0.6), rgb(0,0,1,0.6),
                 rgb(0,1,0,0.6), rgb(0,1,0,0.6), rgb(0,1,0,0.6),
                 rgb(1,0,1,0.6), rgb(1,0,1,0.6), rgb(1,0,1,0.6),
                 rgb(0,1,1,0.6), rgb(0,1,1,0.6), rgb(0,1,1,0.6)
                 )
colors.line <- c( rgb(1,0,0,1), rgb(1,0,0,1), rgb(1,0,0,1),
                 rgb(0,0,1,1), rgb(0,0,1,1), rgb(0,0,1,1),
                 rgb(0,1,0,1), rgb(0,1,0,1), rgb(0,1,0,1),
                 rgb(1,0,1,1), rgb(1,0,1,1), rgb(1,0,1,1),
                 rgb(0,1,1,1), rgb(0,1,1,1), rgb(0,1,1,1)
                 )

xlim <- c(-0.4,0.4 )
width <- 0.05

## ---------------------------------------
## CLIMATE-LAND COUPLED
## ---------------------------------------
par( xaxs="i", yaxs="i", las=1, xpd=TRUE, mar=c(1,4,3,3) )
ylim <- c(-3.6,-0.8)
plot ( xlim, ylim, type="n", axes=FALSE, xlab="", ylab="" )
axis( 1, lwd=2, at=seq(xlim[1],xlim[2],by=0.1), labels=FALSE ); axis( 1, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE, tck=-0.01 )
#axis( 2, lwd=2, labels=FALSE ); axis( 2, at=seq(ylim[1],ylim[2],by=0.1), labels=FALSE )
axis( 3, lwd=2, at=seq(xlim[1],xlim[2],by=0.1), cex.axis=1.5 ); axis( 3, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE, tck=-0.01 )
box( lwd=2 )

# grey background as rectangles and period labels
rect( xlim[1], -2.7, xlim[2]+0.4, -1.7, col=rgb(0,0,0,0.05), border=NA )
rect( xlim[1], ylim[1], xlim[2]+0.4, -2.7, col=rgb(0,0,0,0.1), border=NA )
text( xlim[2]+0.015, -1., labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -2., labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -3., labels="2300", adj=c(0,0), font=2, srt=-90 , cex=1.5 )
# box at left labelling coupling
rect( xlim[1]-0.07, ylim[1], xlim[1], ylim[2], col=rgb(0,0,0,0.05), lwd=2 )
text( xlim[1]-0.04, -1.05, labels="a", adj=c(0,0), font=2, cex=2  )
text( xlim[1]-0.01, -2.7, labels="climate coupled", adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim[1]-0.01, -3, labels="T", adj=c(0,0), font=2, srt=90, cex=2.5  )

par( xpd=FALSE ) 
abline( v=0 )

## rectangles for feedbacks
par( xpd=TRUE )
rect( data$min[1:15], -data$ycord[1:15]-width, data$max[1:15], -data$ycord[1:15]+width, col=colors.rect, border=NA )
rect( data$mean[1:15]-0.002, -data$ycord[1:15]-width, data$mean[1:15]+0.002, -data$ycord[1:15]+width,
     col=colors.line, border=NA )
par( xpd=FALSE ) 

## Legend
left.leg <- rep( -0.3, 5 )
right.leg <- rep( -0.26, 5 )
mid.leg <- rep( -0.28, 5 )
lo.leg <- c(-data$ycord[2],-data$ycord[5],-data$ycord[8],-data$ycord[11],-data$ycord[14])-width
up.leg <- c(-data$ycord[2],-data$ycord[5],-data$ycord[8],-data$ycord[11],-data$ycord[14])+width
rect( left.leg, lo.leg, right.leg, up.leg,
     col=c(colors.rect[2],colors.rect[5],colors.rect[8],colors.rect[11],colors.rect[14]), border=NA )
rect( mid.leg-0.0015, lo.leg, mid.leg+0.0015, up.leg,
     col=c(colors.line[2],colors.line[5],colors.line[8],colors.line[11],colors.line[14]), border=NA )
text( right.leg+0.005, lo.leg, labels=c("full features","no Nr","no C-N interactions","no peatland","no landuse"), adj=c(0,0), font=1, cex=1.2 )



## ---------------------------------------
## CO2-LAND COUPLED
## ---------------------------------------
par( xaxs="i", yaxs="i", las=1, xpd=TRUE, mar=c(1,4,1,3) )
ylim <- c(-3.6,-0.8)
plot ( xlim, ylim, type="n", axes=FALSE, xlab="", ylab=""  )
axis( 1, lwd=2, at=seq(xlim[1],xlim[2],by=0.1), labels=FALSE ); axis( 1, at=seq(xlim[1],xlim[2], by=0.02), labels=FALSE, tck=-0.01 )
#axis( 2, lwd=2, labels=FALSE ); axis( 2, at=seq(ylim[1],ylim[2],by=0.1), labels=FALSE )
axis( 3, lwd=2, at=seq(xlim[1],xlim[2],by=0.1), labels=FALSE ); axis( 3, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE, tck=-0.01 )
box( lwd=2 )

# grey background as rectangles
rect( xlim[1], -2.7, xlim[2]+0.4, -1.7, col=rgb(0,0,0,0.05), border=NA )
rect( xlim[1], ylim[1], xlim[2]+0.4, -2.7, col=rgb(0,0,0,0.1), border=NA )
text( xlim[2]+0.015, -1., labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -2., labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -3., labels="2300", adj=c(0,0), font=2, srt=-90 , cex=1.5 )
# box at left labelling coupling
rect( xlim[1]-0.07, ylim[1], xlim[1], ylim[2], col=rgb(0,0,0,0.05), lwd=2 )
text( xlim[1]-0.04, -1.05, labels="b", adj=c(0,0), font=2, cex=2  )
text( xlim[1]-0.01, -2.7, labels=expression(paste("cCO"[2]," - land coupled")), adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim[1]-0.01, -3, labels="C", adj=c(0,0), font=2, srt=90, cex=2.5  )

par( xpd=FALSE ) 
abline( v=0 )

## rectangles for feedbacks
rect( data$min[16:30], -data$ycord[16:30]-width+3, data$max[16:30], -data$ycord[16:30]+width+3, col=colors.rect, border=NA )
rect( data$min[16:30]-0.002, -data$ycord[16:30]-width+3, data$max[16:30]+0.002, -data$ycord[16:30]+width+3,
     col=colors.line, border=NA)

## ---------------------------------------
## FULLY-LAND COUPLED
## ---------------------------------------
par( xaxs="i", yaxs="i", las=1, xpd=TRUE, mar=c(4,4,1,3) )
ylim <- c(-3.6,-0.8)
plot ( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste("feedback parameter ",italic("r ")," [Wm"^-2,"K"^-1,"]")), ylab="", cex.lab=1.5  )
axis( 1, lwd=2, at=seq(xlim[1],xlim[2],by=0.1), cex.axis=1.5 ); axis( 1, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE, tck=-0.01 )
#axis( 2, lwd=2, labels=FALSE ); axis( 2, at=seq(ylim[1],ylim[2],by=0.1), labels=FALSE )
axis( 3, lwd=2, at=seq(xlim[1],xlim[2],by=0.1), labels=FALSE ); axis( 3, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE, tck=-0.01 )
box( lwd=2 )

# grey background as rectangles
rect( xlim[1], -2.7, xlim[2]+0.4, -1.7, col=rgb(0,0,0,0.05), border=NA )
rect( xlim[1], ylim[1], xlim[2]+0.4, -2.7, col=rgb(0,0,0,0.1), border=NA )
text( xlim[2]+0.015, -1., labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -2., labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -3., labels="2300", adj=c(0,0), font=2, srt=-90 , cex=1.5 )
# box at left labelling coupling
rect( xlim[1]-0.07, ylim[1], xlim[1], ylim[2], col=rgb(0,0,0,0.05), lwd=2 )
text( xlim[1]-0.04, -1.05, labels="c", adj=c(0,0), font=2, cex=2  )
text( xlim[1]-0.01, -2.5, labels="fully coupled", adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim[1]-0.01, -3, labels="CT", adj=c(0,0), font=2, srt=90, cex=2.5  )

par( xpd=FALSE ) 
abline( v=0 )

## rectangles for feedbacks
rect( data$min[31:45], -data$ycord[31:45]-width+6, data$max[31:45], -data$ycord[31:45]+width+6, col=colors.rect, border=NA )
rect( data$mean[31:45]-0.002, -data$ycord[31:45]-width+6, data$mean[31:45]+0.002, -data$ycord[31:45]+width+6,
     col=colors.line, border=NA )


dev.off()


## Value for paper
print(paste("full CT feedback in 2100", data$min[32], data$mean[32], data$max[32]))
print(paste("full CT feedback in 2300", data$min[33], data$mean[33], data$max[33]))
print(paste("feedback uncertainty in 2300", (data$min[32]/data$mean[32])-1, (data$max[32]/data$mean[32])-1 ))

