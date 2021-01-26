redux <- TRUE
data <- read.table( '/alphadata03/roth/projects/multiGHG/feedbacks/procs.dat',
                      col.names=c("ycord","min","mean","max") )

if (redux) {
  ## Use only full setup and no-Dyn setup
  ##  data.redux <- rbind( data[1:3,], data[7:9,], data[16:18,], data[22:24,], data[31:33,], data[37:39,] )
  data <- read.table( '/alphadata01/bstocker/multiGHG_analysis/procs_redux.dat',
                     col.names=c("ycord","min","mean","max") )
}

## panel <- layout(
##                 matrix( c(1:3), ncol=1, nrow=3 ),
##                 widths=7,
##                 heights=c(6.1,5.8,6.4)
##                 )

transp <- 0.6
if (redux) {
  colors.rect <- c(
                   rgb(0,0,1,0.6), rgb(0,0,1,0.6), rgb(0,0,1,0.6),
                   rgb(0,1,0,0.6), rgb(0,1,0,0.6), rgb(0,1,0,0.6)
                   )
  colors.line <- c(
                   rgb(0,0,1,1), rgb(0,0,1,1), rgb(0,0,1,1),
                   rgb(0,1,0,1), rgb(0,1,0,1), rgb(0,1,0,1)
                   )
} else {
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
}

xlim <- c(-0.4,0.4)
if (redux) {
  width <- 0.1
} else {
  width <- 0.05
}



## --------------------------------------------------------------------------------------
## CT
## --------------------------------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_CT_byfeatures.pdf', width=7, height=5.5 )

par( mar=c(4,4,1,3) )
par( xaxs="i", yaxs="i" )
plot ( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste("feedback factor ",italic("r ")," [Wm"^-2,"K"^-1,"]")), ylab="" )
axis( 1, lwd=2, cex.axis=1.5 ); axis( 1, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE )
axis( 3, lwd=2, cex.axis=1.5 ); axis( 3, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
# grey background as rectangles and period labels
rect( xlim[1], -2.7, xlim[2]+0.4, -1.7, col=rgb(0,0,0,0.05), border=NA )
rect( xlim[1], ylim[1], xlim[2]+0.4, -2.7, col=rgb(0,0,0,0.1), border=NA )
text( xlim[2]+0.015, -1., labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -2., labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -3., labels="2300", adj=c(0,0), font=2, srt=-90 , cex=1.5 )
# box at left labelling coupling
rect( xlim[1]-0.07, ylim[1], xlim[1], ylim[2], col=rgb(0,0,0,0.05), lwd=2 )
text( xlim[1]-0.01, -2.7, labels="fully coupled", adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim[1]-0.01, -3, labels="CT", adj=c(0,0), font=2, srt=90, cex=2.5  )

par( xpd=FALSE ) 
abline( v=0 )

## rectangles for feedbacks
par( xpd=TRUE )
if (redux) {
  rect( data$min[13:18], -data$ycord[1:6]-width, data$max[13:18], -data$ycord[1:6]+width, col=colors.rect, border=NA )
  rect( data$mean[13:18]-0.002, -data$ycord[1:6]-width, data$mean[13:18]+0.002, -data$ycord[1:6]+width,
       col=colors.line, border=NA )
} else {
  rect( data$min[31:45], -data$ycord[1:15]-width, data$max[31:45], -data$ycord[1:15]+width, col=colors.rect, border=NA )
  rect( data$mean[31:45]-0.002, -data$ycord[1:15]-width, data$mean[31:45]+0.002, -data$ycord[1:15]+width,
       col=colors.line, border=NA )
}
par( xpd=FALSE ) 

## Legend
if (redux) {
  nfeatures <- 2
} else {
  nfeatures <- 5
}

left.leg <- rep( -0.3, nfeatures )
right.leg <- rep( -0.26, nfeatures )
mid.leg <- rep( -0.28, nfeatures )
if (redux) {
  lo.leg <- c(-data$ycord[2],-data$ycord[5])-width
  up.leg <- c(-data$ycord[2],-data$ycord[5])+width
} else {
  lo.leg <- c(-data$ycord[2],-data$ycord[5],-data$ycord[8],-data$ycord[11],-data$ycord[14])-width
  up.leg <- c(-data$ycord[2],-data$ycord[5],-data$ycord[8],-data$ycord[11],-data$ycord[14])+width
}
rect( left.leg, lo.leg, right.leg, up.leg,
     col=c(colors.rect[2],colors.rect[5],colors.rect[8],colors.rect[11],colors.rect[14]), border=NA )
rect( mid.leg-0.0015, lo.leg, mid.leg+0.0015, up.leg,
     col=c(colors.line[2],colors.line[5],colors.line[8],colors.line[11],colors.line[14]), border=NA )

if (redux) {
  text( right.leg+0.005, lo.leg, labels=c("LPX C-N","LPX C-only"), adj=c(0,0), font=1, cex=1.2 )
} else {
  text( right.leg+0.005, lo.leg, labels=c("full features","no Nr","no C-N interactions","no peatland","no landuse"), adj=c(0,0), font=1, cex=1.2 )
}

dev.off()


## --------------------------------------------------------------------------------------
## T
## --------------------------------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_T_byfeatures.pdf', width=7, height=5.5 )

par( mar=c(4,4,1,3) )
par( xaxs="i", yaxs="i" )
plot ( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste("feedback factor ",italic("r ")," [Wm"^-2,"K"^-1,"]")), ylab="" )
axis( 1, lwd=2, cex.axis=1.5 ); axis( 1, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE )
axis( 3, lwd=2, cex.axis=1.5 ); axis( 3, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
# grey background as rectangles and period labels
rect( xlim[1], -2.7, xlim[2]+0.4, -1.7, col=rgb(0,0,0,0.05), border=NA )
rect( xlim[1], ylim[1], xlim[2]+0.4, -2.7, col=rgb(0,0,0,0.1), border=NA )
text( xlim[2]+0.015, -1., labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -2., labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -3., labels="2300", adj=c(0,0), font=2, srt=-90 , cex=1.5 )
# box at left labelling coupling
rect( xlim[1]-0.07, ylim[1], xlim[1], ylim[2], col=rgb(0,0,0,0.05), lwd=2 )
text( xlim[1]-0.01, -2.7, labels="climate coupled", adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim[1]-0.01, -3, labels="T", adj=c(0,0), font=2, srt=90, cex=2.5  )

par( xpd=FALSE ) 
abline( v=0 )

## rectangles for feedbacks
par( xpd=TRUE )
if (redux) {
  rect( data$min[1:6], -data$ycord[1:6]-width, data$max[1:6], -data$ycord[1:6]+width, col=colors.rect, border=NA )
  rect( data$mean[1:6]-0.002, -data$ycord[1:6]-width, data$mean[1:6]+0.002, -data$ycord[1:6]+width,
       col=colors.line, border=NA )
} else {
  rect( data$min[1:15], -data$ycord[1:15]-width, data$max[1:15], -data$ycord[1:15]+width, col=colors.rect, border=NA )
  rect( data$mean[1:15]-0.002, -data$ycord[1:15]-width, data$mean[1:15]+0.002, -data$ycord[1:15]+width,
       col=colors.line, border=NA )
}
par( xpd=FALSE ) 

## Legend
left.leg <- rep( -0.3, nfeatures )
right.leg <- rep( -0.26, nfeatures )
mid.leg <- rep( -0.28, nfeatures )
if (redux){
  lo.leg <- c(-data$ycord[2],-data$ycord[5])-width
  up.leg <- c(-data$ycord[2],-data$ycord[5])+width
} else {
  lo.leg <- c(-data$ycord[2],-data$ycord[5],-data$ycord[8],-data$ycord[11],-data$ycord[14])-width
  up.leg <- c(-data$ycord[2],-data$ycord[5],-data$ycord[8],-data$ycord[11],-data$ycord[14])+width
}
rect( left.leg, lo.leg, right.leg, up.leg,
     col=c(colors.rect[2],colors.rect[5],colors.rect[8],colors.rect[11],colors.rect[14]), border=NA )
rect( mid.leg-0.0015, lo.leg, mid.leg+0.0015, up.leg,
     col=c(colors.line[2],colors.line[5],colors.line[8],colors.line[11],colors.line[14]), border=NA )

if (redux) {
  text( right.leg+0.005, lo.leg, labels=c("LPX C-N","LPX C-only"), adj=c(0,0), font=1, cex=1.2 )
} else {
  text( right.leg+0.005, lo.leg, labels=c("full features","no Nr","no C-N interactions","no peatland","no landuse"), adj=c(0,0), font=1, cex=1.2 )
}

dev.off()

## --------------------------------------------------------------------------------------
## C
## --------------------------------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_C_byfeatures.pdf', width=7, height=5.5 )

par( mar=c(4,4,1,3) )
par( xaxs="i", yaxs="i" )
plot ( xlim, ylim, type="n", axes=FALSE, xlab=expression(paste("feedback factor ",italic("r ")," [Wm"^-2,"K"^-1,"]")), ylab="" )
axis( 1, lwd=2, cex.axis=1.5 ); axis( 1, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE )
axis( 3, lwd=2, cex.axis=1.5 ); axis( 3, at=seq(xlim[1],xlim[2],by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
# grey background as rectangles and period labels
rect( xlim[1], -2.7, xlim[2]+0.4, -1.7, col=rgb(0,0,0,0.05), border=NA )
rect( xlim[1], ylim[1], xlim[2]+0.4, -2.7, col=rgb(0,0,0,0.1), border=NA )
text( xlim[2]+0.015, -1., labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -2., labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim[2]+0.015, -3., labels="2300", adj=c(0,0), font=2, srt=-90 , cex=1.5 )
# box at left labelling coupling
rect( xlim[1]-0.07, ylim[1], xlim[1], ylim[2], col=rgb(0,0,0,0.05), lwd=2 )
text( xlim[1]-0.01, -2.7, labels=expression(paste("CO"[2]," coupled")), adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim[1]-0.01, -3, labels="C", adj=c(0,0), font=2, srt=90, cex=2.5  )

par( xpd=FALSE ) 
abline( v=0 )

## rectangles for feedbacks
par( xpd=TRUE )
if (redux) {
  rect( data$min[7:12], -data$ycord[1:6]-width, data$max[7:12], -data$ycord[1:6]+width, col=colors.rect, border=NA )
  rect( data$mean[7:12]-0.002, -data$ycord[1:6]-width, data$mean[7:12]+0.002, -data$ycord[1:6]+width,
       col=colors.line, border=NA )
} else {
  rect( data$min[16:30], -data$ycord[1:15]-width, data$max[16:30], -data$ycord[1:15]+width, col=colors.rect, border=NA )
  rect( data$mean[16:30]-0.002, -data$ycord[1:15]-width, data$mean[16:30]+0.002, -data$ycord[1:15]+width,
       col=colors.line, border=NA )
} 
par( xpd=FALSE ) 

## Legend
left.leg <- rep( 0.1, nfeatures )
right.leg <- rep( 0.14, nfeatures )
mid.leg <- rep( 0.12, nfeatures )
if (redux) {
  lo.leg <- c(-data$ycord[2],-data$ycord[5])-width
  up.leg <- c(-data$ycord[2],-data$ycord[5])+width
} else {
  lo.leg <- c(-data$ycord[2],-data$ycord[5],-data$ycord[8],-data$ycord[11],-data$ycord[14])-width
  up.leg <- c(-data$ycord[2],-data$ycord[5],-data$ycord[8],-data$ycord[11],-data$ycord[14])+width
}
rect( left.leg, lo.leg, right.leg, up.leg,
     col=c(colors.rect[2],colors.rect[5],colors.rect[8],colors.rect[11],colors.rect[14]), border=NA )
rect( mid.leg-0.0015, lo.leg, mid.leg+0.0015, up.leg,
     col=c(colors.line[2],colors.line[5],colors.line[8],colors.line[11],colors.line[14]), border=NA )

if (redux) {
  text( right.leg+0.005, lo.leg, labels=c("LPX C-N","LPX C-only"), adj=c(0,0), font=1, cex=1.2 )
} else {
  text( right.leg+0.005, lo.leg, labels=c("full features","no Nr","no C-N interactions","no peatland","no landuse"), adj=c(0,0), font=1, cex=1.2 )
}

dev.off()

