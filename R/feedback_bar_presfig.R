
rcp <- 2

if (rcp==2){
  data.ct <- read.table( '/alphadata03/roth/projects/multiGHG/feedbacks/rcp26/agents_CT.dat',
                        col.names=c("ycord","min","mean","max") )
  data.c <- read.table( '/alphadata03/roth/projects/multiGHG/feedbacks/rcp26/agents_C.dat',
                       col.names=c("ycord","min","mean","max") )
  data.t <- read.table( '/alphadata03/roth/projects/multiGHG/feedbacks/rcp26/agents_T.dat',
                       col.names=c("ycord","min","mean","max") )
}else if (rcp==8){
  data.ct <- read.table( '/alphadata03/roth/projects/multiGHG/feedbacks/agents_CT.dat',
                        col.names=c("ycord","min","mean","max") )
  data.c <- read.table( '/alphadata03/roth/projects/multiGHG/feedbacks/agents_C.dat',
                       col.names=c("ycord","min","mean","max") )
  data.t <- read.table( '/alphadata03/roth/projects/multiGHG/feedbacks/agents_T.dat',
                       col.names=c("ycord","min","mean","max") )
}else{
  print("rcp indicated not available")
}

#data.ct$ycord <- -(data.ct$ycord+0.8)

if (rcp==2){
  xlim1 <- -0.36
  xlim2 <- 0.36
  lboxl <- -0.06
  linethick <- 0.0025
}else if (rcp==8){
  xlim1 <- -0.2
  xlim2 <- 0.2
  lboxl <- -0.04
  linethick <- 0.0015
}

transp <- 0.6
colors.rect <- c( rgb(1,0,0,transp), rgb(1,0,0,transp), rgb(1,0,0,transp),
                 rgb(0,0,1,transp), rgb(0,0,1,transp), rgb(0,0,1,transp),
                 rgb(0,1,0,transp), rgb(0,1,0,transp), rgb(0,1,0,transp),
                 rgb(1,0,1,transp), rgb(1,0,1,transp), rgb(1,0,1,transp)
                 )
colors.line <- c( rgb(1,0,0,1), rgb(1,0,0,1), rgb(1,0,0,1),
                 rgb(0,0,1,1), rgb(0,0,1,1), rgb(0,0,1,1),
                 rgb(0,1,0,1), rgb(0,1,0,1), rgb(0,1,0,1),
                 rgb(1,0,1,1), rgb(1,0,1,1), rgb(1,0,1,1)
                 )

ylim1 <- -3.8
ylim2 <- -0.8
width <- 0.09

## --------------------------------------------------------------------------------------
## CT
## --------------------------------------------------------------------------------------
if (rcp==2){
  pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_CT_rcp26.pdf', width=7, height=5.5 )
}else if (rcp==8){
  pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_CT.pdf', width=7, height=5.5 )
}
par( mar=c(4,4,1,3) )
par( xaxs="i", yaxs="i" )
plot( c(xlim1,xlim2), c(ylim2,ylim1), type="n", axes=FALSE, xlab=expression(paste("feedback factor ",italic("r ")," [Wm"^-2,"K"^-1,"]")), ylab="", cex.lab=1.5 )
axis( 1, lwd=2, cex.axis=1.5 ); axis( 1, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
axis( 3, lwd=2, cex.axis=1.5 ); axis( 3, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
rect( xlim1, -2.8, xlim2+0.4, -1.8, col=rgb(0,0,0,0.05), border=NA )
rect( xlim1, ylim1, xlim2+0.4, -2.8, col=rgb(0,0,0,0.1), border=NA )
rect( xlim1+lboxl, ylim1, xlim1, ylim2, col=rgb(0,0,0,0.05), lwd=2 )
text( xlim1-0.01, -2.5, labels="fully coupled", adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim1-0.01, -3, labels="CT", adj=c(0,0), font=2, srt=90, cex=2.5  )
text( xlim2+0.01, -1.1, labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim2+0.01, -2.1, labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim2+0.01, -3.1, labels="2300", adj=c(0,0), font=2, srt=-90 , cex=1.5 )
par( xpd=FALSE ) 
abline( v=0 )

par( xpd=TRUE )
rect( data.ct$min, -data.ct$ycord-width, data.ct$max, -data.ct$ycord+width, col=colors.rect, border=NA )
rect( data.ct$mean-linethick, -data.ct$ycord-width, data.ct$mean+linethick, -data.ct$ycord+width,
     col=colors.line, border=NA )
par( xpd=FALSE ) 

## Legend
if (rcp==2){
  left.leg <- rep( -0.27, 4 )
  right.leg <- rep( -0.25, 4 )
  mid.leg <- rep( -0.26, 4 )
}else if(rcp==8){
  left.leg <- rep( -0.17, 4 )
  right.leg <- rep( -0.15, 4 )
  mid.leg <- rep( -0.16, 4 )
}
lo.leg <- c(-data.t$ycord[2],-data.t$ycord[5],-data.t$ycord[8],-data.t$ycord[11])-width
up.leg <- c(-data.t$ycord[2],-data.t$ycord[5],-data.t$ycord[8],-data.t$ycord[11])+width
rect( left.leg, lo.leg, right.leg, up.leg, col=c(colors.rect[2],colors.rect[5],colors.rect[8],colors.rect[11]), border=NA )
rect( mid.leg-linethick, lo.leg, mid.leg+linethick, up.leg, col=c(colors.line[2],colors.line[5],colors.line[8],colors.line[11]), border=NA )
text( right.leg+0.005, lo.leg, labels=c("albedo",expression(paste(Delta,"C")),expression(paste("N"[2],"O")),expression(paste("CH"[4]))), adj=c(0,0), font=1, cex=1.5 )

dev.off()

## --------------------------------------------------------------------------------------
## T
## --------------------------------------------------------------------------------------
if (rcp==2){
  pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_T_rcp26.pdf', width=7, height=5.5 )
}else if (rcp==8){
  pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_T.pdf', width=7, height=5.5 )
}
par( mar=c(4,4,1,3) )
par( xaxs="i", yaxs="i" )
plot( c(xlim1,xlim2), c(ylim2,ylim1), type="n", axes=FALSE, xlab=expression(paste("feedback factor ",italic("r ")," [Wm"^-2,"K"^-1,"]")), ylab="", cex.lab=1.5 )
axis( 1, lwd=2, cex.axis=1.5 ); axis( 1, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
axis( 3, lwd=2, labels=FALSE ); axis( 3, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
rect( xlim1, -2.8, xlim2+0.4, -1.8, col=rgb(0,0,0,0.05), border=NA )
rect( xlim1, ylim1, xlim2+0.4, -2.8, col=rgb(0,0,0,0.1), border=NA )
rect( xlim1+lboxl, ylim1, xlim1, ylim2, col=rgb(0,0,0,0.05), lwd=2 )
text( xlim1-0.01, -2.9, labels="climate - land coupled", adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim1-0.01, -3.2, labels="T", adj=c(0,0), font=2, srt=90, cex=2.5  )
text( xlim2+0.01, -1.1, labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim2+0.01, -2.1, labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim2+0.01, -3.1, labels="2300", adj=c(0,0), font=2, srt=-90, cex=1.5  )
par( xpd=FALSE ) 
abline( v=0 )

rect( data.t$min, -data.t$ycord-width, data.t$max, -data.t$ycord+width, col=colors.rect, border=NA  )
rect( data.t$mean-linethick, -data.t$ycord-width, data.t$mean+linethick, -data.t$ycord+width,
     col=colors.line, border=NA )

## Legend
if (rcp==2){
  left.leg <- rep( -0.27, 4 )
  right.leg <- rep( -0.25, 4 )
  mid.leg <- rep( -0.26, 4 )
}else if(rcp==8){
  left.leg <- rep( -0.17, 4 )
  right.leg <- rep( -0.15, 4 )
  mid.leg <- rep( -0.16, 4 )
}
lo.leg <- c(-data.t$ycord[2],-data.t$ycord[5],-data.t$ycord[8],-data.t$ycord[11])-width
up.leg <- c(-data.t$ycord[2],-data.t$ycord[5],-data.t$ycord[8],-data.t$ycord[11])+width
rect( left.leg, lo.leg, right.leg, up.leg, col=c(colors.rect[2],colors.rect[5],colors.rect[8],colors.rect[11]), border=NA )
rect( mid.leg-linethick, lo.leg, mid.leg+linethick, up.leg, col=c(colors.line[2],colors.line[5],colors.line[8],colors.line[11]), border=NA )
text( right.leg+0.005, lo.leg, labels=c("albedo",expression(paste(Delta,"C")),expression(paste("N"[2],"O")),expression(paste("CH"[4]))), adj=c(0,0), font=1, cex=1.5 )

dev.off()

## --------------------------------------------------------------------------------------
## C
## --------------------------------------------------------------------------------------
if (rcp==2){
  pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_C_rcp26.pdf', width=7, height=5.5 )
}else if (rcp==8){
  pdf( '/alphadata01/bstocker/multiGHG_analysis/feedback_bar_C.pdf', width=7, height=5.5 )
}
par( mar=c(4,4,1,3) )
par( xaxs="i", yaxs="i" )
plot( c(xlim1,xlim2), c(ylim2,ylim1), type="n", axes=FALSE, xlab=expression(paste("feedback factor ",italic("r ")," [Wm"^-2,"K"^-1,"]")), ylab="", cex.lab=1.5 )
axis( 1, lwd=2, cex.axis=1.5 ); axis( 1, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
axis( 3, lwd=2, labels=FALSE ); axis( 3, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
rect( xlim1, -2.8, xlim2+0.4, -1.8, col=rgb(0,0,0,0.05), border=NA )
rect( xlim1, ylim1, xlim2+0.4, -2.8, col=rgb(0,0,0,0.1), border=NA )
rect( xlim1+lboxl, ylim1, xlim1, ylim2, col=rgb(0,0,0,0.05), lwd=2 )
text( xlim1-0.005, -3.0, labels=expression(paste("cCO"[2]," - land coupled")), adj=c(0,0), font=1, srt=90, cex=1.5  )
text( xlim1-0.01, -3.3, labels="C", adj=c(0,0), font=2, srt=90, cex=2.5  )
text( xlim2+0.01, -1.1, labels="present", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim2+0.01, -2.1, labels="2100", adj=c(0,0), font=2, srt=-90, cex=1.5  )
text( xlim2+0.01, -3.1, labels="2300", adj=c(0,0), font=2, srt=-90, cex=1.5  )
par( xpd=FALSE ) 
abline( v=0 )

par( xpd=TRUE )
rect( data.c$min, -data.c$ycord-width, data.c$max, -data.c$ycord+width, col=colors.rect, border=NA )
rect( data.c$mean-linethick, -data.c$ycord-width, data.c$mean+linethick, -data.c$ycord+width,
     col=colors.line, border=NA )
par( xpd=FALSE ) 

if (rcp==8){
  arrows( -0.18, -data.c$ycord[4], -0.195, -data.c$ycord[4], col=colors.line[4], length=0.05 )
  text( -0.175, -data.c$ycord[4]-width/2,
       expression(paste("r"^"C","(",Delta,"C) =")),
       col=colors.line[4], adj=c(0,0)
       )
  text( -0.11, -data.c$ycord[4]+width*0.15,
       as.character(format(-data.c$mean[4], digits=3)),
       col=colors.line[4]
     )
}

## Legend
if (rcp==2){
  left.leg <- rep( 0.1, 4 )
  right.leg <- rep( 0.12, 4 )
  mid.leg <- rep( 0.11, 4 )
}else if(rcp==8){
  left.leg <- rep( 0.1, 4 )
  right.leg <- rep( 0.12, 4 )
  mid.leg <- rep( 0.11, 4 )
}
lo.leg <- c(-data.t$ycord[2],-data.t$ycord[5],-data.t$ycord[8],-data.t$ycord[11])-width
up.leg <- c(-data.t$ycord[2],-data.t$ycord[5],-data.t$ycord[8],-data.t$ycord[11])+width
rect( left.leg, lo.leg, right.leg, up.leg, col=c(colors.rect[2],colors.rect[5],colors.rect[8],colors.rect[11]), border=NA )
rect( mid.leg-linethick, lo.leg, mid.leg+linethick, up.leg, col=c(colors.line[2],colors.line[5],colors.line[8],colors.line[11]), border=NA )
text( right.leg+0.005, lo.leg, labels=c("albedo",expression(paste(Delta,"C")),expression(paste("N"[2],"O")),expression(paste("CH"[4]))), adj=c(0,0), font=1, cex=1.5 )

dev.off()

## Numbers for IPCC: r^CT(N2O) at 2100 and 2300
print(paste("r^CT(N2O) at 2100 and 2300: ",data.ct$mean[8:9]))
