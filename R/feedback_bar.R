
rcp <- 8

if (rcp==2){
  # data.ct <- read.table( '/alphadata01/bstocker/multiGHG_analysis/roth/rcp26/agents_CT.dat',
  #                       col.names=c("ycord","min","mean","max") )
  # data.c <- read.table( '/alphadata01/bstocker/multiGHG_analysis/roth/rcp26/agents_C.dat',
  #                      col.names=c("ycord","min","mean","max") )
  # data.t <- read.table( '/alphadata01/bstocker/multiGHG_analysis/roth/rcp26/agents_T.dat',
  #                      col.names=c("ycord","min","mean","max") )

  data.ct <- read.table( '~/data/output_multiGHG/agents_CT.dat',
                        col.names=c("ycord","min","mean","max") )
  data.c <- read.table( '~/data/output_multiGHG/agents_C.dat',
                       col.names=c("ycord","min","mean","max") )
  data.t <- read.table( '~/data/output_multiGHG/agents_T.dat',
                       col.names=c("ycord","min","mean","max") )

}else if (rcp==8){
  # data.ct <- read.table( '/alphadata01/bstocker/multiGHG_analysis/roth/agents_CT.dat',
  #                       col.names=c("ycord","min","mean","max") )
  # data.c <- read.table( '/alphadata01/bstocker/multiGHG_analysis/roth/agents_C.dat',
  #                      col.names=c("ycord","min","mean","max") )
  # data.t <- read.table( '/alphadata01/bstocker/multiGHG_analysis/roth/agents_T.dat',
  #                      col.names=c("ycord","min","mean","max") )

  data.ct <- read.table( '~/data/output_multiGHG/agents_CT.dat',
                        col.names=c("ycord","min","mean","max") )
  data.c <- read.table( '~/data/output_multiGHG/agents_C.dat',
                       col.names=c("ycord","min","mean","max") )
  data.t <- read.table( '~/data/output_multiGHG/agents_T.dat',
                       col.names=c("ycord","min","mean","max") )
  
  data.procs <- read.table( './data/procs_redux.dat',
                     col.names=c("ycord","min","mean","max") )
  
}else{
  print("rcp indicated not available")
}

#data.ct$ycord <- -(data.ct$ycord+0.8)

if (rcp==2){
  pdf( './fig/feedback_bar_rcp26.pdf', width=5, height=10 )
  xlim1 <- -0.36
  xlim2 <- 0.36
  lboxl <- -0.06
  linethick <- 0.0025
}else if (rcp==8){
  pdf( './fig/feedback_bar.pdf', width=5, height=10 )
  xlim1 <- -0.2
  xlim2 <- 0.2
  lboxl <- -0.04
  linethick <- 0.0015
}

panel <- layout(
                matrix( c(1:3), ncol=1, nrow=3 ),
                widths=7,
                heights=c(6.1,5.8,6.4)
                )
#layout.show(panel)
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

par( xaxs="i", yaxs="i" )

## --------------------------------------------------------------------------------------
## CT
## --------------------------------------------------------------------------------------
par( mar=c(1,4,3,3) )
plot( c(xlim1,xlim2), c(ylim2,ylim1), type="n", axes=FALSE, xlab="", ylab="" )
axis( 1, lwd=2, labels=FALSE ); axis( 1, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
axis( 3, lwd=2, cex.axis=1.5 ); axis( 3, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
rect( xlim1, -2.8, xlim2+0.4, -1.8, col=rgb(0,0,0,0.05), border=NA )
rect( xlim1, ylim1, xlim2+0.4, -2.8, col=rgb(0,0,0,0.1), border=NA )
rect( xlim1+lboxl, ylim1, xlim1, ylim2, col=rgb(0,0,0,0.05), lwd=2 )
text( xlim1-0.03, -1.05, labels="a", adj=c(0,0), font=2, cex=2  )
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



## --------------------------------------------------------------------------------------
## T
## --------------------------------------------------------------------------------------
par( mar=c(1,4,1,3) )
plot( c(xlim1,xlim2), c(ylim2,ylim1), type="n", axes=FALSE, xlab="", ylab="" )
axis( 1, lwd=2, labels=FALSE ); axis( 1, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
axis( 3, lwd=2, labels=FALSE ); axis( 3, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
rect( xlim1, -2.8, xlim2+0.4, -1.8, col=rgb(0,0,0,0.05), border=NA )
rect( xlim1, ylim1, xlim2+0.4, -2.8, col=rgb(0,0,0,0.1), border=NA )
rect( xlim1+lboxl, ylim1, xlim1, ylim2, col=rgb(0,0,0,0.05), lwd=2 )
text( xlim1-0.03, -1.05, labels="b", adj=c(0,0), font=2, cex=2  )
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

#rect( left.leg[1]-0.005, lo.leg[4]-0.05, rep(-0.08,4), up.leg[1]+0.05, col=NA )
rect( left.leg, lo.leg, right.leg, up.leg, col=c(colors.rect[2],colors.rect[5],colors.rect[8],colors.rect[11]), border=NA )
rect( mid.leg-linethick, lo.leg, mid.leg+linethick, up.leg, col=c(colors.line[2],colors.line[5],colors.line[8],colors.line[11]), border=NA )
text( right.leg+0.005, lo.leg, labels=c("albedo",expression(paste(Delta,"C")),expression(paste("N"[2],"O")),expression(paste("CH"[4]))), adj=c(0,0), font=1, cex=1.5 )

## --------------------------------------------------------------------------------------
## C
## --------------------------------------------------------------------------------------
par( mar=c(4,4,1,3) )
plot( c(xlim1,xlim2), c(ylim2,ylim1), type="n", axes=FALSE, xlab=expression(paste("feedback factor ",italic("r ")," [Wm"^-2,"K"^-1,"]")), ylab="", cex.lab=1.5 )
axis( 1, lwd=2, cex.axis=1.5 ); axis( 1, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
axis( 3, lwd=2, labels=FALSE ); axis( 3, at=seq(xlim1,xlim2,by=0.02), labels=FALSE )
box( lwd=2 )
par( xpd=TRUE )
rect( xlim1, -2.8, xlim2+0.4, -1.8, col=rgb(0,0,0,0.05), border=NA )
rect( xlim1, ylim1, xlim2+0.4, -2.8, col=rgb(0,0,0,0.1), border=NA )
rect( xlim1+lboxl, ylim1, xlim1, ylim2, col=rgb(0,0,0,0.05), lwd=2 )
text( xlim1-0.03, -1.05, labels="c", adj=c(0,0), font=2, cex=2  )
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
  text( -0.12, -data.c$ycord[4]+width*0.15,
       as.character(format(-data.c$mean[4], digits=3)),
       col=colors.line[4]
     )
}

dev.off()

## Numbers for IPCC: r^CT(N2O) at 2100 and 2300
print(paste("r^CT(N2O) at 2100 and 2300: ",data.ct$mean[8:9]))

char.ct <- as.character( format( data.ct$mean, digits=3 ) ) 
char.c <- as.character( format( data.c$mean, digits=3 ) ) 
char.t <- as.character( format( data.t$mean, digits=3 ) )

char.procs <- as.character( format( data.procs$mean, digits=3 ) )

out <- file("data/terrestrial_feedbacks_table_BStocker2013.out","w")
cat("Source: Stocker et al., 2013, Nature Climate Change","\n",file=out)
cat("Multiple greenhouse gas feedbacks from the land biosphere under future climate change scenarios","\n",file=out)
cat("Evaluated from RCP 8.5 simulation","\n",file=out,sep=" ")
cat("Values corresponding to means in Fig. 5, Stocker et al, 2013","\n",file=out)
cat("------------------------------------------------------------","\n",file=out)
cat("FEEDBACK FACTOR CT     present    2100       2300","\n",file=out)
cat("total              ",char.procs[13],char.procs[14],char.procs[15],"\n",file=out,sep="   ")
cat("deltaC             ",char.ct[4],char.ct[5],char.ct[6],"\n",file=out,sep="   ")
cat("albedo             ",char.ct[1],char.ct[2],char.ct[3],"\n",file=out,sep="   ")
cat("N2O                ",char.ct[7],char.ct[8],char.ct[9],"\n",file=out,sep="   ")
cat("CH4                ",char.ct[10],char.ct[11],char.ct[12],"\n",file=out,sep="   ")
cat("","\n",file=out)
cat("FEEDBACK FACTOR C      present    2100       2300","\n",file=out)
cat("total              ",char.procs[7],char.procs[8],char.procs[9],"\n",file=out,sep="   ")
cat("deltaC             ",char.c[4],char.c[5],char.c[6],"\n",file=out,sep="   ")
cat("albedo             ",char.c[1],char.c[2],char.c[3],"\n",file=out,sep="   ")
cat("N2O                ",char.c[7],char.c[8],char.c[9],"\n",file=out,sep="   ")
cat("CH4                ",char.c[10],char.c[11],char.c[12],"\n",file=out,sep="   ")
cat("","\n",file=out)
cat("FEEDBACK FACTOR T      present    2100       2300","\n",file=out)
cat("total              ",char.procs[1],char.procs[2],char.procs[3],"\n",file=out,sep="   ")
cat("deltaC              ",char.t[4],char.t[5],char.t[6],"\n",file=out,sep="   ")
cat("albedo              ",char.t[1],char.t[2],char.t[3],"\n",file=out,sep="   ")
cat("N2O                 ",char.t[7],char.t[8],char.t[9],"\n",file=out,sep="   ")
cat("CH4                 ",char.t[10],char.t[11],char.t[12],"\n",file=out,sep="   ")
close(out)
