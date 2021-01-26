source('/alphadata01/bstocker/multiGHG_analysis/myspline.R')

## Define scenarios
scen <- c("rcp85","rcp26")
nscen <- length(scen)

## Define setups
sims <- c("r1","r2","r3","r4","r5","r6","r7","r8")
nsims <- length(sims)

## Read list of file names
names.26 <- read.table('/alphadata01/bstocker/multiGHG_analysis/runnames_rcp26.txt', header=F )$V1
n.26 <- length(names.26)
names.85 <- read.table('/alphadata01/bstocker/multiGHG_analysis/runnames_rcp85.txt', header=F )$V1
n.85 <- length(names.85)

nmods <- c( n.85, n.26 )

## Read data, historical period
tmp <- read.table('/alphadata01/bstocker/output_multiGHG/eN2O_r1_historical.dat', header=F)
lhist <- length(tmp[,1])
yrs.hist <- tmp[,1]
e.n2o.hist <- array( NA, dim=c(lhist,nsims) )
e.ch4.hist <- array( NA, dim=c(lhist,nsims) )
dC.hist    <- array( NA, dim=c(lhist,nsims) )
for (i in seq(nsims)){
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/eN2O_',sims[i],'_historical.dat', sep="" )
  e.n2o.hist[,i] <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/eCH4_',sims[i],'_historical.dat', sep="" )
  e.ch4.hist[,i] <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/dCtot_',sims[i],'_historical.dat', sep="" )
  tmp1 <- read.table( filn, header=F )$V2
  dC.hist[,i] <- -(tmp1-tmp1[1])*1e-15
}

## Read data, future
tmp <- read.table(paste('/alphadata01/bstocker/output_multiGHG/eN2O_r1_rcp26_',names.26[5],'.dat', sep="" ))
lfut <- length(tmp[,1])
yrs.fut <- tmp[,1]
e.n2o.fut <- array( NA, dim=c(lfut,nscen,nsims,max(n.26,n.85)))
e.ch4.fut <- array( NA, dim=c(lfut,nscen,nsims,max(n.26,n.85)))
dC.fut    <- array( NA, dim=c(lfut,nscen,nsims,max(n.26,n.85)))
for (i in seq(nscen)){
  for (j in seq(nsims)){
    if (j > 4) {
      k <- 1
      filn <- paste('/alphadata01/bstocker/output_multiGHG/eN2O_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
      tmp <- read.table(filn,header=F)$V2
      e.n2o.fut[1:length(tmp),i,j,k] <- tmp       
      filn <- paste('/alphadata01/bstocker/output_multiGHG/eCH4_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
      tmp <- read.table(filn,header=F)$V2
      e.ch4.fut[1:length(tmp),i,j,k] <- tmp       
      filn <- paste('/alphadata01/bstocker/output_multiGHG/dCtot_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
      tmp <- read.table(filn,header=F)$V2
      dC.fut[1:length(tmp),i,j,k] <- -(tmp-tmp[1])*1e-15 + dC.hist[length(dC.hist[,j]),j]       
    } else {
      if (scen[i]=="rcp26") {
        for (k in seq(n.26)){
          filn <- paste('/alphadata01/bstocker/output_multiGHG/eN2O_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
          tmp <- read.table(filn,header=F)$V2
          e.n2o.fut[1:length(tmp),i,j,k] <- tmp 
          filn <- paste('/alphadata01/bstocker/output_multiGHG/eCH4_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
          tmp <- read.table(filn,header=F)$V2
          e.ch4.fut[1:length(tmp),i,j,k] <- tmp 
          filn <- paste('/alphadata01/bstocker/output_multiGHG/dCtot_',sims[j],'_',scen[i],'_',names.26[k],'.dat', sep="")
          tmp <- read.table(filn,header=F)$V2
          dC.fut[1:length(tmp),i,j,k] <- -(tmp-tmp[1])*1e-15 + dC.hist[length(dC.hist[,j]),j] 
        }
      } else if (scen[i]=="rcp85") {
        for (k in seq(n.85)){
          filn <- paste('/alphadata01/bstocker/output_multiGHG/eN2O_',sims[j],'_',scen[i],'_',names.85[k],'.dat', sep="" )
          tmp <- read.table(filn,header=F)$V2
          e.n2o.fut[1:length(tmp),i,j,k] <- tmp
          filn <- paste('/alphadata01/bstocker/output_multiGHG/eCH4_',sims[j],'_',scen[i],'_',names.85[k],'.dat', sep="" )
          tmp <- read.table(filn,header=F)$V2
          e.ch4.fut[1:length(tmp),i,j,k] <- tmp
          filn <- paste('/alphadata01/bstocker/output_multiGHG/dCtot_',sims[j],'_',scen[i],'_',names.85[k],'.dat', sep="" )
          tmp <- read.table(filn,header=F)$V2
          dC.fut[1:length(tmp),i,j,k] <- -(tmp-tmp[1])*1e-15 + dC.hist[length(dC.hist[,j]),j] 
        }
      }
    }
  }
}

## Take median among models of future emissions ans spline
med.n2o.fut <- apply( e.n2o.fut[,,1,], c(1,2), median, na.rm=TRUE )
med.ch4.fut <- apply( e.ch4.fut[,,1,], c(1,2), median, na.rm=TRUE )
med.dC.fut  <- apply(    dC.fut[,,1,], c(1,2), median, na.rm=TRUE )

spl.n2o.fut <- array( NA, dim=c(lfut,nscen) )
spl.ch4.fut <- array( NA, dim=c(lfut,nscen) )
spl.dC.fut  <- array( NA, dim=c(lfut,nscen) )
for (i in seq(nscen)){
  spl.n2o.fut[,i] <- myspline( yrs.fut, med.n2o.fut[,i], 30 )
  spl.ch4.fut[,i] <- myspline( yrs.fut, med.ch4.fut[,i], 30 )
  spl.dC.fut[,i]  <- myspline( yrs.fut,  med.dC.fut[,i], 30 )
}

## Mean emissions at the end of 21st cent. 
e.n2o.2100 <- apply( e.n2o.fut[85:95,,,], c(2,3,4), mean, na.rm=TRUE )
e.ch4.2100 <- apply( e.ch4.fut[85:95,,,], c(2,3,4), mean, na.rm=TRUE )
dC.2100    <- apply(    dC.fut[85:95,,,], c(2,3,4), mean, na.rm=TRUE )

## Calculate min, max, and median for each simulation and scenario, summarizing models
min.n2o <- apply( e.n2o.2100, c(1,2), min, na.rm=TRUE )
min.ch4 <- apply( e.ch4.2100, c(1,2), min, na.rm=TRUE )
min.dC  <- apply(    dC.2100, c(1,2), min, na.rm=TRUE )

max.n2o <- apply( e.n2o.2100, c(1,2), max, na.rm=TRUE )
max.ch4 <- apply( e.ch4.2100, c(1,2), max, na.rm=TRUE )
max.dC  <- apply(    dC.2100, c(1,2), max, na.rm=TRUE )

med.n2o <- apply( e.n2o.2100, c(1,2), mean, na.rm=TRUE )
med.ch4 <- apply( e.ch4.2100, c(1,2), mean, na.rm=TRUE )
med.dC  <- apply(    dC.2100, c(1,2), mean, na.rm=TRUE )


## ///////////////////////////////////////////////////
## PLOTTING
## ---------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/eGHG.pdf', width=10, height=17 )

mar.left=c(1,5,1,1)
mar.right=c(1,1,1,5)

col.scen <- c( rgb(1,0,0,0.3),rgb(0,0,1,0.2) )

col.line=c(rgb(1,0,0),rgb(0,0,1))
col.line.fine=c(rgb(1,0,0,1),rgb(1,0,0,1),rgb(0,0,1,1),rgb(0,0,1,1))

col.rect.fine=c(rgb(1,0,0,0.5),rgb(1,1,1),rgb(0,0,1,0.5),rgb(1,1,1))
col.rect=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5))

## Panel layout
panel <- layout(
  matrix( c(1:6), 3, 2, byrow=TRUE ),
  widths=c(8,6),
  heights=c(6.35,6,6.7),
  TRUE
  )
#layout.show(panel)
#par(new=TRUE)

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- 6
ylim2 <- 18

## ----------------------------------------------------------------
## N2O
## ----------------------------------------------------------------

## Plot lines
par( xaxs="i", yaxs="i", las=1, oma=c(0,0,0,0), mar=mar.left+c(0,0,2,0),
    cex.lab=2, cex.axis=1.5 )

plot( yrs.hist, e.n2o.hist[,1], type='l', xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
     axes=FALSE, xlab="", ylab=expression(paste("TgNyr"^-1)),
     lwd=2
     )
rect( xlim1, ylim1, yrs.hist[length(yrs.hist)], ylim2, col=rgb(0,0,0,0.1), border=NA )

axis(1, lwd=2, labels=F); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2);  axis(2, at=seq(ylim1,ylim2,by=0.5),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=0.5),labels=F,tck=-0.01)

for (i in seq(nscen)){
  for (k in seq(nmods[i])){
    lines( yrs.fut, e.n2o.fut[,i,1,k], col=col.scen[i], lwd=2 )
  }
  #lines( yrs.fut, spl.n2o.fut[,i], col=col.line, lwd=2 )
}
text( 1910, 16.5, expression(paste(bold("a  "),"eN"[2],"O")), cex=2.5, adj=c(0,0), font=2 )

## Plot rectangles
par(mar=mar.right+c(0,0,2,0), xpd=TRUE )

left <- 0:((nsims*nscen)-1)
left <- left+0.125
right <- 1:(nsims*nscen)
right <- right-0.125

bottom.n2o <- rep(NA,16)
top.n2o <- rep(NA,16)
middle.n2o <- rep(NA,16)

ilook <- c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
jlook <- c(1,2,1,2,3,4,3,4,5,6,5,6,7,8,7,8)

for (k in seq(nsims*nscen)){
  bottom.n2o[k] <- min.n2o[ilook[k],jlook[k]]
  top.n2o[k]    <- max.n2o[ilook[k],jlook[k]]
  middle.n2o[k] <- med.n2o[ilook[k],jlook[k]]
}

plot( 0:16, seq(ylim1,ylim2,by=(ylim2-ylim1)/16), type='n',
     xlab="", ylab="", axes=FALSE
     )
axis(2, lwd=2, labels=FALSE );  axis(2, at=seq(ylim1,ylim2,by=0.5), tck=-0.01, labels=FALSE )
axis(4, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=0.5),labels=F,tck=-0.01)
rect( c(0,8), c(ylim1-0.9,ylim1-0.9), c(4,12), c(ylim2+2,ylim2+2), col=rgb(0,0,0,0.1), border=NA )
box(lwd=2)
rect( left, bottom.n2o, right, top.n2o, col=col.rect.fine,
     border=c(NA,rgb(1,0,0,0.5),NA,rgb(0,0,1,0.5))
     )
rect( left, middle.n2o-0.1, right, middle.n2o+0.1,
     col=c(col.line.fine,col.line.fine,rep(c(rgb(1,0,0),"white",rgb(0,0,1),"white"),2)),
     border=c(rep(NA,8), rep(c(NA,rgb(1,0,0),NA,rgb(0,0,1)),2))
     )
## for (k in seq(nsims*nscen)){
##   segments( left[k]+0.1, middle.n2o[k], right[k]-0.1, middle.n2o[k], lwd=4, col.line[k%%length(col.line)] )
## }

text( 0.25, 18.25, "CT", cex=2.5, font=2, adj=c(0,0) )
text( 1.75, 18.25, "standard", cex=1.1, adj=c(0,0))

text( 4.75, 18.25, "T", cex=2.5, font=2, adj=c(0,0) )
text( 5.75, 18.65, "climate", cex=1.1, adj=c(0,0))
text( 5.75, 18.25, "only", cex=1.1, adj=c(0,0))

text( 8.75, 18.25, "C", cex=2.5, font=2, adj=c(0,0) )
text( 9.85, 18.55, expression(paste("cCO"[2])), cex=1.1, adj=c(0,0))
text( 9.85, 18.25, "only", cex=1.1, adj=c(0,0))

text(12.75, 18.25, "ctrl", cex=2.5, font=2, adj=c(0,0) )

## ----------------------------------------------------------------
## CH4
## ----------------------------------------------------------------
## Plot lines
ylim1 <- 180
ylim2 <- 360
par(mar=mar.left, xpd=FALSE )
plot( yrs.hist, e.ch4.hist[,1], type='l', xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
     axes=FALSE, xlab="", ylab=expression(paste("TgCH"[4],"yr"^-1)),
     lwd=2
     )
rect( xlim1, ylim1, yrs.hist[length(yrs.hist)], ylim2, col=rgb(0,0,0,0.1), border=NA )
axis(1, lwd=2, labels=F ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=-0.01)

for (i in seq(nscen)){
  for (k in seq(nmods[i])){
    lines( yrs.fut, e.ch4.fut[,i,1,k], col=col.scen[i], lwd=2 )
  }
}
text( 1910, 335, expression(paste(bold("b  "),"eCH"[4])), cex=2.5, adj=c(0,0), font=2 )

## Plot rectangles
par(mar=mar.right, xpd=TRUE )

left <- 0:((nsims*nscen/2)-1)
left <- left*2+0.25
right <- 1:(nsims*nscen/2)
right <- right*2-0.25

bottom.ch4 <- rep(NA,nsims*nscen/2)
top.ch4 <- rep(NA,nsims*nscen/2)
middle.ch4 <- rep(NA,nsims*nscen/2)

ilook <- c(1,2,1,2,1,2,1,2)
jlook <- c(1,1,3,3,5,5,7,7)

for (k in seq(nsims*nscen/2)){
  bottom.ch4[k] <- min.ch4[ilook[k],jlook[k]]
  top.ch4[k]    <- max.ch4[ilook[k],jlook[k]]
  middle.ch4[k] <- med.ch4[ilook[k],jlook[k]]
}

plot( 0:16, seq(ylim1,ylim2,by=(ylim2-ylim1)/16), type='n',
     xlab="", ylab="", axes=FALSE
     )
axis(2, lwd=2, labels=FALSE );  axis(2, at=seq(ylim1,ylim2,by=10), tck=-0.01, labels=FALSE )
axis(4, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=-0.01)
rect( c(0,8), c(ylim1-10,ylim1-10), c(4,12), c(ylim2+25,ylim2+25), col=rgb(0,0,0,0.1), border=NA )
box(lwd=2)

rect( left, bottom.ch4, right, top.ch4, col=col.rect, border=NA )
rect( left, middle.ch4-1.5, right, middle.ch4+1.5, col=col.line, border=NA ) 
## for (k in seq(nsims*nscen)){
##   segments( left[k]+0.1, middle.ch4[k], right[k]-0.1, middle.ch4[k], lwd=4, col.line[k%%length(col.line)] )
## }


## ----------------------------------------------------------------
## dC
## ----------------------------------------------------------------
## Plot lines

ylim1 <- -120
ylim2 <- 410

par(mar=mar.left+c(4,0,0,0), xpd=FALSE )

plot( yrs.hist, dC.hist[,1], type='l', xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
     axes=FALSE, xlab="year AD", ylab="GtC",
     lwd=2
     )
rect( xlim1, ylim1, yrs.hist[length(yrs.hist)], ylim2, col=rgb(0,0,0,0.1), border=NA )
axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2, labels=c("100","0","-100","-200","-300","-400"), at=c(-100,0,100,200,300,400) );  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(2, at=seq(ylim1,ylim2,by=20),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(4, at=seq(ylim1,ylim2,by=20),labels=F,tck=-0.01)

for (i in seq(nscen)){
  for (k in seq(nmods[i])){
    lines( yrs.fut, dC.fut[,i,1,k], col=col.scen[i], lwd=2 )
  }
}
text( 1910, 350, expression(paste(bold("c  "),Delta,"C")), cex=2.5, adj=c(0,0), font=2 )
text( 1923, 320, "relative to 1765 AD", cex=2, adj=c(0,0), font=1 )

segments( 1900, 0, 1915, 0, lty=5 )
arrows( rep(1910,2), rep(0,2), rep(1910,2), c(-60,60), length=0.1 )
text( rep(1910,2), c(-77,65), c("sink","source"), cex=1.5, adj=c(0,0) )
legend( "bottomright", c("RCP 2.6", "RCP 8.5"), lty=c(1,1), bty="n", lwd=5, col=c("blue","red"), cex=2 )

## Plot rectangles
par(mar=mar.right+c(4,0,0,0), xpd=TRUE )

bottom.dC <- rep(NA,nsims*nscen/2)
top.dC <- rep(NA,nsims*nscen/2)
middle.dC <- rep(NA,nsims*nscen/2)

for (k in seq(nsims*nscen)){
  bottom.dC[k] <- min.dC[ilook[k],jlook[k]]
  top.dC[k]    <- max.dC[ilook[k],jlook[k]]
  middle.dC[k] <- med.dC[ilook[k],jlook[k]]
}

plot( 0:16, seq(ylim1,ylim2,by=(ylim2-ylim1)/16), type='n',
     xlab="", ylab="", axes=FALSE
     )
axis(2, labels=F, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(2, at=seq(ylim1,ylim2,by=20),labels=F,tck=-0.01)
axis(4, lwd=2, cex.axis=1.5, labels=c("100","0","-100","-200","-300","-400"), at=c(-100,0,100,200,300,400));  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(4, at=seq(ylim1,ylim2,by=20),labels=F,tck=-0.01)

rect( c(0,8), c(ylim1,ylim1), c(4,12), c(ylim2+25,ylim2+25), col=rgb(0,0,0,0.1), border=NA )
box(lwd=2)
rect( left, bottom.dC, right, top.dC, col=col.rect, border=NA )
rect( left, middle.dC-5, right, middle.dC+5, col=col.line, border=NA ) 
## for (k in seq(nsims*nscen)){
##   segments( left[k]+0.1, middle.dC[k], right[k]-0.1, middle.dC[k], lwd=4, col.line[k%%length(col.line)] )
## }



dev.off()


## VALUES FOR PAPER:

e.n2o.preind.ct <- mean(e.n2o.hist[1:31,1])
e.n2o.pres.ct <- mean(e.n2o.hist[233:242,1])
e.n2o.2108.ct <- c(bottom.n2o[1], top.n2o[1])
e.n2o.2102.ct <- c( bottom.n2o[3], top.n2o[3])

e.n2o.preind.0 <- mean(e.n2o.hist[1:31,7])
e.n2o.pres.0 <- mean(e.n2o.hist[233:242,7])
e.n2o.2108.0 <- c(bottom.n2o[13], top.n2o[13])
e.n2o.2102.0 <- c(bottom.n2o[15], top.n2o[15])

e.n2o.preind.noNr <- mean(e.n2o.hist[1:31,2])
e.n2o.pres.noNr   <- mean(e.n2o.hist[233:242,2])
e.n2o.2108.noNr   <- c(bottom.n2o[2], top.n2o[2])
e.n2o.2102.noNr   <- c(bottom.n2o[4], top.n2o[4])

dC.preind.ct <- mean(dC.hist[1:31,1])
dC.pres.ct <- mean(dC.hist[233:242,1])
dC.2108.ct <- c(bottom.dC[1], top.dC[1])
dC.2102.ct <- c( bottom.dC[2], top.dC[2])

dC.preind.t <- mean(dC.hist[1:31,3])
dC.pres.t <- mean(dC.hist[233:242,3])
dC.2108.t <- c(bottom.dC[3], top.dC[3])
dC.2102.t <- c( bottom.dC[4], top.dC[4])

dC.preind.c <- mean(dC.hist[1:31,5])
dC.pres.c <- mean(dC.hist[233:242,5])
dC.2108.c <- c(bottom.dC[5], top.dC[5])
dC.2102.c <- c( bottom.dC[6], top.dC[6])

dC.preind.0 <- mean(dC.hist[1:31,7])
dC.pres.0 <- mean(dC.hist[233:242,7])
dC.2108.0 <- c(bottom.dC[7], top.dC[7])
dC.2102.0 <- c(bottom.dC[8], top.dC[8])

## from emissionfactor.jnl
dn2o.agr <- 5.040

print(paste("eN2O, CT: preind.: ", e.n2o.preind.ct ))
print(paste("eN2O, CT: present: ", e.n2o.pres.ct))
print(paste("eN2O, CT: 2100, RCP 8.5: ", e.n2o.2108.ct ) )
print(paste("eN2O, CT: 2100, RCP 2.6: ", e.n2o.2102.ct ) )
print(paste("Delta eN2O, CT: 2100, RCP 8.5: ", e.n2o.2108.ct-e.n2o.pres.ct ) )
print(paste("Delta eN2O, CT: 2100, RCP 2.6: ", e.n2o.2102.ct-e.n2o.pres.ct ) )
print("------------------------------------------")

print(paste("eN2O, ctrl: preind.: ", e.n2o.preind.0 ))
print(paste("eN2O, ctrl: present: ", e.n2o.pres.0 ))
print(paste("eN2O, ctrl: 2100, RCP 8.5: ", e.n2o.2108.0 ) )
print(paste("eN2O, ctrl: 2100, RCP 2.6: ", e.n2o.2102.0 ) )
print(paste("Delta eN2O, ctrl: 2100, RCP 8.5: ", e.n2o.2108.0-e.n2o.pres.0 ) )
print(paste("Delta eN2O, ctrl: 2100, RCP 2.6: ", e.n2o.2102.0-e.n2o.pres.0 ) )
print("------------------------------------------")

print(paste("eN2O, noNr: preind.: ", e.n2o.preind.noNr ))
print(paste("eN2O, noNr: present: ", e.n2o.pres.noNr ))
print(paste("eN2O, noNr: 2100, RCP 8.5: ", e.n2o.2108.noNr ) )
print(paste("eN2O, noNr: 2100, RCP 2.6: ", e.n2o.2102.noNr ) )
print(paste("Delta eN2O, noNr: 2100, RCP 8.5: ", e.n2o.2108.noNr-e.n2o.pres.noNr ) )
print(paste("Delta eN2O, noNr: 2100, RCP 2.6: ", e.n2o.2102.noNr-e.n2o.pres.noNr ) )
print("------------------------------------------")

print(paste("fraction due to external forcings: rcp85 ",  (e.n2o.2108.0-e.n2o.pres.0)/(e.n2o.2108.ct-e.n2o.pres.ct)   ))
print(paste("fraction due to external forcings: rcp26 ",  (e.n2o.2102.0-e.n2o.pres.0)/(e.n2o.2102.ct-e.n2o.pres.ct)   ))
print(paste("fraction due to external forcings: today ",  (e.n2o.pres.0-e.n2o.preind.0)/(e.n2o.pres.ct-e.n2o.preind.ct)   ))
print(paste("fraction due to CT alone : rcp85 ",  (e.n2o.2108.noNr-e.n2o.pres.noNr)/(e.n2o.2108.ct-e.n2o.pres.ct)   ))
print(paste("fraction due to CT alone : rcp26 ",  (e.n2o.2102.noNr-e.n2o.pres.noNr)/(e.n2o.2102.ct-e.n2o.pres.ct)   ))

print(paste("reduction in n2o increase when Nr is avoided:", (e.n2o.2108.noNr-e.n2o.pres.noNr) / (e.n2o.2108.ct-e.n2o.pres.ct) ))

## IPCC statement:
print("this much of the N2O increase since preindustrial in 21s century is due to CO2 and climate:")
print(paste("RCP2.6: ",(e.n2o.2102.ct-e.n2o.preind.ct)-(e.n2o.2102.0-e.n2o.preind.0)))
print(paste("RCP8.5: ",(e.n2o.2108.ct-e.n2o.preind.ct)-(e.n2o.2108.0-e.n2o.preind.0)))

print("this much of the N2O increase since present-day in 21s century is due to CO2 and climate:")
print(paste("RCP2.6: ",(e.n2o.2102.ct-e.n2o.pres.ct)-(e.n2o.2102.0-e.n2o.pres.0)))
print(paste("RCP8.5: ",(e.n2o.2108.ct-e.n2o.pres.ct)-(e.n2o.2108.0-e.n2o.pres.0)))

print("****************************************")

print(paste("eCH4, CT: preind.: ", mean(e.ch4.hist[1:31,1])))
print(paste("eCH4, CT: present: ", mean(e.ch4.hist[233:242,1])))
print(paste("eCH4, CT: 2100, RCP 8.5: ", bottom.ch4[1], top.ch4[1] ) )
print(paste("eCH4, CT: 2100, RCP 2.6: ", bottom.ch4[2], top.ch4[2] ) )
print(paste("eCH4, ctrl: preind.: ", mean(e.ch4.hist[1:31,7])))
print(paste("eCH4, ctrl: present: ", mean(e.ch4.hist[233:242,7])))
print(paste("eCH4, ctrl: 2100, RCP 8.5: ", bottom.ch4[7], top.ch4[7] ) )
print(paste("eCH4, ctrl: 2100, RCP 2.6: ", bottom.ch4[8], top.ch4[8] ) )

print("****************************************")

print(paste("eDC, CT: preind.: ", dC.preind.ct ))
print(paste("eDC, CT: present: ", dC.pres.ct ))
print(paste("C lost during RCP 8.5, CT: ", dC.2108.ct-dC.pres.ct ))
print(paste("C lost during RCP 2.6, CT: ", dC.2102.ct-dC.pres.ct ))
print(paste("C lost during RCP 8.5, ctrl: ", dC.2108.0-dC.pres.0 ))
print(paste("C lost during RCP 8.5, ctrl: ", dC.2102.0-dC.pres.0 ))

print(paste("C lost during RCP 8.5, T: ", dC.2108.t-dC.pres.t ))
print(paste("C lost during RCP 2.6, T: ", dC.2102.t-dC.pres.t ))
print(paste("C lost during RCP 8.5, C: ", dC.2108.c-dC.pres.c ))
print(paste("C lost during RCP 8.5, C: ", dC.2102.c-dC.pres.c ))

print(paste("C lost due to climate on top of ctrl, RCP 8.5 : ", (dC.2108.t - dC.pres.t) - (dC.2108.0 - dC.pres.0) ))
print(paste("C lost due to CO2     on top of ctrl, RCP 8.5 : ", (dC.2108.c - dC.pres.c) - (dC.2108.0 - dC.pres.0) ))

print(paste("C lost due to climate on top of ctrl, RCP 2.6 : ", (dC.2102.t - dC.pres.t) - (dC.2102.0 - dC.pres.0) ))
print(paste("C lost due to CO2     on top of ctrl, RCP 2.6 : ", (dC.2102.c - dC.pres.c) - (dC.2102.0 - dC.pres.0) ))

print("****************************************")


## ///////////////////////////////////////////////////
## PLOTTING single gas for presentations
## ---------------------------------------------------


## ----------------------------------------------------------------
## N2O
## ----------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/eN2O.pdf', width=12, height=7 )

mar.left=c(1,5,1,1)
mar.right=c(1,1,1,5)

col.scen <- c( rgb(1,0,0,0.3),rgb(0,0,1,0.2) )

col.line=c(rgb(1,0,0),rgb(0,0,1))
col.line.fine=c(rgb(1,0,0,1),rgb(1,0,0,1),rgb(0,0,1,1),rgb(0,0,1,1))

col.rect.fine=c(rgb(1,0,0,0.5),rgb(1,1,1),rgb(0,0,1,0.5),rgb(1,1,1))
col.rect=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5))

## Panel layout
panel <- layout(
  matrix( c(1:2), 1, 2, byrow=TRUE ),
  widths=c(8,6),
  heights=c(6.35),
  TRUE
  )
#layout.show(panel)
#par(new=TRUE)

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- 6
ylim2 <- 18

## Plot lines
par( xaxs="i", yaxs="i", las=1, oma=c(0,0,0,0), mar=mar.left+c(0,0,1.5,0),
    cex.lab=2, cex.axis=1.5 )

plot( yrs.hist, e.n2o.hist[,1], type='l', xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
     axes=FALSE, xlab="year AD", ylab=expression(paste("TgNyr"^-1)),
     lwd=2
     )
rect( xlim1, ylim1, yrs.hist[length(yrs.hist)], ylim2, col=rgb(0,0,0,0.1), border=NA )

axis(1, lwd=2); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2);  axis(2, at=seq(ylim1,ylim2,by=0.5),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=0.5),labels=F,tck=-0.01)

for (i in seq(nscen)){
  for (k in seq(nmods[i])){
    lines( yrs.fut, e.n2o.fut[,i,1,k], col=col.scen[i], lwd=2 )
  }
  #lines( yrs.fut, spl.n2o.fut[,i], col=col.line, lwd=2 )
}
text( 1910, 16.5, expression(paste("eN"[2],"O")), cex=2.5, adj=c(0,0), font=2 )

## Plot rectangles
par(mar=mar.right+c(0,0,1.5,0), xpd=TRUE )

left <- 0:((nsims*nscen)-1)
left <- left+0.125
right <- 1:(nsims*nscen)
right <- right-0.125

bottom.n2o <- rep(NA,16)
top.n2o <- rep(NA,16)
middle.n2o <- rep(NA,16)

ilook <- c(1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2)
jlook <- c(1,2,1,2,3,4,3,4,5,6,5,6,7,8,7,8)

for (k in seq(nsims*nscen)){
  bottom.n2o[k] <- min.n2o[ilook[k],jlook[k]]
  top.n2o[k]    <- max.n2o[ilook[k],jlook[k]]
  middle.n2o[k] <- med.n2o[ilook[k],jlook[k]]
}

plot( 0:16, seq(ylim1,ylim2,by=(ylim2-ylim1)/16), type='n',
     xlab="", ylab="", axes=FALSE
     )
axis(2, lwd=2, labels=FALSE );  axis(2, at=seq(ylim1,ylim2,by=0.5), tck=-0.01, labels=FALSE )
axis(4, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=0.5),labels=F,tck=-0.01)
rect( c(0,8), c(ylim1,ylim1), c(4,12), c(ylim2+2,ylim2+2), col=rgb(0,0,0,0.1), border=NA )
box(lwd=2)
rect( left, bottom.n2o, right, top.n2o, col=col.rect.fine,
     border=c(NA,rgb(1,0,0,0.5),NA,rgb(0,0,1,0.5))
     )
rect( left, middle.n2o-0.1, right, middle.n2o+0.1,
     col=c(col.line.fine,col.line.fine,rep(c(rgb(1,0,0),"white",rgb(0,0,1),"white"),2)),
     border=c(rep(NA,8), rep(c(NA,rgb(1,0,0),NA,rgb(0,0,1)),2))
     )

text( 0.15, 18.25, "CT", cex=2, font=2, adj=c(0,0) )
text( 2.1, 18.65, "stan-", cex=1.1, adj=c(0,0))
text( 2.1, 18.25, "dard", cex=1.1, adj=c(0,0))

text( 4.45, 18.25, "T", cex=2, font=2, adj=c(0,0) )
text( 5.45, 18.65, "climate", cex=1.1, adj=c(0,0))
text( 5.45, 18.25, "only", cex=1.1, adj=c(0,0))

text( 8.65, 18.25, "C", cex=2, font=2, adj=c(0,0) )
text( 9.75, 18.55, expression(paste("cCO"[2])), cex=1.1, adj=c(0,0))
text( 9.75, 18.25, "only", cex=1.1, adj=c(0,0))

text(12.65, 18.25, "ctrl", cex=2, font=2, adj=c(0,0) )

dev.off()

## ----------------------------------------------------------------
## CH4
## ----------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/eCH4.pdf', width=12, height=7 )

mar.left=c(1,5,2,1)
mar.right=c(1,1,2,5)

col.scen <- c( rgb(1,0,0,0.3),rgb(0,0,1,0.2) )

col.line=c(rgb(1,0,0),rgb(0,0,1))
col.line.fine=c(rgb(1,0,0,1),rgb(1,0,0,1),rgb(0,0,1,1),rgb(0,0,1,1))

col.rect.fine=c(rgb(1,0,0,0.5),rgb(1,1,1),rgb(0,0,1,0.5),rgb(1,1,1))
col.rect=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5))

## Panel layout
panel <- layout(
  matrix( c(1:2), 1, 2, byrow=TRUE ),
  widths=c(8,6),
  heights=c(6.35),
  TRUE
  )

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- 6
ylim2 <- 18

## Plot lines
par( xaxs="i", yaxs="i", las=1, oma=c(0,0,0,0), mar=mar.left+c(0,0,0.5,0),
    cex.lab=1.5, cex.axis=1.5 )
ylim1 <- 180
ylim2 <- 360
par(mar=mar.left, xpd=FALSE )
plot( yrs.hist, e.ch4.hist[,1], type='l', xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
     axes=FALSE, xlab="year AD", ylab=expression(paste("TgCH"[4],"yr"^-1)),
     lwd=2
     )
rect( xlim1, ylim1, yrs.hist[length(yrs.hist)], ylim2, col=rgb(0,0,0,0.1), border=NA )
axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=-0.01)

for (i in seq(nscen)){
  for (k in seq(nmods[i])){
    lines( yrs.fut, e.ch4.fut[,i,1,k], col=col.scen[i], lwd=2 )
  }
}
text( 1910, 335, expression(paste("eCH"[4])), cex=2.5, adj=c(0,0), font=2 )

## Plot rectangles
par(mar=mar.right, xpd=TRUE )

left <- 0:((nsims*nscen/2)-1)
left <- left*2+0.25
right <- 1:(nsims*nscen/2)
right <- right*2-0.25

bottom.ch4 <- rep(NA,nsims*nscen/2)
top.ch4 <- rep(NA,nsims*nscen/2)
middle.ch4 <- rep(NA,nsims*nscen/2)

ilook <- c(1,2,1,2,1,2,1,2)
jlook <- c(1,1,3,3,5,5,7,7)

for (k in seq(nsims*nscen/2)){
  bottom.ch4[k] <- min.ch4[ilook[k],jlook[k]]
  top.ch4[k]    <- max.ch4[ilook[k],jlook[k]]
  middle.ch4[k] <- med.ch4[ilook[k],jlook[k]]
}

plot( 0:16, seq(ylim1,ylim2,by=(ylim2-ylim1)/16), type='n',
     xlab="", ylab="", axes=FALSE
     )
axis(2, lwd=2, labels=FALSE );  axis(2, at=seq(ylim1,ylim2,by=10), tck=-0.01, labels=FALSE )
axis(4, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=-0.01)
rect( c(0,8), c(ylim1,ylim1), c(4,12), c(ylim2+25,ylim2+25), col=rgb(0,0,0,0.1), border=NA )
box(lwd=2)

rect( left, bottom.ch4, right, top.ch4, col=col.rect, border=NA )
rect( left, middle.ch4-1.5, right, middle.ch4+1.5, col=col.line, border=NA ) 

text( 0.15, 363, "CT", cex=2, font=2, adj=c(0,0) )
text( 2.1, 368, "stan-", cex=1.1, adj=c(0,0))
text( 2.1, 363, "dard", cex=1.1, adj=c(0,0))

text( 4.45, 363, "T", cex=2, font=2, adj=c(0,0) )
text( 5.45, 368, "climate", cex=1.1, adj=c(0,0))
text( 5.45, 363, "only", cex=1.1, adj=c(0,0))

text( 8.65, 363, "C", cex=2, font=2, adj=c(0,0) )
text( 9.75, 368, expression(paste("cCO"[2])), cex=1.1, adj=c(0,0))
text( 9.75, 363, "only", cex=1.1, adj=c(0,0))

text(12.65, 363, "ctrl", cex=2, font=2, adj=c(0,0) )

dev.off()

preind    <- mean(e.ch4.hist[1:31,1])
relchange <- (middle.ch4/preind-1)*100
char.rel  <- as.character( format( relchange, digits=3 ) )

out <- file("eCH4_table_BStocker2013.out","w")
cat("Source: Stocker et al., 2013, Nature Climate Change","\n",file=out)
cat("Multiple greenhouse gas feedbacks from the land biosphere under future climate change scenarios","\n",file=out)
cat("Values corresponding to means in Fig. 2, Stocker et al, 2013","\n",file=out)
cat("------------------------------------------------------------","\n",file=out)
cat("Delta(eCH4), [% change in 2100 AD rel. to PI]","\n",file=out)
cat("","\n",file=out)
cat("           RCP 2.6  RCP 8.5","\n",file=out)
cat("CT       ",char.rel[2],char.rel[1],"\n",file=out,sep="    ")
cat("C        ",char.rel[6],char.rel[5],"\n",file=out,sep="    ")
cat("T        ",char.rel[4],char.rel[3],"\n",file=out,sep="    ")
close(out)


## ----------------------------------------------------------------
## dC
## ----------------------------------------------------------------
pdf( '/alphadata01/bstocker/multiGHG_analysis/dC.pdf', width=12, height=7 )

mar.left=c(1,5,2,1)
mar.right=c(1,1,2,5)

col.scen <- c( rgb(1,0,0,0.3),rgb(0,0,1,0.2) )

col.line=c(rgb(1,0,0),rgb(0,0,1))
col.line.fine=c(rgb(1,0,0,1),rgb(1,0,0,1),rgb(0,0,1,1),rgb(0,0,1,1))

col.rect.fine=c(rgb(1,0,0,0.5),rgb(1,1,1),rgb(0,0,1,0.5),rgb(1,1,1))
col.rect=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5))

## Panel layout
panel <- layout(
  matrix( c(1:2), 1, 2, byrow=TRUE ),
  widths=c(8,6),
  heights=c(6.35),
  TRUE
  )

xlim1 <- 1900
xlim2 <- 2100
ylim1 <- -120
ylim2 <- 410

## Plot lines
par( xaxs="i", yaxs="i", las=1, oma=c(0,0,0,0), mar=mar.left+c(0,0,0.5,0),
    cex.lab=1.5, cex.axis=1.5 )
par(mar=mar.left, xpd=FALSE )

plot( yrs.hist, dC.hist[,1], type='l', xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
     axes=FALSE, xlab="year AD", ylab="GtC",
     lwd=2
     )
rect( xlim1, ylim1, yrs.hist[length(yrs.hist)], ylim2, col=rgb(0,0,0,0.1), border=NA )
axis(1, lwd=2 ); axis(1, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(2, lwd=2, labels=c("100","0","-100","-200","-300","-400"), at=c(-100,0,100,200,300,400) );  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(2, at=seq(ylim1,ylim2,by=20),labels=F,tck=-0.01)
axis(3, labels=F, lwd=2); axis(3, at=seq(xlim1,xlim2,by=10),labels=F,tck=-0.01 )
axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(4, at=seq(ylim1,ylim2,by=20),labels=F,tck=-0.01)

for (i in seq(nscen)){
  for (k in seq(nmods[i])){
    lines( yrs.fut, dC.fut[,i,1,k], col=col.scen[i], lwd=2 )
  }
}
text( 1910, 350, expression(paste(Delta,"C")), cex=2.5, adj=c(0,0), font=2 )
text( 1910, 320, "relative to 1765 AD", cex=2, adj=c(0,0), font=1 )

segments( 1900, 0, 1915, 0, lty=5 )
arrows( rep(1910,2), rep(0,2), rep(1910,2), c(-60,60), length=0.1 )
text( rep(1910,2), c(-77,65), c("sink","source"), cex=1.5, adj=c(0,0) )
legend( "bottomright", c("RCP 2.6", "RCP 8.5"), lty=c(1,1), bty="n", lwd=5, col=c("blue","red"), cex=2 )

## Plot rectangles
par(mar=mar.right, xpd=TRUE )

left <- 0:((nsims*nscen/2)-1)
left <- left*2+0.25
right <- 1:(nsims*nscen/2)
right <- right*2-0.25

bottom.dC <- rep(NA,nsims*nscen/2)
top.dC <- rep(NA,nsims*nscen/2)
middle.dC <- rep(NA,nsims*nscen/2)

for (k in seq(nsims*nscen)){
  bottom.dC[k] <- min.dC[ilook[k],jlook[k]]
  top.dC[k]    <- max.dC[ilook[k],jlook[k]]
  middle.dC[k] <- med.dC[ilook[k],jlook[k]]
}

plot( 0:16, seq(ylim1,ylim2,by=(ylim2-ylim1)/16), type='n',
     xlab="", ylab="", axes=FALSE
     )
axis(2, labels=F, lwd=2 );  axis(2, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(2, at=seq(ylim1,ylim2,by=20),labels=F,tck=-0.01)
axis(4, lwd=2, cex.axis=1.5, labels=c("100","0","-100","-200","-300","-400"), at=c(-100,0,100,200,300,400));  axis(4, at=seq(ylim1,ylim2,by=10),labels=F,tck=0,lwd=2);  axis(4, at=seq(ylim1,ylim2,by=20),labels=F,tck=-0.01)

rect( c(0,8), c(ylim1,ylim1), c(4,12), c(ylim2+60,ylim2+60), col=rgb(0,0,0,0.1), border=NA )
box(lwd=2)
rect( left, bottom.dC, right, top.dC, col=col.rect, border=NA )
rect( left, middle.dC-5, right, middle.dC+5, col=col.line, border=NA ) 

text( 0.15, 420, "CT", cex=2, font=2, adj=c(0,0) )
text( 2.1, 440, "stan-", cex=1.1, adj=c(0,0))
text( 2.1, 420, "dard", cex=1.1, adj=c(0,0))

text( 4.45, 420, "T", cex=2, font=2, adj=c(0,0) )
text( 5.45, 440, "climate", cex=1.1, adj=c(0,0))
text( 5.45, 420, "only", cex=1.1, adj=c(0,0))

text( 8.65, 420, "C", cex=2, font=2, adj=c(0,0) )
text( 9.75, 435, expression(paste("cCO"[2])), cex=1.1, adj=c(0,0))
text( 9.75, 420, "only", cex=1.1, adj=c(0,0))

text(12.65, 420, "ctrl", cex=2, font=2, adj=c(0,0) )

dev.off()
