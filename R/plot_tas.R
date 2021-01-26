ilenHis <- 241
ilenFut <- 95

## Read run names from file
## ----------------------------------------------------------
runnam <- read.table(
                     'runnames.txt',
                     head=FALSE,
                     sep = "\n",
                     as.is=TRUE
                     )
runnam <- unlist(runnam)
runnam <- runnam[2:length(runnam)]
nsets <- length(runnam)

## Define colors
## ----------------------------------------------------------
## colors_modl <- t(
##                  array(
##                        c(150,0,0,255,243,0,255,102,51,255,0,204,204,0,38,51,51,255,153,0,255,0,51,204,51,255,255,255,51,255),
##                        c(3,10)
##                        )
##                  )/255
colors_modl <- c("red","chocolate1","darkorange","firebrick1","brown1","blue","royalblue","steelblue1","deepskyblue3","dodgerblue1")
#colors_modl <- c("brown1","brown1","brown1","brown1","brown1","brown1","brown1","brown1","brown1","brown1")
modl <- c(1,1,1,2,2,2,3,3,3,3,4,5,5,5,5,5,6,6,6,6,7,7,7,8,8,8,9,10,10,10,10,10)

## Read temperature data
## ----------------------------------------------------------
tasHis <- read.table('tas_run1.0_historical.dat')
tasRcp <- array(NA,c(ilenFut,nsets))
for (i in 1:nsets){
  tmp <- read.table(paste('tas_',runnam[i],'.dat',sep=""))
  tmp2 <- tmp$V2
  ltmp <- length(tmp2)
  tasRcp[1:ltmp,i] <- tmp2
}
tasRcp <- cbind(tmp$V1,tasRcp)

tasMean <- mean(tasHis$V2[196:226])
tasHis[,2] <- tasHis[,2]-tasMean
#tasRcp[tasRcp==-1e34] <- NA
#tasRcp[,2:(nsets+1)] <- tasRcp[,2:(nsets+1)]-tasMean

## Plot the data
## ----------------------------------------------------------
plot(
     tasHis$V1,
     tasHis$V2,
     type="l",
     xlim=c(2098,2100),
     ylim=c(-1,7),
     xlab="year AD",
     ylab=~degree~C
     )
for (i in 2:nsets){
  par(new=TRUE)
  plot(
       tasRcp[,1],
       tasRcp[,i],
       type="l",
       col=colors_modl[modl[i-1]],
       xlim=c(2098,2100),
       ylim=c(-1,7),
       xlab="",
       ylab="",
       xaxt="n",
       yaxt="n"
       )
}
