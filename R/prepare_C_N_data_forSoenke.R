source('/alphadata01/bstocker/multiGHG_analysis/myspline.R')


## Define setups
sims <- c("r1","r2","r3","r4","r5","r6","r7","r8")
nsims <- length(sims)


############################################################
## Historical
##---------------------------------------------------------- 
names.hist <- read.table('/alphadata01/bstocker/multiGHG_analysis/runnames_historical.txt', header=F )$V1

## Read data
historical <- list()

for (i in seq(nsims)){

  filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_historical.vegc.out', sep="" )
  vegc <- read.table( filn, header=F )$V2
  year <- read.table( filn, header=F )$V1
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_historical.vegn.out', sep="" )
  vegn <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_historical.soilc.out', sep="" )
  soilc <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_historical.soiln.out', sep="" )
  soiln <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_historical.litterc_ag.out', sep="" )
  aglitterc <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_historical.litterc_bg.out', sep="" )
  bglitterc <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_historical.nloss.out', sep="" )
  nloss <- read.table( filn, header=F )$V2

  filn <- paste( '/alphadata01/bstocker/output_multiGHG/eN2O_',sims[i],'_historical.dat', sep="" )
  en2o <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/eCH4_',sims[i],'_historical.dat', sep="" )
  ech4 <- read.table( filn, header=F )$V2
  filn <- paste( '/alphadata01/bstocker/output_multiGHG/dCtot_',sims[i],'_historical.dat', sep="" )
  tmp1 <- read.table( filn, header=F )$V2
  dctot <- -(tmp1-tmp1[1])*1e-15

  eval(parse(text=paste(
               sims[i]," <- data.frame(
                                year=year
                                ,vegc=vegc
                                ,vegn=vegn
                                ,soilc=soilc
                                ,soiln=soiln
                                ,aglitterc=aglitterc
                                ,bglitterc=bglitterc
                                ,nloss=nloss
                                ,en2o=en2o
                                ,ech4=ech4
                                ,dctot=dctot
                                )"
               , sep="")
             )
       )

  ## attach data frame for this simulation to list
  eval(parse(
             text=paste("historical$",sims[i]," <- ",sims[i], sep="" )
             )
       )
}
save( historical, file="multiGHG_historical_timeseries_global.Rdata" )

############################################################
## RCP2.6
##---------------------------------------------------------- 
## Read list of file names
names.rcp26 <- read.table('/alphadata01/bstocker/multiGHG_analysis/runnames_rcp26.txt', header=F )$V1
shortnames.rcp26 <- read.table('/alphadata01/bstocker/multiGHG_analysis/shortnames_rcp26.txt', header=F )$V1
nmodls <- length(names.rcp26)

## Read data
rcp26 <- list()

for (i in seq(nsims)){

  ## create list for this simulation containing each model
  eval(parse(
             text=paste(sims[i]," <- list()")
             ))
  
  for (j in seq(nmodls)){
  
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp26_',names.rcp26[j],'.vegc.out', sep="" )
    vegc <- read.table( filn, header=F )$V2
    year <- read.table( filn, header=F )$V1
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp26_',names.rcp26[j],'.vegn.out', sep="" )
    vegn <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp26_',names.rcp26[j],'.soilc.out', sep="" )
    soilc <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp26_',names.rcp26[j],'.soiln.out', sep="" )
    soiln <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp26_',names.rcp26[j],'.litterc_ag.out', sep="" )
    aglitterc <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp26_',names.rcp26[j],'.litterc_bg.out', sep="" )
    bglitterc <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp26_',names.rcp26[j],'.nloss.out', sep="" )
    nloss <- read.table( filn, header=F )$V2
    
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/eN2O_',sims[i],'_rcp26_',names.rcp26[j],'.dat', sep="" )
    en2o <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/eCH4_',sims[i],'_rcp26_',names.rcp26[j],'.dat', sep="" )
    ech4 <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/dCtot_',sims[i],'_rcp26_',names.rcp26[j],'.dat', sep="" )
    tmp1 <- read.table( filn, header=F )$V2
    dctot <- -(tmp1-tmp1[1])*1e-15

    ## create data frame for this model
    eval(parse(text=paste(
                 shortnames.rcp26[j]," <- data.frame(
                                year=year
                                ,vegc=vegc
                                ,vegn=vegn
                                ,soilc=soilc
                                ,soiln=soiln
                                ,aglitterc=aglitterc
                                ,bglitterc=bglitterc
                                ,nloss=nloss
                                ,en2o=en2o
                                ,ech4=ech4
                                ,dctot=dctot
                                )"
                 , sep="")
               )
         )

    ## attach data frame for this simulation to list
    eval(parse(
               text=paste( sims[i],"$",shortnames.rcp26[j]," <- ", shortnames.rcp26[j], sep="" )
               )
         )
    
  }

  ## attach list of data frames for each simulation to list 'rcp26'
  eval(parse(
             text=paste( "rcp26$",sims[i]," <- ",sims[i], sep="" )
             )
       )
  
}
save( rcp26, file="multiGHG_rcp26_timeseries_global.Rdata" )

############################################################
## RCP8.5
##---------------------------------------------------------- 
## Read list of file names
names.rcp85 <- read.table('/alphadata01/bstocker/multiGHG_analysis/runnames_rcp85.txt', header=F )$V1
shortnames.rcp85 <- read.table('/alphadata01/bstocker/multiGHG_analysis/shortnames_rcp85.txt', header=F )$V1
nmodls <- length(names.rcp85)

## Read data
rcp85 <- list()

for (i in seq(nsims)){

  ## create list for this simulation containing each model
  eval(parse(
             text=paste(sims[i]," <- list()")
             ))
  
  for (j in seq(nmodls)){
  
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp85_',names.rcp85[j],'.vegc.out', sep="" )
    vegc <- read.table( filn, header=F )$V2
    year <- read.table( filn, header=F )$V1
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp85_',names.rcp85[j],'.vegn.out', sep="" )
    vegn <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp85_',names.rcp85[j],'.soilc.out', sep="" )
    soilc <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp85_',names.rcp85[j],'.soiln.out', sep="" )
    soiln <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp85_',names.rcp85[j],'.litterc_ag.out', sep="" )
    aglitterc <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp85_',names.rcp85[j],'.litterc_bg.out', sep="" )
    bglitterc <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/trans_',sims[i],'_rcp85_',names.rcp85[j],'.nloss.out', sep="" )
    nloss <- read.table( filn, header=F )$V2
    
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/eN2O_',sims[i],'_rcp85_',names.rcp85[j],'.dat', sep="" )
    en2o <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/eCH4_',sims[i],'_rcp85_',names.rcp85[j],'.dat', sep="" )
    ech4 <- read.table( filn, header=F )$V2
    filn <- paste( '/alphadata01/bstocker/output_multiGHG/dCtot_',sims[i],'_rcp85_',names.rcp85[j],'.dat', sep="" )
    tmp1 <- read.table( filn, header=F )$V2
    dctot <- -(tmp1-tmp1[1])*1e-15

    ## create data frame for this model
    eval(parse(text=paste(
                 shortnames.rcp85[j]," <- data.frame(
                                year=year
                                ,vegc=vegc
                                ,vegn=vegn
                                ,soilc=soilc
                                ,soiln=soiln
                                ,aglitterc=aglitterc
                                ,bglitterc=bglitterc
                                ,nloss=nloss
                                ,en2o=en2o
                                ,ech4=ech4
                                ,dctot=dctot
                                )"
                 , sep="")
               )
         )

    ## attach data frame for this simulation to list
    eval(parse(
               text=paste( sims[i],"$",shortnames.rcp85[j]," <- ", shortnames.rcp85[j], sep="" )
               )
         )
    
  }

  ## attach list of data frames for each simulation to list 'rcp85'
  eval(parse(
             text=paste( "rcp85$",sims[i]," <- ",sims[i], sep="" )
             )
       )
  
}
save( rcp85, file="multiGHG_rcp85_timeseries_global.Rdata" )

