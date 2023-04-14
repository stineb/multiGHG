##########################################################################
## N2O concentration
##------------------------------------------------------------------------

calc_cN2O <- function(eN2O, e2000){
  
  ## Parameters
  spinup <- 1200
  g_per_ppb <- 4.8
  
  ## Spinup
  em_mean <- mean(eN2O[1:31])
  conc = eN2O*0
  for (i in seq(spinup)){
    conc[1] <- conc[1] + em_mean - conc[1]/tau_n2o(em_mean, e2000)
    }
  
  ## Transient
  for (i in 2:length(eN2O)){
    conc[i] <- conc[i-1] + eN2O[i-1] - conc[i-1]/tau_n2o(eN2O[i-1], e2000)
    }
  conc <- conc/g_per_ppb
  return(conc)
}

tau_n2o <- function(e_t, e2000){
  out = 120*(e_t/e2000)^(-0.055)
}

##########################################################################
## CH4 concentration
##------------------------------------------------------------------------

## PRE-INDUSTRIAL (no data for NOx, CO, and VOC)
calc.cCH4.preind <- function( eCH4_anth, eCH4_land, eCH4_xtra ){
  
  ## Parameters (data for year 2000 AD from RCP data base)
  spinup = 120
  Tg_per_ppb = 2.78
  
  cCH4_2000 = 1751.02
  eNOx_2000 = 38.16
  eCO_2000 = 1068.00
  eVOC_2000 = 210.62

  cCH4_1765 = 450.0
  eNOx_1765 = 0.0
   eCO_1765 = 0.0
  eVOC_1765 = 0.0
  ## cCH4_1765 = cCH4_2000
  ## eNOx_1765 = eNOx_2000
  ##  eCO_1765 = eCO_2000
  ## eVOC_1765 = eVOC_2000
 
  
  ## Spinup
  conc = eCH4_anth*0
  for (i in 1:spinup){
    rOH = exp(
              - 0.32  * log( cCH4_1765 / cCH4_2000 )
              + 0.0042   * ( eNOx_1765  - eNOx_2000 )
              - 0.000105 * (  eCO_1765  -  eCO_2000 )
              - 0.000315 * ( eVOC_1765  - eVOC_2000 )
              )
    tau = 1/(rOH/9.58+1/68.2)
    #print(tau)
    conc[1] = conc[1] + eCH4_anth[1] + mean(eCH4_land[1:31]) + eCH4_xtra[1] - conc[1]/tau
  }
  
 
  ## Transient
  for (i in 2:length(eCH4_anth)){
    rOH = exp(
          - 0.32  * log( cCH4_1765 / cCH4_2000 )
          + 0.0042   * ( eNOx_1765  - eNOx_2000 )
          - 0.000105 * (  eCO_1765  -  eCO_2000 )
          - 0.000315 * ( eVOC_1765  - eVOC_2000 )
          ) 
    tau = 1/(rOH/9.58+1/68.2)
    conc[i] = conc[i-1] + eCH4_anth[i] + eCH4_land[i] + eCH4_xtra[i] - conc[i-1]/tau

    #print(paste("tau ",tau))
    
  }

  conc = conc/Tg_per_ppb
  return(conc)

}

## INDUSTRIAL (data available for NOx, CO, and VOC)
calc.cCH4 <- function( e.ch4.land, e.ch4.ext, e.ch4.xtra, c.ch4.data, e.nox, e.co, e.voc, timevec ){

  #print(head(e.ch4.land))
  
  ## Parameters (data for year 2000 AD from RCP data base)
  spinup = 120
  Tg_per_ppb = 2.78
  
  ## Spinup
  conc = e.ch4.ext*0

  for (i in 1:spinup){
    rOH = exp(
      - 0.32  * log( mean( c.ch4.data[1:31]) / c.ch4.data[timevec==2000.5] )
      + 0.0042   * ( mean( e.nox$emissions[1:31]) - e.nox$emissions[e.nox$year==2000] )
      - 0.000105 * ( mean( e.co$emissions[1:31])  - e.co$emissions[e.co$year==2000] )
      - 0.000315 * ( mean( e.voc$emissions[1:31]) - e.voc$emissions[e.voc$year==2000] )
      )
    tau = 1/(rOH/9.58+1/68.2)
    conc[1] = conc[1] + e.ch4.ext[1] + mean(e.ch4.land[1:31]) + e.ch4.xtra[1] - conc[1]/tau
  }
  
  
  ## Transient
  for (i in 2:length(timevec)){
    rOH = exp(
      - 0.32  * log( c.ch4.data[i] / c.ch4.data[timevec==2000.5] )
      + 0.0042   * ( e.nox$emissions[i] - e.nox$emissions[e.nox$year==2000] )
      - 0.000105 * (  e.co$emissions[i] - e.co$emissions[e.co$year==2000] )
      - 0.000315 * ( e.voc$emissions[i] - e.voc$emissions[e.voc$year==2000] )
      ) 
    tau = 1/(rOH/9.58+1/68.2)
    #print(tau)
    conc[i] = conc[i-1] + e.ch4.ext[i] + e.ch4.land[i] + e.ch4.xtra[i] - conc[i-1]/tau

    print(paste("tau in year ",timevec[i],tau))
    
  }
  
  conc = conc/Tg_per_ppb
  return(conc)
  
}


##########################################################################
## DIAGNOSE N2O EMISSIONS
##------------------------------------------------------------------------

diag_n2o <- function( em, conc, e2000 ){
  
  g_per_ppb <- 4.8
  
  diag <- em*0
  concmass <- conc*g_per_ppb
  
  for (i in 2:(length(conc)-1)){
    diag[i] <- concmass[i+1] - concmass[i] - em[i] + concmass[i] / tau_n2o( diag[i-1]+em[i-1], e2000 )
  }
 
  return(diag)
  
}

##########################################################################
## DIAGNOSE CH4 EMISSIONS
##------------------------------------------------------------------------

diag.ch4 <- function( eCH4_land, eCH4_anth, conc, eNOx, eCO, eVOC, timevec ) {

  Tg_per_ppb = 2.78
  
  diag = eCH4_anth*0
  concmass = conc*Tg_per_ppb

  for (i in 2:length(conc)-1){
    rOH = exp(
        - 0.32*log(conc[i] / conc[timevec==2000.5])
        + 0.0042 * (eNOx$emissions[i]- eNOx$emissions[eNOx$year==2000])
        - 0.000105 * (eCO$emissions[i] - eCO$emissions[eCO$year==2000])
        - 0.000315 * (eVOC$emissions[i] - eVOC$emissions[eVOC$year==2000])
        )
    tau = 1/(rOH/9.58+1/68.2)
    diag[i] = concmass[i+1] - concmass[i]
              - eCH4_anth[i] - eCH4_land[i]
              + concmass[i]/tau
  }

  return(diag)
  
}
