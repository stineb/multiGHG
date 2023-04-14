## ////////////////////////////////////////////////////////////////////////////////////////////////
## This is the pendant to the matlab routine with the same name
## Calculates concentrations resulting from different setups
## beni@climate.unibe.ch
## ------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)

## Chose whether to diagnose flux or use explicit ocean source
diag <- TRUE

source(paste0(here::here(), '/R/calcConc.R'))
source(paste0(here::here(), '/R/myspline.R'))

## ////////////////////////////////////////////////////////////////////////////////////////////////
## READ FILES
## ------------------------------------------------------------------------------------------------

## Read list of file names
names <- read.table(paste0(here::here(), '/data/runnames_historical.txt'), header=F )$V1
nsims <- length(names)

## Read N2O concentration data to diagnose flux
df_c_n2o <- read.table(paste0(here::here(), '/data/cN2O_etheridge.dat'), header=TRUE) |> 
  as_tibble()

df_c_n2o |> 
  ggplot(aes(N2OgasAge, N2O.ppb.)) +
  geom_point()

## Read N2O emissions ANTH (=fossil+fire+manure) from Zaehle
# df_e_n2o_ext <- read.table('/alphadata01/bstocker/input_data/ghg_data/n2o_data/eN2Oext_bysources_rcp26_HARMONIZED.dat', head=TRUE )  # on KUP server

# recovered for Yunke
df_e_n2o_ext <- read.table(paste0(here::here(), "/data/eN2O_anth_zaehle_rcp85_harm.dat"), col.names=c("year","ext")) |> 
  as_tibble()

## Read oceanic N2O source
# df_e_n2o_oc <- read.table('/alphadata01/bstocker/multiGHG_analysis/eN2O_oc_historical.dat', col.names=c("year","emission"))  # on KUP server

# recovered for Yunke
df_e_n2o_oc <- read.table(paste0(here::here(), '/data/eN2O_oc_historical.dat'), col.names=c("year","oc")) |> 
  as_tibble()

# tmp <- data.frame( year=1765:(df_e_n2o_oc$year[1]-1), emission=rep(df_e_n2o_oc$emission[1],length(1765:(df_e_n2o_oc$year[1]-1))) )
# df_e_n2o_oc <- rbind( tmp, df_e_n2o_oc)

## Read N2O emissions from LPX simulation
# # tmp <- read.table('/alphadata01/bstocker/output_multiGHG/eN2O_r1_historical.dat', header=F)   # on KUP server
# lhist <- length(tmp[,1])
# yrs.hist <- tmp[,1]
# e.n2o <- array( NA, dim=c(lhist,nsims) )
# for (i in seq(nsims)){
#   filn <- paste( here::here(), '/data/eN2O_',names[i],'.dat', sep="" )
#   e.n2o[,i] <- read.table( filn, header=F )$V2
# }

# read from just single simulation
df_e_n2o_land <- read.table(paste0(here::here(), '/data/eN2O_r1_historical.dat'), col.names=c("year","land")) |> 
  as_tibble() |> 
  mutate(year = floor(year))

# combine data
df_e_n2o <- df_e_n2o_ext |> 
  left_join(
    df_e_n2o_oc,
    by = "year"
  ) |> 
  left_join(
    df_e_n2o_land,
    by = "year"
  ) |> 
  drop_na()

# scale down total emissions to match observations
scale <- 0.955
df_e_n2o <- df_e_n2o |> 
  mutate(oc = scale * oc,
         land = scale * land,
         ext = scale * ext) |> 
  mutate(
    tot = ext + oc + land
  )

## ////////////////////////////////////////////////////////////////////////////////////////////////
## CALCULATE CONCENTRATIONS
## ------------------------------------------------------------------------------------------------
## Define emissions (in TgN/yr) for year 2000
e2000 <- df_e_n2o |> 
  filter(year %in% 1996:2005) |> 
  summarise(tot = mean(tot)) |> 
  pull(tot)

## create data, assume constant emissions after year 2005 until 3000, average over 1996-2005
df_n2o_syn <- df_e_n2o |> 
  select(year, tot) |> 
  bind_rows(
    purrr::map_dfr(
      as.list(2006:3000),
      ~tibble(year = ., tot = e2000)
    )
  )

## calculate concentrations
df_n2o_syn <- df_n2o_syn |> 
  mutate(concentration = calc_cN2O(tot, e2000)) |> 
  rename(emissions = tot)


## ////////////////////////////////////////////////////////////////////////////////////////////////
## PLOT historical concentrations
## ------------------------------------------------------------------------------------------------
## time series
ggplot() + 
  geom_line(aes(year, concentration), data = df_n2o_syn |> filter(year > 1900 & year < 2006), color = "red") +
  geom_point(aes(N2OgasAge, N2O.ppb., shape = SampleType), data = df_c_n2o |> filter(N2OgasAge > 1900), color = "grey50") +
  theme_classic()

## time series
ggplot() + 
  geom_line(aes(year, concentration), data = df_n2o_syn |> filter(year > 1900), color = "red") +
  geom_point(aes(N2OgasAge, N2O.ppb., shape = SampleType), data = df_c_n2o |> filter(N2OgasAge > 1900), color = "grey50") +
  theme_classic()


## ////////////////////////////////////////////////////////////////////////////////////////////////
## Enhanced emissions -> increased concentrations
## ------------------------------------------------------------------------------------------------
RR <- 1.2  # response ratio

## Define emissions (in TgN/yr) for year 2000
df_2000 <- df_e_n2o |> 
  filter(year %in% 1996:2005) |> 
  summarise(across(where(is.numeric), mean))

## create data, assume constant emissions after year 2005 until 3000, average over 1996-2005
df_n2o_syn <- df_e_n2o |> 
  mutate(land_elevated = land) |> 
  select(-tot) |> 
  bind_rows(
    purrr::map_dfr(
      as.list(2006:3000),
      ~tibble(year = ., ext = df_2000$ext[1], oc = df_2000$oc[1], land = df_2000$land[1], land_elevated = df_2000$land[1] * RR)
    )
  ) |> 
  mutate(tot = ext + oc + land,
         tot_elevated = ext + oc + land_elevated)

## calculate concentrations
df_n2o_syn <- df_n2o_syn |> 
  mutate(concentration = calc_cN2O(tot, df_2000$tot[1]),
         concentration_elevated = calc_cN2O(tot_elevated, df_2000$tot[1])) |> 
  rename(emissions = tot,
         emissions_elevated = tot_elevated)

## time series
ggplot() + 
  geom_line(aes(year, concentration), data = df_n2o_syn |> filter(year > 1900), color = "red") +
  geom_line(aes(year, concentration_elevated), data = df_n2o_syn |> filter(year > 1900), color = "red", linetype = "dashed") +
  geom_point(aes(N2OgasAge, N2O.ppb., shape = SampleType), data = df_c_n2o |> filter(N2OgasAge > 1900), color = "grey50") +
  theme_classic() +
  ylab("N2O concentration (ppb)")

## save to file
ggsave(paste0(here::here(), "/fig/n2o_concentrations_perturbed.pdf"), width = 8, height = 5)


# ## ////////////////////////////////////////////////////////////////////////////////////////////////
# ## DIAGNOSE EMISSIONS
# ## ------------------------------------------------------------------------------------------------
# 
# ## Call concentration function with full vector for each sim
# e.n2o.diag <- array( NA, dim=c(lhist,nsims) )
# e.n2o.diag.spl <- array( NA, dim=c(lhist,nsims) )
# for (i in seq(nsims)){
#   e.n2o.diag[,i] <- diag_n2o( e.n2o[,i]
#                           + df_e_n2o_ext$total[df_e_n2o_ext$year > 1764 & df_e_n2o_ext$year < 2007],
#                           df_c_n2o$concentration[df_c_n2o$year > 1764 & df_c_n2o$year < 2007],
#                           e2000
#                           )
# }
# 
# 
# ## ////////////////////////////////////////////////////////////////////////////////////////////////
# ## PLOT
# ## ------------------------------------------------------------------------------------------------
# cols=c("black","red","green","blue")
# xlim <- c(1800,2005)
# 
# if (diag){
# 
#   e.n2o.diag.spl <- myspline( yrs.hist[2:241], e.n2o.diag[2:241,1], 30 )
# 
#   ylim <- c(2,6)
#   for (i in c(1,2,3,5)){
#     plot( yrs.hist, e.n2o.diag[,i], 
#           type="l", col=cols[min(i,length(cols))], 
#           xlim=c(1765,2005), #ylim=c(260,270),
#           axes=F,
#           #xlim=c(1765,2005), ylim=c(260,310) 
#           )
#     par(new=T)
#   }
#   axis(1); axis(2); axis(3); axis(4)
# 
#   pdf( '/fig/n2o_diag.pdf', width=7, height=6 )
#   par(new=F,xaxs="i",yaxs="i",las=1)
#   
#   plot(yrs.hist, e.n2o.diag[,1],
#        type="l", axes=FALSE, lwd=1,
#        xlim=xlim, ylim=ylim,
#        xlab="year AD", ylab=expression(paste("TgNyr"^-1))
#        )
#   lines(yrs.hist[2:241], e.n2o.diag.spl, lwd=2, col="red" )
#   
#   axis(1, lwd=2 ); axis(1, at=seq(xlim[1],xlim[2],by=10),labels=F,tck=-0.01 )
#   axis(2, lwd=2 );  axis(2, at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
#   axis(3, labels=F, lwd=2); axis(3, at=seq(xlim[1],xlim[2],by=10),labels=F,tck=-0.01 )
#   axis(4, labels=F, lwd=2);  axis(4, at=seq(ylim[1],ylim[2],by=0.2),labels=F,tck=-0.01)
#   box(lwd=2)
# 
#   text( 1810, 5.3, expression(paste("eN"[2],"O"^"diag")), cex=2, adj=c(0,0), font=2 )
#   
#   dev.off()
#   
#   print(      "----------------------------------------------")
#   print(paste("MEAN MISSING FLUX FOR r1_historical, 1870:1900"))
#   print(mean(e.n2o.diag[,1][yrs.hist>=1870 & yrs.hist<=1901]))
#   print(      "----------------------------------------------")
#   
# } else {
#   
#   for (i in c(1,2,3,5)){
#     plot( yrs.hist, c.n2o[,i], 
#           type="l", col=cols[min(i,length(cols))], 
#           #xlim=c(1765,1910), ylim=c(260,310),
#           axes=F,
#           xlim=c(1765,2005), ylim=c(280,325) 
#           )
#     par(new=T)
#   }
#   axis(1); axis(2); axis(3); axis(4)
#   box()
# #   par(new=T)
# #   plot( df_c_n2o,
# #         lty=2, 
# #         xlim=c(1765,1910), ylim=c(260,270)
# #         )
# }
# 
# 
# ## ////////////////////////////////////////////////////////////////////////////////////////////////
# ## WRITE TO FILE
# ## ------------------------------------------------------------------------------------------------
# out <- data.frame( c.n2o )
# colnames(out) <- names
# year <- data.frame( year=yrs.hist )
# out <- cbind(year,out)
# write.csv( out, file=paste0(here::here(), "/data/cN2O_hist.dat"), row.names=F, quote=FALSE )
# 
# out <- data.frame( e.n2o.diag )
# colnames(out) <- names
# year <- data.frame( year=yrs.hist )
# out <- cbind(year,out)
# write.csv( out, file=paste0(here::here(), "/data/eN2O_diag.dat"), row.names=F, quote=FALSE )
# 
# 
