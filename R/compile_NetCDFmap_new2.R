"create" <-
##  function(name,lonhelp,lathelp){
  function(DUMMY){
    library(RNetCDF)

    ## Argument of this function is obtained from a bash function
    ## wich loops over output of 'ls *.lu'

    firstyear <- 2004
    lastyear <- 2004

    ## Open land mask (gicew.cdf)
    maskfil <- open.nc("/alphadata01/bstocker/RCP_data/gicew.cdf")

    ## Get lon/lat dimensions from land mask file
    lon <- var.get.nc(maskfil,"LONGITUDE")
    lat <- var.get.nc(maskfil,"LATITUDE")
    gicew <- var.get.nc(maskfil,"GICEW")
    
    close.nc(maskfil)
    
    nlon <- length(lon)
    nlat <- length(lat)
    
    ## Loop over years
    for (n in firstyear:lastyear){

      ## Open annual file (contains all transitions for all grid cells)
      filecontent <- read.table(paste(n,".lu",sep=""), header=TRUE)
      allvarnames <- names(filecontent)
      ncats <- length(filecontent[1,])  
      ntrans <- ncats-3 # The first three categories are lon, lat and year
      
      annualmatrix <- array(NA,c(nlat,nlon,ntrans))

      for (k in seq(length(filecontent[,1]))){
        lonhelp <- filecontent$lon[k]
        lathelp <- filecontent$lat[k]
        i <- (lonhelp-0.25)*2+1
        j <- (lathelp-0.25)*2+1
        print(paste("i,j: ",i,j))
        for (m in seq(ntrans)){
          annualmatrix[j,i,m] <- filecontent[k,m+3]
        }
      }
      
    ## Write NetCDF file for year i with a seperate variable for each transition

    ## Create a new NetCDF dataset and define 3 dimensions
      name <- paste("LUH_",n,"_onlytrans.cdf",sep="")
      nc=create.nc(name)
      
      dim.def.nc(nc,"LATITUDE",nlat)
      dim.def.nc(nc,"LONGITUDE",nlon)
    
    ## Create 2 coordinate variables
      var.def.nc(nc, "LATITUDE", "NC_FLOAT", "LATITUDE")
      var.def.nc(nc, "LONGITUDE", "NC_FLOAT", "LONGITUDE")

    ## The dimensions must be in the reverse order, as the fastest
    ## varying comes first and the slowest varying last.
      for (m in seq(ntrans)){
        var.def.nc(nc,allvarnames[m+3], "NC_FLOAT", c(0,1))
      }
      
      print("0")
    
    ## Add attributes
      att.put.nc(nc,"LONGITUDE", "units", "NC_CHAR", "degrees_east")
      att.put.nc(nc,"LONGITUDE", "long_name", "NC_CHAR", "longitude")
      att.put.nc(nc,"LONGITUDE", "GridType", "NC_CHAR", "Cylindrical Equidistant projection Grid")
      att.put.nc(nc,"LATITUDE", "units", "NC_CHAR", "degrees_north")
      att.put.nc(nc,"LATITUDE", "long_name", "NC_CHAR", "latitude")
      att.put.nc(nc,"LATITUDE", "GridType", "NC_CHAR", "Cylindrical Equidistant projection Grid")
      for (m in seq(ntrans)){
        att.put.nc(nc,allvarnames[m+3], "units", "NC_CHAR", "grid cell area fraction")
        att.put.nc(nc,allvarnames[m+3], "missing_value", "NC_FLOAT", -9999.)
        att.put.nc(nc,allvarnames[m+3], "_FillValue", "NC_FLOAT", -9999.)
      }
      att.put.nc(nc,"NC_GLOBAL", "title", "NC_CHAR", "Land use transitions")
      att.put.nc(nc,"NC_GLOBAL", "history", "NC_CHAR", paste("Created on", date(), "by Beni Stocker"))
    
      print("2")
    
    ## Put the data
      var.put.nc(nc,"LONGITUDE", lon, NA, NA, na.mode=0)
      var.put.nc(nc,"LATITUDE", lat, NA, NA, na.mode=0)
    
      print("2.5")

      for (m in seq(ntrans)){
        var.put.nc(nc,allvarnames[m+3],annualmatrix[,,m])
     }
            
      print("3")    

      sync.nc(nc)
      print("done")
      close.nc(nc)
    

    ## next year
    }  #n

}
