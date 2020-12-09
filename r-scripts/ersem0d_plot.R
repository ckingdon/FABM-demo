require(ncdf4)


setwd("/Users/mdrowe/Documents/pml/fabm/src/build/fabm-0d/plots")
ncfn <- "/Users/mdrowe/Documents/pml/fabm/src/build/fabm-0d/run/output.nc"
nc <- nc_open(ncfn, readunlim = FALSE)
ncvar_get(nc, "variables")

date <- ncvar_get(nc, "time")
date <- as.POSIXct("1998-01-01", tz="GMT") + date

names <- names(nc$var)

for(ni in 1:length(names)){
  
  name <- names[ni]
  values <- ncvar_get(nc, name, start=c(1,1,1), count=c(1,1,-1))
  
  png(file=paste0(name,".png"), width=4, height=4, units="in", pointsize = 10, res=300)
  
  plot(date, values, type="l", main=name)
  
  graphics.off()
}

