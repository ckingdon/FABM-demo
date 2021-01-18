# library for reading NetCDF files
require(ncdf4)
require(fields)
rm(list=ls()) # delete all variables

# setup working environment and read NetCDF file(s)
setwd("~/projects/fabm-demo/1d-gotm-couette/")
ncfnlist = Sys.glob("~/projects/fabm-demo/1d-gotm-couette/*.nc")
#ncfnlist = Sys.glob("~/projects/fabm-demo/gotm-npzd-co3/out_netcdf/gotm-npzd-carbonate-w_10-s_DAT-dic_2100.nc")

nc = nc_open(ncfnlist[1], readunlim = FALSE) # read NetCDF file into R object 'nc'

# write nc variables to file with sink()
sink(paste0(ncfnlist[1],".txt"))
print(nc)
sink()

xvar = "u"
#xvalues = ncvar_get(nc, xvar, start=c(1,1,1,1), count=c(1,1,1,-1))
xvalues = ncvar_get(nc, xvar)
xunits = ncatt_get(nc,xvar,"units")
xunits = xunits$value

yvar = "z"
#yvar = "v"
#yvalues = ncvar_get(nc, yvar, start=c(1,1,1,1), count=c(1,1,1,-1))
yvalues = ncvar_get(nc, yvar)
yunits = ncatt_get(nc,yvar,"units")
yunits = yunits$value

plot(xvalues, yvalues, type="l", xlab=paste(xvar,'[',xunits,']'), ylab=paste(yvar,'[',yunits,']'))

image.plot(xvalues)
plot(xvalues[,25])
plot(xvalues[,25],yvalues[,1])
str(xvalues)
#par(ask=TRUE) # disable "Hit <Return> to see next plot"
for (i in seq(1,25,1)){
  plot(xvalues[,i],yvalues[,1],main=i,type="l", xlab=paste(xvar,'[',xunits,']'), ylab=paste(yvar,'[',yunits,']'))
  Sys.sleep(0.3)
}
#par(ask=FALSE) # disable "Hit <Return> to see next plot"

#varnames = names(nc$var)
#seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds
#date = as.POSIXct("1998-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
#var = varnames[ni]
#values = ncvar_get(nc, var, start=c(1,1,1), count=c(1,1,-1))
#values = ncvar_get(nc, var, start=c(1,1,1,1), count=c(1,1,1,-1))
#vunits = ncatt_get(nc,var,"units")
#vunits = vunits$value
#plot(date,values,type="l",main=var,ylab=paste('[',vunits,']',sep=' '),xlab="")
#plot(date,values,type="l",ylab=paste('[',vunits,']',sep=' '),xlab="")
#title(var, line = 0.5)


