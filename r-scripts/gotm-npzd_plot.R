# based on sample code from Mark Rowe and https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/
# https://www.rdocumentation.org/packages/ncdf4/versions/1.17/topics/ncvar_get)


# library for reading NetCDF files
require(ncdf4)

rm(list=ls())
# setup working environment and read NetCDF file
setwd("~/projects/fabm-demo/gotm-npzd/varplots")
ncfn = "~/projects/fabm-demo/gotm-npzd/gotm-npzd.nc"
nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'

# get some details about NetCDF file
print(nc)  # show all attributes
nc$nvars # number of variables
nc$ndims # number of dimensions
nc$dim$time$len # dimension of time variable
nc$dim$lat$len # dimension of lat variable
attributes(nc) # global attributes
attributes(nc)$names # global attributes "names"
names(nc$var) # list variables in nc
attributes(nc$var)$names # another way to list variables

# details on npzd_nut
nc$var$npzd_nut$name
nc$var$npzd_nut$longname
nc$var$npzd_nut$units

# make array of dates in POSIX format
# env_nns_annual.dat started at 1999-01-01 00:00:00 and has record for each hour until 1999-01-01 00:00:00
seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds
date = as.POSIXct("1998-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates

# plot single variable
var = "npzd_phy"
#var = "total_nitrogen_calculator_result"
vunits = ncatt_get(nc,var,"units")
vunits = vunits$value
values = ncvar_get(nc,var, start = c(1,1,1), count=c(1,1,-1)) 
# start= indicates where to start reading the array
# count= tells how many values to read
plot(date,values,type="l",main=var,ylab=paste(var,'[',vunits,']',sep=' '))

# loop to plot all variables
varnames = names(nc$var) # get variables from nc
par(ask=TRUE) # setting for "Hit <Return> to see next plot"
for(ni in 1:length(varnames)){
  
  var = varnames[ni]
  values = ncvar_get(nc, var, start=c(1,1,1), count=c(1,1,-1))
  vunits = ncatt_get(nc,var,"units"); vunits = vunits$value
  
  # uncomment png() and graphics.off() to create .png file
  #png(file=paste0(var,".png"), width=4, height=4, units="in", pointsize = 10, res=300)
  plot(date,values,type="l",main=var,ylab=paste(var,'[',vunits,']',sep=' '))
  #graphics.off()
}

par(ask=FALSE) # disable "Hit <Return> to see next plot"
