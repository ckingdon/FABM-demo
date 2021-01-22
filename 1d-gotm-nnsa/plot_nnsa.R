# library for reading NetCDF files
require(ncdf4) # nc_open(), ncvar_get()
require(fields) # image(), image.plot()
rm(list=ls()) # delete all variables
graphics.off()

# setup working environment and read NetCDF file(s)
setwd("~/projects/fabm-demo/1d-gotm-nnsa/")
ncfnlist = Sys.glob("~/projects/fabm-demo/1d-gotm-nnsa/out_netcdf/*.nc")
ncfnlist
ncfn = ncfnlist[1]
nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
ncfnprefix = tools::file_path_sans_ext(basename(ncfn)) 

# array of dates in POSIX format
# start: 1999-01-01 00:00:00 | end: 1999-01-01 00:00:00 | time step is 3600 s
seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds
date = as.POSIXct("1998-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
# get depth range
depth = ncvar_get(nc, "z") # depth in NetCDF is in meters
depth = depth[,1]

# specify variables .. many more to choose from
varlist = c("temp","salt", "rho", "u")

# make plots
#png(file=paste0(ncfnprefix,".png"), width=16, height=9, units="in", pointsize = 16, res=300, type="cairo")
##par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), oma=c(0,0,0,0), las=0) # defaults
par(mfrow=c(2,2))
par(mar=c(2,4,2,1)) # plot margins c(bottom, left, top, right)
par(oma=c(0,2,0,5)) # outer margin c(bottom, left, top, right)
par(mgp=c(1.5,0.5,0)) # adjust plot axis text

for(var in varlist) { 
  values = ncvar_get(nc, var, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
  units = ncatt_get(nc,var,"units")
  units = units$value
  image.plot(date,depth,t(values),axes=FALSE,
           xlab="",ylab="depth[m]",axis.args=list(cex.axis=0.75,line=0))
  title(main=paste0(var," [",units,"]"),cex.main=0.9)
  axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
  axis(2, at=seq(round(max(depth)),round(min(depth)),-25))
}
#dev.off()
#graphics.off()

# simple plots to explore data
str(values)
values = ncvar_get(nc, "temp", start=c(1,1,1,1), count=c(-1,-1,-1,-1))
units = ncatt_get(nc,var,"units")
units = units$value
plot(values[,3000],depth,type="l")
plot(values[,5000],depth,type="l")
plot(values[,8000],depth,type="l")
image.plot(t(values))  # simple plot
image.plot(date,depth,t(values),axes=FALSE,xlab="") # simple plot
axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
axis(2, at=seq(round(max(depth)),round(min(depth)),-25))


# write NetCDF header to file
##print(nc)
#sink(paste0(ncfnlist[1],".txt"))
#print(nc)
#sink()