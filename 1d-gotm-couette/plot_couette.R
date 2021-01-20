require(ncdf4)  # for NetCDF
require(fields) # for image.plot()
rm(list=ls())   # clear all variables

# setup working environment and read NetCDF file(s)
setwd("~/projects/fabm-demo/1d-gotm-couette/")
ncfnlist = Sys.glob("~/projects/fabm-demo/1d-gotm-couette/*.nc")
nc = nc_open(ncfnlist[1], readunlim = FALSE) # read NetCDF file into R object 'nc'
ncfnprefix = tools::file_path_sans_ext(basename(ncfnlist[1])) 

# write nc variables to file with sink()
#sink(paste0(ncfnlist[1],".hdr"))
#print(nc)
#sink()
names(nc$var)  # list NetCDF variables

# get depth axis
yvar = "z"  # depth in yvar
#yvalues = ncvar_get(nc, yvar)
yvalues = ncvar_get(nc, yvar, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
yunits = ncatt_get(nc,yvar,"units")
yunits = yunits$value

# --> velocity profile
xvar = "u"  # horizontal velocity in xvar
#xvalues = ncvar_get(nc, xvar)
xvalues = ncvar_get(nc, xvar, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
xunits = ncatt_get(nc,xvar,"units")
xunits = xunits$value

# plot velocity profile
png(file=paste0(ncfnprefix,"-",xvar,".png"), width=16, height=9, units="in", pointsize = 10, res=400)
plot(xvalues[,25],yvalues[,1],main="velocity",
     type="l", xlab=paste(xvar,'[',xunits,']'), ylab=paste(yvar,'[',yunits,']'),xlim=c(min(xvalues),max(xvalues)))
graphics.off()

# animate velocity profile
for (i in seq(1,25,1)){
  plot(xvalues[,i],yvalues[,1],main=paste("velocity\nhour ",i),
       type="l", xlab=paste(xvar,'[',xunits,']'), ylab=paste(yvar,'[',yunits,']'),xlim=c(min(xvalues),max(xvalues)))
  Sys.sleep(0.3)
}

# --> turbulent diffusivity of momentum profile
xvar = "num" # put diffusivity in xvar
#xvalues = ncvar_get(nc, xvar)
xvalues = ncvar_get(nc, xvar, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
xunits = ncatt_get(nc,xvar,"units")
xunits = xunits$value

# plot diffusivity profile
png(file=paste0(ncfnprefix,"-",xvar,".png"), width=16, height=9, units="in", pointsize = 10, res=400)
plot(xvalues[,25][1:100],yvalues[,1],main="turbulent diffusivity of momentum",
   type="l", xlab=paste(xvar,'[',xunits,']'), ylab=paste(yvar,'[',yunits,']'),xlim=c(0,0.05))
graphics.off()

# animate diffusivity profile
for (i in seq(1,25,1)){
  plot(xvalues[,i][1:100],yvalues[,1],main=paste("turbulent diffusivity of momentum\nhour ",i),
     type="l", xlab=paste(xvar,'[',xunits,']'), ylab=paste(yvar,'[',yunits,']'),xlim=c(0,0.05))
  Sys.sleep(0.3)
}

# --> turbulent momentum flux profile
xvar = "taux" # put "turbulent flux of momentum (x)" in xvar
#xvalues = ncvar_get(nc, xvar)
xvalues = ncvar_get(nc, xvar, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
xunits = ncatt_get(nc,xvar,"units")
xunits = xunits$value

# plot diffusivity profile
png(file=paste0(ncfnprefix,"-",xvar,".png"), width=16, height=9, units="in", pointsize = 10, res=400)
plot(xvalues[,2][1:100],yvalues[,1],main="turbulent flux of momentum (x)",
   type="l", xlab=paste(xvar,'[',xunits,']'), ylab=paste(yvar,'[',yunits,']'),xlim=c(min(xvalues),max(xvalues)))
graphics.off()

# animate diffusivity profile
for (i in seq(1,25,1)){
  plot(xvalues[,i][1:100],yvalues[,1],main=paste("turbulent diffusivity of momentum\nhour ",i),
     type="l", xlab=paste(xvar,'[',xunits,']'), ylab=paste(yvar,'[',yunits,']'),xlim=c(min(xvalues),max(xvalues)))
  Sys.sleep(0.5)
}





#image.plot(t(xvalues))
#image.plot(t(yvalues))
#image.plot(t(yvalues[c(1:10),c(1:5)]))


#par(ask=TRUE) # disable "Hit <Return> to see next plot"
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


