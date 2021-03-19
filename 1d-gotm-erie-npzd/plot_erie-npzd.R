# library for reading NetCDF files
require(ncdf4) # nc_open(), ncvar_get()
require(fields) # image(), image.plot()

rm(list=ls()) # delete all variables
graphics.off()

# setup working environment and read NetCDF file(s)
setwd("~/projects/fabm-demo/1d-gotm-erie-npzd/")
source("../99-src_examples/f.prep1Dgotm.R")
ncfnlist = Sys.glob("~/projects/fabm-demo/1d-gotm-erie-npzd/out_netcdf/*.nc")
ncfnlist
#par(mfrow=c(1,2))
ncfn = ncfnlist[1]
ncfn = ncfnlist[length(ncfnlist)]
ncfn = ncfnlist[length(ncfnlist)-1]
#ncfn = ncfnlist[length(ncfnlist)-2]
ncfn
ncfnprefix = tools::file_path_sans_ext(basename(ncfn)) 
ncfnprefix
#nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'

ncvar=c("temp")
timestart = "2018-05-01 00:00:00"
timestop = "2018-10-31 00:00:00"
#gotm1d = prep1Dgotm(ncfn,ncvar,timestart,timestop,depthstart,depthstop)

gotm1d = prep1Dgotm(ncfn,ncvar,timestart,timestop)

gotm1d = prep1Dgotm(ncfn,ncvar)

values = gotm1d$temp_values
units = gotm1d$temp_units
depth = gotm1d$depth
date = gotm1d$datetime

#png(file=paste0("~/projects/zzgraphics/",ncfnprefix,".png"), 
#    width=4, height=4, units="in", pointsize = 5, res=300)
image.plot(date,depth,t(values), axes=FALSE)
image.plot(date,depth,t(values), zlim=c(4,30), axes=FALSE)
#axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
axis(1, at=date[seq(1,length(date),length=5)],labels=format(date[seq(1,length(date),length=5)],"%b"))
axis(2, at=seq(round(max(depth)),round(min(depth)),-5))
tmain=paste0(ncfnprefix,"\n",ncvar," [",units,"]")
title(main=tmain,cex.main=0.9)
dev.off()
print("bort")



nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
d0 = ncatt_get(nc,"time","units")
d0 = regmatches(d0$value,regexpr("\\d{4}-\\d{2}-\\d{2}", d0$value))
#dindex = as.numeric(as.POSIXct("2018-05-31") - as.POSIXct(d0), units="days") 
dindex = as.numeric(as.POSIXct("2018-05-01"), units="days") 
lines(c(dindex,dindex),c(depth[1],depth[length(depth)]), lwd = 3)

zz = locator(n=1)




ncvar=c("temp")
for (ncfn in ncfnlist){
  nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
  ncfnprefix = tools::file_path_sans_ext(basename(ncfn)) 
  t0 = ncatt_get(nc,"time","units")
  t0 = regmatches(t0$value,regexpr("\\d{4}-\\d{2}-\\d{2}", t0$value))
  tstart = "2018-05-31"  # >= start time for netcdf
  tstop = "2018-10-02"  # <= end time for netcdf
  trange = as.POSIXct(tstop) - as.POSIXct(tstart)
  trange = as.numeric(trange, units="days")
  seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds
  date = as.POSIXct(t0, tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
  date[c(1,length(date))]
  # crop VALUES to date range
  itstart = as.POSIXct(tstart) - as.POSIXct(t0)
  itstart = as.numeric(itstart, units="hours") + 1 # hours bc GOTM model dt is 3600s
  itstop = as.POSIXct(tstop) - as.POSIXct(tstart)
  itstop = itstart + as.numeric(itstop, units="hours")
  # crop DATE to date range
  date = date[c(itstart:itstop)]
  depth = ncvar_get(nc, "z") # depth in NetCDF is in meters
  depth = depth[,1]
  ncvar=c("temp")
  units = ncatt_get(nc,ncvar,"units") 
  units = units$value
  values = ncvar_get(nc, ncvar, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
  values = values[,c(itstart:itstop)]
  png(file=paste0("~/projects/zzgraphics/2021-03-02/",ncfnprefix,".png"), 
    width=4, height=4, units="in", pointsize = 5, res=300)
  image.plot(date,depth,t(values), axes=FALSE)
  axis(1, at=date[seq(1,length(date),length=5)],labels=format(date[seq(1,length(date),length=5)],"%b"))
  axis(2, at=seq(round(max(depth)),round(min(depth)),-5))
  tmain=paste0(ncfnprefix,"\n",ncvar," [",units,"]")
  title(main=tmain,cex.main=0.9)
  dev.off()
  message(tmain)
  print(date[c(1,length(date))])
}
print("bort")


# ------------
# delete this block once above works
ncvar=c("temp")
nc = nc_open(ncfnlist[1], readunlim = FALSE) # read NetCDF file into R object 'nc'
#seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds
#date = as.POSIXct("1998-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
#date = as.POSIXct("1998-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
#units = ncatt_get(nc,ncvar,"units") 
#units = units$value
#t0 = ncatt_get(nc,"time","units")
#t0 = regmatches(t0$value,regexpr("\\d{4}-\\d{2}-\\d{2}", t0$value))
t0 = "2018-05-31"
tend = "2018-10-31"
trange = as.POSIXct(tend) - as.POSIXct(t0)
trange = as.numeric(trange, units="days")
seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds
date = as.POSIXct(t0, tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
for (ncfn in ncfnlist){
  nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
  ncfnprefix = tools::file_path_sans_ext(basename(ncfn)) 
  units = ncatt_get(nc,ncvar,"units") 
  units = units$value
  print(ncfn)
  #png(file=paste0(ncfnprefix,".png"), width=16, height=9, units="in", pointsize = 10, res=400)
  #png(file=paste0("zzgraphics/",ncfnprefix,".png"), width=4, height=4, units="in", pointsize = 10, res=400)
  png(file=paste0("~/projects/zzgraphics/2021-03-02/",ncfnprefix,".png"), 
    width=4, height=4, units="in", pointsize = 5, res=300)
  #nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
  depth = ncvar_get(nc, "z") # depth in NetCDF is in meters
  depth = depth[,1]
  values = ncvar_get(nc, ncvar, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
  str(values)
  image.plot(date,depth,t(values), axes=FALSE)
  #axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
  axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
  axis(2, at=seq(round(max(depth)),round(min(depth)),-5))
  tmain=paste0(ncfnprefix,"\n",ncvar," [",units,"]")
  title(main=tmain,cex.main=0.9)
  dev.off()
}
print("bort")
# ------------



#ncvars = c("temp", "rho", "num")
ncvars = c("temp", "rho", "u", "v", "num", "tke", "avh", 
           "npzd_nut", "npzd_phy", "npzd_zoo", "npzd_det", "npzd_PPR", "npzd_NPR")
nc = nc_open("out_netcdf/erie-11.nc", readunlim = FALSE) # read NetCDF file into R object 'nc'
seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds
date = as.POSIXct("1998-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
ncfnprefix = tools::file_path_sans_ext(basename(ncfn)) 
depth = ncvar_get(nc, "z") # depth in NetCDF is in meters
depth = depth[,1]
for (v in ncvars){
  print(v)
  pngprefix=paste0(ncfnprefix,"-", v)
  png(file=paste0("zzgraphics/",pngprefix,".png"), width=4, height=4, units="in", pointsize = 10, res=400)
  values = ncvar_get(nc, v, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
  if (nrow(values) > length(depth)){
    values = values[1:length(depth),] # cut off last value for grid faces rather than center
  }
  units = ncatt_get(nc, v, "units") 
  units = units$value
  image.plot(date,depth,t(values), axes=FALSE)
  axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
  axis(2, at=seq(round(max(depth)),round(min(depth)),-5))
  tmain=paste0(ncfnprefix,"\n", v," [",units,"]")
  title(main=tmain,cex.main=0.9)
  print(pngprefix)
  dev.off()
}
print("bort")



# compare erie-xx .nc matrices
ctr=1
for (ncfn in ncfnlist){
  print(ncfn)
  ncfnprefix = tools::file_path_sans_ext(basename(ncfn)) 
  nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
  #values = ncvar_get(nc, "temp", start=c(1,1,1,1), count=c(-1,-1,-1,-1))
  assign(ncfnprefix,ncvar_get(nc, "temp", start=c(1,1,1,1), count=c(-1,-1,-1,-1)) )
}

ncfnlist = Sys.glob("~/projects/fabm-demo/1d-gotm-erie-npzd/out_netcdf/*.nc")
ncfnlist
ncfn = ncfnlist[3]
ncfn = ncfnlist[5]
ncfn = ncfnlist[6]
ncfn
nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
values = ncvar_get(nc, "temp", start=c(1,1,1,1), count=c(-1,-1,-1,-1))
erie_02 = values
erie_04 = values
erie_05 = values
identical(`erie_01`,`erie_02`)
identical(`erie_02`,`erie_04`)
all.equal(`erie_02`,`erie_04`)
identical(`erie_02`,`erie_05`)
all.equal(`erie_02`,`erie_05`)
identical(`erie_04`,`erie_05`)
all.equal(`erie_04`,`erie_05`)


print("bort")



plot(values[,1],depth)
plot(values[,2],depth)
plot(values[,3],depth)
plot(values[,10],depth)
plot(values[,1000],depth)
plot(values[,2000],depth)
plot(values[,3000],depth)
plot(values[,4000],depth)
plot(values[,5000],depth)
plot(values[,6000],depth)
plot(values[,7000],depth)

#ncfnprefix = tools::file_path_sans_ext(basename(ncfn)) 
#ncfnprefix

# array of dates in POSIX format                                                                         
# start: 1999-01-01 00:00:00 | end: 1999-01-01 00:00:00 | time step is 3600 s                            
seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds                                           
date = as.POSIXct("2018-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
# get depth range                                                                                        
z = ncvar_get(nc, "z") # depth in NetCDF is in meters                                                
z = z[,1]                                                                                        
str(z)
zi = ncvar_get(nc, "zi") # depth in NetCDF is in meters                                                
zi = zi[,1]                                                                                        
str(zi)

values = ncvar_get(nc, "temp", start=c(1,1,1,1), count=c(-1,-1,-1,-1))
#values = ncvar_get(nc, "taux", start=c(1,1,1,1), count=c(-1,-1,-1,-1))
str(values)
image.plot(t(values))
image.plot(date,z,t(values),axes=FALSE)
#axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
#axis(2, at=seq(0,min(z),-5))

plot(values[,1],z)
plot(values[,2],z)
plot(values[,3],z)
plot(values[,10],z)
plot(values[,1000],z)
plot(values[,2000],z)
plot(values[,3000],z)
plot(values[,4000],z)
plot(values[,5000],z)
plot(values[,6000],z)
plot(values[,7000],z)






#var = "temp"
#values = ncvar_get(nc, var)
#values = ncvar_get(nc, var, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
#units = ncatt_get(nc,var,"units")
#units = units$value

#varlist = c("temp","salt", "rho", "npzd_NPR", "npzd_nut", "npzd_phy","npzd_zoo", "npzd_det")
varlist = c("temp", "rho", "npzd_NPR", "npzd_nut", "npzd_phy","npzd_zoo", "npzd_det")
#varlist = c("temp","salt", "carbonate_dic", "carbonate_pH", "npzd_nut", "npzd_phy","npzd_zoo", "carbonate_pCO2")
varlist = c("temp", "carbonate_dic", "carbonate_pH", "npzd_nut", "npzd_phy","npzd_zoo", "carbonate_pCO2")
varlist = c("temp","rho")

# make plot for *all* netcdf variables
#png(file=paste0(ncfnprefix,".png"), width=16, height=9, units="in", pointsize = 10, res=400, type="cairo")
png(file=paste0(ncfnprefix,".png"), width=16, height=9, units="in", pointsize = 10, res=400)
par(mfrow=c(2,4))
par(mar=c(2,4,3,2)) # c(bottom, left, top, right)
par(mgp=c(1.3,0.5,0)) # adjust plot axis text
#zplotlist = list(); z=0
for(var in varlist) { 
  #z = z+1
  ##zname = paste("z_", var, sep="")
  values = ncvar_get(nc, var, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
  units = ncatt_get(nc,var,"units")
  units = units$value
  #plot.new()              # * don't print to display
  #dev.control("enable")   # * don't print to display
  image.plot(date,depth,t(values),axes=FALSE,legend.mar=0,xlab="",legend.cex=10)
  title(main=paste0(var," [",units,"]"))
  axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
  axis(2, at=seq(0,-110,-25))
  #zplot = recordPlot()
  #dev.off()               # * don't print to display
  ##assign(zname, zplot)
  ##zplotlist = append(zplotlist,zname=zplot)
  #zplotlist[[z]] = zplot
}
graphics.off()
#rm(list=ls(pattern="^z"))
#rm(list=c('z','zplot'))

# put multiple plots on page
for (i in 1:length(zplotlist)){
  replayPlot(zplotlist[[i]])
}



#values = values[101:111,]
#image.plot(date,depth[101:111],t(values),axes=FALSE)
title(main=paste0(var," [",units,"]"))
axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
#axis(2, at=seq(0,-10,-1))
axis(2, at=seq(0,-110,-25))

# compare data from NetCDFs
var = "temp_obs"
ncfn1 = ncfnlist[1]
ncfn2 = ncfnlist[2]
nc1 = nc_open(ncfn1, readunlim = FALSE) # read NetCDF file into R object 'nc'
nc2 = nc_open(ncfn2, readunlim = FALSE) # read NetCDF file into R object 'nc'
values1 = ncvar_get(nc1, var, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
values2 = ncvar_get(nc2, var, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
values1 = ncvar_get(nc1, "u", start=c(1,1,1,1), count=c(-1,-1,-1,-1))
values2 = ncvar_get(nc1, "u_obs", start=c(1,1,1,1), count=c(-1,-1,-1,-1))
#values1 == values2
identical(values1,values2)
all.equal(values1,values2)
valdiff = values1 - values2
image.plot(date,depth,t(valdiff),col=tim.colors(),axes=FALSE)
image.plot(date,depth,t(valdiff),col=rev(tim.colors()),axes=FALSE)
axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
axis(2, at=seq(0,-110,-25))

#image.plot(t(values))
#image.plot(t(values),axes=FALSE)
image.plot(t(values))
image.plot(date,depth,t(values))
#image(date,depth,t(values),axes=FALSE)
#axis(1)
#axis(2)
#axis(1, at=date[seq(1,8761,1000)])


plot(values[,25])
plot(values[,25],yvalues[,1])
str(values)

#values.df = as.data.frame(t(values))
#values.df = as.data.frame((values))
#colnames(values.df)
#colnames(values.df) = paste(c(1:110))
#colnames(values.df) = paste("d_",c(1:110))
#rownames(values.df)
#rownames(values.df) = date

#yvar = "z"
#yvalues = ncvar_get(nc, yvar, start=c(1,1,1,1), count=c(1,1,2,-1))
#yvalues = ncvar_get(nc, yvar)
#yunits = ncatt_get(nc,yvar,"units")
#yunits = yunits$value
#plot(values, yvalues, type="l", xlab=paste(var,'[',units,']'), ylab=paste(yvar,'[',yunits,']'))

#values.df = as.data.frame(t(values))
#values.df = as.data.frame((values))
#colnames(values.df)
#colnames(values.df) = paste("d_",c(1:110))
#rownames(values.df)
#rownames(values.df) = date

##print(nc)
#sink(paste0(ncfnlist[1],".txt"))
#print(nc)
#sink()
