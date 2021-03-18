# library for reading NetCDF files
require(ncdf4) # nc_open(), ncvar_get()
require(fields) # image(), image.plot()

# clean up
rm(list=ls()) # delete all variables
graphics.off()

# setup working environment
setwd("~/projects/fabm-demo/1d-gotm-nnsa-npzd/")
source("../99-src_examples/f.prep1Dgotm.R")  # prep1Dgotm() function reads 1D GOTM NetCDF
ncfnlist = Sys.glob("~/projects/fabm-demo/1d-gotm-nnsa-npzd/out_netcdf/*.nc")
ncfnlist
ncfn = ncfnlist[1]
ncfnprefix = tools::file_path_sans_ext(basename(ncfn)) 
ncfnprefix

# pick a variable and date range
ncvar=c("temp")
timestart = "1998-05-01 00:00:00"
timestop = "1998-11-30 00:00:00"
#gotm1d = prep1Dgotm(ncfn,ncvar) # fetch gotm1d list of lists
gotm1d = prep1Dgotm(ncfn,ncvar,timestart,timestop)
#gotm1d = prep1Dgotm(ncfn,ncvar,timestart,timestop,depthstart,depthstop)

# get R variables from GOTM 1D dataset from prep1Dgotm()
vvalues = gotm1d$values
vvarname = gotm1d$name
vdepth = gotm1d$depth
vdate = gotm1d$datetime
vunits = gotm1d$units
vname = gotm1d$name

#png(file=paste0("~/projects/zzgraphics/",ncfnprefix,".png"), 
#    width=4, height=4, units="in", pointsize = 5, res=300)
image.plot(vdate,vdepth,t(vvalues), axes=FALSE)
axis(1, at=vdate[seq(1,length(vdate),length=5)],labels=format(vdate[seq(1,length(vdate),length=5)],"%b"))
axis(2, at=seq(round(max(vdepth)),round(min(vdepth)),-5))
tmain=paste0(ncfnprefix,"\n",vname," [",vunits,"]")
title(main=tmain,cex.main=0.9)
#dev.off()
print("--break--")

graphics.off; clean.par = par()

#png(file=paste0("~/projects/zzgraphics/",ncfnprefix,".png"), 
#    width=4, height=2.5, units="in", pointsize = 5, res=300)

# spec variables and plot layout
#vlist = c("temp","salt")
#par(mfrow=c(1,2))
vlist = c("temp","salt", "rho", "npzd_nut", "npzd_phy","npzd_zoo", "npzd_PPR", "npzd_NPR")
par(mfrow=c(2,4))
#vlist = c("temp","salt", "rho", "npzd_nut", "npzd_phy","npzd_zoo", "npzd_PAR", "npzd_PPR", "npzd_NPR")
#par(mfrow=c(3,3))
par(mar=c(2,4,3,2)) # c(bottom, left, top, right)
par(mgp=c(1.3,0.5,0)) # adjust plot axis text

for(ncvar in vlist) { 
  gotm1d = prep1Dgotm(ncfn,ncvar)
  vunits = gotm1d$units
  vname = gotm1d$name
  vvalues = gotm1d$values
  vdepth = gotm1d$depth
  vdate = gotm1d$datetime
#  png(file=paste0("~/projects/zzgraphics/",ncfnprefix,"_",vname,".png"), 
#    width=4, height=4, units="in", pointsize = 5, res=300)
  image.plot(vdate,vdepth,t(vvalues), axes=FALSE)
  axis(1, at=vdate[seq(1,length(vdate),length=5)],labels=format(vdate[seq(1,length(vdate),length=5)],"%b"))
  axis(2, at=seq(round(max(vdepth)),round(min(vdepth)),-5))
  tmain=paste0(ncfnprefix,"\n",vname," [",vunits,"]")
  title(main=tmain,cex.main=0.9)
#  dev.off()
}

#dev.off()




