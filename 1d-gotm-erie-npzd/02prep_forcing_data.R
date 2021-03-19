# library for reading NetCDF files
require(ncdf4) # nc_open(), ncvar_get()
require(fields) # image(), image.plot()
require(PBSmapping)
#install.packages("zoo",dependencies = TRUE)
#require(zoo)  # rollapply
rm(list=ls()) # delete all variables
graphics.off()

# follow up with Peter/Mark about which element/node to select
# --- why are dimensions in NetCDF files listed in reverse order to what I expect ? ---

# setup working environment and read NetCDF file(s)
setwd("~/projects/fabm-demo/1d-gotm-erie-npzd/")
# erie_forcing_interp.nc is symlink to 
# /mnt/projects/hpc/anderson/models/erie/realtime/grid_v3/input_2018/erie_forcing_interp.nc
nc = nc_open("erie_forcing_interp.nc", readunlim = FALSE) # read NetCDF file into R object 'nc'
#str(nc)
#attributes(nc)$names

# sorting out datetime
#time = ncvar_get(nc, "time") # float days since 1858-11-17
#itime = ncvar_get(nc, "Itime") # integer days since 1858-11-17
#itime2 = ncvar_get(nc, "Itime2") # integer msec since 00:00:00 (midnight)
##for (i in c(1:12)){
## print( paste(i,time[i],itime[i],itime2[i]) )
##}
#as.POSIXct(time[2]*24*60*60,origin="1858-11-17")
#as.POSIXct(itime[2]*24*60*60 + itime2[2]/1000,origin="1858-11-17")
#as.POSIXct(itime[1:5]*24*60*60 + itime2[1:5]/1000,origin="1858-11-17")
#datetime = as.POSIXct(itime*24*60*60 + itime2/1000, origin="1858-11-17", tz="UTC")

# from Peter's script .. much easier/quicker
# but I don't understand the Times(time, DateStrLen) variable
# why is count=c(-1,-1) .. don't get it
datetime =  ncvar_get(nc, "Times", start=c(1,1), count=c(-1,-1))
datetime = as.POSIXct(datetime, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
date = as.character(datetime, format="%Y-%m-%d", tz="UTC")
time = as.character(datetime, format="%H:%M:%S", tz="UTC")


options(digits = 10)
# set three decimal places for all values
# https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
# https://stackoverflow.com/questions/5458729/keeping-trailing-zeros
fnround = function(inval,rnval){
  #round(inval,rnval)
  #as.double(format(round(inval,rnval), nsmall=2))
  #sprintf("%.3f",(round(inval,rnval)))
  #round(inval,rnval)
  sprintf("%.3f",(round(inval,rnval)))
}
fnround(-15.57112,6)

# (time, nele)
# extract "simple" variables (i.e. one values at each element/centroid)
#node=3630 # middle of central basin, see 01_prep_forcing_geosearchnodes.R
#element=c(6909) # element directly south of node 3630

element=c(5332) # element southeast of CHRP4 mooring
#elements=c(6840,6909,6910,6911)
evars = c("uwind_stress", "vwind_stress", "U10", "V10", "uwind_speed", "vwind_speed")
#evars = c("uwind_stress", "vwind_stress")
#uwind_stress = ncvar_get(nc, "uwind_stress", start=c(element,1), count=c(1,-1))
evals = sapply(evars, function(x) ncvar_get(nc, x, start=c(element,1), count=c(1,-1)))
evals[1:5,]
evals = as.data.frame(evals)
evals[1:5,]
evals = sapply(evals, fnround, 6)
evals = as.data.frame(evals)
evals[1:5,]
str(evals)
#colnames(evals) = as.character(evars) # not needed
evals = lapply(evals, function(x) as.numeric(as.character(x))) # convert from factors to numbers
evals = as.data.frame(evals)
str(evals)
evals = cbind(datetime = datetime, evals)

# PLOT WIND SPEED: FVCOM
#plot(datetime,abs(evals$U10),type="l")
#plot(datetime[3000:6500],abs(evals$U10[3000:6500]),type="l")
ma <- function(x, n = 168){filter(x, rep(1 / n, n), sides = 2)} # one week
ma <- function(x, n = 72){filter(x, rep(1 / n, n), sides = 2)}
#plot(datetime,ma(evals$U10),type="l")
plot(datetime[3000:6500],evals$U10[3000:6500],type="l", col="grey")
#plot(datetime[3000:6500],ma(evals$U10[3000:6500]),type="l")
lines(datetime[3000:6500],ma(evals$U10[3000:6500]),col="red")
#plot(datetime,ma(abs(evals$V10)),type="l")
#png(file="~/projects/zzgraphics/2021-03-02/CHRP4-2018-fvcom-u10.png", 
#    width=4, height=4, units="in", pointsize = 5, res=300)
#u10lims = c(min(abs(evals$U10)),max(abs(evals$U10))*0.6)
u10lims = c(0,10)
plot(datetime[3000:6500],abs(evals$U10[3000:6500]),type="l", col="grey",ylim=u10lims)
lines(datetime[3000:6500],ma(abs(evals$U10[3000:6500])),col="red",lwd=2.5)
#dev.off()
#png(file="~/projects/zzgraphics/2021-03-02/CHRP4-2018-fvcom-v10.png", 
#    width=4, height=4, units="in", pointsize = 5, res=300)
#v10lims = c(min(abs(evals$V10)),max(abs(evals$V10))*0.6)
v10lims = c(0,10)
plot(datetime[3000:6500],abs(evals$V10[3000:6500]),type="l", col="grey",ylim=v10lims)
lines(datetime[3000:6500],ma(abs(evals$V10[3000:6500])),col="red", lwd=2.5)
#dev.off()



# PLOT WIND SPEED: NDBC 45164
b45164 = read.csv("~/projects/ndbc/zz.txt", header=TRUE, sep=",")
buoy = b45164
str(buoy)
#bdate = as.POSIXct(paste(buoy$YY,buoy$MM,buoy$DD, sep="-"))
#btime = paste(buoy$hh,buoy$mm,"00", sep=":")
bdatetime = as.POSIXct(paste(paste(buoy$YY,buoy$MM,buoy$DD, sep="-")," ",paste(buoy$hh,buoy$mm,"00", sep=":")))
bwdir = buoy$WDIR  # at 1m
bwspd = buoy$WSPD  # at 1m
# scale to 10m using wind profile power law
# https://www.ndbc.noaa.gov/adjust_wind.shtml
# https://en.wikipedia.org/wiki/Wind_profile_power_law
alpha = 0.11
alpha = 0.143
bu10 = (bwspd * cos(bwdir)) * ((1 / 10 ) ^ alpha)
bu1 = bwspd * cos(bwdir)
bv10 = bwspd * sin(bwdir)
bvals = as.data.frame(cbind(datetime=bdatetime,bwdir,bwspd,bu10,bv10))
bvals$datetime = as.POSIXct(bvals$datetime, origin="1970-01-01")
str(bvals)
#bvals = bvals[bvals$bdatetime >= "2018-05-01" & bvals$bdatetime <= "2018-10-31", ]  # trim times
#xlim = c("2018-05-01","2018-11-02")
#png(file="~/projects/zzgraphics/2021-03-02/ndbc45164-2018-u10.png", 
#    width=4, height=4, units="in", pointsize = 5, res=300)
    #width=16, height=12, units="in", pointsize = 20, res=400)
#bu10lims = c(min(abs(bu10)),max(abs(bu10))*0.6)
bu10lims = c(0,10)
plot(bvals$datetime,abs(bvals$bu10),type="l", col="grey",ylim=bu10lims)
lines(bvals$datetime,ma(abs(bvals$bu10)),col="red", lwd=2.5)
#dev.off()
#png(file="~/projects/zzgraphics/2021-03-02/ndbc45164-2018-v10.png", 
#    width=4, height=4, units="in", pointsize = 5, res=300)
    #width=16, height=12, units="in", pointsize = 20, res=400)
#bv10lims = c(min(abs(bv10)),max(abs(bv10))*0.6)
bv10lims = c(0,10)
plot(bvals$datetime,abs(bvals$bv10),type="l", col="grey",ylim=bv10lims)
lines(bvals$datetime,ma(abs(bvals$bv10)),col="red", lwd=2.5)
#dev.off()

# comparing wind
str(evals)
evals$wind_mag = sqrt(evals$U10^2 + evals$V10^2)
str(bvals)
bvals$wind_mag = sqrt(bvals$bu10^2 + bvals$bv10^2)

winds = merge(evals,bvals, by="datetime")
names(winds)[names(winds) == "wind_mag.x"] = "e_wind_mag"
names(winds)[names(winds) == "wind_mag.y"] = "b_wind_mag"
str(winds)

#png(file="~/projects/zzgraphics/wind_speed_normalized.png", 
#    width=4, height=4, units="in", pointsize = 5, res=300)
#plot(winds$e_wind_mag,winds$b_wind_mag, col="grey",xlim=c(0,20),ylim=c(0,20))
plot(winds$b_wind_mag,winds$e_wind_mag, col="grey",xlim=c(0,20),ylim=c(0,20))
ebwindlm = lm(winds$e_wind_mag~winds$b_wind_mag)
summary(ebwindlm)
abline(ebwindlm, col="red")
abline(0,1)
#dev.off()

plot(winds$b_wind_mag,winds$e_wind_mag, col="grey",xlim=c(0,20),ylim=c(0,20))

#png(file="~/projects/zzgraphics/wind_speed_u.png", 
#    width=4, height=4, units="in", pointsize = 5, res=300)
plot(winds$U10,winds$bu10,col="grey",xlim=c(-20,20),ylim=c(-20,20))
#plot(winds$U10,winds$bv10,col="grey")
plot(winds$V10,winds$bv10,col="grey",xlim=c(-20,20),ylim=c(-20,20))
#plot(winds$V10,winds$bu10,col="grey")
uwindlm = lm(winds$U10~winds$bu10)
vwindlm = lm(winds$V10~winds$bv10)
summary(uwindlm)
summary(vwindlm)


# (time, node)
# get the three nodes associated with element
threenodes = ncvar_get(nc, "nv", start=c(element,1), count=c(1,-1))
threenodes
# extract values, average values, write to matrix
nvars = c("short_wave", "net_heat_flux", "long_wave", "air_pressure",
          "air_temperature", "dew_point", "cloud_cover", "SPQ", "relative_humidity")
#nvars = c("short_wave", "net_heat_flux")
#nvars = c("air_pressure")
nvals = matrix(nrow = length(evals[,1]))
for (nvar in nvars){
  print(nvar)
  tn1 = ncvar_get(nc, nvar, start=c(threenodes[1],1), count=c(1,-1))
  tn2 = ncvar_get(nc, nvar, start=c(threenodes[2],1), count=c(1,-1))
  tn3 = ncvar_get(nc, nvar, start=c(threenodes[3],1), count=c(1,-1))
  tnmean = cbind(tn1,tn2,tn3)
  tnmean = rowMeans(tnmean)
  #print(paste(tn1[100],tn2[100],tn3[100],tnmean[100]))
  nvals = cbind(nvals,tnmean)
}
nvals=nvals[,2:ncol(nvals)]
colnames(nvals)=nvars
nvals[1:5,]
nvals = as.data.frame(nvals)
nvals[1:5,]
#nvals$air_pressure = nvals$air_pressure/100.0 # convert Pa to kPa (no need bc gotm use HECTOPASCALS)
nvals$air_pressure = nvals$air_pressure/10.0 # convert Pa to hPa
nvals[1:5,]
nvals = sapply(nvals, fnround, 6)
nvals = as.data.frame(nvals)
nvals[1:5,]
str(nvals)
nvals = lapply(nvals, function(x) as.numeric(as.character(x))) # convert from factors to numbers
nvals = as.data.frame(nvals)
plot(datetime,nvals$net_heat_flux,type="l")
plot(datetime,nvals$short_wave,type="l")


# write forcing variables to respective files 
# meteo, surface, fluxes, etc
#nvars = c("short_wave", "net_heat_flux", "long_wave", "air_pressure",
#       "air_temperature", "dew_point", "cloud_cover", "SPQ", "relative_humidity")
#evars = c("uwind_stress", "vwind_stress", "U10", "V10", "uwind_speed", "vwind_speed")

# gotm.yaml: meteo: u10, v10, airp, airt, hum, cloud, swr
zz = as.data.frame(cbind(date,time,
                   evals[c("U10","V10")],
                   nvals[c("air_pressure","air_temperature","relative_humidity","cloud_cover","short_wave")]))
colnames(zz); names(zz)
colnames(zz)[1] = "!date"  # ! is comment character for fortran, used for header row
write.table(zz, file = "in_dat/surface_meteo.dat", sep=" ", quote=FALSE, row.names=FALSE)

# gotm.yaml: surface: fluxes: heat (no data for momentum flux available from FVCOM forcings)
#zz = as.data.frame(cbind(date,time,nvals[c("net_heat_flux")]))
zz = as.data.frame(cbind(date,time,nvals[c("net_heat_flux")], evals[c("uwind_stress","vwind_stress")]))
colnames(zz); names(zz)
#colnames(zz) = c("!date","time","net_heat_flux")  # ! is comment character for fortran
colnames(zz)[1] = "!date"  # ! is comment character for fortran, used for header row
write.table(zz, file = "in_dat/surface_fluxes.dat", sep=" ", quote=FALSE, row.names=FALSE)

# gotm.yaml: surface: longwave:
zz = as.data.frame(cbind(date,time,nvals[c("long_wave")]))
colnames(zz); names(zz)
colnames(zz)[1] = "!date"  # ! is comment character for fortran, used for header row
write.table(zz, file = "in_dat/surface_longwave.dat", sep=" ", quote=FALSE, row.names=FALSE)

# gotm.yaml: mimic_3d: ext_pressure: dpdx, dpdy
# my understanding of this may be (likely is) incorrect:
# I don't think these variables actually represent "ext_pressure"
zz = as.data.frame(cbind(date,time,evals[c("uwind_stress","vwind_stress")]))
colnames(zz); names(zz)
colnames(zz)[1] = "!date"  # ! is comment character for fortran, used for header row
write.table(zz, file = "in_dat/mimic3d_extpress.dat", sep=" ", quote=FALSE, row.names=FALSE)


# SST from ~/projects/greatlakes_sst/2018
# this is from a stack of geotiffs from GLSEA program
# from geotiff metadata:
# TIFFTAG_IMAGEDESCRIPTION=temperature = (color_index * 0.1255) - 2
#jsst = read.table("~/projects/greatlakes_sst/2018/sst-chrp4-2018-rows.dat")
jsst = read.table("~/projects/greatlakes_sst/2018/zz-rows.dat")
# add index dates: band_1 is May 1 , band_184 is Oct 31 (julian day 121 and 304)
#isst = seq(1,length(jsst$V1)) + 120  # for May 1 start date
isst = seq(1,length(jsst$V1)) + 0
sstdf = as.data.frame(cbind(isst,jsst$V1))
names(sstdf) = c("julian","sst_dn")
sstdf[1:10,]
#sstdf$date=as.Date(sstdf$julian,origin="2017-12-31")
#sstdf$date=as.POSIXct(strptime(sstdf$date,"%Y-%m-%d %H:%M:%S"))
sstdf$date = strftime(as.Date(sstdf$julian, 
                             origin=as.POSIXct('2017-12-31', tz = 'GMT')), "%Y-%m-%d %H:%M:%S")
sstdf[1:10,]
sstdf$date = as.POSIXct(sstdf$date)
sstdf$date = sstdf$date+17*60*60  # assuming noon ET for temperature .. 17:00 UTC
class(sstdf$date)
sstdf[1:10,]
sstdf$sst_c = sstdf$sst_dn * 0.1255 - 2
sstdf$sst_c_fix = sstdf$sst_c
sstdf$sst_c_fix[sstdf$sst_c_fix < 4] = 5.0
sstdf[1:10,]
sst_date = as.character(strftime(sstdf$date,"%Y-%m-%d"))
sst_time = as.character(strftime(sstdf$date,"%H:%M:%S"))
zz = as.data.frame(cbind(sst_date, sst_time, sstdf$sst_c, sstdf$sst_c_fix))
zz[1:10,]
colnames(zz); names(zz)
colnames(zz) = c("!date","time","sst_c","sst_c_fix")  # ! is comment character for fortran, used for header row
zz[1:10,]
write.table(zz, file = "in_dat/surface_sst.dat", sep=" ", quote=FALSE, row.names=FALSE)

png(file="~/projects/zzgraphics/CHRP4-2018-SST.png", 
    width=4, height=4, units="in", pointsize = 5, res=300)
    #width=16, height=12, units="in", pointsize = 30, res=400)
#plot(datetime,ma(evals$U10),type="l")
plot(sstdf$date,sstdf$sst_c,type="l", col="grey",lwd=4.5)
lines(sstdf$date,sstdf$sst_c_fix,type="l", col="red",lwd=1.5)
plot(sstdf$date,sstdf$sst_c_fix,type="l", col="red",lwd=4.5)
dev.off()

zz = locator(n=1)
print("bort")




# get other information about node
# location of Peter's forcings: "/mnt/projects/hpc/alsip/le-gem/archive/model_year_2017/20200203-1/"
# files named "erie_00MM.nc" are what he used to get kh and temp to create his 1d forcing files
pncfn = "/mnt/projects/hpc/alsip/le-gem/archive/model_year_2017/20200203-1/erie_0001.nc"
pnc = nc_open(pncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
# maybe I can find depth here?

# element lon, lat, and depth
options(digits = 10)
elon = -(360-ncvar_get(nc, "lonc", start=c(element), count=c(1)))
elat = ncvar_get(nc, "latc", start=c(element), count=c(1))
elon
elat
load(file=paste0("~/projects/erie_grid/erie05lcc_grid_coords.Rdata"))
coordse$lon[element] # from erie05lcc_grid_coords.Rdata
coordse$lat[element]
-(360-ncvar_get(pnc, "lonc", start=c(element), count=c(1)))
ncvar_get(pnc, "latc", start=c(element), count=c(1))
edep = ncvar_get(pnc, "h_center", start=c(element), count=c(1))
edep


print("bort")



# scratch notes below

#image.plot(t(values))

plot(date,values,xaxt="n",type="l")
axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))

sec = msec/1000
date = as.POSIXct("2018-01-01", tz="GMT") + sec # add POSIX prefix to date array to make proper dates
date = date[60:210]
str(msec)
str(date)
plot(msec,values, type="l")
plot(msec,values)
plot(date,values, type="l")
plot(date,values)
plot(date,values,xaxt="n")
axis(1, at=date[seq(1,8761,length=5)],labels=format(date[seq(1,8761,length=5)],"%b"))
units = ncatt_get(nc,var,"units")
units = units$value

# array of dates in POSIX format                                                                         
# start: 1999-01-01 00:00:00 | end: 1999-01-01 00:00:00 | time step is 3600 s                            
seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds                                           
date = as.POSIXct("1998-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates

#dim(esegs) = c( dim(esegs)[1]*dim(esegs)[2] ) 
# https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
#split(esegs, rep(1:ncol(esegs), each = nrow(esegs)))
#esegs = split(esegs, rep(1:nrow(esegs), each = ncol(esegs)))
