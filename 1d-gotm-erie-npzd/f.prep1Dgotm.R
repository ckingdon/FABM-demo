
# pre-processing/extracting GOTM 1D time profile

require(ncdf4) # nc_open(), ncvar_get()
#require(fields) # image(), image.plot()

#rm(list=ls()) # delete all variables
# ncfn # starttime # stoptime # startdepth # stopdepth # gotmdt
#timestart = "2018-05-31 06:00:00"
#timestop = "2018-10-30 13:00:00"
#ncvar = "temp"

prep1Dgotm <- function(ncfn,ncvar,timestart,timestop,depthstart,depthstop){
  
  #setwd("~/projects/fabm-demo/")
  #ncfn = "./1d-gotm-erie-npzd/out_netcdf/erie-30.nc"
  #ncfn = "./1d-gotm-nnsa-npzd/out_netcdf/nnsa-npzd.nc"
  nc = nc_open(ncfn, readunlim = FALSE) # read NetCDF file into R object 'nc'
  
  # check for missing arguments 
  if(missing(ncfn)){
    stop("netCDF file name must be provided")
  }
  if(missing(ncvar)){
    stop("netCDF variable name must be provided")
  }
  if(missing(timestart)){
    message("using t0")
    timestart=NULL
    #message(paste("timestart is.null?: ",is.null(timestart)))
  }
  if(missing(timestop)){
    message("using last time")
    timestop=NULL
  }
  if(missing(depthstart)){
    message("using shallowest depth")
  }
  if(missing(depthstop)){
    message("using deepest depth")
  }
 
  
  # date/time management
  time0 = ncatt_get(nc,"time","units") # returns "seconds since" start time
  #message("time0: ",time0)
  time0date = regmatches(time0$value,regexpr("\\d{4}-\\d{2}-\\d{2}", time0$value)) # get date string
  time0time = regmatches(time0$value,regexpr("\\d{2}:\\d{2}:\\d{2}", time0$value))  # get time string
  time0posix = as.POSIXct(paste0(time0date," ",time0time), tz="UTC") 
  secondssince = ncvar_get(nc, "time") # time in NetCDF is in seconds
  posixtime = as.POSIXct(paste0(time0date," ",time0time), tz="UTC") + secondssince # add POSIX prefix to date array to make proper dates
  dt = secondssince[2] - secondssince[1] # calculate GOTM dt timestep
  #message(dt)
  
  # indices of start and stop times 
  if(is.null(timestart)){message("timestart is.null");timestart = time0posix[1]}
  #message("timestart: ",timestart) 
  idxtstart = as.POSIXct(timestart) - time0posix  # returns "Time difference of __ days"
  #message(idxtstart)
  idxtstart = as.numeric(idxtstart) # number of days
  #message(idxtstart)
  idxtstart = idxtstart / (dt/60/60/24) + 1  # + 1 bc R indices start at 1, not 0
  #message(idxtstart)
  if(is.null(timestop)){message("timestop is.null"); timestop = posixtime[length(posixtime)]}
  #message("timestop: ",timestop) 
  idxtstop = as.POSIXct(timestop) - as.POSIXct(timestart) # returns "Time difference of __ days"
  #message(idxtstop)
  idxtstop = as.numeric(idxtstop) # number of days
  #message(idxtstop)
  idxtstop = idxtstart + (idxtstop / (dt/60/60/24))  # + 1 bc R indices start at 1, not 0
  #message(idxtstop)
  posixtime = posixtime[c(idxtstart:idxtstop)]  # crop DATE to date range
  vec_posixdate = posixtime
 
  # depth managment 
  depth = ncvar_get(nc, "z") # depth in NetCDF is in meters
  depth = depth[,1]
  vec_depth = depth
  # add functionality to crop depth later
  
  # NetCDF variable
  # future: add functionality to take vector containing many variables
  str_ncvarname = ncvar
  units = ncatt_get(nc,ncvar,"units") 
  units = units$value
  str_ncvarunits = units
  values = ncvar_get(nc, ncvar, start=c(1,1,1,1), count=c(-1,-1,-1,-1))
  values = values[,c(idxtstart:idxtstop)]
  #matrix_ncvarvalues = t(as.matrix(values)) # transpose in prep for image.plot
  matrix_ncvarvalues = as.matrix(values)
 
  #return(vec_posixdate, vec_depth, matrix_ncvarvalues, str_ncvarunits)
  list_return = list(vec_posixdate, vec_depth, matrix_ncvarvalues, str_ncvarname, str_ncvarunits)
  #names(list_return) = c("datetime","depth",paste0(ncvar,"_values"),paste0(ncvar,"_units"))
  names(list_return) = c("datetime","depth","values","name","units")
  return(list_return)
}
