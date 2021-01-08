# library for reading NetCDF files
require(ncdf4)
rm(list=ls()) # delete all variables

# setup working environment and read NetCDF file(s)
setwd("~/projects/fabm-demo/gotm-npzd-co3/variable_plots")
ncfnlist = Sys.glob("~/projects/fabm-demo/gotm-npzd-co3/out_netcdf/*.nc")
#ncfnlist = Sys.glob("~/projects/fabm-demo/gotm-npzd-co3/out_netcdf/gotm-npzd-carbonate-w_10-s_DAT-dic_2100.nc")

# loop over NetCDF file(s), plot variables
for (nn in 1:length(ncfnlist)) {
  ncfnprefix = tools::file_path_sans_ext(basename(ncfnlist[nn]))
  nc = nc_open(ncfnlist[nn], readunlim = FALSE) # read NetCDF file into R object 'nc'
  
  # make array of dates in POSIX format
  seconds = ncvar_get(nc, "time") # time in NetCDF is in seconds
  date = as.POSIXct("1998-01-01", tz="GMT") + seconds # add POSIX prefix to date array to make proper dates
  
  # specify variables
  varnames = c("temp","salt","carbonate_alk", "carbonate_CO2_flux","carbonate_pH",
               "npzd_nut", "npzd_phy", "carbonate_pCO2", "carbonate_dic", 
               "carbonate_Carb","npzd_zoo", "npzd_det", 
               "attenuation_coefficient_of_photosynthetic_radiative_flux_calculator_result",
               "carbonate_CarbA", "carbonate_Bicarb")
  
  # uncomment png() and graphics.off() to create .png files
  png(file=paste0(ncfnprefix,".png"), width=16, height=9, units="in", pointsize = 10, res=300)

  par(mfrow=c(3,5))
  par(mar=c(2,4,3,2)) # c(bottom, left, top, right)
  par(mgp=c(2,1,0)) # adjust plot axis text
  
  # loop: plot all variables into png
  for(ni in 1:length(varnames)){
    var = varnames[ni]
    values = ncvar_get(nc, var, start=c(1,1,1), count=c(1,1,-1))
    vunits = ncatt_get(nc,var,"units")
    vunits = vunits$value
    
    #plot(date,values,type="l",main=var,ylab=paste('[',vunits,']',sep=' '),xlab="")
    plot(date,values,type="l",ylab=paste('[',vunits,']',sep=' '),xlab="")
    title(var, line = 0.5)
    mtext(ncfnprefix, outer=TRUE, cex=1, line=-1.5) # page title
  }
}
graphics.off()