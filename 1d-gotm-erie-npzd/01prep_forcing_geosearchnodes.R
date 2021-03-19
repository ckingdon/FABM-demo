# library for reading NetCDF files
require(ncdf4) # nc_open(), ncvar_get()
require(fields) # image(), image.plot()
require(PBSmapping)
rm(list=ls()) # delete all variables
graphics.off()

# setup working environment and read NetCDF file(s)
setwd("~/projects/fabm-demo/1d-gotm-erie-npzd/")
nc = nc_open("erie_forcing_interp.nc", readunlim = FALSE) # read NetCDF file into R object 'nc'
#str(nc)
#attributes(nc)$names

load("~/projects/chrp_moorings/chrp2018.Rdata")
str(moorings2018)
moorings2018$lon
moorings2018$lat
chrp4LL=moorings2018[which(moorings2018$Station=='4'),c("lon","lat")][1,]
chrp4LL

# SPATIAL CONTEXT
# ELEMENT is centroid/id of triangle
# NODES are vertices of triangle

# plot points on map using Peter's Rdata geo data
load(file=paste0("~/projects/erie_fvcom_grid/erie05lcc_grid_coords.Rdata"))
load(file=paste0("~/projects/erie_fvcom_grid/erie05lcc_grid_polys.Rdata"))
znode1=3630
znode2=2809
znode=2809
#png(file="~/projects/zzgraphics/2021-03-02/FVCOM-elements-overview.png", 
#    width=4, height=3, units="in", pointsize = 7, res=300)
plotPolys(shorelineLL)
#points(coordsn$lon[znode],coordsn$lat[znode], pch=17, cex=1.75, col="lightblue")
#text(coordsn$lon[znode],coordsn$lat[znode],labels=coordsn$node[znode],cex=0.7, col="blue4")
points(coordsn$lon[znode1],coordsn$lat[znode1], pch=17, cex=1.75, col="lightblue")
text(coordsn$lon[znode1],coordsn$lat[znode1],labels=coordsn$node[znode1],cex=0.7, col="blue4")
points(coordsn$lon[znode2],coordsn$lat[znode2], pch=17, cex=1.75, col="lightblue")
text(coordsn$lon[znode2],coordsn$lat[znode2],labels=coordsn$node[znode2],cex=0.7, col="blue4")
points(coordsn[c(5472,5509,2868),c("X","Y")]/1000, pch = 16)
#points(chrp4LL, pch=15, cex=1.75, col="red")
#dev.off()
points(chrp4LL, pch=18, cex=2.75, col="green")
text(chrp4LL,labels="CHRP4",cex=0.7, col="black")
#bnds = locator(2) # pick bounding zoom area
# zoom to area around node 8
#znode=8
znode=3630
znode=2809
zcoords=c(coordsn$lon[znode],coordsn$lat[znode])
zcoords=chrp4LL
#zscale = 0.0001
zscale = 0.0009
#zscale = 0.0025
bnds = list(x=c(zcoords[1] - zcoords[1]*zscale, zcoords[1] + zcoords[1]*zscale), 
            y=c(zcoords[2] - zcoords[2]*zscale, zcoords[2] + zcoords[2]*zscale) )
png(file="~/projects/zzgraphics/2021-03-02/FVCOM-elements-nodes.png", 
    width=4, height=3, units="in", pointsize = 7, res=300)
plotPolys(shorelineLL, xlim=range(bnds$x), ylim=range(bnds$y), lwd=3, border="pink")
points(coordsn$lon,coordsn$lat, pch=17, cex=1.75, col="lightblue")
text(coordsn$lon,coordsn$lat,labels=coordsn$node,cex=0.7, col="blue4")
points(coordse$lon,coordse$lat, pch=16, cex=1.5, col="pink")
text(coordse$lon,coordse$lat,labels=coordse$node,cex=0.7, col="red4")
esegs = seq(1:length(coordse$lon))
esegs = ncvar_get(nc, "nv", start=c(esegs[1],1), count=c(length(esegs),-1))
#esegs = seq(1:30)
#esegs = ncvar_get(nc, "nv", start=c(esegs[1],1), count=c(length(esegs),-1))
# put polygons on map
apply(esegs, 1, function(x) polygon(coordsn$lon[x],coordsn$lat[x],lwd=0.5, lty=3))
# put polygons on maps: another way 
#for (t in (1:nrow(esegs))){
#  trow = esegs[t,]
#  polygon(coordsn$lon[trow],coordsn$lat[trow],lwd=0.5, lty=3)
#}
# better to use 'apply`
#apply(esegs, 1, mean)
chrp4LL
points(chrp4LL, pch=18, cex=2.75, col="green")
text(chrp4LL,labels="CHRP4",cex=0.7, col="black")
dev.off()

# confirm point ids are same between Peter's Rdata and erie_forcing_interp.nc
# get lon / lat for nodes from erie_forcing_interp.nc
nlon = -(360 - ncvar_get(nc, "lon", start=c(1), count=c(-1)))
nlat = ncvar_get(nc, "lat", start=c(1), count=c(-1))
nid = c(1:length(nlon))
# get lon / lat for elements
elon = -(360 - ncvar_get(nc, "lonc", start=c(1), count=c(-1)))
elat = ncvar_get(nc, "latc", start=c(1), count=c(-1))
eid = c(1:length(elon))
points(nlon,nlat, pch=17, cex=0.75, col="blue")
text(nlon,nlat,labels=nid,cex=0.75, col="blue")
points(elon,elat, pch=16, cex=0.75, col="red")
text(elon,elat,labels=eid,cex=0.7,col="red")


# (time, nele) vs (time, node)
# get the three nodes associated with an element
# or, get the element that is nearest to the node
# netcdf variable nv is "nodes surrounding element"
element=c(6909) # directly south of node 3630
nodes = ncvar_get(nc, "nv", start=c(element,1), count=c(1,-1))
elements=c(6840,6909,6910,6911)
# matrix of three nodes surroundign elements from elements=c()
sapply(elements, function(x) ncvar_get(nc, "nv", start=c(x,1), count=c(1,-1)))
enodes = sapply(elements, function(x) ncvar_get(nc, "nv", start=c(x,1), count=c(1,-1)))
colnames(enodes) = as.character(elements)
enodes
