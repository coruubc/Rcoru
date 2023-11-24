rm(list=ls())
#function to check if a package is installed
install_if_missing <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  }
}

#check installed required functions
install_if_missing("raster")
install_if_missing("rgdal")
install_if_missing("sp")
install_if_missing("gstat")
install_if_missing("rgeos")
install_if_missing("rnaturalearth")
install_if_missing("ncdf4")

#call libraries
library(raster)
library(rgdal)
library(sp)
library(gstat)
library(rgeos)
library(rnaturalearth)
library(ncdf4)

#open netcdf file
#ncdffile<-c("/Users/william/CORU Dropbox/Cheung, William/Mac/Downloads/ESM_TEST/o2_Omon_GFDL-ESM4_ssp245_r1i1p1f1_gr_201501-210012.nc_btm.nc_regrid.nc")
ncdffile<-c("/Users/william/Library/CloudStorage/GoogleDrive-wwlcheung@gmail.com/My Drive/Projects/DBEM Development/ESM_TEST/o2_Omon_GFDL-ESM4_ssp126_r1i1p1f1_gr_185001-210012.nc_btm.nc_regrid.nc")
varncdf<-nc_open(ncdffile)

#read in variable names !!!!need to check careful as variable names differ between files!!!
#Time 
varname_Time<-varncdf$dim[[4]]$name

#Climate variable
varname=varncdf$var[[4]]$name

#Lontitude
varname_Lon<-varncdf$dim[[1]]$name

#Latittude
varname_Lat<-varncdf$dim[[3]]$name

#Read variable size !!! check carefully as the list index differs between files
varsize  = varncdf$var[[4]]$varsize  

#mising values code
missval=varncdf$var[[2]]$missval

#get latitude array
lat <- ncvar_get(varncdf, varname_Lat)

#get longitude array
lon <- ncvar_get(varncdf, varname_Lon)

#get time array
timeframe <- ncvar_get(varncdf, varname_Time)

#Open a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

#Create a 1 degree resolution global raster
raster_1D <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, res=1)


#rasterize world map into 1 degree resolution raster
land_mask_1D <- rasterize(world, raster_1D, field=NA, fun='first', background=-9999)

#Create a 0.5 degree resolution global raster
new_raster <- raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90,resolution = 0.5,vals=-9999,crs=CRS("+proj=longlat +datum=WGS84"))

#rasterize world map into 0.5 degree resolution raster
raster_0_5D <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, res=0.5)

land_mask_0_5D <- rasterize(world, raster_0_5D, field=NA, fun='first', background=NA)

#mask the 0.5 degree raster by land
new_data_masked <- mask(new_raster, land_mask_0_5D)

#loop by year (12 months)
for (yr in 1:(varsize[3]/12)){
    #start time step of netcdf extraction
    ti_start=(yr-1)*12+1
    start = c(1,1,ti_start)
    count = c(varsize[1],varsize[2],12)
    
    #get climate variable (12 monthly layers)
    vartemp = ncvar_get(nc=varncdf,varname,start,count)
    
    #Apply mean to calculate average annual
    vartemp<-apply(vartemp, c(1, 2), mean,na.rm=T)
    
    #rotate grid to correct orientation
    vartemp<-t(vartemp[,c(180:1)])
    
    #make sure missing or inappropriate values are set as NA
    vartemp[vartemp==missval] <- NA
    
    #no negative value for oxygen (disable for temperature)
    vartemp[vartemp<0]<-NA
    
    #nc_close(varncdf)
    
    #rasterize the extracted variable
    raw_raster <- raster(vartemp, xmn=0, xmx=360, ymn=-90, ymx=90, crs=CRS("+proj=longlat +datum=WGS84"))
    
    #change from 0 - 360 degree to -180 - 180 degree format
    rotated_raster <- rotate(raw_raster)
    
    #apply land mask
    ocean_data_masked <- mask(rotated_raster, land_mask_1D)
    
    #interpolate the data using bilinear method
    interpolated_data <- resample(ocean_data_masked, new_data_masked, method="bilinear")
    
    #store data
    array_data<-getValues(interpolated_data)
    if(yr==1){
      array_data_fnl=array_data 
    } else {
      array_data_fnl=cbind(array_data_fnl,array_data)
    }
}

#Make sure land area in our grid system has NA value
warea<-read.table("/Users/william/Library/CloudStorage/GoogleDrive-wwlcheung@gmail.com/My Drive/Projects/Nutrition/Data/WArea.txt")

array_data_fnl[warea[,1]==0,]<-NA

save(array_data_fnl,
     file=paste("/Users/william/Library/CloudStorage/GoogleDrive-wwlcheung@gmail.com/My Drive/Projects/DBEM Development/ESM_TEST/",
                varname,"_GFDL26",".RData",sep="")
)





