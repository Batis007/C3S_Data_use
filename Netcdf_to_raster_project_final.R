####################################################################
# Eduardo Batista                                                  #
# Project MDR                                                      #
# PhD Program in Biology and Ecology of Global Change BEGC         #
# 28-03-2019                                                       #
####################################################################

# Info & tutorial
# https://www.youtube.com/watch?v=roMf6xzB9NI
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm
# http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

# Loading packages
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf) # package for sf objects
library(openxlsx) # package for xlsx data

#   load script and define file paths
main_dir <- "Define your working directory"
setwd(main_dir)
# I recommend you to create a folder named datasets and outputs inside the main dir
datasets <- paste(getwd(), 'datasets', sep ='/') #optional
outputs <- paste(main_dir, 'outputs', sep = '/') #optional
setwd(datasets) # set your working directory

#Open the nc file 
nc_data <- nc_open('t2m2years.nc')

# Save the print(nc) dump to a text file 
{
  sink('t2m2years_nc_metadata.txt')
  print(nc_data)
  sink()
}
# Check the nc attributes file in your working directory

#Loading all the dimensions 
lon <- ncvar_get(nc_data, "longitude")
lon = ifelse(lon < 360,-(360 - lon), lon)
lat <- ncvar_get(nc_data, "latitude", verbose = F)
t <- ncvar_get(nc_data, "time")

t2m.array <- ncvar_get(nc_data, "t2m") # store the data in a 3-dimensional array

t2m.array <- t2m.array - 273.15 # Transform K in C
dim(t2m.array)

#Fill NA values

fillvalue <- ncatt_get(nc_data, "t2m", "_FillValue")
fillvalue

nc_close(nc_data) 
t2m.array[t2m.array == fillvalue$value] <- NA

# Calculate mean of brick
r_brick <-
  brick(
    t2m.array,
    xmn = min(lat),
    xmx = max(lat),
    ymn = min(lon),
    ymx = max(lon),
    crs = CRS(
      "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
    )
  )

mean <- calc(r_brick, fun = mean, na.rm = T)
plot(mean)

#Transpose raster
mean <- t(mean)

# Plot Map
plot(mean)

# Save map
setwd(outputs)
writeRaster(x = r, filename = "ncdf_testfinal.tif", driver = "GeoTiff")

#Crop map and plot again
r1 <- crop(mean, extent(-180,-0.25,-90,90))
r2 <- crop(mean, extent(-360,-180,-90,90))

#Change the extent of r1
r <- raster()
bb <- extent(-180,-0.25,-90,90)
extent(r1) <- bb
r1 <- setExtent(r1, bb, keepres=TRUE)

#Change the extent of r2
r <- raster()
bb <- extent(-0.25,180,-90,90)
extent(r2) <- bb
r2 <- setExtent(r2, bb, keepres=TRUE)

#Join r1 and r2
r <- merge(r1,r2, overlap = TRUE)
plot(r)


#Extract polygon
setwd(datasets)
PT <- readOGR("Portugal_distritos.shp")
cropped <- crop(x = r, y = extent(PT))
plot(cropped)
plot(PT, add=TRUE)

masked <- mask(x = r, mask = PT)
cropped <- crop(x = masked, y = extent(PT))
plot(cropped)
plot(PT, add=TRUE)

#Save Plot
setwd(outputs)
writeRaster(x = r, filename = "ncdf_pt.tif", driver = "GeoTiff")




#Extract values by location
#Load coordinates points
BOT <-
  read.xlsx(
    '03-06-2019_netcdf_coords_to_extract.xlsx',
    sheet = 1,
    startRow = 1,
    colNames = TRUE
  )
#Prepare a data frame
BOT1 <-
  data.frame(
    id = BOT$id,
    genus = BOT$genus,
    sp = BOT$sp,
    lat = BOT$lat,
    lon = BOT$lon
  )

#Clean data
BOT1 <- BOT1[complete.cases(BOT1), ] # remove NA cells
BOT2 <- data.frame(lon = BOT1$lon, lat = BOT1$lat)

# make BOT spatial
coordinates(BOT2) <- ~lon + lat

# get the clim data
t2m <- extract(r, BOT2)
t2m1 <- cbind(BOT1, t2m) 
# Check the NA values for modeling
t2m1 <- t2m1[complete.cases(t2m1),] # remove NA cells

#Plot
plot(cropped)
plot(PT, add=TRUE)
plot(BOT2, add=TRUE)

#Save data
setwd(outputs)
write.xlsx(t2m1,"data_points.xlsx")
