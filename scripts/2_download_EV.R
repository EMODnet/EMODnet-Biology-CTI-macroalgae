rm(list=ls())

library(openxlsx)
library(raster)
library(terra)
library(dplyr)
library(ggplot2)
library(biooracler)

a <- biooracler::list_layers(simplify = T) #Check all layers
info_layer("tas_baseline_2000_2020_depthsurf") #Get layer info

#Dataset selection and constraints - Download BO 3.0
dataset <- "thetao_baseline_2000_2019_depthsurf"
time = c('2010-01-01T00:00:00Z', '2010-01-01T00:00:00Z')
latitude = c(-89.975, 89.975)
longitude = c(-179.975, 179.975)
constraints = list(time, latitude, longitude)
names(constraints) = c("time", "latitude", "longitude")
variables <- c("thetao_ltmax", "thetao_ltmin", "thetao_mean", "thetao_range")

download_layers(dataset_id = dataset,
                variables = variables,
                fmt = "raster",
                constraints = constraints ,
                directory = "D:/Proyectos/EMODNET_DP2/Analisis/Environmental_Variables/raw")

# bati <- "terrain_characteristics"
# time = c("1970-01-01T00:00:00Z", "1970-01-01T00:00:00Z")
# latitude = c(-89.975, 89.975)
# longitude = c(-179.975, 179.975)
# constraints = list(time, latitude, longitude)
# names(constraints) = c("time", "latitude", "longitude")
# variables <- c("bathymetry_max", "bathymetry_min", "bathymetry_mean")


batiraw <- raster(terra::rast("D:/Modelado/Modelos/GIS/GEBCO_2019/GEBCO_2019.nc")) # Bathymetry from GEBCO 15arc-second (downloaded from: https://download.gebco.net/)
temp_data <- stack(terra::rast("D:/Proyectos/EMODNET_DP2/Analisis/Environmental_Variables/raw/Air_Temp_2010.nc")) # Load downloaded SST BO variables

# Coastal mesh
extent100m <- Which(batiraw > -100 & batiraw < 0, cells = FALSE) #Select cells between 0 and 100m depth
writeRaster(extent100m, 
            format = "ascii", 
            filename = "D:/Proyectos/EMODNET_DP2/Analisis/Environmental_Variables/extent100m/extent100m.asc")

# check extents and resolutions
extent(extent100m)
extent(temp_data[[1]])
xres(extent100m) 
xres(temp_data[[1]]) 

# Homogeneize extension and resolution from BO and coastal GEBCO
SST <- crop(temp_data, extent(extent100m), keepres=TRUE)
mask100m <- resample(x=extent100m, y=temp_data[[1]], method="bilinear")
xres(mask100m)
mask100m[mask100m == 0] <- NA
NAvalues <- mask100m*SST[[1]] # To propagate NA values
SST_vars <- mask(SST, NAvalues) # Remove pixels with depths far from macroalgae potential habitats (>100m depth)

for (i in 1:length(SST_vars@data@names)) {
  writeRaster(SST_vars[[i]], 
              format = "ascii", 
              filename = paste0("D:/Proyectos/EMODNET_DP2/Analisis/Environmental_Variables/Air_Temp/", SST_vars[[i]]@data@names, "_2010.asc"))
}

rm(time, variables, longitude, latitude, dataset, constraints, NAvalues)
