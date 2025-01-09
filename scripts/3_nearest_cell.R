rm(list=ls())

library(gam)
library(openxlsx)
library(raster)
library(terra)
library(spThin)
library(biomod2)
library(dplyr)
library(ggplot2)
library(biooracler)
library(sf)

setwd("D:/Proyectos/EMODNET_DP2/Analisis/Models")

# This step is optional and requires expert checking. Some records may be identified very close to mesh limits (i.e. intertidal regions) and
# fall outside the mesh. This script is to assign those records to the nearest cell/pixel. However, Some records might be missidentifications 
# or errors and should be manually removed. Visual inspection of occurrences is strongly recommended before running the scripts.

## Open occurrence data
occ_data_thinned <- na.omit(openxlsx::read.xlsx("D:/Proyectos/EMODNET_DP2/Analisis/Occurrences/Occurrences_thinned.xlsx", sheet = 1))


## Open temperature data
SST_mean <- raster("D:/Proyectos/EMODNET_DP2/Analisis/Environmental_Variables/SST/thetao_mean_2010.asc")


## Extract values
temp <- raster::extract(SST_mean, occ_data_thinned[-1])
occ <- cbind(occ_data_thinned, temp)
rm(occ_data_thinned, temp)

occ %>%
  filter(is.na(temp)) %>%
  count()

## Assign occurrences outside the mesh to the closest mesh cell
occ_sf <- st_as_sf(occ, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
malla <- na.omit(raster::as.data.frame(SST_mean,xy=TRUE))
malla_sf <- st_as_sf(malla, coords = c("x", "y"), crs = 4326, remove = FALSE)
rm(malla)

i=1
for (i in 1:length(occ_sf$temp)) {
  if(is.na(occ_sf$temp[i])) {
        nearest_index <- apply(st_distance(occ_sf[i,], malla_sf), 1, which.min)
        closest_points <- malla_sf[nearest_index,]
        occ_sf$Longitude[i] <- closest_points$x
        occ_sf$Latitude[i] <- closest_points$y
        occ_sf$temp[i] <- closest_points$thetao_mean
        rm(nearest_index, closest_points)
        print(i)
        
  } else {
    next
  }
}

occ_corr <- as.data.frame(occ_sf)[,c(1,2,3,4)]
occ_corr <- unique(occ_corr, by="name")
occ_corr_save <- occ_corr[,c(1,2,3)]

write.xlsx(occ_corr_save, "D:/Proyectos/EMODNET_DP2/Analisis/Occurrences/Occurrences_thinned_corr.xlsx")

## Remove outlier values to reduce the risk of including possible errors and missidentifications. It could not replace visual inspection.
## Be careful with rare species, species with a low number of records or species in expansion.

a <- occ_corr %>%
  group_by(name) %>%
  summarize(p25= quantile(temp, probs=c(.25), na.rm = TRUE),
            p50= quantile(temp, probs=c(.50), na.rm = TRUE),
            p75= quantile(temp, probs=c(.75), na.rm = TRUE),
            IQR = IQR(temp, na.rm = TRUE),
            lower = quantile(temp, probs=c(.25), na.rm = TRUE)-3*IQR(temp, na.rm = TRUE),
            upper = quantile(temp, probs=c(.75), na.rm = TRUE)+3*IQR(temp, na.rm = TRUE))

occ_corr_2 <- left_join(occ_corr, a, by = "name")

bb <- occ_corr_2 %>%
  filter(temp < lower | temp > upper) %>%
  group_by(name) %>%
  count()
rm(bb)

occ_no_outlier <- occ_corr_2 %>%
  filter(temp > lower & temp < upper)

occ_no_outlier_save <- occ_no_outlier[,c(1,2,3)]
write.xlsx(occ_no_outlier_save, "D:/Proyectos/EMODNET_DP2/Analisis/Occurrences/Occurrences_thinned_corr_no.xlsx")
