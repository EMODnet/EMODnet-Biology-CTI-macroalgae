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

## Open occurrence data
occ_data_thinned <- openxlsx::read.xlsx("D:/Proyectos/EMODNET_DP2/Analisis/Occurrences/Occurrences_thinned_corr.xlsx", sheet = 1)
occ_data_thinned <- occ_data_thinned[,c(1,2,3)]

## Open temperature data
SST_mean <- raster("D:/Proyectos/EMODNET_DP2/Analisis/Environmental_Variables/SST/thetao_mean_2010.asc")

temp_data_max <- raster("D:/Proyectos/EMODNET_DP2/Analisis/Environmental_Variables/SST/thetao_ltmax_2010.asc")
temp_data_min <- raster("D:/Proyectos/EMODNET_DP2/Analisis/Environmental_Variables/SST/thetao_ltmin_2010.asc")
expl.var <- stack(temp_data_max, temp_data_min)
rm(temp_data_min, temp_data_max)
plot(expl.var)

bb <- occ_data_thinned %>%
  dplyr::group_by(name) %>%
  count()
rm(bb)

## Model thermal distribution
STI_species_full <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), c('name', 'STI_mod', 'STR_mod', 'IQR_mod', 'STI_obs', 'STR_obs', 'IQR_obs'))
model_evaluations <- list()
error <- setNames(data.frame(matrix(ncol = 1, nrow = 1)), c('error'))

for (i in unique(occ_data_thinned$name)) {
   a <- occ_data_thinned %>%
    dplyr::filter(name == i)
   
  if(length(a$name) < 25) {
    e <- paste(i, "has not enough occurrences")
    error <- rbind(error, e)
    
  }  else {
    a <- a[,-1]
    a <- na.omit(a)
    a$Occurrence <- 1
    coordinates(a) <- c('Longitude','Latitude')
    sp_name <- i
    
    biomod_format <- BIOMOD_FormatingData(resp.var = a,
                                          expl.var = expl.var,
                                          resp.name = sp_name,
                                          PA.nb.rep = 1, 
                                          PA.nb.absences = 10000,
                                          PA.strategy = 'random')
    biomod_param <- BIOMOD_ModelingOptions(GLM = list(type = 'quadratic', 
                                                      interaction.level = 0, 
                                                      myFormula = NULL, 
                                                      test = 'AIC', 
                                                      family = 'binomial'),
                                           MAXENT.Phillips = list(path_to_maxent.jar = 'D:/Modelado/Modelos/2_Maxent_3_4_1/maxent',
                                                                          product = FALSE, 
                                                                          threshold = FALSE,
                                                                          linear = TRUE,
                                                                          quadratic = TRUE,
                                                                          hinge = TRUE,
                                                                          betamultiplier = 1,
                                                                          maximumiterations = 1000))
    biomod_model <- BIOMOD_Modeling(data = biomod_format,
                                    models = c('MAXENT.Phillips'),
                                    models.options = biomod_param,
                                    NbRunEval = 1,
                                    DataSplit=75,
                                    Prevalence = 0.5, 
                                    VarImport = 5, 
                                    do.full.models = FALSE,
                                    models.eval.meth = c('TSS','ROC'))
    biomod_projection <- BIOMOD_Projection(modeling.output = biomod_model,
                                           new.env = expl.var,
                                           proj.name = 'Historico',
                                           build.clamping.mask = FALSE,
                                           selected.models = 'all',
                                           binary.meth = c('TSS'))
    evaluation <- get_evaluations(biomod_model)
    model_evaluations[[i]] <- evaluation
    
    binary <- raster(gsub(".grd", "_TSSbin.grd", biomod_projection@proj@link))
    binary <- as.data.frame(binary, xy=TRUE)
    binary <- na.omit(binary)
    binary$temps <- raster::extract(SST_mean, binary[,-3])
    binary <- binary[binary$layer == 1,]
    binary$species <- i
    binary <- binary[,c(5,4)]
    
    STI_species <- setNames(data.frame(matrix(ncol = 1, nrow = 1)), c('name'))
    STI_species$name <- i
    STI_species$STI_mod <- median(binary$temps)
    STI_species$STR_mod <- quantile(binary$temps, 0.9) - quantile(binary$temps, 0.1)
    STI_species$IQR_mod <- IQR(binary$temps)
    b <- na.omit(raster::extract(SST_mean, a))
    STI_species$STI_obs <- median(b)
    STI_species$STR_obs <- quantile(b, 0.9) - quantile(b, 0.1)
    STI_species$IQR_obs <- IQR(b)
    
    STI_species_full <- rbind(STI_species_full, STI_species)
    
    rm(a,b, sp_name, biomod_format, biomod_param, biomod_model, biomod_projection, evaluation, binary, STI_species)
  }
}

error[-1,]

STI_species_full <- STI_species_full[-1,]

write.xlsx(STI_species_full, "D:/Proyectos/EMODNET_DP2/Analisis/Indices/STI_MAXENT_minmax.xlsx")

