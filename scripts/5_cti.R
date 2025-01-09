rm(list=ls())


library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

Data <- openxlsx::read.xlsx("C:/Users/sainzvs/UNICAN/EMODNET BIO (PHASE V) - Documentos/02_Tareas/2.03_WP3/CTI/Analisis/Intertidal_Data/Datos_IntertidalOBIS_rev.xlsx", sheet = 1)
Data$parameter_value <- as.double(Data$parameter_value)
Data$quad <- ifelse(Data$Plot == "a", "C1",
                    ifelse(Data$Plot == "b", "C2", 
                           ifelse(Data$Plot == "c", "C3",
                                  ifelse(Data$Plot == "d", "C4",
                                         ifelse(Data$Plot == "e", "C5","C6")))))
Data$CODIGO <- paste(Data$Site,Data$Transect,Data$quad,Data$Year, sep="")
Data$CODIGO2 <- paste(Data$Site,Data$Transect,Data$quad, sep="")
Data$code <- paste0(Data$CODIGO,"_",Data$valid_name)
Data$Dups <- duplicated((Data$code))
Data[Data$parameter_value < 1,]$parameter_value <- 1 #Those cover below 1% are changed to 1%
Data_nodups <- Data %>%
  group_by(yearcollected, Site, Transect, Plot, quad, intertidal_level, decimallongitude, decimallatitude, aphiaidaccepted, scientificnameaccepted, CODIGO, CODIGO2, code) %>%
  summarize(cover = sum(parameter_value)) #Group species with different morphological phases
Data_nodups <- Data_nodups[Data_nodups$yearcollected %in% c(2011,2017),]  

STI <- openxlsx::read.xlsx("C:/Users/sainzvs/UNICAN/EMODNET BIO (PHASE V) - Documentos/02_Tareas/2.03_WP3/CTI/Analisis/Indices/STI_MAXENT_minmax.xlsx", sheet = 1)
Data_full <- left_join(Data_nodups, STI, by = join_by(scientificnameaccepted == name))
Data_full <- na.omit(Data_full) 

Cover <- Data_full %>%
  group_by(yearcollected, CODIGO2)%>%
  summarise(full_cover = sum(cover))
Data_full <- left_join(Data_full, Cover, by = c("yearcollected", "CODIGO2"))

Data_full$weights <- Data_full$cover/Data_full$full_cover
Data_full$STIw_mod <- Data_full$STI_mod*Data_full$weights
Data_full$STRw_mod <- Data_full$STR_mod*Data_full$weights
Data_full$STIw_obs <- Data_full$STI_obs*Data_full$weights
Data_full$STRw_obs <- Data_full$STR_obs*Data_full$weights

CTI <- Data_full %>%
  group_by(yearcollected, Site, intertidal_level, CODIGO2)%>%
  summarise(CTI_mod = sum(STIw_mod),
            CTR_mod = sum(STRw_mod),
            CTI_obs = sum(STIw_obs),
            CTR_obs = sum(STRw_obs))

CTI_summary <- CTI %>%
  group_by(yearcollected, Site, intertidal_level)%>%
  summarise(CTI_mod_mean = mean(CTI_mod), CTI_mod_sd = sd(CTI_mod), CTI_mod_n = n(),
            CTR_mod_mean = mean(CTR_mod), CTR_mod_sd = sd(CTR_mod), CTR_mod_n = n(),
            CTI_obs_mean = mean(CTI_obs), CTI_obs_sd = sd(CTI_obs), CTI_obs_n = n(),
            CTR_obs_mean = mean(CTR_obs), CTR_obs_sd = sd(CTR_obs), CTR_obs_n = n())

# Coordinates of each site
Coord <- Data_full[Data_full$Transect == "T1" & Data_full$yearcollected == "2011",] #T1 exists in all sites, so their coordinates are used to define the area
Coord <- Coord[,c(2,7,8)]
Coord <- unique(Coord)

# Write csv
CTI_csv <- CTI_summary[,c(1,2,3,4)]
CTI_csv <- left_join(CTI_csv, Coord)
CTI_csv <- CTI_csv[,c(5,6,1:4)]
names(CTI_csv) <- c("Longitude", "Latitude", "Year", "Site", "Intertidal_Level", "CTI")
write.csv(CTI_csv, 
          "C:/Users/sainzvs/UNICAN/EMODNET BIO (PHASE V) - Documentos/02_Tareas/2.03_WP3/CTI/Analisis/Final_Products/cti/cti.csv",
          row.names = FALSE,
          fileEncoding = "UTF-16LE")