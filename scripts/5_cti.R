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
new <- c("Cies", "Lanzada", "Area Basta", "Coido Cuño", 
         "Lobeira", "Sorrizo", "Lobadiz", "San Pedro", "Tapia Casariego",
         "Campiechos", "Concha Artedo", "Luanco", "Vega", "Vidiago", "Oyambre", "Maruca",
         "Sonabia", "Zumaia")
old <- c("Ci", "La", "Ar", "Coi","Lobe", "Sor", "Loba", "Sa", "Ta",
         "Ca", "Con", "Lu", "Ve", "Vi", "Oy", "Ma","Son", "Zu") 
CTI_csv$Site[CTI_csv$Site %in% old] <- new[match(CTI_csv$Site, old, nomatch = 0)]
rm(old,new)
write.csv(CTI_csv, 
          "C:/Users/sainzvs/UNICAN/EMODNET BIO (PHASE V) - Documentos/02_Tareas/2.03_WP3/CTI/Analisis/Final_Products/cti/cti.csv",
          row.names = FALSE,
          fileEncoding = "UTF-16LE")

# Plots
# Full names
new <- c("Cies", "Lanzada", "Area Basta", "Coido Cuño", 
         "Lobeira", "Sorrizo", "Lobadiz", "San Pedro", "Tapia Casariego",
         "Campiechos", "Concha Artedo", "Luanco", "Vega", "Vidiago", "Oyambre", "Maruca",
         "Sonabia", "Zumaia")
old <- c("Ci", "La", "Ar", "Coi","Lobe", "Sor", "Loba", "Sa", "Ta",
         "Ca", "Con", "Lu", "Ve", "Vi", "Oy", "Ma","Son", "Zu") 
CTI_summary$Site[CTI_summary$Site %in% old] <- new[match(CTI_summary$Site, old, nomatch = 0)]
rm(old,new)
#Intermareal Medio
plot_CTImod_Med <- CTI_summary %>% 
  mutate(Site = factor(Site, levels=rev(c("Cies", "Lanzada", "Area Basta", "Coido Cuño", 
                                          "Lobeira", "Sorrizo", "Lobadiz", "San Pedro", "Tapia Casariego",
                                          "Campiechos", "Concha Artedo", "Luanco", "Vega", "Vidiago", "Oyambre", "Maruca",
                                          "Sonabia", "Zumaia")))) %>%
  filter(intertidal_level == "Mid intertidal") %>%
  ggplot(aes(y=as.factor(Site), x=CTI_mod_mean, fill=as.factor(yearcollected))) +
  geom_linerange(aes(xmin = CTI_mod_mean, xmax = CTI_mod_mean+CTI_mod_sd), position = position_dodge(.9), color = "black") +
  geom_col(position = position_dodge(), colour="black") +
  scale_fill_manual(values = c("white", "gray50", "gray10")) +
  xlim(0,18) +
  ggtitle("Mid intertidal") + xlab("CTI (ºC)") + ylab(NULL) + labs(fill=NULL, color=NULL) +
  #facet_wrap(~Typologies, scales = "free_y", ncol=1) +
  theme_bw() + 
  theme(legend.position='bottom')

#Intermareal Bajo
plot_CTImod_Low <- CTI_summary %>% 
  mutate(Site = factor(Site, levels=rev(c("Cies", "Lanzada", "Area Basta", "Coido Cuño", 
                                          "Lobeira", "Sorrizo", "Lobadiz", "San Pedro", "Tapia Casariego",
                                          "Campiechos", "Concha Artedo", "Luanco", "Vega", "Vidiago", "Oyambre", "Maruca",
                                          "Sonabia", "Zumaia")))) %>%
  filter(intertidal_level == "Low intertidal") %>%
  ggplot(aes(y=as.factor(Site), x=CTI_mod_mean, fill=as.factor(yearcollected))) +
  geom_linerange(aes(xmin = CTI_mod_mean, xmax = CTI_mod_mean+CTI_mod_sd), position = position_dodge(.9), color = "black") +
  geom_col(position = position_dodge(), colour="black") +
  scale_fill_manual(values = c("white", "gray50", "gray10")) +
  xlim(0,18) +
  ggtitle("Low intertidal") + xlab("CTI (ºC)") + ylab(NULL) + labs(fill=NULL, color=NULL) +
  #facet_wrap(~Typologies, scales = "free_y", ncol=1) +
  theme_bw() + 
  theme(legend.position='bottom')

final_plot <- plot_CTImod_Low | plot_CTImod_Med
final_plot <-final_plot + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

