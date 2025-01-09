rm(list=ls())

#devtools::install_github("lifewatch/eurobis")

library(emodnet.wfs)
library(dplyr)
library(stringr)
library(spocc)
library(openxlsx)
library(spThin)

#### Data processing #####

# Download and format dataset including the following data:
# Cover of Intertidal Macroalgae along the N and NW Coast of the Iberian Peninsula (dasid = 5973)
wfs_bio <- emodnet_init_wfs_client("biology_occurrence_data")
a <- emodnet_get_layers(
  wfs_bio,
  layers = "eurobis-obisenv",
  viewParams = "datasetid%3A5973",
  simplify = TRUE
)
a <- as.data.frame(a)
a <- a[a$parameter_original_measurement_type == "Cover",]
a <- a[,-which(names(a) %in% c("id", "gml_id","event"))]
a <- unique(a)
var_list <- c("eventid", "yearcollected", "decimallongitude",
              "decimallatitude", "scientificnameaccepted",
              "parameter", "parameter_value", "aphiaidaccepted")
a <- a %>% select(all_of(var_list))

b <- as.data.frame(str_split_fixed(a$eventid, "_", 2)) 
names(b) <- c("Area", "Code")

c <- cbind(b,a)
c$Code <- gsub('\\B([[:upper:]])', '_\\1', c$Code)
c$Code <- gsub('([0-9])([[:alpha:]])', '\\1_\\2', c$Code)

cover_data <- as.data.frame(str_split_fixed(c$Code, "_", 4))
cover_data <- cbind(cover_data,c)
names(cover_data)[1:4] <- c("Site", "Transect", "Plot", "Year")

cover_data$intertidal_level <- ifelse(cover_data$Plot %in% c("a","b","c"), "Low intertidal", 
                                      ifelse(cover_data$Plot %in% c("d","e","f"), "Mid intertidal", "High intertidal"))
cover_data$aphiaidaccepted <- gsub('.*(=)([0-9])', '\\2', cover_data$aphiaidaccepted)

rm(a,b,c,var_list)

cover_data <- cover_data[!is.na(cover_data$scientificnameaccepted),]
cover_data <- cover_data[!(cover_data$Site == "Ra"|cover_data$Site == "Po"|cover_data$Site == "Fr"),]
cover_data <- cover_data[!(cover_data$intertidal_level == "High intertidal"),]

# Homogenize data and correct/modify specific records from the dataset
cover_data[(cover_data$yearcollected == 2011 
                           & cover_data$Site == "Ci" 
                           & cover_data$scientificnameaccepted == "Laminariaceae"
                           & cover_data$Transect == "T2" 
                           & cover_data$Plot == "b"),]$scientificnameaccepted <- "Laminaria ochroleuca"
cover_data[(cover_data$yearcollected == 2011 
            & cover_data$Site == "Ci" 
            & cover_data$aphiaidaccepted == "143755"
            & cover_data$Transect == "T2" 
            & cover_data$Plot == "b"),]$aphiaidaccepted <- "145728"

cover_data[(cover_data$scientificnameaccepted == "Petrocelis"),]$scientificnameaccepted <- "Mastocarpus stellatus"
cover_data[(cover_data$aphiaidaccepted == "144163"),]$aphiaidaccepted <- "145650"

cover_data <- cover_data[!(cover_data$scientificnameaccepted == "Ralfsia"),] 
 
old <- c("Hypoglossum", "Mesophyllum", "Gigartina", "Laminariaceae", "Phyllophora",
        "Nemalion", "Porphyra", "Fucus", "Gelidium", "Peyssonnelia", "Rhodymenia") # First identified just to genus
new <- c("Hypoglossum hypoglossoides", "Mesophyllum lichenoides", "Gigartina pistillata",
         "Saccorhiza polyschides", "Phyllophora crispa", "Nemalion elminthoides", "Porphyra linearis", 
         "Fucus vesiculosus", "Gelidium corneum", "Peyssonnelia atropurpurea", "Rhodymenia pseudopalmata") # Now identified to species level
cover_data$scientificnameaccepted[cover_data$scientificnameaccepted %in% old] <- new[match(cover_data$scientificnameaccepted, old, nomatch = 0)]
rm(old,new)

# Identify unique taxons
taxa_list <- unique(cover_data$scientificnameaccepted)
taxa_list_1 <- taxa_list[1:50] #Divide datasets in two parts as wm_records_taxamatch function only accept 50 taxons at a time
taxa_list_2 <- taxa_list[51:92]

# Check if all taxon names are currently accepted
taxa_match1 <- worrms::wm_records_taxamatch(taxa_list_1)
taxa_match2 <- worrms::wm_records_taxamatch(taxa_list_2)
taxa_match1 <- do.call(rbind, taxa_match1)
taxa_match2 <- do.call(rbind, taxa_match2)
taxa_match <- rbind(taxa_match1, taxa_match2)
rm(taxa_match1, taxa_match2, taxa_list_1, taxa_list_2)

# Identify if non-matching records do exist
non_worms <- taxa_list %in% taxa_match$scientificname
non_worms <- data.frame(cbind(taxa_list, non_worms))
non_worms
rm(non_worms)

# Count number of identified individuals by rank
taxa_match %>%
  group_by(rank) %>%
  count()

# Check for matching errors (e.g. animals)
taxa_match %>%
  group_by(kingdom) %>%
  count()
taxa_match <- taxa_match[!taxa_match$kingdom == "Animalia",]
taxa_match <- taxa_match[!taxa_match$kingdom == "Fungi",]

# Remove duplicated records
taxa_match <- taxa_match[!duplicated(taxa_match$scientificname),]

# Write intertidal data csv
accepted <- as.data.frame(taxa_match[,c("scientificname","valid_name")])
cover_data <- left_join(cover_data, accepted, by = join_by(scientificnameaccepted==scientificname))
write.xlsx(cover_data, "C:/Users/sainzvs/UNICAN/EMODNET BIO (PHASE V) - Documentos/02_Tareas/2.03_WP3/CTI/Analisis/Intertidal_Data/Datos_IntertidalOBIS_rev.xlsx")
rm(accepted)

# Group by taxon level
species_list <- taxa_match[taxa_match$rank == "Species",] #Species
genus_list <- taxa_match[taxa_match$rank == "Genus",] #Genus
order_list <- taxa_match[taxa_match$rank == "Order"|taxa_match$rank == "Family",]#Order/Family

species_list_aphia <- unique(species_list$valid_AphiaID)
species_list_SN <- unique(species_list$valid_name)
genus_list_aphia <- unique(genus_list$valid_AphiaID)
genus_list_SN <- unique(genus_list$valid_name)
order_list_aphia <- unique(order_list$valid_AphiaID)
order_list_SN <- unique(order_list$valid_name)

#### Downloading options
gbif_opt <- list(basisOfRecord = "HUMAN_OBSERVATION", occurrenceStatus = "Present", year = "1990,2024")
obis_opt <- list(basisOfRecord = "HUMAN_OBSERVATION", occurrenceStatus = "present", startdate = "1990-01-01", enddate = "2024-12-31")
emodnet_opt <- list(period = c(1990,2024), basisOfRecord = c("O", "HumanObservation", "observation"))

#### Download data for those taxons identified to species level ####
occ.Emodnet <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c('name','longitude','latitude'))
options(timeout = 240)
i=1
for (i in 1:length(species_list_aphia)){
  url1 <-"http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=2.0.0&request=GetFeature&typeName=Dataportal%3Aeurobis-obisenv_full&resultType=results&viewParams=context%3A0100%3Baphiaid%3A"
  url2 <-"&outputFormat=csv"
  urlEmodnet <- paste(url1, species_list_aphia[i], url2, sep = "")
  temp.Emodnet <- read.csv(url(urlEmodnet))
  temp.Emodnet <- filter(temp.Emodnet, temp.Emodnet$yearcollected > emodnet_opt[["period"]][1] & temp.Emodnet$yearcollected < emodnet_opt[["period"]][2])
  temp.Emodnet <- temp.Emodnet[temp.Emodnet$basisofrecord %in% emodnet_opt[["basisOfRecord"]],]
  temp.Emodnet <- dplyr::select(temp.Emodnet, scientificnameaccepted, decimallongitude, decimallatitude)
  colnames(temp.Emodnet)<-c("name","longitude","latitude")
  occ.Emodnet <- bind_rows(occ.Emodnet, temp.Emodnet)
  rm(temp.Emodnet,url1,url2,urlEmodnet)
}
occ.Emodnet <- occ.Emodnet[-c(1), ]
unique(occ.Emodnet$name)

occ.gbif.obis <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c('name','longitude','latitude'))
i=1
for(i in 1:length(species_list_SN)) {
  didwarn <- tryCatch({occ(query = species_list_SN[i], from = c('gbif','obis'), limit = 10000, gbifopts = gbif_opt, obisopts =  obis_opt); FALSE}, warning = function(w) return(TRUE))
  if(didwarn) {
    print(paste("no data", species_list_SN[i]))
    next
  }
  else {
    temp.gbif.obis <-occ(query = species_list_SN[i], from = c('gbif','obis'), limit = 10000, gbifopts = gbif_opt, obisopts =  obis_opt)
    temp.gbif.obis <- dplyr::select(occ2df(temp.gbif.obis), name, longitude, latitude)
    occ.gbif.obis <- bind_rows(occ.gbif.obis, temp.gbif.obis)
    rm(temp.gbif.obis)
  }
}
occ.gbif.obis <- occ.gbif.obis[-c(1), ]
unique(occ.gbif.obis$name)

species_occurrences <- rbind(occ.Emodnet,occ.gbif.obis)
rm(occ.Emodnet,occ.gbif.obis)

names_species_occurrences <- data.frame(unique(species_occurrences$name))
names(names_species_occurrences) <- "raw"
names_species_occurrences$valid_name <- NA
names_species_occurrences$valid_name2 <- NA

for(i in 1:length(names_species_occurrences$raw)) {
  alfa <- tryCatch({worrms::wm_records_taxamatch(names_species_occurrences$raw[i])}, error = function(e) NA)
  if (is.na(alfa)){
    names_species_occurrences[i,2]<- alfa
    rm(alfa)
  }
  else{
    alfa <- alfa[[1]][["valid_name"]] 
    if (length(alfa) > 1) {
      names_species_occurrences[i,2]<- alfa[1]
      names_species_occurrences[i,3]<- alfa[2]
      rm(alfa)
    }
    else {
      names_species_occurrences[i,2]<- alfa
      rm(alfa)
    }
  }
}


species_occurrences <- left_join(species_occurrences, names_species_occurrences, by = join_by(name==raw)) #Assign valid name to each record
species_occurrences$origin <- species_occurrences$valid_name %in% species_list_SN #Check if any species record is within the original species list
species_occurrences <- species_occurrences[,c(4,5,1,2,3,6)]
species_occurrences <- species_occurrences[-2]

species_occurrences_final <- species_occurrences[species_occurrences$origin == TRUE,] # Select records in species_list_SN

#### Download data for those taxons identified to genus level ####
# Some taxons were not assign to any species, due to the complexity of its taxonomy. For those taxons, occurrence records were downloaded only for the
# species with existing records at GBIF/OBIS in the study area (i.e. North Iberian Peninsula). The first step was the identification of those species:

area <- c(-9.85, 40.80, -0.86, 44.72) # Bounding box

occ.gbif.obis.genus.IP <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c('name','longitude','latitude'))
i=1
for(i in 1:length(genus_list_SN)) {
  didwarn <- tryCatch({occ(query = genus_list_SN[i], from = c('gbif','obis'), limit = 1000, geometry = area, gbifopts = gbif_opt, obisopts =  obis_opt); FALSE}, warning = function(w) return(TRUE))
  if(didwarn) {
    print(paste("no data", genus_list_SN[i]))
    next
  }
  else {
    temp.gbif.obis <-occ(query = genus_list_SN[i], from = c('gbif','obis'), limit = 1000, geometry = area, gbifopts = gbif_opt, obisopts =  obis_opt)
    temp.gbif.obis <- dplyr::select(occ2df(temp.gbif.obis), name, longitude, latitude)
    occ.gbif.obis.genus.IP <- bind_rows(occ.gbif.obis.genus.IP, temp.gbif.obis)
    rm(temp.gbif.obis)
  }
}

occ.gbif.obis.genus.IP <- occ.gbif.obis.genus.IP[-c(1), ]
occ.gbif.obis.genus.IP <- occ.gbif.obis.genus.IP[!grepl("BOLD", occ.gbif.obis.genus.IP$name),] #Remove records without defined species name

# Identify species
genus_sp <- unique(occ.gbif.obis.genus.IP$name)
a <- data.frame(str_split_fixed(genus_sp, " ", 3))
genus_sp <- cbind(genus_sp, a)
names(genus_sp) <- c("Full_names", "Genus", "Species", "Authorship")
rm(a)
genus_sp <- genus_sp[-c(1,4)]

genus_sp[genus_sp == ""] <- NA #Remove undefined species
genus_sp <- na.omit(genus_sp)  #Remove undefined species
genus_sp <- genus_sp[!grepl("^[A-Z]", genus_sp$Species),] #Remove undefined species

genus_sp$full_names <- paste(genus_sp$Genus, genus_sp$Species, sep=" ")
genus_sp <- data.frame(unique(genus_sp$full_names))
names(genus_sp) <- "Species"

# Check if any of those species is in the original list and remove it
genus_sp$duplicated <- genus_sp$Species %in% taxa_match$valid_name
genus_sp <- genus_sp[genus_sp$duplicated == FALSE,]

# Check if they are accepted taxon
genus_sp$valid_name <- NA
genus_sp$valid_AphiaID<- NA
i=1
for (i in 1:length(genus_sp$Species)){
  alfa <- worrms::wm_records_taxamatch(genus_sp$Species[i])
  a <- alfa[[1]][["valid_name"]]
  b <- alfa[[1]][["valid_AphiaID"]]
  if (length(a) > 1) {
    genus_sp[i,3]<- a[1]
    genus_sp[i,4]<- b[1]
  }
  else {
    genus_sp[i,3]<- a
    genus_sp[i,4]<- b
    rm(alfa, a, b)
  }
}

genus_sp_aphia <- unique(genus_sp$valid_AphiaID)
genus_sp_SN <- unique(genus_sp$valid_name)

rm(occ.gbif.obis.genus.IP)

# Download all records for those species
occ.Emodnet.genus <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c('name','longitude','latitude'))
options(timeout = 240)
i=1
for (i in 1:length(genus_sp_aphia)){
  url1 <-"http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=2.0.0&request=GetFeature&typeName=Dataportal%3Aeurobis-obisenv_full&resultType=results&viewParams=context%3A0100%3Baphiaid%3A"
  url2 <-"&outputFormat=csv"
  urlEmodnet <- paste(url1, genus_sp_aphia[i], url2, sep = "")
  temp.Emodnet <- read.csv(url(urlEmodnet))
  temp.Emodnet <- filter(temp.Emodnet, temp.Emodnet$yearcollected > emodnet_opt[["period"]][1] & temp.Emodnet$yearcollected < emodnet_opt[["period"]][2])
  temp.Emodnet <- temp.Emodnet[temp.Emodnet$basisofrecord %in% emodnet_opt[["basisOfRecord"]],]
  temp.Emodnet <- dplyr::select(temp.Emodnet, scientificnameaccepted, decimallongitude, decimallatitude)
  colnames(temp.Emodnet)<-c("name","longitude","latitude")
  if(dim(temp.Emodnet)[1] != 0){
    occ.Emodnet.genus <- bind_rows(occ.Emodnet.genus, temp.Emodnet)
    rm(temp.Emodnet,url1,url2,urlEmodnet)
  }
  else{
    cat(paste("no data", genus_sp_aphia[i], "; "))
    rm(temp.Emodnet,url1,url2,urlEmodnet)
  }
  
}
occ.Emodnet.genus <- occ.Emodnet.genus[-c(1), ]
unique(occ.Emodnet.genus$name)

occ.gbif.obis.genus <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c('name','longitude','latitude'))
i=1
for(i in 1:length(genus_sp_SN)) {
  didwarn <- tryCatch({occ(query = genus_sp_SN[i], from = c('gbif','obis'), limit = 10000, gbifopts = gbif_opt, obisopts =  obis_opt); FALSE}, warning = function(w) return(TRUE))
  if(didwarn) {
    print(paste("no data", genus_sp_SN[i]))
    next
  }
  else {
    temp.gbif.obis <-occ(query = genus_sp_SN[i], from = c('gbif','obis'), limit = 10000, gbifopts = gbif_opt, obisopts =  obis_opt)
    temp.gbif.obis <- dplyr::select(occ2df(temp.gbif.obis), name, longitude, latitude)
    occ.gbif.obis.genus <- bind_rows(occ.gbif.obis.genus, temp.gbif.obis)
    rm(temp.gbif.obis)
  }
}
occ.gbif.obis.genus <- occ.gbif.obis.genus[-c(1), ]

genus_occurrences <- rbind(occ.Emodnet.genus, occ.gbif.obis.genus) #Merge EMODNET and GBIF/OBIS datasets
genus_occurrences <- genus_occurrences[!grepl("BOLD", genus_occurrences$name),] #Remove errors - species without name

names_genus_occurrences <- data.frame(unique(genus_occurrences$name)) #Check all existing species
names(names_genus_occurrences) <- "raw"
names_genus_occurrences$valid_name <- NA #Create column to assign the valid name for those species according to WORMS
names_genus_occurrences$valid_name_2 <- NA #Create column to assign the valid name for those species according to WORMS when more than one species possible


for(i in 1:length(names_genus_occurrences$raw)) {
  alfa <- tryCatch({worrms::wm_records_taxamatch(names_genus_occurrences$raw[i])}, error = function(e) NA)
  if (is.na(alfa)){
    names_genus_occurrences[i,2]<- alfa
    rm(alfa)
  }
  else{
    alfa <- alfa[[1]][["valid_name"]] 
    if (length(alfa) > 1) {
      names_genus_occurrences[i,2]<- alfa[1]
      names_genus_occurrences[i,3]<- alfa[2]
      rm(alfa)
    }
    else {
      names_genus_occurrences[i,2]<- alfa
      rm(alfa)
    }
  }
}

genus_occurrences <- left_join(genus_occurrences, names_genus_occurrences, by = join_by(name==raw)) #Asign valid name to each record
genus_occurrences$duplicated <- genus_occurrences$valid_name %in% species_list_SN #Check if any species record is within the original species list
genus_occurrences$genus <- str_split_fixed(genus_occurrences$valid_name, " ", 2)[,1]
genus_occurrences <- genus_occurrences[,c(7,4,5,1,2,3,6)]
genus_occurrences <- genus_occurrences[-3]
genus_occurrences$genus[genus_occurrences$genus == "Xiphosiphonia"] <- "Pterosiphonia"
genus_occurrences$origin <- genus_occurrences$genus %in% genus_list_SN #Check if the genus coincide with the original genus list

genus_occurrences_final <- genus_occurrences[genus_occurrences$duplicated == FALSE,] #Remove records matching species_list_SN
genus_occurrences_final <- genus_occurrences_final[genus_occurrences_final$origin == TRUE,] # Remove records not matching genus_list_SN

#### Create Full Dataset ####
genus_full <- genus_occurrences_final[-c(2,3,6,7)]
species_full <- species_occurrences_final[-c(2,5)]
names(genus_full)[1] <- "name"
names(species_full)[1] <- "name"
occurrences_full <- rbind(species_full, genus_full)
rm(genus_full, species_full)

# Remove duplicated occurrences
occurrence_nonduplicated <- unique(occurrences_full, by="name")

a <- occurrence_nonduplicated %>%
  group_by(name) %>%
  count() # Count number of records

occurrence_nonduplicated %>%
  filter(name == "Gelidium corneum") %>%
  with(plot(longitude, latitude))

#### Write data ####
OUT <- createWorkbook() # Create a blank workbook
addWorksheet(OUT, "Species") # Species data
addWorksheet(OUT, "Genus") # Genus data
addWorksheet(OUT, "Full_ND") # Full Non Duplicated

# Write the data to the sheets
writeData(OUT, sheet = "Species", x = species_occurrences_final)
writeData(OUT, sheet = "Genus", x = genus_occurrences_final)
writeData(OUT, sheet = "Full_ND", x = occurrence_nonduplicated)

# Export the file
saveWorkbook(OUT, "C:/Users/sainzvs/UNICAN/EMODNET BIO (PHASE V) - Documentos/02_Tareas/2.03_WP3/CTI/Analisis/Occurrences/rev_version_26_11_2024/Occurrences.xlsx")


# Downsample occurrence data - 10km
occ_data <- occurrence_nonduplicated
occ_data_thinned <- setNames(data.frame(matrix(ncol = 3, nrow = 1)), c('name','Longitude','Latitude'))
for (i in unique(occ_data$name)) {
  a <- occ_data %>%
    dplyr::filter(name == i)
  b <- thin(loc.data = a, 
            lat.col= "latitude", long.col = "longitude", spec.col = "name", 
            thin.par = 10, reps = 1, 
            locs.thinned.list.return = TRUE,
            write.files = FALSE,
            write.log.file = FALSE)
  b <- as.data.frame(b[[1]])
  b$name <- unique(a$name)
  occ_data_thinned <- rbind(occ_data_thinned, b)
  rm(a,b)
}
occ_data_thinned <- occ_data_thinned[-1,] 

occ_data_thinned %>%
  filter(name == "Ceramium") %>%
  with(plot(Longitude, Latitude))

write.xlsx(occ_data_thinned, "C:/Users/sainzvs/UNICAN/EMODNET BIO (PHASE V) - Documentos/02_Tareas/2.03_WP3/CTI/Analisis/Occurrences/rev_version_26_11_2024/Occurrences_thinned.xlsx")
