# 04 Merge Algae ASCI Data with Flow Data for Period of Record
## R. Peek/K Irving

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)
library(tidylog)

# Load Data ---------------------------------------------------------------

# asci
### algae_coms_dat (all data for selected site pairs), 
### sel_algae_coms_final (just coms and id)
### algae_coms_dat_trim (all data for selected site pairs btwn Jun-Sep)
load(file="output_data/03_selected_final_algae_stations_dat_all_gages.rda")
names(algae_coms_dat)

# FISH REGIONS
ca_sp_regions <- read_sf("input_data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# nhd streamlines
load("input_data/07_umbrella_sp_regions.rda") # mainstems_all

# get all functional flow metric data (percentiles, alt status, ffmetrics)
load("input_data/02_usgs_all_ffm_data.rda")

# Set Basemaps ------------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Make algae POR FF Dataset -----------------------------------------------

# make gage_id as character for join:
sel_algae_coms_final <- sel_algae_coms_final %>% 
  mutate(gage_id_c = gsub("^T", "", ID))

sel_algae_coms_final_trimmed <- sel_algae_coms_final %>% 
  mutate(gage_id_c = gsub("^T", "", ID)) %>% 
  mutate(sampledate = lubridate::ymd(sampledate)) %>% 
  filter(lubridate::month(sampledate)>4, lubridate::month(sampledate)<10)

# check stations match for trimmed data (n=320):
algae_coms_dat_trim %>% st_drop_geometry() %>% distinct(SampleID_old.x , ID) %>% dim()
algae_coms_dat %>% st_drop_geometry() %>% distinct(SampleID_old.x , ID) %>% dim() # this should be 331
names(algae_coms_dat)

# Make ANNUAL Dataset -----------------------------------------------------

#g_all_ffc %>% group_by(gage_id, Year) %>% distinct() %>% dim()

# get raw functional flow metrics (for every year)
ffm <- g_all_ffc %>% mutate(Year=as.integer(Year)) %>% 
  distinct() # filter out duplication

# make it long not wide for joins:
ffm <- pivot_longer(ffm, cols= c(DS_Dur_WS:Peak_Fre_5), names_to = "ffm_metric", values_to = "ffm_value") %>% 
  # drop nas
  filter(!is.na(ffm_value))

## MAKE UNTRIMMED DATA SET (ALL SAMPLE ALGAE MONTHS)
# untrimmed = 331
algae_sampleid <- algae_coms_dat %>% st_drop_geometry() %>% 
  mutate(gage_id_c = gsub("^T", "", ID)) %>% 
  dplyr::distinct(SampleID_old.x , ID, .keep_all = TRUE) %>%
  mutate(YYYY = as.numeric(YY), DD= as.numeric(DD))

# make just sampleID dataset (trimmed = 300)
algae_sampleid_trimmed <- algae_coms_dat_trim %>% st_drop_geometry() %>% 
  mutate(gage_id_c = gsub("^T", "", ID)) %>% 
  dplyr::distinct(SampleID_old.x , ID, .keep_all = TRUE) %>%
  mutate(YYYY = as.numeric(YY), DD= as.numeric(DD))


# join for annual data by GAGE ID, and BMI Sample YEAR = USGS Year
algae_asci_ffm_ann <- left_join(algae_sampleid, ffm, by=c("gage_id_c"="gage_id", "YYYY"="Year"))

# join for annual data by GAGE ID, and BMI Sample YEAR = USGS Year
algae_asci_ffm_ann_trim <- left_join(algae_sampleid_trimmed, ffm, by=c("gage_id_c"="gage_id", "YYYY"="Year"))

names(algae_asci_ffm_ann_trim)
# Add HUC Regions --------------------------------------------------

# make spatial:
algae_asci_ffm_ann <- algae_asci_ffm_ann %>% st_as_sf(coords=c("Longitude.x","Latitude.x"), crs=4326, remove=FALSE)
algae_asci_ffm_ann_trim <- algae_asci_ffm_ann_trim %>% st_as_sf(coords=c("Longitude.x","Latitude.x"), crs=4326, remove=FALSE)

# transform to match proj
ca_sp_regions <- ca_sp_regions %>% st_transform(4326)

# join with regions and add huc_region, make sure both df are in 4326
algae_asci_ffm_ann <- st_join(algae_asci_ffm_ann, left = TRUE, ca_sp_regions["huc_region"])
algae_asci_ffm_ann_trim <- st_join(algae_asci_ffm_ann_trim, left = TRUE, ca_sp_regions["huc_region"])

### need to check and change these!!!
## sites to add to central valley
cvalley_add <- c("514FC1278", "514RCR001", "534DCC167")

## sites to add to great_basin
gbasin_add <- c("603MAM004", "630PS0005")

## sites to add to southcoast
scoast_add <- c("628PS1307","628PS1179","719MISSCK","719TRMDSS","719FCA001")

# Amargosa site is "609PS0053" = mojave?
str(algae_asci_ffm_ann_trim)

algae_asci_ffm_ann_trim <- algae_asci_ffm_ann_trim %>%
  mutate(huc_region = as.character(huc_region))

algae_asci_ffm_ann <- algae_asci_ffm_ann %>%
  mutate(huc_region = as.character(huc_region))

# use case_when to replace: ANN
algae_asci_ffm_ann_trim <- algae_asci_ffm_ann_trim %>%
  mutate(huc_region = case_when(
    StationID %in% cvalley_add ~ "central_valley",
    StationID %in% gbasin_add ~ "great_basin",
    StationID %in% scoast_add ~ "south_coast",
    TRUE ~ huc_region))

algae_asci_ffm_ann <- algae_asci_ffm_ann %>%
  mutate(huc_region = case_when(
    StationID %in% cvalley_add ~ "central_valley",
    StationID %in% gbasin_add ~ "great_basin",
    StationID %in% scoast_add ~ "south_coast",
    TRUE ~ huc_region))


# SAVE OUT ----------------------------------------------------------------

save(algae_asci_ffm_ann, file="output_data/04a_selected_algae_asci_ffm_ann.rda")
save(algae_asci_ffm_ann_trim, file="output_data/04a_selected_algae_ascii_ffm_ann_trim.rda")

