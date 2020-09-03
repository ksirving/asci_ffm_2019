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
load("output_data/02_selected_nhd_mainstems_gages.rda")
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
# str(sel_algae_coms_final)
sel_algae_coms_final_trimmed <- sel_algae_coms_final %>% 
  mutate(gage_id_c = gsub("^T", "", ID)) %>% 
  # separate(SampleID, into=c("site", "sampledate"), sep = "_", remove = FALSE) %>% 
  mutate(sampledate = lubridate::ymd(sampledate)) %>% 
  # mutate(sampledate = if_else(is.na(sampledate), lubridate::mdy("06282009"), sampledate)) %>% 
  # select(-site) %>% 
  filter(lubridate::month(sampledate)>4, lubridate::month(sampledate)<10)

# check stations match for trimmed data (n=321):
algae_coms_dat_trim %>% st_drop_geometry() %>% distinct(StationID, ID) %>% dim()
algae_coms_dat %>% st_drop_geometry() %>% distinct(StationID, ID) %>% dim() # this should be 331
names(algae_coms_dat)
names(sel_algae_coms_final)

# join together selected asci data with ffm alteration status data (all data not trimmed)
algae_asci_por <-  inner_join(sel_algae_coms_final, g_all_alt,
                            #by=c("comid")) #%>% # n=2688
                            #by=c("comid", "gage_id_c"="gage_id")) # %>% # n=1550
                            # since only want observed data at USGS gage:
                            by=c("gage_id_c"="gage_id")) %>%   # n=7519
  distinct(StationID, metric, gage_id, .keep_all=TRUE) %>% 
  rename(comid_algae = comid.x, comid_ffc = comid.y) # n=7129


# join together selected asci data with ffm alteration status data (this is the "untrimmed" dataset)
algae_asci_por <-  inner_join(sel_algae_coms_final, g_all_alt,
                            #by=c("comid")) #%>% # n=2688
                            #by=c("comid", "gage_id_c"="gage_id")) # %>% # n=1550
                            # since only want observed data at USGS gage:
                            by=c("gage_id_c"="gage_id")) %>%   # n=7519
  distinct(StationID, metric, gage_id, .keep_all=TRUE) %>% 
  rename(comid_algae = comid.x, comid_ffc = comid.y) # n=7129

# now trimmed data
algae_asci_por_trim <-  inner_join(sel_algae_coms_final_trimmed, g_all_alt,
                                 by=c("gage_id_c"="gage_id")) %>%   # n=7294
  distinct(StationID, metric, gage_id, .keep_all=TRUE) %>% 
  rename(comid_algae = comid.x, comid_ffc = comid.y) # n=6904
names(algae_asci_por_trim)
# see how many distinct sites
length(unique(algae_asci_por_trim$gage_id_c)) #Gages (n=146), 
length(unique(algae_asci_por_trim$StationID)) # algae Stations (n=254), 

# how many of each gage type
algae_asci_por_trim %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 106, REF = 40

algae_asci_por %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 109, REF = 42

# and originally? : so we lost 6 ref sites :(
sel_algae_coms_final %>% st_drop_geometry() %>% 
  dplyr::distinct(ID, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() # ALT = 109, REF = 46

# Visualize ---------------------------------------------------------------

library(ggthemes)

hist(month(algae_asci_por_trim$sampledate))

names(algae_asci_por_trim)

# function to get data
stat_box_data <- function(y, upper_limit = max(algae_asci_por_trim$MMI.hybrid, na.rm = TRUE)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}


# plot ASCI w/ NAs 
ggplot(data=algae_asci_por_trim %>% filter(status!="not_enough_data"), aes(x=CEFF_type, y=MMI.hybrid)) + 
  geom_boxplot(aes(fill=status), show.legend = T) +
  stat_summary(fun.data=stat_box_data, geom="text", cex=3, hjust=1, vjust=0.9) +
  labs(y="ASCI", x="CEFF Gage Type", subtitle="ASCI Score by FFC Alteration Status") #+
  # theme_bw(base_family = "Roboto Condensed") + facet_grid(.~status) #+
  # scale_fill_colorblind()
#ggsave(filename = "figs/05_asci_scores_by_alteration_status_ceff_type.png", height = 8, width = 11, units = "in",dpi=300)

# Add HUC Regions --------------------------------------------------

# check crs:
st_crs(algae_asci_por_trim)
st_crs(algae_asci_por)
st_crs(ca_sp_regions)

ca_sp_regions <- ca_sp_regions %>% st_transform(4326)
algae_asci_por_trim <- algae_asci_por_trim %>% st_transform(4326)
algae_asci_por <- algae_asci_por %>% st_transform(4326)

# join with regions and add huc_region, make sure both df are in 4326
algae_asci_por_trim <- st_join(algae_asci_por_trim, left = TRUE, ca_sp_regions["huc_region"])
algae_asci_por <- st_join(algae_asci_por, left = TRUE, ca_sp_regions["huc_region"])

# make a simpler layer for just editing:
algae_asci_sites <- algae_asci_por %>%
  dplyr::distinct(StationID, ID, .keep_all = TRUE)
length(unique(algae_asci_sites$StationID)) # 263

algae_asci_sites_trim <- algae_asci_por_trim %>%
  dplyr::distinct(StationID, ID, .keep_all = TRUE)
length(unique(algae_asci_sites_trim$StationID))

# view and update w mapedit
mapview(algae_asci_sites, col.regions="orange") + 
  mapview(algae_asci_sites_trim, col.regions="green", cex=2.5) +
  mapview(ca_sp_regions)

install.packages("mapedit")
library(mapedit)
library(leafpm)
library(leaflet)

## use this to select features (returns a list of stationIDs)
# selectMap(
#   leaflet() %>%
#     addTiles() %>%
#     addPolygons(data=ca_sp_regions, layerId = ~huc_region, color = "orange") %>%
#     addCircleMarkers(data = algae_asci_sites, layerId = ~StationID)
#   )
# 

## sites to add to central valley
cvalley_add <- c("514FC1278", "514RCR001", "534DCC167", "534PS0114", "514DNCLDC")

## sites to add to great_basin
gbasin_add <- c("603MAM004", "630PS0005", "601PS0065")

## sites to add to southcoast
scoast_add <- c("628PS1307","628PS1179","719MISSCK","719TRMDSS","719FCA001", "628PS0715", "MJMJ1", "628PS1019")

## remove gages and sites from visual check
rem_sites <- read.csv("input_data/sites_to_remove.csv")
rem_sites <- rem_sites$sample_site
rem_gages <- read.csv("input_data/gages_to_remove.csv")
rem_gages <- rem_gages$Gage

head(algae_asci_por_trim)
unique(algae_asci_por_trim$StationID)


algae_asci_por_trim <- algae_asci_por_trim %>%
  filter(!StationID %in% rem_sites,
         !gage_id %in% rem_gages)

#removed 817 rows (12%)

algae_asci_por <- algae_asci_por %>%
  filter(!StationID %in% rem_sites,
         !gage_id %in% rem_gages)

#removed 817 rows (11%)

# so 341 NA's in each
summary(as.factor(algae_asci_por$huc_region))
summary(as.factor(algae_asci_por_trim$huc_region))

algae_asci_por_trim$huc_region <- as.character(algae_asci_por_trim$huc_region)
algae_asci_por$huc_region <- as.character(algae_asci_por$huc_region)

# # use case_when to replace
algae_asci_por_trim <- algae_asci_por_trim %>%
  mutate(huc_region = case_when(
    StationID %in% cvalley_add ~ "central_valley",
    StationID %in% gbasin_add ~ "great_basin",
    StationID %in% scoast_add ~ "south_coast",
    TRUE ~ huc_region))

algae_asci_por <- algae_asci_por %>%
  mutate(huc_region = case_when(
    StationID %in% cvalley_add ~ "central_valley",
    StationID %in% gbasin_add ~ "great_basin",
    StationID %in% scoast_add ~ "south_coast",
    TRUE ~ huc_region))

# no NAs!!!! Yay1
summary(as.factor(algae_asci_por_trim$huc_region))
table(algae_asci_por_trim$huc_region)

## map and double check:
mapview(algae_asci_por_trim, zcol="huc_region", layer.name="Selected Sites", viewer.suppress=FALSE) +
  mapview(ca_sp_regions, zcol="huc_region", layer.name="HUC Regions", alpha.regions=0.1)

# Make GAGE/algae geoms -----------------------------------------------------

# make SF geometry fields for algae
algae_asci_por_algae <- algae_asci_por %>% 
  rename("geom_algae"=geometry) 

# make a USGS geom field
algae_asci_por_usgs <- algae_asci_por %>% st_drop_geometry() %>% 
  st_as_sf(., coords=c("LONGITUDE","LATITUDE"), crs = 4326, remove=FALSE) %>% 
  # rename the geometry col
  rename("geom_usgs"=geometry)

# quick view - not working!!!!!
mapview(algae_asci_por_algae, cex=7, col.regions="orange", 
        layer.name="Selected algae comids") +
  mapview(algae_asci_por_usgs, col.regions="skyblue", cex=4, color="blue2", layer.name="Selected USGS Gages") +
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines")

# Export Cleaned Data -----------------------------------------------------

# save the algae_asci_por
write_rds(algae_asci_por, path = "output_data/04_selected_algae_stations_w_asci_ffm_alt_por.rds")
write_rds(algae_asci_por_trim, path = "output_data/04_selected_algae_stations_w_asci_ffm_alt_por_trim.rds")

# sf specific files for usgs & algae
save(algae_asci_por_algae, algae_asci_por_usgs, file="output_data/04_selected_algae_asci_por_sf.rda")

save(algae_asci_por_trim, file = "output_data/04_selected_algae_asci_por_trim_w_huc_region.rda")
save(algae_asci_por, file = "output_data/04_selected_algae_asci_por_w_huc_region.rda")
