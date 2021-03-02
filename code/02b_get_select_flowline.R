# 02b Spatially Linking algae & selected USGS Gages by NHD Flowlines
## R. Peek 2020

## Spatially link the algae station data with the USGS FFC gages that occur in same flowline and h12
getwd()
# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(glue)
library(here)
library(lubridate)
library(beepr) # to tell us when stuff is done


devtools::install_github("USGS-R/nhdplusTools", force=T)
library(nhdplusTools)

# 01. Load Data ---------------------------------------------------------------

# selected HUC12s
sel_h12_algae <- read_rds("output_data/02a_sel_h12_w_algae_asci.rds")
sel_h12_gages <- read_rds("output_data/02a_sel_h12_w_ffc_gages.rds")

# selected algae and gages
sel_gages_algae <- read_rds("output_data/02a_sel_ffc_gages_by_h12.rds")
sel_algae_gages_asci <- read_rds("output_data/02a_sel_algae_stations_asci_by_h12.rds")
sel_algae_station_gages_h12 <- read_rds("output_data/02a_sel_algae_stations_h12.rds")
head(sel_gages_algae)
# algae COMIDs (from Section 02)
# algae_comids <- readRDS("output_data/02b_algae_stations_comids_revised.rds")
algae_comids <- read_rds("output_data/02_algae_all_stations_comids.rds")
head(algae_comids)
algae_comids <- algae_comids %>%
  rename(StationCode = StationID, COMID_algae = comid)
  
# 02. algae COMIDS: See Existing COMIDs --------------------------

# # read in comids from raf:
# algae_comids_raw <- rio::import("data/algae/algae_comids_for_ryan.xlsx") %>% 
#   rename(COMID_algae=comid)
# 
# # check/update the COMID for each algae site (run once)
# # ADD COMID (comid=USGS gage, COMID_algae=algae)
# 
# # transform to same datum/crs
sel_algae_station_gages_h12 <- st_transform(sel_algae_station_gages_h12, crs = 3310) # use CA Teale albs metric
sel_algae_gages_asci <- st_transform(sel_algae_gages_asci, crs = 3310) # use CA Teale albs metric
sel_gages_algae <- st_transform(sel_gages_algae, crs=3310)
head(sel_algae_station_gages_h12)
# Create dataframe for looking up COMIDS (here use all stations)
algae_segs <- sel_algae_station_gages_h12 %>%
  select(StationCode, Longitude, Latitude, comid) %>%
  rename(COMID_gage = comid) %>%
  distinct(StationCode, .keep_all = TRUE)
head(algae_segs)
# compare with raw COMIDS to see how many match:
# algae_segs <- left_join(algae_segs, algae_comids_raw, by="StationCode" )

# add in COMID from algae_comid (nhdtools)
algae_segs <- left_join(algae_segs, algae_comids, by="StationCode")
str(algae_segs)
algae_segs$COMID_algae <- as.numeric(algae_segs$COMID_algae)
# algae_segs$COMID_algae <- as.numeric(algae_segs$COMID_algae)

# finally read merge into one dataset (keep algae comid except where NA, fill with NHD version)
# algae_segs <- algae_segs %>%
#   mutate(COMID = case_when(
#     is.na(COMID_algae) ~ comid,
#     TRUE ~ COMID_algae
#   ))


# # Save out: 
write_rds(algae_segs, file="output_data/02b_algae_stations_comids_revised_Mar2021.rds")

## 02b. algae COMIDs from NHDTools ------------------------------------------------

# # use nhdtools to get comids
algae_all_coms <- algae_segs %>%
  st_transform(4326) %>%
  # slice(1:100) %>%
  group_split(StationCode) %>%
  set_names(algae_segs$StationCode) %>%
  map(~discover_nhdplus_id(.x$geometry))
beepr::beep(2)

algae_all_coms %>% purrr::map_lgl(~ length(.x)>1) %>% table()

# algae_all_coms
# flatten into single dataframe instead of list
algae_segs_df <-algae_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "StationCode")

# rm COMIDs starting with "V" (this is remnant of old version)
#algae_comids <- algae_segs_df %>% filter(!grepl("^V", StationCode))

# write back out
write_rds(algae_comids, file="output_data/02b_algae_stations_comids.rds")

# clean up
rm(algae_all_coms, algae_segs_df, algae_segs, algae_coms)

# 03. GET UPSTREAM FLOWLINES FROM GAGE --------------------------------------------------

# use list of gage NHD comids to make a list to pass to nhdplusTools to get flowlines
# pull data from 10 km upstream

## transform datum for flowlines
sel_algae_gages_asci <- st_transform(sel_algae_gages_asci, crs=3310) # use CA Teale albs metric
sel_gages_algae <- st_transform(sel_gages_algae, crs=3310)

# check for missing comids?
summary(sel_algae_gages_asci$comid) # gage sites

# Use the GAGE com_list
coms_list <- map(sel_gages_algae$comid, ~list(featureSource = "comid", featureID=.x))
length(coms_list)

# check
coms_list[[200]] # should list feature source and featureID

# Get upstream mainstem streamlines (10 km limit) from gages
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x, 
                                             mode="UM", # upstream main 
                                             distance_km = 10))
beep(2)

# check length (for NAs?) (n=415 if no missing)
mainstemsUS %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# transform the sf layer to match mainstems crs (4326)
sel_gages_algae <- sel_gages_algae %>% st_transform(4326)

# make a single flat layer
mainstems_flat_us <- mainstemsUS %>%
  set_names(., sel_gages_algae$site_id) %>%
  map("UM_flowlines") %>% # this needs to match the col from 'mode' on line 67
  #map2_df(unique(bd_coms_df$comid), ~mutate(.x, orig_comid=.y))
  map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_us, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "UM")


# rm temp files
rm(mainstems_flat_us, mainstemsUS)

## Map and Save ---------------------------------

# preview
mapview(mainstems_us) + 
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

write_rds(mainstems_us, file = "output_data/02b_sel_gage_mainstems_us.rds")


# 04. GET DOWNSTREAM MAIN FLOWLINES FROM GAGE ------------------------------------------------

# get NHD segments downstream of selected USGS gages, 10 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="DM",
                                             distance_km = 10))
beep(2)

# check length (for NAs?)
mainstemsDS %>% 
  map("DM_flowlines") %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_algae$site_id) %>%
  map("DM_flowlines") %>% # this needs to match the col from 'mode' on line 67
  map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_ds, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DM")

rm(mainstems_flat_ds, mainstemsDS)

## Map and Save ------------------------

mapview(mainstems_ds, color="yellow3") +
  mapview(mainstems_us, color="darkgreen") +
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

# save 
write_rds(mainstems_ds, file = "output_data/02b_sel_gage_mainstems_ds.rds")


# 05. GET DOWNSTREAM DIVERSION MAIN FLOWLINES FROM GAGE ------------------------------------------------

# get diversions
mainstemsDD <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="DD",
                                             distance_km = 10))
beep(2)

# check length (for NAs?)
mainstemsDD %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_dd <- mainstemsDD %>%
  set_names(., sel_gages_algae$site_id) %>%
  map("DD_flowlines") %>% # 
  map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_dd <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_dd, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_dd <- mainstems_dd %>% 
  mutate(from_gage = "DD")

rm(mainstemsDD, mainstems_flat_dd)

## Map and Save ------------------------

# mapview
mapview(mainstems_us, color="yellow") + mapview(mainstems_ds, color="blue") +
  mapview(mainstems_dd, color="purple") +
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

write_rds(mainstems_dd, file = "output_data/02b_sel_gage_mainstems_dd.rds")

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds, mainstems_dd)
save(mainstems_all, file="output_data/02b_sel_gage_mainstems_all.rda")


