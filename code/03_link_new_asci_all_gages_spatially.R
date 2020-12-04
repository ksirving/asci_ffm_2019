
# install.packages("readr")
library(readr)
library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(lubridate)

#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

# data needed
## all asci data with scores
load(file="output_data/01a_all_asci_data_clean.RData") # algae4
algae_samples_distinct_asci <- algae4 %>%
  select(-D_ASCI) %>%
  distinct()
algae_samples_distinct_asci <- na.omit(algae_samples_distinct_asci)  

## just sites
load(file="output_data/01a_algae_stations_distinct.RData") # algae_stations_distinct
algae_stations_distinct <- na.omit(algae_stations_distinct)

# ALL GAGES W FFC DATA
# read from ffm_comparison repo: https://github.com/ryanpeek/ffm_comparison

gages_ffc <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds")) %>% 
  distinct(gageid, .keep_all=TRUE) # n=959

# get all gages and merge for sf
gages_sf <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/data/usgs_ca_all_dv_gages.rds"))

ffc_gages <- inner_join(gages_sf, gages_ffc, by=c("site_id"="gageid")) %>% 
  select(-c(metric:median_in_iqr)) %>% 
  # st_transform(coords=c("lon", "lat"), crs=4326, remove=F) %>%
  # drop San Pablo Bay Gage: (11182030)
  filter(!site_id=="11182030")

st_crs(ffc_gages) <- 4326

# HUC12s
load("output_data/huc12_sf.rda") # CA h12s

# algae COMIDs (from Section 04)
algae_comids <- readRDS("data_output/03_algae_all_stations_comids.rds") ## do this later!

# 02. Make Data Spatial -------------------------------------------------------

# make spatial
# algae_clean <- algae_clean %>% 
#   st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

algae_samples_distinct_asci <- algae_samples_distinct_asci %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)

algae_stations_distinct <- algae_stations_distinct %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)

# check projs are same
st_crs(algae_clean)
st_crs(algae_stations_distinct)
st_crs(ffc_gages)
st_crs(h12)

# join COMIDs for algae sites - NEED TO DO THIS!!!!
# algae_samples_distinct_asci <- left_join(algae_samples_distinct_asci, algae_comids, by=c("StationCode"))
# summary(algae_samples_distinct_asci$COMID)
# 
# algae_stations_distinct <- left_join(algae_stations_distinct, algae_comids, by=c("StationCode"))
# summary(algae_stations_distinct$COMID)

# 03. INTERSECT algae & Gages by H12 -----------------------------------

# Add H12 to algae and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using algae DISTINCT STATIONS
algae_h12 <- st_join(algae_stations_distinct, left = TRUE, h12[c("HUC_12")])

# Add H12 to gages
gages_h12 <- st_join(ffc_gages, left=TRUE, h12[c("HUC_12")]) %>%
  st_drop_geometry()

# now join based on H12: what algae stations share same H12 as USGS gage? (N=1915)
sel_algae_gages <- inner_join(algae_h12, gages_h12, by="HUC_12") %>% 
  distinct(StationCode, site_id, .keep_all = T) # n=1000

# number of unique HUC12
length(unique(factor(sel_algae_gages$HUC_12))) # h12=258

# number of unique gages
length(unique(sel_algae_gages$site_id)) # gages=420

# number of unique algae stations
length(unique(sel_algae_gages$StationCode)) # algae Stations=841
names(sel_algae_gages_asci)
names(sel_algae_gages)
# make sure these have asci scores: of those in same H12, how many have asci scores? N=1040
sel_algae_gages_asci <- left_join(sel_algae_gages, st_drop_geometry(algae_samples_distinct_asci), by="StationCode") %>% 
  filter(!is.na(H_ASCI)) %>% 
  distinct(StationCode, site_id, .keep_all=TRUE)

# number of unique?
length(unique(factor(sel_algae_gages_asci$HUC_12))) # h12=258
length(unique(sel_algae_gages_asci$site_id)) # gages=420
length(unique(sel_algae_gages_asci$StationCode)) # algae Stations=841

# Get Selected Gages ONLY:  # n=415 (that have asci scores) - none removed
sel_gages_algae <- ffc_gages %>% 
  filter(site_id %in% sel_algae_gages_asci$site_id) %>% 
  distinct(site_id, .keep_all = T)

# select H12s that have points inside: # n=163
sel_h12_algae <- h12[sel_algae_gages_asci, ] # 258
sel_h12_gages <- h12[ffc_gages, ] # 597

# * Map of Filtered algae & Gages  ------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

# a map of all gages and algae stations that fall within the same H12

# get the gages not selected (n=538)
gages_not_selected <- ffc_gages %>% 
  filter(!site_id %in% sel_algae_gages_asci$site_id)
# can compare with types (alt vs. ref) later

# get algae NOT selected with asci (n=1090)
algae_not_selected <- algae_samples_distinct_asci %>% 
  filter(!is.na(H_ASCI)) %>% 
  filter(!StationCode %in% sel_algae_gages_asci$StationCode) %>% 
  distinct(StationCode, .keep_all=TRUE)

# this map of all sites selected U/S and D/S
m1 <- mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
              layer.name="Selected Algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all algae or gages in same H12 but not selected
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(algae_not_selected, col.regions="gold2", color="gray20", cex=3.2, 
          layer.name="Other algae Sites w asci Scores") + 
  mapview(algae_stations_distinct, col.regions="gray", color="gray20", cex=3, 
          layer.name="All Algae Sites") + 
  mapview(sel_h12_algae, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=FALSE, layer.name="HUC12") + 
  mapview(sel_h12_gages, col.regions="gray50", alpha.region=0.1, 
          color="darkblue", legend=FALSE, layer.name="HUC12 Gages")

m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")



# * Save Out -----------------------------------------------------------------

# save out
write_rds(sel_h12_algae, file="output_data/03_selected_h12_all_gages.rds")
write_rds(sel_gages_algae, file="output_data/03_selected_usgs_h12_all_gages.rds")
write_rds(sel_algae_gages_asci, file="output_data/03_selected_algae_h12_all_gages_asci.rds")
write_rds(sel_algae_gages, file="output_data/03_selected_algae_h12_all_gages.rds")

# 04. algae COMIDS: GET NEW/MISSING COMIDS --------------------------

# no NA's
summary(sel_algae_gages_asci$COMID)

# IF NEEDED

library(nhdplusTools)
#  
# ## TRANSFORM TO SAME DATUM
sel_algae_gages <- st_transform(sel_algae_gages, crs = 3310) # use CA Teale albs metric
sel_gages_algae <- st_transform(sel_gages_algae, crs=3310)

# Create dataframe for looking up COMIDS (here use all stations)
algae_segs <- sel_algae_gages %>%
  select(StationCode, Longitude, Latitude) %>%
  distinct(StationCode,  Longitude, Latitude)

# use nhdtools to get comids
algae_all_coms <- algae_segs %>%
  group_split(StationCode) %>%
  set_names(., algae_segs$StationCode) %>%
  map(~discover_nhdplus_id(.x$geometry))


# flatten into single dataframe instead of list
algae_segs_df <-algae_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "StationCode")

# rm COMIDs starting with "V"
algae_comids <- algae_segs_df %>% filter(!grepl("^V", StationCode))

# bind with existing algae_comids:
algae_coms <- readRDS("data_output/03_algae_all_stations_comids.rds")

# bind
algae_comids <- bind_rows(algae_coms, algae_comids)

# write back out
write_rds(algae_comids, file="data_output/03_algae_all_stations_comids.rds")

# clean up
rm(algae_all_coms, algae_segs_df, algae_segs, algae_coms)

# 05. GET UPSTREAM FLOWLINES FROM GAGE --------------------------------------------------

## TRANSFORM TO UTM datum for flowlines
sel_algae_gages_asci <- st_transform(sel_algae_gages_asci, crs=3310) # use CA Teale albs metric
sel_gages_algae <- st_transform(sel_gages_algae, crs=3310)

# use a list of comids to make a list to pass to the nhdplusTools function

# check for missing comids?
summary(sel_algae_gages_asci$comid) # gage sites

# Use the GAGE com_list
coms_list <- map(sel_gages_algae$comid, ~list(featureSource = "comid", featureID=.x))

# check
coms_list[[200]] # should list feature source and featureID

library(beepr)

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
  map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_us, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_us <- mainstems_us %>% 
  mutate(from_gage = "UM")

# rm temp files
rm(mainstems_flat_us, mainstemsUS)

# preview
mapview(mainstems_us) + 
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

# save as both for now
#save(mainstems_us, file = "data_output/03_selected_nhd_mainstems_gages_us.rda")

# 06. GET DOWNSTREAM FLOWLINES FROM GAGE ------------------------------------------------

# get NHD segments downstream of selected USGS gages, 20 km buffer
mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamMain",
                                             distance_km = 20))
beep(2)

# check length (for NAs?)
mainstemsDS %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_algae$site_id) %>%
  map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_ds, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_ds <- mainstems_ds %>% 
  mutate(from_gage = "DM")

rm(mainstems_flat_ds, mainstemsDS)

mapview(mainstems_us, color="yellow") + mapview(mainstems_ds, color="blue") +
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")


# get diversions
mainstemsDD <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamDiversions",
                                             distance_km = 20))
beep(2)

# check length (for NAs?)
mainstemsDD %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table()

# make a single flat layer
mainstems_flat_dd <- mainstemsDD %>%
  set_names(., sel_gages_algae$site_id) %>%
  map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_dd <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_dd, use.names = TRUE, fill = TRUE))

# add direction to gage col
mainstems_dd <- mainstems_dd %>% 
  mutate(from_gage = "DD")

rm(mainstems_flat_ds, mainstemsDS, mainstemsDD, mainstems_flat_dd)

# save each
save(mainstems_us, mainstems_ds, mainstems_dd, file = "data_output/03_selected_nhd_mainstems_gages_us_ds.rda")

# mapview
mapview(mainstems_us, color="yellow") + mapview(mainstems_ds, color="blue") +
  mapview(mainstems_dd, color="purple") +
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

# bind all mainstems
mainstems_all <- rbind(mainstems_us, mainstems_ds, mainstems_dd)

# 07. SAVE OUT STREAMLINES FOR GAGES ------------------------------------------

save(mainstems_all, file="data_output/03_selected_nhd_mainstems_gages.rda")

# preview
mapview(mainstems_all, zcol="from_gage") + 
  mapview(sel_algae_gages_asci, cex=6, col.regions="orange", 
          layer.name="Selected algae Stations") +  
  mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") +
  mapview(sel_h12_algae, col.regions="slateblue", alpha.regions=0.4)


# 08. FILTER TO algae SITES IN USGS MAINSTEM COMIDS -----------------------------

# reload sites/data here
sel_algae_gages_asci <- readRDS("data_output/03_selected_algae_h12_all_gages_asci.rds")
sel_algae_gages <- readRDS("data_output/03_selected_algae_h12_all_gages.rds")
sel_gages_algae <- readRDS("data_output/03_selected_usgs_h12_all_gages.rds")
sel_h12_algae <- readRDS("data_output/03_selected_h12_all_gages.rds")
load("data_output/03_selected_nhd_mainstems_gages.rda")

# get distinct segs only 
mainstems_distinct <- mainstems_all %>% distinct(nhdplus_comid, .keep_all=TRUE)

# all algae comids that occur in list of mainstem NHD comids: (n=723)
sel_algae_coms_final <- sel_algae_gages_asci %>% 
  filter(COMID %in% mainstems_distinct$nhdplus_comid)

# distinct comid/station/gages combinations:
sel_algae_coms_final %>% st_drop_geometry() %>% 
  distinct(StationCode, site_id) %>% tally() # n=723

# distinct algae COMIDs
sel_algae_coms_final %>% st_drop_geometry() %>% distinct(COMID) %>% tally() # 347

# distinct GAGES COMIDS
sel_algae_coms_final %>% st_drop_geometry() %>% distinct(site_id) %>% tally() # 319

# 09. FINAL MAP -------------------------------------------------------

# create a final map of selected gages and algae + huc12 + flowlines

# get all algae not selected...check why not on map
algae_not_selected <- sel_algae_gages_asci %>% filter(!COMID %in% mainstems_distinct$nhdplus_comid) # should be 317 (loss of 70% of data)

# get all gages selected (n=319)
gages_selected <- sel_gages_algae %>% 
  filter(site_id %in% sel_algae_coms_final$site_id)

# get the gages not selected (n=96)
gages_not_selected <- sel_gages_algae %>% 
  filter(!site_id %in% sel_algae_coms_final$site_id)

# get the hucs selected (n=198)
hucs_selected <- sel_h12_algae %>% 
  filter(HUC_12 %in% sel_algae_coms_final$HUC_12)

# get the hucs not selected (n=53)
hucs_not_selected <- sel_h12_algae %>% 
  filter(!HUC_12 %in% sel_algae_coms_final$HUC_12)


## 
mapviewOptions(fgb = FALSE)

# this map of all sites selected U/S and D/S
m2 <- mapview(sel_algae_coms_final, cex=6, col.regions="orange", 
              layer.name="Selected algae comids") +  
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all algae or gages in same H12 but not selected
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(algae_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other algae Sites in H12") + 
  mapview(hucs_selected, col.regions="orange3", alpha.region=0.1, 
          color="orange", legend=F, layer.name="Selected HUC12") +
  mapview(hucs_not_selected, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="Other HUC12")

m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save this final map out as:"map_of_final_gages_algae_stations_all_gages"
#mapshot(m2, url = paste0(here::here(),"/figs/03_map_of_final_algae_stations_gages_h12s.html"))


# ADD SITES MANUALLY ------------------------------------------------------

## UPDATED 11-30-2020

gages_to_drop <- c(11084500)
gages_to_add <- c(11063000, 11063510, 11152300, 11414000, 11379000, 11477500, 11527000)
asci_to_add <- c("905SDBDN9", "801S01523","801RB8309", "801RB8483", 
                 "801RB8396", "SMCR8_327", "309SAC","113GAR084", "519CE0531",
                 "517PS0030", "509FCA054", "111CE0089", "106WE0580")
asci_to_drop <- c("801RB8593")


# do the thing:
sel_algae_coms_final_v2 <- st_drop_geometry(sel_algae_coms_final) %>% 
  # add n=13
  bind_rows(., filter(st_drop_geometry(sel_algae_gages_asci), StationCode %in% asci_to_add)) %>%
  # filter out the stuff we don't want
  filter(!StationCode %in% asci_to_drop, 
         !site_id %in% gages_to_drop) 

# re-make the geom using algae stations
sel_algae_coms_final_v2 <- st_as_sf(sel_algae_coms_final_v2, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

### MAP AGAIN

# not selected algae
algae_not_selected_v2 <- sel_algae_gages_asci %>% filter(!StationCode %in% sel_algae_coms_final_v2$StationCode) # n=303

# get all gages selected (n=326)
gages_selected_v2 <- sel_gages_algae %>% 
  filter(site_id %in% sel_algae_coms_final_v2$site_id)

# get the gages not selected (n=89)
gages_not_selected_v2 <- sel_gages_algae %>% 
  filter(!site_id %in% sel_algae_coms_final_v2$site_id)

# get hucs selected (n=205)
hucs_selected_v2 <- sel_h12_algae %>% 
  filter(HUC_12 %in% sel_algae_coms_final_v2$HUC_12)

# this map of all sites selected U/S and D/S
m3 <- mapview(sel_algae_coms_final_v2, cex=6, col.regions="orange", 
              layer.name="Selected algae comids") +  
  mapview(mainstems_all, zcol="from_gage", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all algae or gages in same H12 but not selected
  mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(algae_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other algae Sites in H12") + 
  mapview(hucs_selected_v2, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# SAVE OUT ----------------------------------------------------------------

# load the full dataset from 00
load("data_output/00_algae_cleaned_all.rda") # all data

# pull algae sites and get list of data, first join with orig full dataset:
algae_coms_dat <- left_join(sel_algae_coms_final_v2, st_drop_geometry(algae_clean) %>% select(StationCode, SampleID, MM:problemFinalID), by=c("StationCode", "SampleID")) 

# now look at how many unique asci samples are avail: n=437 unique samples
algae_coms_dat %>% st_drop_geometry() %>% distinct(SampleID) %>% tally

# now look at how many unique stations: n=270 stations
algae_coms_dat %>% st_drop_geometry() %>% distinct(StationCode) %>% tally

# now look at how many unique gageID: n=326
algae_coms_dat %>% st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% tally()

# see how many are ref vs alt
ref_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_ref_gages_list.csv")
alt_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_alt_gages_list.csv")

# add CEFF alt type
algae_coms_dat <- algae_coms_dat %>% 
  mutate(CEFF_type = case_when(
    algae_coms_dat$site_id %in% ref_gages$site_id ~ "REF",
    algae_coms_dat$site_id %in% alt_gages$site_id ~ "ALT"
  ))

# look at gages by type
algae_coms_dat %>% 
  st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% group_by(CEFF_type) %>% tally()

#ALT         245
#REF          81

# summary
summary(algae_coms_dat)
hist(algae_coms_dat$MM) # what months?

# if trim to summer months how many records do we lose? (14% of data)
algae_coms_dat_trim <- algae_coms_dat %>% filter(MM>4 & MM<10) 
hist(algae_coms_dat_trim$MM)

# if trimming we lose a few gages: ALT=100, REF=42
algae_coms_dat_trim %>% st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% count(CEFF_type)

#ALT 221
#REF 78

# save out
save(algae_coms_dat, algae_coms_dat_trim, sel_algae_coms_final_v2, file = "data_output/03_selected_final_algae_stations_dat_all_gages.rda")

