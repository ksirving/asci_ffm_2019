# 02b Spatially Linking algae & selected USGS Gages by NHD Flowlines
## R. Peek 2020

## Spatially link the algae station data with the USGS FFC gages that occur in same flowline and h12

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(glue)
library(here)

# 01. Load Data ---------------------------------------------------------------

# selected HUC12s
sel_h12_algae <- read_rds("output_data/02a_sel_h12_w_algae_asci.rds")
sel_h12_gages <- read_rds("output_data/02a_sel_h12_w_ffc_gages.rds")

# selected algae and gages
sel_gages_algae <- read_rds("output_data/02a_sel_ffc_gages_by_h12.rds")
sel_algae_gages_asci <- read_rds("output_data/02a_sel_algae_stations_asci_by_h12.rds")
sel_algae_station_gages_h12 <- read_rds("output_data/02a_sel_algae_stations_h12.rds")

# algae COMIDs
algae_comids <- readRDS("output_data/02b_algae_stations_comids.rds") #%>% 
  # st_drop_geometry() %>% select(StationCode, starts_with("COMID"))

# mainstem flowlines
load("output_data/02b_sel_gage_mainstems_all.rda")

# 02. TIDY UP -------------------------------------------------------------

# MERGE COMIDS with algaes
sel_algae_gages_asci <- left_join(sel_algae_gages_asci, algae_comids, by="StationCode")
sel_algae_station_gages_h12 <- left_join(sel_algae_station_gages_h12, algae_comids, by="StationCode")

# get distinct segs that are DS only (either DD or DS)
mainstems_distinct <- mainstems_all %>% 
  filter(from_gage %in% c("DS", "DD")) %>% 
  distinct(nhdplus_comid, .keep_all=TRUE)

# 03. FILTER TO MAINSTEM COMIDS DS of GAGE -------------------------------
names(sel_algae_gages_asci)
# select all algae COMIDs that occur in downstream mainstem NHD comids: (n=889)
sel_algae_coms_final <- 
  sel_algae_gages_asci %>% # has asci for all samples
  #sel_algae_station_gages_h12 %>%  # doesn't have asci for all samples
  filter(COMID_algae %in% mainstems_distinct$nhdplus_comid)


## 03b. DESCRIBE SITES SELECTED -------------------------------------------------

# distinct comid/station/gages combinations:
sel_algae_coms_final %>% st_drop_geometry() %>% 
  distinct(StationCode, site_id) %>% tally() 
# if using sel_algae_station_gages_h12: n=839 (but only half have asci?)
# if using sel_algae_gages_asci: n=448 (but all have asci)

# distinct algae COMIDs
sel_algae_coms_final %>% st_drop_geometry() %>% distinct(COMID_algae) %>% tally() # 296 (n=187 w asci)

# distinct GAGES COMIDS
sel_algae_coms_final %>% st_drop_geometry() %>% distinct(site_id) %>% tally() # 277 (n=216 w asci)

# get all algae not selected
algae_not_selected <- sel_algae_gages_asci %>% filter(!COMID_algae %in% mainstems_distinct$nhdplus_comid) # n=905 (loss of 50% of data)

# get all gages selected (n=216)
gages_selected <- sel_gages_algae %>% 
  filter(site_id %in% sel_algae_coms_final$site_id)

# get the gages not selected (n=204)
gages_not_selected <- sel_gages_algae %>% 
  filter(!site_id %in% sel_algae_coms_final$site_id)

# get the hucs selected (n=123)
hucs_selected <- sel_h12_algae %>% 
  filter(HUC_12 %in% sel_algae_coms_final$HUC_12)

# get the hucs not selected (n=135)
hucs_not_selected <- sel_h12_algae %>% 
  filter(!HUC_12 %in% sel_algae_coms_final$HUC_12)

# 04. FINAL MAP -------------------------------------------------------

# set mapview so we can save to html
mapviewOptions(fgb = FALSE)

# this map of all sites selected U/S and D/S
m1 <- mapview(sel_algae_coms_final, cex=6, col.regions="orange", 
              layer.name="Selected algae comids") +  
  mapview(mainstems_all %>% filter(from_gage=="UM"), color="forestgreen", cex=3, 
          layer.name="NHD Flowlines US") +
  mapview(mainstems_distinct, color="steelblue", cex=3, 
          layer.name="NHD Flowlines DS") +
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

m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save out
mapshot(m1, url = paste0(here::here(),"/figs/02c_map_of_selected_algae_gage_h12s_DS.html"))

#### do the below later!!!!!
# 05. ADD/DROP SITES MANUALLY ---------------------------------------------------

## UPDATED 2020-12-14
# visually inspect the map above, and add/revise sites

# site_adjustments <- read.csv("/Users/katieirving/Documents/git/asci_ffm_2019/input_data/manual_sites_adjustments.csv")
# names(site_adjustments)
# gages_to_dropx <- site_adjustments %>%
#   filter(action == )
# DROP THESE SITES
gages_to_drop <- c("11404100","10338000","10308794",
                   "11238270", "11238400", "11238380",
                   "11118400", "11062800", "11062700",
                   "11062820", "11058500", "11055350",
                   "11049590", "11049500", "11065000",
                   "11046360", "10336645")

asci_to_drop <- c("634R10GNL", "SMC00382", "801SAR110",
                  "SMC00382", "911COPPER")

# ADD THESE SITES
gages_to_add <- c("11388000", # STONY C BL BLACK BUTTE DAM NR ORLAND CA: algae:504PS0147
                  "11379500", # ELDER C NR PASKENTA CA (site is 10.4km d/s): "	504PS0083", 634WRD029
                  "11230215", # SF SAN JOAQUIN R BL HOOPER C NR FLORENCE LAKE CA: 540SSJ200, 
                  # "	ME-VR2", 402M00002
                  "11109800" # "	PIRU CREEK BELOW SANTA FELICIA DAM CA": 403R4S193, 403M01522
)
asci_to_add <- c(
  "504PS0147",
  "504PS0083",
  "634WRD029",
  "540SSJ200",
  "ME-VR2",
  "402M00002",
  "403R4S193",
  "403M01522"
 )

# pull out asci sites to add first
add_algae_asci <- sel_algae_gages_asci %>%
  filter(StationCode %in% asci_to_add) 
head(add_gages)
dim(add_gages)
# now add gages that are missing (and one row for each station)
add_gages <- sel_gages_algae %>% filter(site_id %in% gages_to_add) %>% 
  bind_rows(., sel_gages_algae %>% filter(site_id=="11388000")) %>% 
  bind_rows(., sel_gages_algae %>% filter(site_id=="11379500")) %>%
  bind_rows(., sel_gages_algae %>% filter(site_id=="11230215")) %>%
  bind_rows(., sel_gages_algae %>% filter(site_id=="11109800")) %>% 
  dplyr::arrange(site_id) %>% 
  # add station codes in same order as gage ID
  mutate(StationCode = c(   "504PS0147",
                            "504PS0083",
                            "634WRD029",
                            "540SSJ200",
                            "ME-VR2",
                            "402M00002",
                            "403R4S193",
                            "403M01522"), .before=agency_cd)
names(sel_algae_gages_asci)
# filter to algae sites that have this
add_algae_w_gages <- filter(sel_algae_gages_asci, StationCode %in% add_gages$StationCode) %>% 
  # drop gage info:
  select(StationCode:Longitude.x, HUC_12, SampleID:COMID_algae) %>% 
  left_join(., st_drop_geometry(add_gages), by="StationCode")

# view added gages w algae
mapview(add_gages, col.regions="skyblue") + mapview(add_algae_w_gages, col.regions="orange", cex=4)

# bind back with final list
sel_algae_coms_final_v2 <- sel_algae_coms_final %>% 
  st_drop_geometry() %>% 
  # filter out the stuff we don't want
  filter(!StationCode %in% asci_to_drop, 
         !site_id %in% gages_to_drop) %>% 
  bind_rows(., add_algae_w_gages) %>% 
  select(-geometry) # drop all geoms

# kept exact same number?

# re-make the geom using algae stations
sel_algae_coms_final_v2 <- st_as_sf(sel_algae_coms_final_v2, coords=c("Longitude.x", "Latitude.x"), crs=4269, remove=FALSE)


## Re-Map Final Map -------------------------------------------------------

# not selected algae ### filter them later!!!!
algae_not_selected_v2 <- sel_algae_gages_asci %>% filter(!StationCode %in% sel_algae_coms_final_v2$StationCode) # n=884
algae_not_selected_v2 <- sel_algae_gages_asci

# get all gages selected (n=226)
gages_selected_v2 <- sel_gages_algae %>% 
  filter(site_id %in% sel_algae_coms_final_v2$site_id)

# get the gages not selected (n=189)
gages_not_selected_v2 <- sel_gages_algae %>% 
  filter(!site_id %in% sel_algae_coms_final_v2$site_id)

# get hucs selected (n=146)
hucs_selected_v2 <- sel_h12_algae %>% 
  filter(HUC_12 %in% sel_algae_coms_final_v2$HUC_12)

# get hucs not selected (n=105)
hucs_not_selected_v2 <- sel_h12_algae %>% 
  filter(!HUC_12 %in% sel_algae_coms_final_v2$HUC_12)

# this map of all sites selected U/S and D/S
m2 <- mapview(sel_algae_coms_final_v2, cex=6, col.regions="orange", 
              layer.name="Selected algae comids") +  
  mapview(mainstems_all %>% filter(from_gage=="UM"), color="forestgreen", cex=3, 
          layer.name="NHD Flowlines US") +
  mapview(mainstems_distinct, color="steelblue", cex=3, 
          layer.name="NHD Flowlines DS") +
  mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all algae or gages in same H12 but not selected
  mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(algae_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other algae Sites in H12") + 
  mapview(hucs_selected_v2, col.regions="orange3", alpha.region=0.1, 
          color="orange", legend=F, layer.name="Selected HUC12") +
  mapview(hucs_not_selected_v2, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="Other HUC12")

m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# 06. SAVE OUT ----------------------------------------------------------------

# algae_final <- sel_algae_coms_final
algae_final <- sel_algae_coms_final_v2 

# now look at how many unique asci samples are avail: n=247 unique samples
algae_final %>% st_drop_geometry() %>% distinct(SampleID) %>% tally

# now look at how many unique algae stations: n=247 stations
algae_final %>% st_drop_geometry() %>% distinct(StationCode) %>% tally

# now look at how many unique USGS gages: n=200
algae_final %>% st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% tally()

# add CEFF alt type
# look at gages by type
algae_final %>% 
  st_drop_geometry() %>% 
  distinct(site_id, .keep_all=TRUE) %>% group_by(CEFF_type) %>% tally()
# ALT         152
# REF          48

# summary
summary(algae_final)


# load the full dataset from 00
load("output_data/01a_all_asci_data_clean.RData") # all data
names(algae4)
view(algae4)
algae_clean <- algae4 %>% select(-D_ASCI) %>% 
  separate(SampleDate, c("YYYY", "MM", "DD")) %>%
  mutate(MM = as.numeric(MM)) %>%
  distinct(.keep_all=TRUE)

# join with orig full dataset to add month/date
algae_final_dat <- left_join(algae_final, algae_clean, by=c("StationCode", "SampleID"))
# view(algae_final_dat)
# str(algae_final_dat)
## need to join with all data to get the month info...
hist(algae_final_dat$MM) # what months?

# if trim to summer months how many records do we lose? (2% of data)
algae_final_dat_trim <- algae_final_dat %>% filter(MM>4 & MM<10) 
hist(algae_final_dat_trim$MM)

# if trimming we lose a few gages: 
algae_final_dat_trim %>% st_drop_geometry() %>% distinct(site_id, .keep_all=TRUE) %>% count(CEFF_type)

#ALT 151
#REF 48

## SAVE OUT
write_rds(algae_final_dat, file="output_data/02c_selected_final_algae_asci_dat.rds")
write_rds(algae_final_dat_trim, file="output_data/02c_selected_final_algae_asci_dat_trim.rds")

algae_final_dat_trim <- read_rds(file="output_data/02c_selected_final_algae_asci_dat_trim.rds")
names(algae_final_dat_trim)
algae_final_dat_trim <- algae_final_dat_trim %>%
  select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y, -H_ASCI.y) %>%
  rename(H_ASCI = H_ASCI.x)

write_rds(algae_final_dat_trim, file="output_data/02c_selected_final_algae_asci_dat_trim.rds")

# save all ### save later once checked gages!!!!!
save(algae_final_dat, algae_not_selected_v2, 
     gages_selected_v2, gages_not_selected_v2,
     hucs_selected_v2, hucs_not_selected_v2,
     add_algae_w_gages, file = "output_data/02c_selected_final_algae_dat_all.rda")

# save(algae_final_dat, file = "output_data/02c_selected_final_algae_dat_all.rda")

