######### spatial join with gauges

library(tidyverse)
# install.packages("tidyverse")
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

# data needed
load(file="output_data/01_clean_algae.RData") # algae
load(file="input_data/01_usgs_all_gages.rda") # all gages 
load(file="input_data/huc12_sf.rda") # h12 - huc 12 data
# load(file="input_data/00_usgs_ca_all_daily_flow_gages.rda") # ca_usgs_gages  all gauges CA

str(usgs_final_all)
names(algae)

#  coords into numeric
algae$Latitude <- as.numeric(as.character(algae$Latitude))
algae$Longitude <- as.numeric(as.character(algae$Longitude))

#  remove NAs
# sum(is.na(algae)) # 50 x NAs but not registering until formatted to a number, remaining are nas fro some variables
# #  where are the NAs? 
# na_ind <- which(is.na(algae)) 
# na_ind

algae <- na.omit(algae)

## make df with just sites and coords for slage sites
algae
algae_stations_distinct <- algae[,c(1:2,4:5)]
algae_stations_distinct<- algae_stations_distinct[!duplicated(algae_stations_distinct$StationID), ]
dim(algae_stations_distinct) ## 1688
head(algae_stations_distinct)

## make spatial
algae <- algae %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)

algae_stations_distinct <- algae_stations_distinct %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)

gages <- usgs_final_all %>% st_transform(4326)
rm(usgs_final_all)

head(algae)

# check projs are same
st_crs(algae)
st_crs(gages)
st_crs(h12)
st_crs(algae_stations_distinct)



# Filter to same temporal scale as algae data, years must be post 1994 (n=483 remaining)
gages_all_filt <- gages %>% filter(end_yr > 2004)
table(gages_all_filt$CEFF_type) ## ALT = 334 REF = 122 
 

# FILTER-Intersect algae/Gages by H12 -----------------------------------

# Add H12 to points to algae and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using algae
algae_h12 <- st_join(algae_stations_distinct, left = TRUE, h12[c("HUC_12")])
head(algae_h12)
head(h12)

# Add H12 to all gages
gages_h12 <- st_join(gages_all_filt, left=TRUE, h12[c("HUC_12")]) %>%
  st_drop_geometry()
gages_h12
# now join based on H12: what algae stations share same H12 as USGS gage? (N=1000)
sel_algae_gages <- inner_join(algae_h12, gages_h12, by="HUC_12") %>% 
  distinct(StationID, ID, .keep_all = T) # n=1000
sel_algae_gages
# number of unique?
length(unique(factor(sel_algae_gages$HUC_12))) # h12=153
length(unique(sel_algae_gages$ID)) # gages=189
length(unique(sel_algae_gages$StationID)) # algae Stations=422

sel_algae_gages
algae_stations_distinct
## already have this - sel_algae_gages (algae sites, asci values, gage and huc12 data)
# # make sure these have ASCI scores: of those in same H12, how many have asci scores? N=552
sel_algae_gages_asci <- left_join(sel_algae_gages, st_drop_geometry(algae_stations_distinct), by="StationID") %>%
  # filter(!is.na(asci)) %>%
  distinct(StationID, ID, .keep_all=TRUE)

#Get Selected Gages ONLY:  # n=189 (that have ASCI scores)
  sel_gages_algae <- gages_all_filt %>% 
  filter(ID %in% sel_algae_gages$ID) %>% 
  distinct(ID, .keep_all = T)
names(sel_algae_gages)
# length(unique(sel_gages_algae$ID))
  
# select H12s that have points inside: # n=163
  sel_h12_algae <- h12[sel_gages_algae, ]
  sel_h12_gages <- h12[gages_all_filt, ]
  
  # * Map of Filtered Gages ------------------------------------------------------
  
  # set background basemaps:
  basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                    "OpenTopoMap", "OpenStreetMap", 
                    "CartoDB.Positron", "Stamen.TopOSMFeatures")
  
  mapviewOptions(basemaps=basemapsList)
  
  # a map of all gages and algae stations that fall within the same H12
  
  # get the gages not selected
  gages_not_selected <- gages_all_filt %>% 
    filter(!ID %in% sel_gages_algae$ID)
  
  save(gages_not_selected, file="output_data/02_gages_not_selected.RData")
  
  # get the gages  selected
  gages_selected <- gages_all_filt %>% 
    filter(ID %in% sel_gages_algae$ID)
  
  save(gages_selected, file="output_data/02_gages_selected.RData")
  
  table(sel_gages_algae$CEFF_type) # ALT=137  REF=52
  table(gages_not_selected$CEFF_type) # ALT=188  REF=70

  # get  algae NOT selected with ASCI
  algae_not_selected <- algae %>% 
    # filter(!is.na(asci)) %>% 
    filter(!StationID %in% sel_algae_gages$StationID) %>% 
    distinct(StationID, .keep_all=TRUE)
  
  save(algae_not_selected, file="output_data/02_algae_sites_not_selected.RData")
  
  # this map of all sites selected U/S and D/S
  m1 <- mapview(sel_algae_gages, cex=6, col.regions="orange", 
                layer.name="Selected Algae Stations") +  
    mapview(sel_gages_algae, col.regions="skyblue", cex=7, color="blue2",
            layer.name="Selected USGS Gages") + 
    # these are all algae or gages in same H12 but not selected
    mapview(gages_not_selected, col.regions="slateblue", color="gray20",
            cex=3.2, layer.name="Other USGS Gages") + 
    mapview(algae_not_selected, col.regions="gold2", color="gray20", cex=3.2, 
            layer.name="Other Algae Sites w ASCI Scores") + 
    mapview(algae_stations_distinct, col.regions="gray", color="gray20", cex=3, 
            layer.name="All Algae Sites") + 
    mapview(sel_h12_algae, col.regions="dodgerblue", alpha.region=0.1, 
            color="darkblue", legend=FALSE, layer.name="HUC12") + 
    mapview(sel_h12_gages, col.regions="gray50", alpha.region=0.1, 
            color="darkblue", legend=FALSE, layer.name="HUC12 Gages")
  
  m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
  
  # * Save Out -----------------------------------------------------------------
  
  # save out
  write_rds(sel_h12_algae, path="output_data/02_selected_h12_all_gages.rds")
  write_rds(sel_gages_algae, path="output_data/02_selected_usgs_h12_all_gages.rds")
  write_rds(sel_algae_gages_asci, path="output_data/02_selected_algae_h12_all_gages_asci.rds")
  write_rds(sel_algae_gages, path="output_data/02_selected_algae_h12_all_gages.rds")
  
  #05. algae COMIDS: GET NEW/MISSING COMIDS --------------------------
    
    # IF NEEDED
    
  library(nhdplusTools)
  
  names(sel_algae_gages)
  names(algae_stations_distinct)

  ## TRANSFORM TO SAME DATUM
  sel_algae_gages <- st_transform(sel_algae_gages, crs = 3310) # use CA Teale albs metric
  sel_gages_algae <- st_transform(sel_gages_algae, crs=3310)

  # Create dataframe for looking up COMIDS (here use all stations)
  algae_segs <- st_transform(algae_stations_distinct, crs=3310) %>%
    #select(StationID, longitude, latitude) %>%
    mutate(comid=NA)

  # use nhdtools to get comids
  algae_all_coms <- algae_segs %>%
    group_split(StationID) %>%
    set_names(., algae_segs$StationID) %>%
    map(~discover_nhdplus_id(.x$geometry))

  # # flatten into single dataframe instead of list
  algae_segs_df <-algae_all_coms %>% flatten_dfc() %>% t() %>%
    as.data.frame() %>%
    rename("comid"=V1) %>% rownames_to_column(var = "StationID")

  # rm COMIDs starting with "V"
  algae_comids <- algae_segs_df %>% filter(!grepl("^V", StationID))

  # save out
  write_rds(algae_comids, path="output_data/02_algae_all_stations_comids.rds")

  # clean up
  rm(algae_all_coms, algae_segs_df, algae_segs)

  # * Load And Join -----------------------------------------------------------
  
  # load previous stuff run from Step 05 algae COMIDS
  algae_comids <- read_rds("output_data/02_algae_all_stations_comids.rds")
  # head(algae_comids)
  # dim(algae_comids) ## 1680
  # head(sel_algae_gages)
  # dim(sel_algae_gages)
  # rejoin with the selected algae sites in same HUC12 as gage
  sel_algae_gages <- sel_algae_gages %>% left_join(., algae_comids, by="StationID") # n=2188

  summary(sel_algae_gages$comid) # 1 missing, site VRMT3
  nas <- which(is.na(sel_algae_gages$comid)) ## 532
  sel_algae_gages[nas,]

  write_rds(sel_algae_gages, path="output_data/02_selected_algae_h12_all_gages.rds")
  
  # 06. GET GAGE COMIDS --------------------------------------------------
  
  # if any comids missing use code below
  
  ## TRANSFORM TO UTM datum for flowlines
  # sel_algae_gages <- st_transform(sel_algae_gages, crs=3310) # use CA Teale albs metric
  # sel_gages_algae <- st_transform(sel_gages_algae, crs=3310)
  # 
  # # get the COMID for each gage in list
  # usgs_segs <- sel_gages_algae %>% split(.$StationID) %>%
  #   map(~discover_nhdplus_id(.x$geometry))
  # 
  # # now have a list of all the missing COMIDs, check for dups
  # usgs_segs %>%
  #   purrr::map_lgl(~ length(.x)>1) %>%
  #   #table() # 3 are FALSE
  #   .[.==TRUE] # get values that are TRUE
  # 
  # # view comids
  # usgs_segs["11186000"]
  # 
  # # fix 326 and 327 which pull two segs
  # usgs_segs["11186000"] <- 14971709
  # usgs_segs["11186001"] <- 14971711
  # usgs_segs["11404240"] <- 2775510
  # 
  # # double check again:
  # usgs_segs %>%
  #   purrr::map_lgl(~ length(.x)>1) %>% table() # all FALSE
  # 
  # save the USGS station COMIDs file:
  # write_rds(usgs_segs, path="data_output/02_selected_usgs_gages_comids.rds")
  

  ## check comids
  
  head(sel_algae_gages)
  sum(is.na(sel_algae_gages$NHDV1_COMID)) ## 0 all present!
  
  # 07. GET UPSTREAM FLOWLINES FROM GAGE --------------------------------------------------
  
  # install.packages("devtools")
  # devtools::install_github("USGS-R/nhdplusTools")
  # install.packages("data.table")
  library(nhdplusTools)
  library(data.table)
  
  ## TRANSFORM TO UTM datum for flowlines
  sel_algae_gages_asci <- st_transform(sel_algae_gages_asci, crs=3310) # use CA Teale albs metric
  sel_gages_algae <- st_transform(sel_gages_algae, crs=3310)
  
  # use a list of comids to make a list to pass to the nhdplusTools function
  # important to use NHDV2 COMID here or it will skip/miss out on sites
  coms_list <- map(sel_gages_algae$NHDV2_COMID, ~list(featureSource = "comid", featureID=.x))
  coms_list[[100]] # tst check, should list feature source and featureID
  coms_list
  # Get Mainstem Segs, needed to do in chunks if needed and rbind
  mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                               mode="upstreamMain",
                                               data_source = ""))
  
  # check length (for NAs?)
  mainstemsUS %>% 
    purrr::map_lgl(~ length(.x)>1) %>% table()
  
  # transform the sf layer to match mainstems crs (4326)
  sel_gages_algae <- sel_gages_algae %>% st_transform(4326)
  
  # make a single flat layer
  mainstems_flat_us <- mainstemsUS %>%
    set_names(., sel_gages_algae$gage_id) %>%
    map2(sel_gages_algae$gage_id, ~mutate(.x, gageID=.y))
  
  # bind together
  mainstems_us <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_us, use.names = TRUE, fill = TRUE))
  
  # add direction to gage col
  mainstems_us <- mainstems_us %>% 
    mutate(from_gage = "US")
  
  # rm temp files
  rm(mainstems_flat_us, mainstemsUS)
  
  # save as both for now
  save(mainstems_us, file = "output_data/02_selected_nhd_mainstems_gages_us_ds.rda")
  
  
  # * ADD ADDITIONAL GAGE SITES? ----------------------------------------------
  
  ### ADD ADDITIONAL SITES?
  # T11206500, T11208000, T11152050
  
  gages_to_add <- sel_gages_algae %>% filter(ID %in% c("T11206500", "T11208000", "T11152050", "T11153650"))
  
  # double check?
  missing_segs <- gages_to_add %>% split(.$ID) %>%
    map(~discover_nhdplus_id(.x$geometry))
  
  gages_to_list <- map(gages_to_add$NHDV1_COMID, ~list(featureSource = "comid", featureID=.x))
  
  gages_to_list # tst check, should list feature source and featureID
  
  # Get Mainstem Segs, needed to do in chunks if needed and rbind
  mainstemsUS_miss <- map(gages_to_list, ~navigate_nldi(nldi_feature = .x,
                                                        mode="upstreamMain",
                                                        data_source = ""))
  
  # check length (for NAs (==FALSE))
  mainstemsUS_miss %>% 
    purrr::map_lgl(~ length(.x)>1) %>% table()
  
  mainstems_miss_us <- mainstemsUS_miss %>%
    set_names(., gages_to_add$ID) %>%
    map2(gages_to_add$ID, ~mutate(.x, gageID=.y))
  mainstems_miss_us <- sf::st_as_sf(data.table::rbindlist(mainstems_miss_us, use.names = TRUE, fill = TRUE))
  
  mapview(mainstems_miss_us)
  
  # 08. GET DOWNSTREAM FLOWLINES FROM GAGE ------------------------------------------------
  
  # get NHD segments downstream of selected USGS gages, 10 km buffer
  mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                               mode="downstreamMain",
                                               distance_km = 10,
                                               data_source = ""))
  
  # check length (for NAs?)
  mainstemsDS %>% 
    purrr::map_lgl(~ length(.x)>1) %>% table()
  
  # make a single flat layer
  mainstems_flat_ds <- mainstemsDS %>%
    set_names(., sel_gages_algae$gage_id) %>%
    map2(sel_gages_algae$gage_id, ~mutate(.x, gageID=.y))
  
  # bind together
  mainstems_ds <- sf::st_as_sf(data.table::rbindlist(mainstems_flat_ds, use.names = TRUE, fill = TRUE))
  
  
  # add direction to gage col
  mainstems_ds <- mainstems_ds %>% 
    mutate(from_gage = "DS")
  
  rm(mainstems_flat_ds, mainstemsDS)
  
  # save as both
  save(mainstems_us, mainstems_ds, file = "output_data/02_selected_nhd_mainstems_gages_us_ds.rda")
  
  # bind all mainstems
  mainstems_all <- rbind(mainstems_us, mainstems_ds)
  
  # 09. SAVE OUT STREAMLINES FOR GAGES ------------------------------------------
  
  save(mainstems_all, file="output_data/02_selected_nhd_mainstems_gages.rda")
  
  # 10. FILTER TO algae SITES IN USGS MAINSTEM COMIDS -----------------------------
  
  # reload sites/data here
  sel_algae_gages_asci <- readRDS("output_data/02_selected_algae_h12_all_gages_asci.rds")
  sel_algae_gages <- readRDS("output_data/02_selected_algae_h12_all_gages.rds")
  sel_gages_algae <- readRDS("output_data/02_selected_usgs_h12_all_gages.rds")
  sel_h12_algae <- readRDS("output_data/02_selected_h12_all_gages.rds")
  load("output_data/02_selected_nhd_mainstems_gages.rda")
  names(sel_algae_gages_asci)
  # get distinct segs only
  mainstems_distinct <- mainstems_all %>% distinct(nhdplus_comid, .keep_all=TRUE)
  # head(sel_algae_gages_asci)
  # all algae comids that occur in list of mainstem NHD comids: (n=353)
  sel_algae_coms_final <- sel_algae_gages %>% 
    filter(comid %in% as.integer(mainstems_distinct$nhdplus_comid))
  
  # distinct comid/station/gages combinations:
  sel_algae_coms_final %>% st_drop_geometry() %>% 
    distinct(StationID, ID) %>% tally() # n=331
  
  # distinct algae COMIDs
  sel_algae_coms_final %>% st_drop_geometry() %>% distinct(comid) %>% tally() # 217
  
  # distinct GAGES COMIDS
  sel_algae_coms_final %>% st_drop_geometry() %>% distinct(ID) %>% tally() # 155
  
  # 11. FINAL MAP -------------------------------------------------------
  
  # create a final map of selected gages and algae + huc12 + flowlines
  
  # get all algae not selected...check why not on map
  algae_not_selected <- sel_algae_gages %>% filter(!as.character(comid) %in% mainstems_distinct$nhdplus_comid) # should be 199 (loss of 64% of data)
  dim(algae_not_selected)
  # get all gages selected (n=201)
  gages_selected <- sel_gages_algae %>% 
    filter(gage_id %in% sel_algae_coms_final$gage_id)
  dim(gages_selected) ## 155 - ALL COMIDs SELECTED - WHY?
  # get the gages not selected (n=51)
  gages_not_selected <- sel_gages_algae %>% 
    filter(!gage_id %in% sel_algae_coms_final$gage_id)
  
  table(gages_selected$CEFF_type) # ALT=109  REF=46
  
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
            layer.name="Other Algae Sites in H12") +
    mapview(sel_h12_algae, col.regions="dodgerblue", alpha.region=0.1,
            color="darkblue", legend=F, layer.name="HUC12")
  
  m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
  
  ## comids fixed!!
  
  # install.packages("here")
  library(here)
   #here() starts at /Users/katieirving/Documents/git/asci_ffm_2019
  # save this final map out as:"map_of_final_gages_algae_stations_all_gages"
  mapshot(m2, url = paste0(here::here(),"/figs/02_map_of_final_algae_stations_gages_h12s.html"))
  
  ## look at this in detail later!!!!!!
  # ADD SITES MANUALLY ------------------------------------------------------
  
  # TO ADD: these sites were added because they likely didn't snap to NHD line
  ## - Arroyo Seco, ID = T11152050, StationCode = 309SET
  ## - SANTA GERTRUDIS, ID = T11042900, Station Code = 902MCGSxx
  ## - ARROYO TRABUCO, StationCode = "901M14134"
  ## - CAJON CK, ID = T11063510, StationCode = 801RB8483
  ## - CAJON CK, ID = T11063510, StationCode = 801RB8396
  ## - CAJON CK, ID = T11063510, StationCode = SMCR8_327
  ## - SAN GABRIEL R = T11085000, StationCode = SGUR010
  ## - MARBLE F KAWEAH = T11208000 , StationCode = 553WER224
  
  # TO ADD MAYBE??
  ## - AMARGOSA = T10251300, StationCode = 609PS0053 # this is about 12 km d/s, no inputs btwn
  ## - TUOLUMNE = T11290000, StationCode = 535CR0910 # this is about 12 km d/s, but no inputs btwn
  
  # TO DROP: these sites were dropped because they are not co-located properly, to far, off channel, etc
  ## - ID=T11048600, StationCode=801RB8593
  ## - ID=T11048553. StationCode=801RB8593
  ## StationCode=412LARSCO, LALT501, 901ATCTCx, 901TCSMP1, 
  ## ID = T11087020 (7 sites over 15km downstream)
  ## StationCode = 403FCA038, 403STC019
  
  # # do the thing:
  # sel_algae_coms_final_v2 <- st_drop_geometry(sel_algae_coms_final) %>% bind_rows(
  #   # original final dataset (keep)
  #   .,
  #   # update with these records (n=13)
  #   filter(st_drop_geometry(sel_algae_gages_asci), 
  #          StationCode %in% c("309SET", "902MCGSxx", "901M14134","801RB8483", 
  #                             "801RB8396", "SMCR8_327", "SGUR010",  "553WER224",
  #                             # the maybes
  #                             "609PS0053", "535CR0910"))
  # ) %>% 
  #   # now filter out the stuff we don't want (n=21)
  #   filter(!StationCode %in% c("801RB8593", "412LARSCO", "LALT501", "901ATCTCx", "901TCSMP1",
  #                              "403FCA038", "403STC019"), 
  #          !ID %in% c("T11087020")) 
  # 
  # re-make the geom
  sel_algae_coms_final_v2 <- st_as_sf(sel_algae_coms_final, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)
  
  ### MAP AGAIN
  
  # not selected algae
  algae_not_selected_v2 <- sel_algae_gages_asci %>% filter(!as.character(StationID) %in% sel_algae_coms_final_v2$StationID) # n=203
  
  # get all gages selected (n=160)
  gages_selected_v2 <- sel_gages_algae %>% 
    filter(ID %in% sel_algae_coms_final_v2$ID)
  
  # get the gages not selected (n=47)
  gages_not_selected_v2 <- sel_gages_algae %>% 
    filter(!ID %in% sel_algae_coms_final_v2$ID)
  
  table(gages_selected_v2$CEFF_type) # ALT=116  REF=44
  
  # this map of all sites selected U/S and D/S
  m3 <- mapview(sel_algae_coms_final_v2, cex=6, col.regions="orange", 
                layer.name="Selected algae comids") +  
    mapview(mainstems_all, color="steelblue", cex=3, 
            layer.name="NHD Flowlines") +
    mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
            layer.name="Selected USGS Gages") + 
    # these are all algae or gages in same H12 but not selected
    mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
            cex=3.2, layer.name="Other USGS Gages") + 
    mapview(algae_not_selected, col.regions="gold", color="gray20", cex=3.2, 
            layer.name="Other algae Sites in H12") + 
    mapview(sel_h12_algae, col.regions="dodgerblue", alpha.region=0.1, 
            color="darkblue", legend=F, layer.name="HUC12")
  
  m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
  
  ### do this, add/remove sites later!!!!
  # SAVE OUT ----------------------------------------------------------------
  
  # load the full dataset from 00
  
  str(algae)
  algae <- as_tibble(algae) 
  # pull algae sites and get list of data, first join with orig full dataset:
  names(algae)
  
  algae <- separate(algae, col = sampledate , into=c("YY", "MM", "DD"), remove = F)

  algae_coms_dat <- left_join(sel_algae_coms_final, algae , by=c("StationID")) 
  names(algae_coms_dat)
  # now look at how many unique samples are avail: n=426 unique samples
  algae_coms_dat %>% st_drop_geometry() %>% distinct(SampleID_old.x) %>% tally
  
  # now look at how many unique stations: n=268 stations
  algae_coms_dat %>% st_drop_geometry() %>% distinct(StationID) %>% tally
  
  # now look at how many unique gageID: n=160 stations (ALT=109, REF=46)
  algae_coms_dat %>% st_drop_geometry() %>% distinct(ID, .keep_all=TRUE) %>% count(CEFF_type)
  names(algae_coms_dat)
  
  
  
  # summary
  summary(algae_coms_dat)
  algae_coms_dat$MM <- as.numeric(as.character(algae_coms_dat$MM))
  hist(as.numeric(as.character(algae_coms_dat$MM))) # what months?
  # if trim to summer months how many records do we lose? 
  algae_coms_dat_trim <- algae_coms_dat %>% filter(MM>4 & MM<10) 
  hist(algae_coms_dat_trim$MM)
  
  # if trimming we lose a few gages: ALT=106, REF=45 - but not many!!!
  algae_coms_dat_trim %>% st_drop_geometry() %>% distinct(ID, .keep_all=TRUE) %>% count(CEFF_type)
  
  
  # save out
  save(algae_coms_dat, algae_coms_dat_trim, sel_algae_coms_final, file = "output_data/03_selected_final_algae_stations_dat_all_gages.rda")
  
  ## continue here if needed!
  # Z-ARCHIVE: Measuring Nearest and Line Lengths --------------------------------------
  
  # this is mostly experimental code snapping points to lines and updating/measuring distances
  
  # get a site, mainstem river and gage
  algae1 <- st_transform(sel_algae_coms_final[1,], 4326) # the site
  ln1 <- mainstems_distinct %>% filter(gageID==11532500) # mainstem
  gage1 <- sel_gages_algae %>% filter(gage_id==11532500) # the gage
  
  # calculates nearest point from each riverline segment to single point
  pts_nearest <- st_nearest_points(ln1, algae1)
  
  # find shortest difference from point to nearest line and cast to point
  pt_best <- st_cast(pts_nearest[which.min(st_length(pts_nearest))], "POINT")[1]
  
  # quick map
  mapview(pt_best, col.regions="orange") + 
    mapview(algae1, color="red") + 
    mapview(ln1, color="green")
  
  # this generates poins on line that has lat/lon 
  ln_points <- ln1 %>% st_transform(3310) %>% 
    group_by(nhdplus_comid) %>%
    # with lat/lon: units::set_units(25, m)
    st_segmentize(., dfMaxLength = units::set_units(25, m)) %>% 
    st_sf() %>%
    st_cast('POINT') %>% 
    ungroup()
  
  # mm1 <- mapview(ln_points) + mapview(ln_points2)
  # mm1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
  
  # find index of point closest to algae site
  ln_pt_best <- which.min(st_distance(st_transform(algae1, 4326), st_transform(ln_points, 4326)))
  ln_pt_nearest <- ln_points[ln_pt_best,]
  
  # make a segment from split pt to end
  segment1 <- ln_points[ln_pt_best:nrow(ln_points),] %>% 
    group_by(nhdplus_comid) %>% 
    dplyr::summarize(do_union = FALSE) %>%
    st_cast("LINESTRING") %>% 
    ungroup()
  
  # make a segment from split to beginning
  segment2 <- ln_points[1:ln_pt_best,] %>% 
    group_by(nhdplus_comid) %>% 
    dplyr::summarize(do_union = FALSE) %>%
    st_cast("LINESTRING") %>% 
    ungroup %>% 
    st_union() %>% # merge back into a single line
    st_as_sf()
  
  # make a final map
  mapview(segment1, lwd=5, color="orange", legend=FALSE) +
    mapview(segment2, lwd=5, color="blue", legend=FALSE) +
    mapview(algae1, col.regions="red") + 
    mapview(ln_pt_nearest, col.regions="yellow")+
    mapview(ln1, lwd=2, color="green")
  
  
  
