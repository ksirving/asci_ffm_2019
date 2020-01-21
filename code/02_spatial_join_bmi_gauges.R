######### spatial join with bmi and gauges

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

#  load clean algae data

load(file="output_data/01_clean_algae.RData") # algae
head(algae) 
dim(algae) #2231   13

## convert to spatial for pairing - 1) with bmi (overlap), 2) ref gauges

#  coords into numeric
algae$Latitude <- as.numeric(as.character(algae$Latitude))
algae$Longitude <- as.numeric(as.character(algae$Longitude))

#  remove NAs
sum(is.na(algae)) # 50 x NAs but not registering until formatted to a number, remaining are nas fro some variables
#  where are the NAs? 
na_ind <- which(is.na(algae)) 
na_ind

#  check in unformatted dataset for NAs 
# 
# load(file="output_data/clean_algae.RData") # algae
# head(algae)
# sum(is.na(algae))
# algae[na_ind,] # just NAs - full rows

algae <- na.omit(algae)

#  spatial point df
# ?st_as_sf
# str(algae)

algae <- algae %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F) # define coords to make spatial

save(algae, file="output_data/02_algae_spatial.RData")

#  component metrics for Algae - only OoverE & MMI for diatoms and soft bodied
#  awaiting dataset

#  bug data

load(file="input_data/bmi_cleaned_all.rda")
head(bmi_clean)
str(bmi_clean)
# ?distinct
#  which is bmi site data from bmi_ffm? in script 03 it's bmi_final_station_list.rda but not in output data file??
#  get df with only sites

bmi_sites <- bmi_clean[,c(1,6:7)]
# dim(bmi_sites) # 310216      3
bmi_sites <- distinct(bmi_sites)
save(bmi_sites, file="output_data/02_bmi_sites.RData") 
dim(bmi_sites) # 2935    3


bmi_sites <- bmi_sites %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) # define coords to make spatial

#  algae and bug sites 
head(algae) # rep here
head(bmi_sites)

#  pair sites
#  distance between sites

# install.packages("spatstat")
library(spatstat)

######  pair algae - gauge sites
#  crs same in all data - bmi, algae, gages
#   match spatially 
#  match gage with algae temporally - post 2007

#   count pairs in HUC 12
# subset full algae dataset to match asci scores data?? i.e. file="output_data/clean_algae.RData"
## look at api gage data - calculate ffm?

### 250 ref gauges from Ryan - bmi_ffm_links repo
load(file="input_data/gages_final_250.rda")
# head(gages_final)
# dim(gages_final)
# str(gages_final)

# update one col
gages_final <- gages_final %>% 
  mutate(REF_END_YEAR=as.integer(REF_END_YEAR))

#  check same crs
st_crs(bmi_sites)
st_crs(algae)
st_crs(gages_final)


# HUC 12 data - to check how many in each huc 12

load(file="input_data/huc12_sf.rda")
str(h12)

# add watershed area in sqkm
h12 <- h12 %>% 
  mutate(h12_area_sqkm=Shape_Area/1e6)

## what is bmi_clean for?

# so 98 gages meet temporal scale of algae  - do they have 10 years of data?
gages_final2 <- gages_final %>% filter(REF_END_YEAR>2007)
dim(gages_final2)
head(gages_final2)
gages_final2 <- as.data.frame(gages_final2)
gages_final2 <- gages_final2 %>% 
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326, remove=F) # define coords to make spatial


# Intersect algae/Gages by H12 ----------------------------------------------

# how many 
# Add H12 to points to algae and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE)
algae_h12 <- st_join(algae, left = FALSE, h12[c("HUC_12","h12_area_sqkm")]) #
head(algae_h12) #rep here
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
# ?st_join
gages_h12 <- st_join(gages_final2, left=FALSE, h12[c("HUC_12")]) #%>% 
  # select(ID, HUC_12) %>% as_tibble() %>% select(-geometry)
# class(gages_h12)
# class(algae_h12)
# head(algae_h12)
# head(gages_h12)
# names(gages_h12)
# names(algae_h12)

#  change to dataframe
gages_h12 <- as.data.frame(gages_h12)


# now join based on H12: how many are in same?
sel_algae_gages <- inner_join(algae_h12, gages_h12, by="HUC_12") %>% distinct(SampleID_old, .keep_all = T) #StationID if not applying reps as individual samples
dim(sel_algae_gages) #164
head(sel_algae_gages)

# number of unique h12s?
length(unique(factor(sel_algae_gages$HUC_12))) # 40 unique h12
length(unique(sel_algae_gages$ID)) # 40 unique gages
length(unique(sel_algae_gages$SampleID_old)) # StationID- 126 unique algae sites - 164 when treating each rep as individual samples
# so 126 possible algae sites, 40 gages, in 40 HUC12's 
save(sel_algae_gages, file="output_data/02_paired_gages_algae_merged.RData") #algae data plus gage and huc12 

# how many gages? 40
sel_gages_algae <- gages_final2 %>% filter(ID %in% sel_algae_gages$ID)
head(sel_gages_algae)
dim(sel_gages_algae) 
save(sel_gages_algae, file="output_data/02_paired_only_gages_algae.RData") # same but only the paired gages n=40
# select H12s that have points inside:
sel_h12_algae <- h12[sel_algae_gages, ]
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
save(sel_h12_algae, file="output_data/02_selected_h12_contain_algae_gage.rda")

# Get algae COMIDs ----------------------------------------------------------
#  COMIDS for algae??
#bmi_comids <- readxl::read_excel("data/BMI_COMIDs_for_Ryan.xlsx")
#sel_bmi_gagestst <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode")

# Mapview -----------------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

m1 <- mapview(sel_algae_gages, col.regions="orange", layer.name="Algae", alpha=0.5, cex=9) +
  mapview(sel_gages_algae, col.regions="blue", layer.name="Gages", cex=4) + 
  mapview(sel_h12_algae, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1)

# add measure option  
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# Look for Nearest Gages --------------------------------------------------

# look for nearest gages (need to use UTM proj)
library(RANN)
# install.packages("RANN")

## TRANSFORM TO SAME DATUM
sel_algae_sf <- st_transform(sel_algae_gages, crs = 3310) # use CA Teal albs metric
sel_gages_sf <- st_transform(sel_gages_algae, crs=3310)

head(sel_algae_sf)
head(sel_gages_sf)
# get coordinate matrices, could be points and lines or lines and points
sel_algae_coords <- do.call(rbind, st_geometry(sel_algae_sf))
sel_algae_coords <- cbind(sel_algae_coords, 1:nrow(sel_algae_coords), sel_algae_sf$StationID) %>% as_tibble() %>% 
  mutate(V3=as.integer(V3))
head(sel_algae_coords)
# Warning message:
# `as_tibble.matrix()` requires a matrix with column names or a `.name_repair` argument. Using compatibility `.name_repair`.
sel_graph_coords <- do.call(rbind, st_geometry(sel_gages_sf))

# fast nearest neighbour search for single nearest
sel_closest <- nn2(sel_algae_coords[,1:2], sel_graph_coords, k = 1, searchtype = "standard")
head(sel_closest)
sel_closest <- sel_closest %>% bind_cols() %>% as_tibble() %>% 
  left_join(., as_tibble(sel_algae_coords[,c(3:4)]), by=c("nn.idx"="V3")) %>% 
  dplyr::rename(StationID=V4)
#  Warning message:
# Column `nn.idx`/`V3` has different attributes on LHS and RHS of join 
head(sel_algae_sf)
# join with spatial data
sel_closest_sf <- left_join(sel_closest, sel_algae_sf,  by=c("StationID")) %>% 
  st_as_sf(., sf_column_name="geometry.x", crs=3310)
# Error in st_sf(x, ..., agr = agr, sf_column_name = sf_column_name) : 
#   sf_column_name %in% all_sfc_names is not TRUE
# ?st_as_sf
# head(sel_closest_sf)


# fast search for radius 
sel_closest_5k <- nn2(sel_algae_coords[,1:2], sel_graph_coords, k=3, searchtype = "radius", radius = 5000) # in meters
sel_closest_5k <- sapply(sel_closest_5k, cbind) %>% as_tibble() %>% 
  left_join(., as_tibble(sel_algae_coords[,c(3:4)]), by=c("nn.idx"="V3")) %>% dplyr::rename(StationID=V4) %>% 
  # filter NAs
  filter(!is.na(StationID))

# join with spatial data
sel_closest_5k_sf <- left_join(sel_closest_5k, sel_algae_sf,  by=c("StationID")) %>% 
  st_as_sf(., sf_column_name="geometry.x", crs=3310)


# MAP IT (closest)
m2 <- mapview(sel_algae_gages, col.regions="orange", layer.name="Algae", alpha=0.5, cex=3) +
  mapview(sel_gages_algae, col.regions="blue", layer.name="Gages", cex=3, alpha=0.5) + 
  mapview(sel_h12_algae, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1) +
  mapview(sel_closest_sf, layer.name="Nearest Algae site to Gage", cex=9, col.regions="maroon") +
  mapview(sel_closest_5k_sf, layer.name="5k Radius Algae to Gage", cex=9, col.regions="salmon")

# add measure option  
m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

#### # Intersect algae/bmi by H12 ----------------------------------------------

head(algae)
head(bmi_sites)
dim(bmi_sites)
# how many 
# Add H12 to points to algae and bmi (adds ATTRIBUTES, retains ALL pts if left=TRUE)
algae_h12 <- st_join(algae, left = FALSE, h12[c("HUC_12","h12_area_sqkm")]) #
# although coordinates are longitude/latitude, st_intersects assumes that they are planar

bmi_h12 <- st_join(bmi_sites, left=FALSE, h12[c("HUC_12")]) #%>% 
# select(ID, HUC_12) %>% as_tibble() %>% select(-geometry)
# class(bmi_h12)
# class(algae_h12)
head(algae_h12)
head(bmi_h12)
# names(bmi_h12)
# names(algae_h12)

#  change to dataframe
# bmi_h12 <- as.data.frame(bmi_h12)
# select not working
# Error in (function (classes, fdef, mtable)  : 
# unable to find an inherited method for function ‘select’ for signature ‘"sf"’

# now join based on H12: how many are in same?
# sel_algae_bmi <- inner_join(algae_h12, bmi_h12, by="HUC_12") %>% distinct(StationID, .keep_all = T)
sel_algae_bmi <- st_join(algae_h12, bmi_h12, by="HUC_12", left=F) #%>% distinct(StationCode, .keep_all = T)

# ?st_join
dim(sel_algae_bmi) # 736


# number of unique h12s?
length(unique(factor(sel_algae_bmi$HUC_12.x))) # 390 unique h12
length(unique(sel_algae_bmi$StationCode)) # 527 unique bmi
length(unique(sel_algae_bmi$StationID)) #  527 unique algae sites
# so 527 possible algae sites, 527 bmi sites, in 390 HUC12's ??????? check this!! 

# how many bmi? 512
sel_bmi_algae <- bmi_sites %>% filter(StationCode %in% sel_algae_bmi$StationID)
dim(sel_bmi_algae)


# select H12s that have points inside:
sel_h12_algae <- h12[sel_algae_bmi, ]
dim(sel_h12_algae) # 390
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
save(sel_h12_algae, file="output_data/02_selected_h12_contain_algae_bmi.rda")

# count sites that don't match per algae data then per bmi data
sel_algae_bmi <- st_join(algae_h12, bmi_h12, by="HUC_12") #%>% distinct(StationCode, .keep_all = T)
sel_bmi_algae <- st_join(bmi_h12, algae_h12,by="HUC_12") #%>% distinct(StationID, .keep_all = T)

# ?st_join
dim(sel_algae_bmi) # 2214 - all algae sites, only matched bmi sites - StationCode for only matched sites
head(sel_algae_bmi)
dim(sel_bmi_algae) # 3144
head(sel_bmi_algae) # all bmi sites, only matched algae sites - StationID for only matched sites

# number of unique h12s?
length(unique(factor(sel_algae_bmi$HUC_12.x))) # 757 unique h12
length(unique(sel_algae_bmi$StationCode)) # 530 unique bmi
length(unique(sel_algae_bmi$StationID)) #  1690 unique algae sites


# number of unique h12s?
length(unique(factor(sel_bmi_algae$HUC_12.x))) # 1082 unique h12
length(unique(sel_bmi_algae$StationCode)) # 2935 unique bmi
length(unique(sel_bmi_algae$StationID)) #  530 unique algae sites

#  subset the sites that match - to check
#  by bmi
match_by_bmi <- unique(sel_algae_bmi$StationCode)
match_by_bmi # 530
bmi_u <- sel_algae_bmi$StationID %in% match_by_bmi

match_algae_bmi <- sel_algae_bmi[bmi_u,]
unique(match_algae_bmi$StationCode) #$StationID# 517 matches, 

#  by algae
match_by_algae <- unique(sel_bmi_algae$StationID)
match_by_algae # 530

alg_u <- sel_bmi_algae$StationCode %in% match_by_algae

match_bmi_algae <- sel_bmi_algae[alg_u,]
unique(match_bmi_algae$StationCode) ## 514 matched sites

#  some matched sites missing this 

#  create dataset of unmatched sites - to look at location

#  all sites df 
# all_bio_sites <- merge(sel_bmi_algae, sel_algae_bmi, by.x="StationCode", by.y="StationID", all=T)
all_bio_sites <- st_join(sel_algae_bmi, sel_bmi_algae, by="HUC_12")
head(all_bio_sites)
dim(all_bio_sites) # 3034
sel_h12_bio <- h12[all_bio_sites, ]
dim(sel_h12_bio) # 757

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

m1 <- mapview(sel_algae_bmi, col.regions="orange", layer.name="Algae", alpha=0.5, cex=9) +
  mapview(sel_bmi_algae, col.regions="blue", layer.name="BMI", cex=4) + 
  mapview(sel_h12_bio, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1)

# add measure option  
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Get algae COMIDs ----------------------------------------------------------
#  COMIDS for algae??
#bmi_comids <- readxl::read_excel("data/BMI_COMIDs_for_Ryan.xlsx")
#sel_bmi_gagestst <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode")

# Mapview -----------------------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList)

m1 <- mapview(sel_algae_bmi, col.regions="orange", layer.name="Algae", alpha=0.5, cex=9) +
  mapview(sel_bmi_algae, col.regions="blue", layer.name="BMI", cex=4) + 
  mapview(sel_h12_algae, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1)

# add measure option  
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
#  shows only bmi/algae sites that match - would be good to see sites that don't match and where



# GET COMIDS FOR Algae POINTS -----------------------------------

#install.packages("devtools")
library(devtools)
# devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)

head(sel_algae_gages)

algae_segs<- sel_algae_gages[,c(2:5)]
algae_segs$comid <- NA
head(algae_segs) #reps here
dim(algae_segs) #126 - 162 with reps

  
# get the comid for the BMI points w no comids using purrr

algae_missing_coms <- algae_segs %>% st_drop_geometry() %>% as.data.frame()

algae_missing_coms <-algae_missing_coms %>% rowid_to_column() %>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F) %>%
  st_transform(3310) %>% group_split(rowid) %>%
  map(~discover_nhdplus_id(.x$geometry))

head(algae_missing_coms)
algae_missing_coms
# one by one:
#discover_nhdplus_id(bmi_segs$geometry[1]) # 17683290

# flatten
algae_segs_df <-algae_missing_coms %>% flatten_dfc() %>% t() %>% as.data.frame() %>% 
  rename("comid"=V1) %>% 
  mutate(StationID = algae_segs$StationID)
#save(bmi_segs_df, file = "data_output/03_selected_bmi_missing_comids.rda")
algae_segs_df
algae_segs_df <- algae_segs_df[!duplicated(algae_segs_df),] #several duplicates due to replicates. remove here, shuld not affect the reps in main dataset


# save back out:
save(algae_segs_df, file="output_data/02_algae_all_stations_comids.rda")


# GET UPSTREAM FLOWLINES --------------------------------------------------

## TRANSFORM TO SAME DATUM
sel_algae_sf <- st_transform(sel_algae_gages, crs=3310) # use CA Teal albs metric
sel_gages_sf <- st_transform(sel_gages_algae, crs=3310)

save(sel_gages_sf, sel_algae_sf, file = "output_data/02_selected_gages_algae_sf_3310.rda")

usgs_segs <- sel_gages_algae %>% split(.$ID) %>%
  map(~discover_nhdplus_id(.x$geometry))

# search by a single comid
# nldi_feature <- list(featureSource = "comid",
#                      featureID = sel_gages_sf$NHDV1_COMID[[1]])
# discover_nldi_navigation(nldi_feature)
# 
# # get all upstream comid segments
# flowline_usgs <- navigate_nldi(nldi_feature = nldi_feature,
#                                 mode = "upstreamMain", 
#                                 data_source = "")

# use purrr
coms <- sel_gages_algae$NHDV2_COMID
coms_list <- map(coms, ~list(featureSource = "comid", featureID=.x))
coms_list[[30]] # tst check

# test against function:
#feat_check <- map(coms_list, ~discover_nldi_navigation(.x))

# test with mainstem segs
mainstems <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                           mode="upstreamMain",
                                           data_source = ""))

mainstemsDS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamMain",
                                             distance_km = 15,
                                             data_source = ""))

# IT WORKSSSSSS!!!!!
mapview(mainstems, col.regions="blue", col="blue", legend=F, lwd=2.5) +
  mapview(mainstemsDS, color="skyblue4", lwd=4, legend=F) + 
  mapview(sel_algae_sf, col.regions="orange", legend=F) + 
  mapview(sel_gages_sf, col.regions="dodgerblue", legend=F, cex=5) 
#  some sites off the main stem still

# make a single flat layer
mainstems_flat_ds <- mainstemsDS %>%
  set_names(., sel_gages_sf$ID) %>%
  map2(sel_gages_sf$ID, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_ds)

# make a single flat layer
mainstems_flat_us <- mainstems %>%
  set_names(., sel_gages_sf$ID) %>%
  map2(sel_gages_sf$ID, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_us)

rm(mainstems_flat_ds, mainstems_flat_us)

save(mainstems_us, mainstems_ds, file = "output_data/02_selected_nhd_flowlines_mainstems.rda")

mapview(mainstems_ds) + mapview(mainstems_us, color="purple")


# RELOAD AND MAP ----------------------------------------------------------

load("output_data/02_paired_gages_algae_merged.RData")
load("output_data/02_selected_nhd_flowlines_mainstems.rda")
load("output_data/02_selected_h12_contain_algae_gage.rda")

mapview(mainstems_ds, color="slateblue", legend=F) +
  mapview(mainstems_us, color="darkblue", legend=F) +
  mapview(sel_gages_algae, col.regions="purple", layer.name="Gages", cex=8) + 
  mapview(sel_algae_gages, col.regions="orange", layer.name="Algae", cex=6)

