######### spatial join with bmi and gauges

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")


# data needed
load(file="output_data/02_algae_spatial.RData") # algae - asci data spatial
load(file="output_data/02_bmi_sites.RData") # bmi_sites bug data - spatial
load(file="input_data/00_usgs_ca_all_daily_flow_gages.rda") # ca_usgs_gages  all gauges CA
load(file="input_data/huc12_sf.rda") # h12 - huc 12 data

#  algae and bug sites 
head(algae) 
head(bmi_sites)
head(ca_usgs_gages)
head(h12)
length(unique(ca_usgs_gages$site_id)) ## 2401
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

# add watershed area in sqkm
h12 <- h12 %>% 
  mutate(h12_area_sqkm=Shape_Area/1e6) %>% 
  st_transform(4269)

# update start year and end year and make spatial
gages <- ca_usgs_gages %>% 
  mutate(end_yr = as.integer(year(date_end))) 

gages <- gages %>% 
  mutate(start_yr = as.integer(year(date_begin)))


# make spatial
algae <- algae %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>% 
  st_transform(4269)

bmi_sites <- bmi_sites %>% 
  st_transform(4269)

#  check same crs
st_crs(bmi_sites)
st_crs(algae)
st_crs(gages)
st_crs(h12)

# HUC 12 data - to check how many in each huc 12
str(h12)

# so 1062 gages meet temporal scale of algae  - do they have 10 years of data?
gages2 <- gages %>% filter(end_yr>2007)
dim(gages2)
head(gages2)

# Intersect algae/Gages by H12 ----------------------------------------------

# how many 
# Add H12 to points to algae and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE)
algae_h12 <- st_join(algae, left = FALSE, h12[c("HUC_12","h12_area_sqkm")]) #
head(algae_h12) 


# although coordinates are longitude/latitude, st_intersects assumes that they are planar
# ?st_join
gages_h12 <- st_join(gages2, left=FALSE, h12[c("HUC_12")]) #%>% 

# class(gages_h12)
# class(algae_h12)
# head(algae_h12)
head(gages_h12)
# names(gages_h12)
# names(algae_h12)

#  change to dataframe
gages_h12 <- as.data.frame(gages_h12)


# now join based on H12: how many are in same?
sel_algae_gages <- inner_join(algae_h12, gages_h12, by="HUC_12") %>% distinct(SampleID_old, .keep_all = T) #StationID if not applying reps as individual samples
dim(sel_algae_gages) # 778
head(sel_algae_gages)

# check end years -
sort(unique(sel_algae_gages$end_yr))

## 2008-2019 - whats the minimum overlap with algae? 

# number of unique h12s?
length(unique(factor(sel_algae_gages$HUC_12))) # 226 unique h12
length(unique(sel_algae_gages$site_id)) # 226 unique gages
length(unique(sel_algae_gages$SampleID_old)) #  - 778 algae sites when treating each rep as individual samples
length(unique(sel_algae_gages$StationID))  #StationID- 602 unique algae sites 
# so 778 possible algae sites, 226 gages, in 226 HUC12's 
save(sel_algae_gages, file="output_data/02b_paired_all_gages_algae_merged.RData") #algae data plus gage and huc12 

# how many gages? 226
sel_gages_algae <- gages2 %>% filter(site_id %in% sel_algae_gages$site_id)
head(sel_gages_algae)
dim(sel_gages_algae) 
save(sel_gages_algae, file="output_data/02b_paired_only_all_gages_algae.RData") # same but only the paired gages n=40
# select H12s that have points inside:
sel_h12_algae <- h12[sel_algae_gages, ]
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
save(sel_h12_algae, file="output_data/02b_selected_h12_contain_algae_all_gage.rda")

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
sel_closest_5k <- nn2(sel_algae_coords[,1:2], sel_graph_coords, k=3, searchtype = "radius", radius = 10000) # in meters - changed to 10km
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
  mapview(sel_closest_5k_sf, layer.name="10k Radius Algae to Gage", cex=9, col.regions="salmon")

# add measure option  
m2@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# #### # Intersect algae/bmi by H12 ----------------------------------------------
# 
# head(algae)
# head(bmi_sites)
# dim(bmi_sites)
# # how many 
# # Add H12 to points to algae and bmi (adds ATTRIBUTES, retains ALL pts if left=TRUE)
# algae_h12 <- st_join(algae, left = FALSE, h12[c("HUC_12","h12_area_sqkm")]) #
# # although coordinates are longitude/latitude, st_intersects assumes that they are planar
# 
# bmi_h12 <- st_join(bmi_sites, left=FALSE, h12[c("HUC_12")]) #%>% 
# # select(ID, HUC_12) %>% as_tibble() %>% select(-geometry)
# # class(bmi_h12)
# # class(algae_h12)
# head(algae_h12)
# head(bmi_h12)
# # names(bmi_h12)
# # names(algae_h12)
# 
# #  change to dataframe
# # bmi_h12 <- as.data.frame(bmi_h12)
# # select not working
# # Error in (function (classes, fdef, mtable)  : 
# # unable to find an inherited method for function ‘select’ for signature ‘"sf"’
# 
# # now join based on H12: how many are in same?
# # sel_algae_bmi <- inner_join(algae_h12, bmi_h12, by="HUC_12") %>% distinct(StationID, .keep_all = T)
# sel_algae_bmi <- st_join(algae_h12, bmi_h12, by="HUC_12", left=F) #%>% distinct(StationCode, .keep_all = T)
# 
# # ?st_join
# dim(sel_algae_bmi) # 736
# 
# 
# # number of unique h12s?
# length(unique(factor(sel_algae_bmi$HUC_12.x))) # 390 unique h12
# length(unique(sel_algae_bmi$StationCode)) # 527 unique bmi
# length(unique(sel_algae_bmi$StationID)) #  527 unique algae sites
# # so 527 possible algae sites, 527 bmi sites, in 390 HUC12's ??????? check this!! 
# 
# # how many bmi? 512
# sel_bmi_algae <- bmi_sites %>% filter(StationCode %in% sel_algae_bmi$StationID)
# dim(sel_bmi_algae)
# 
# 
# # select H12s that have points inside:
# sel_h12_algae <- h12[sel_algae_bmi, ]
# dim(sel_h12_algae) # 390
# # although coordinates are longitude/latitude, st_intersects assumes that they are planar
# save(sel_h12_algae, file="output_data/02b_selected_h12_contain_algae_bmi.rda")
# 
# # count sites that don't match per algae data then per bmi data
# sel_algae_bmi <- st_join(algae_h12, bmi_h12, by="HUC_12") #%>% distinct(StationCode, .keep_all = T)
# sel_bmi_algae <- st_join(bmi_h12, algae_h12,by="HUC_12") #%>% distinct(StationID, .keep_all = T)
# 
# # ?st_join
# dim(sel_algae_bmi) # 2214 - all algae sites, only matched bmi sites - StationCode for only matched sites
# head(sel_algae_bmi)
# dim(sel_bmi_algae) # 3144
# head(sel_bmi_algae) # all bmi sites, only matched algae sites - StationID for only matched sites
# 
# # number of unique h12s?
# length(unique(factor(sel_algae_bmi$HUC_12.x))) # 757 unique h12
# length(unique(sel_algae_bmi$StationCode)) # 530 unique bmi
# length(unique(sel_algae_bmi$StationID)) #  1690 unique algae sites
# 
# 
# # number of unique h12s?
# length(unique(factor(sel_bmi_algae$HUC_12.x))) # 1082 unique h12
# length(unique(sel_bmi_algae$StationCode)) # 2935 unique bmi
# length(unique(sel_bmi_algae$StationID)) #  530 unique algae sites
# 
# #  subset the sites that match - to check
# #  by bmi
# match_by_bmi <- unique(sel_algae_bmi$StationCode)
# match_by_bmi # 530
# bmi_u <- sel_algae_bmi$StationID %in% match_by_bmi
# 
# match_algae_bmi <- sel_algae_bmi[bmi_u,]
# unique(match_algae_bmi$StationCode) #$StationID# 517 matches, 
# 
# #  by algae
# match_by_algae <- unique(sel_bmi_algae$StationID)
# match_by_algae # 530
# 
# alg_u <- sel_bmi_algae$StationCode %in% match_by_algae
# 
# match_bmi_algae <- sel_bmi_algae[alg_u,]
# unique(match_bmi_algae$StationCode) ## 514 matched sites
# 
# #  some matched sites missing this 
# 
# #  create dataset of unmatched sites - to look at location
# 
# #  all sites df 
# # all_bio_sites <- merge(sel_bmi_algae, sel_algae_bmi, by.x="StationCode", by.y="StationID", all=T)
# all_bio_sites <- st_join(sel_algae_bmi, sel_bmi_algae, by="HUC_12")
# head(all_bio_sites)
# dim(all_bio_sites) # 3034
# sel_h12_bio <- h12[all_bio_sites, ]
# dim(sel_h12_bio) # 757
# 
# # set background basemaps:
# basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
#                   "OpenTopoMap", "OpenStreetMap", 
#                   "CartoDB.Positron", "Stamen.TopOSMFeatures")
# 
# mapviewOptions(basemaps=basemapsList)
# 
# m1 <- mapview(sel_algae_bmi, col.regions="orange", layer.name="Algae", alpha=0.5, cex=9) +
#   mapview(sel_bmi_algae, col.regions="blue", layer.name="BMI", cex=4) + 
#   mapview(sel_h12_bio, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1)
# 
# # add measure option  
# m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
# 

# Get algae COMIDs ----------------------------------------------------------
#  COMIDS for algae??
#bmi_comids <- readxl::read_excel("data/BMI_COMIDs_for_Ryan.xlsx")
#sel_bmi_gagestst <- sel_bmi_gages %>% left_join(., bmi_comids, by="StationCode")

# Mapview -----------------------------------------------------------------

# set background basemaps:
# basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
#                   "OpenTopoMap", "OpenStreetMap", 
#                   "CartoDB.Positron", "Stamen.TopOSMFeatures")
# 
# mapviewOptions(basemaps=basemapsList)
# 
# m1 <- mapview(sel_algae_bmi, col.regions="orange", layer.name="Algae", alpha=0.5, cex=9) +
#   mapview(sel_bmi_algae, col.regions="blue", layer.name="BMI", cex=4) + 
#   mapview(sel_h12_algae, layer="H12", color="dodgerblue", col.regions="cyan", alpha=0.8, lwd=1)
# 
# # add measure option  
# m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
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
dim(algae_segs) #778 with reps


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
save(algae_segs_df, file="output_data/02b_algae_all_stations_comids.rda")
# head(sel_gages_algae)
# head(sel_algae_gages)
# dim(sel_gages_algae) #226
# dim(sel_algae_gages) # 778 site_id = gages, StationID = algae site
# head(algae_segs_df)
# dim(algae_segs_df) # 602 = sample sites without the reps
# str(sel_gages_algae)

# merge with comids
sel_algae_gages_test <- merge(sel_algae_gages, algae_segs_df, by="StationID", all=T)
head(sel_algae_gages_test)
dim(sel_algae_gages_test)
# check NAs
sum(is.na(sel_algae_gages_test))
na_ind <- which(is.na(sel_algae_gages_test))
sel_algae_gages_test[na_ind,] # all row is na so ok to remove

sel_algae_gages_test <- na.omit(sel_algae_gages_test)

names(sel_algae_gages_test)

# extract gage sites, algae sites and comids 
gage_algae_comid <- sel_algae_gages_test[, c(1,16,31)]
gage_algae_comid <- as.data.frame(gage_algae_comid)
# dim(gage_algae_comid) # 639
# str(gage_algae_comid)
# str(sel_gages_algae)

sel_gages_algae_test <- merge(gage_algae_comid, sel_gages_algae, by="site_id", all=T)
dim(sel_gages_algae_test) # 685

sum(is.na(sel_gages_algae_test)) #138
na_indx <- which(is.na(sel_gages_algae_test))
sel_gages_algae_test[na_indx,] ## ok to remove
sel_gages_algae_test <- na.omit(sel_gages_algae_test)

## make spatial agin

sel_gages_algae_test <- sel_gages_algae_test %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4269, remove=F) %>% 
  st_transform(3310)


# GET UPSTREAM FLOWLINES --------------------------------------------------

## TRANSFORM TO SAME DATUM
sel_algae_sf <- st_transform(sel_algae_gages_test, crs=3310) # use CA Teal albs metric
sel_gages_sf <- st_transform(sel_gages_algae_test, crs=3310)
head(sel_gages_sf)
head(sel_algae_sf)
save(sel_gages_sf, sel_algae_sf, file = "output_data/02b_selected_gages_algae_sf_3310.rda")
save(gage_algae_comid, file="output_data/02b_siteid_stationid_comid.RData")


usgs_segs <- sel_gages_algae %>% split(.$site_id) %>%
  map(~discover_nhdplus_id(.x$geometry))
length(usgs_segs)
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
coms <- sel_gages_sf$comid
coms
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
  set_names(., sel_gages_sf$site_id) %>%
  map2(sel_gages_sf$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_ds <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_ds)

# make a single flat layer
mainstems_flat_us <- mainstems %>%
  set_names(., sel_gages_sf$site_id) %>%
  map2(sel_gages_sf$site_id, ~mutate(.x, gageID=.y))

# bind together
mainstems_us <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_us)

rm(mainstems_flat_ds, mainstems_flat_us)

save(mainstems_us, mainstems_ds, file = "output_data/02b_selected_nhd_flowlines_mainstems_all_gages.rda")

mapview(mainstems_ds) + mapview(mainstems_us, color="purple")


# RELOAD AND MAP ----------------------------------------------------------

load("output_data/02b_paired_all_gages_algae_merged.RData")
load("output_data/02b_selected_nhd_flowlines_mainstems_all_gages.rda")
load("output_data/02b_selected_h12_contain_algae_all_gage.rda")
load("output_data/02b_paired_only_all_gages_algae.RData")

mapview(mainstems_ds, color="slateblue", legend=F) +
  mapview(mainstems_us, color="darkblue", legend=F) +
  mapview(sel_gages_algae, col.regions="purple", layer.name="Gages", cex=8) + 
  mapview(sel_algae_gages, col.regions="orange", layer.name="Algae", cex=6)

## all present, includes gauges/algae sites not on the main stems
## does it have sites less than 10km? remove sites not on mainstem
