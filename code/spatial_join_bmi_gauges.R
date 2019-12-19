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

load(file="output_data/clean_algae.RData") # algae
head(algae)


## convert to spatial for pairing - 1) with bmi (overlap), 2) ref gauges

#  coords into numeric
algae$Latitude <- as.numeric(as.character(algae$Latitude))
algae$Longitude <- as.numeric(as.character(algae$Longitude))

#  remove NAs
sum(is.na(algae)) #50 - already NAs but not registering until formatted to a number
#  where are the NAs? 
na_ind <- which(is.na(algae)) 
algae <- na.omit(algae)

#  spatial point df
# ?st_as_sf
str(algae)

algae <- algae %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F) # define coords to make spatial

save(algae, file="output_data/algae_spatial.RData")


#  component metrics for Algae - only OoverE & MMI for diatoms and soft bodied

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
save(bmi_sites, file="output_data/bmi_sites.RData") 
dim(bmi_sites) # 2935    3


bmi_sites <- bmi_sites %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) # define coords to make spatial

#  algae and bug sites 
head(algae)
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
# select not working
# Error in (function (classes, fdef, mtable)  : 
# unable to find an inherited method for function ‘select’ for signature ‘"sf"’

# now join based on H12: how many are in same?
sel_algae_gages <- inner_join(algae_h12, gages_h12, by="HUC_12") %>% distinct(StationID, .keep_all = T)
dim(sel_algae_gages) #124

# number of unique h12s?
length(unique(factor(sel_algae_gages$HUC_12))) # 39 unique h12
length(unique(sel_algae_gages$ID)) # 39 unique gages
length(unique(sel_algae_gages$StationID)) #  124 unique algae sites
# so 124 possible algae sites, 39 gages, in 39 HUC12's ??????? check this!! 

# how many gages? 39
sel_gages_algae <- gages_final2 %>% filter(ID %in% sel_algae_gages$ID)

# select H12s that have points inside:
sel_h12_algae <- h12[sel_algae_gages, ]
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
save(sel_h12_algae, file="output_data/selected_h12_contain_algae_gage.rda")

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
# how many 
# Add H12 to points to algae and bmi (adds ATTRIBUTES, retains ALL pts if left=TRUE)
algae_h12 <- st_join(algae, left = FALSE, h12[c("HUC_12","h12_area_sqkm")]) #
# although coordinates are longitude/latitude, st_intersects assumes that they are planar

bmi_h12 <- st_join(bmi_sites, left=FALSE, h12[c("HUC_12")]) #%>% 
# select(ID, HUC_12) %>% as_tibble() %>% select(-geometry)
# class(bmi_h12)
# class(algae_h12)
# head(algae_h12)
# head(bmi_h12)
# names(bmi_h12)
# names(algae_h12)

#  change to dataframe
# bmi_h12 <- as.data.frame(bmi_h12)
# select not working
# Error in (function (classes, fdef, mtable)  : 
# unable to find an inherited method for function ‘select’ for signature ‘"sf"’

# now join based on H12: how many are in same?
# sel_algae_bmi <- inner_join(algae_h12, bmi_h12, by="HUC_12") %>% distinct(StationID, .keep_all = T)
sel_algae_bmi <- st_join(algae_h12, bmi_h12, by="HUC_12", left=F) #%>% distinct(StationID, .keep_all = T)

dim(sel_algae_bmi) #732
head(sel_algae_bmi)

# number of unique h12s?
length(unique(factor(sel_algae_bmi$HUC_12.x))) # 388 unique h12
length(unique(sel_algae_bmi$StationCode)) # 523 unique bmi
length(unique(sel_algae_bmi$StationID)) #  523 unique algae sites
# so 523 possible algae sites, 523 bmi sites, in 388 HUC12's ??????? check this!! 

# how many bmi? 508
sel_bmi_algae <- bmi_sites %>% filter(StationCode %in% sel_algae_bmi$StationID)
dim(sel_bmi_algae)

# select H12s that have points inside:
sel_h12_algae <- h12[sel_algae_bmi, ]
dim(sel_h12_algae) # 388
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
save(sel_h12_algae, file="output_data/selected_h12_contain_algae_bmi.rda")

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



###### continue here!!!!!!!!

# Look for Upstream Sections from Point -----------------------------------

#st_layers("data_output/eflows_bmi.gpkg")
#gages <- st_read(dsn = "data_output/eflows_bmi.gpkg",layer = "gages_ref_20190315",  as_tibble=TRUE, geometry_column="geometry")

#install.packages("devtools")
#devtools::install_github("USGS-R/nhdplusTools")
library(nhdplusTools)

# pick a specific point
#start_point <- st_sfc(st_point(c(-121.057, 38.852)), crs = 4269)
#start_comid <- discover_nhdplus_id(st_sfc(sel_gages_sf$geometry)[1])

# get the comid for the BMI points w no comids using purrr
bmi_segs <- sel_bmi_gages %>% filter(is.na(comid)) %>% select(StationCode, lat, lon, ID, comid)

bmi_missing_coms <- bmi_segs %>% split(.$StationCode) %>% 
  map(~discover_nhdplus_id(.x$geometry))

# flatten
bmi_segs_df <- bmi_missing_coms %>% flatten_df() %>% t() %>% as.data.frame() %>% rownames_to_column("StationCode") %>% rename("comid"=V1)
#save(bmi_segs_df, file = "data_output/sel_bmi_missing_comids.rda")

bmi_comids_rev <- bind_rows(bmi_segs_df, bmi_comids)
# rejoin to get full comids
sel_bmi_gages <- sel_bmi_gages %>% left_join(., bmi_comids_rev, by="StationCode")
summary(sel_bmi_gages)

save(sel_bmi_gages, sel_gages_bmi, file="data_output/sel_bmi_and_gages.rda")

