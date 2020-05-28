######### spatial join with gauges

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
load(file="input_data/01_usgs_all_gages.rda") # all gages & ffc data
load(file="input_data/huc12_sf.rda") # h12 - huc 12 data

head(algae)
head(usgs_final_all) ## error
dim(usgs_final_all) ## 826
head(h12)
str(usgs_final_all)
Cstack_info()
unique(usgs_final_all$gage_id)
# Error: C stack usage  7969184 is too close to the limit
     
usgs_final_all$gage_id
usgs_final_all$ID
usgs_final_all$NHDV1_COMID
usgs_final_all$NHDV2_COMID

gages <- usgs_final_all %>% st_transform(4326)
rm(usgs_final_all)