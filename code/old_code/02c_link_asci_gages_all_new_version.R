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
load(file="output_data/02_algae_spatial.RData") # algae - asci data spatial
load(file="input_data/01_usgs_all_gages.rda") # all gages & ffc data
load(file="input_data/huc12_sf.rda") # h12 - huc 12 data
# load(file="input_data/00_usgs_ca_all_daily_flow_gages.rda") # ca_usgs_gages  all gauges CA

str(usgs_final_all)

# # update start year and end year and make spatial
# gages <- usgs_final_all %>% 
#   mutate(end_yr = as.integer(year(date_end))) 
# 
# gages <- gages %>% 
#   mutate(start_yr = as.integer(year(date_begin)))

gages <- usgs_final_all %>% st_transform(4326)
rm(usgs_final_all)

# check projs are same
st_crs(algae)
st_crs(gages)
st_crs(h12)


# Filter to same temporal scale as BMI data, years must be post 1994 (n=483 remaining)
gages_all_filt <- gages %>% filter(end_yr > 2004)
table(gages_all_filt$CEFF_type) ## ALT = 334 REF = 122 
 
# Add H12 to points to algae and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE)
algae_h12 <- st_join(algae, left = TRUE, h12[c("HUC_12")]) #
head(algae_h12) 

# FILTER-Intersect algae/Gages by H12 -----------------------------------

# Add H12 to points to algae and Gages (adds ATTRIBUTES, retains ALL pts if left=TRUE), using algae
algae_h12 <- st_join(algae, left = TRUE, h12[c("HUC_12")])

# Add H12 to all gages
gages_h12 <- st_join(gages_all_filt, left=TRUE, h12[c("HUC_12")]) %>%
  st_drop_geometry()

# now join based on H12: what BMI stations share same H12 as USGS gage? (N=1000)
sel_algae_gages <- inner_join(algae_h12, gages_h12, by="HUC_12") %>% 
  distinct(StationID, ID, .keep_all = T) # n=1000

# number of unique?
length(unique(factor(sel_algae_gages$HUC_12))) # h12=153
length(unique(sel_algae_gages$ID)) # gages=189
length(unique(sel_algae_gages$StationID)) # BMI Stations=422


