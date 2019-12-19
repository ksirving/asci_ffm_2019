### get ref gauges

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

### 250 ref gauges from Ryan - bmi_ffm_links repo

load(file="input_data/gages_final_250.rda")
head(gages_final)

## subset to only coords and ID

gages_coords <- gages_final[, c(1,5:6)]
head(gages_coords)
gages_coords <- as.data.frame(gages_coords)
#  make spatial
gages_coords <-gages_coords%>% 
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326, remove=F)
str(gages_coords)

