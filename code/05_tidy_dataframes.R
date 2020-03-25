## 06a Calc BMI Metrics and Merge with Flow datasets
## R. Peek/Katie Irving
# Calc bug metrics with ASCI and merge with flow metrics for respective datasets (annual, lag1, lag2, POR)

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(CSCI)
library(BMIMetrics)
library(lubridate)
library(tidylog)
# install.packages("tidylog")

# Data --------------------------------------------------------------------

# load("output_data/algae_all_stations_comids.rda") # algae_segs_df - algae sites and comids
# load("output_data/selected_h12_contain_algae_gage.rda") # sel_h12s_algae - huc 12s
load("output_data/02b_selected_nhd_flowlines_mainstems_all_gages.rda") # mainstems_us, mainstems_ds mainstems us/ds
# load("output_data/paired_gages_algae_merged.RData") # sel_algae_gages - 126 algae sites, 40 gages
# load("output_data/paired_only_gages_algae.RData") # sel_gages_algae paired gages - no algae data
load("output_data/03_gages_comids_algae_mets.RData") # algae_coms asci metrics, gages and comids
# load("output_data/clean_algae.RData") # algae - all data
load("output_data/04_algae_gage_flow_metrics_POR.RData") #algae_asci_flow_por - algae metrics, gages, comids and FFM for period of record
load("output_data/04_algae_gage_flow_metrics_Lag_and_Ann.RData") #algae_asci_flow_lagann

# load("/Users/katieirving/Documents/git/bmi_ffm_links/data_output/02_final_bmi_stations_dat_reference.rda")

#  organise data sets

load("output_data/03_gages_comids_algae_mets.RData") # algae_coms asci metrics, gages and comids
names(algae_coms)
head(algae_coms)
# need algae_coms with ds/us info

algae_coms_red <- algae_coms[,c(1:6, 15:17, 59, 62)]
save(algae_coms_red, file= "output_data/05_gages_comids_algae.RData") # algae sites, gages, hucs and comids

# re order cols & drop duplicates
algae_coms_red <- algae_coms_red %>% 
  select(StationID, Longitude, Latitude, HUC_12, 
         h12_area_sqkm, ID:to_gage, geometry) %>% 
  distinct(StationID, ID, .keep_all=TRUE) 

load("output_data/02_selected_nhd_flowlines_mainstems.rda")
# make a mainstems all file
mainstems_all <- rbind(mainstems_us, mainstems_ds)
rm(mainstems_ds, mainstems_us)

# load mapview bases
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

#############################################
## flow data
load("output_data/04_algae_gage_flow_metrics_POR.RData") 
names(algae_asci_flow_por)
algae_asci_flow_por <- algae_asci_flow_por[, -c(96,97, 60,61,48:58, 18:41)]
names(algae_asci_flow_por)[21:22] <- c("maxYr", "minYr")

save(algae_asci_flow_por, file="output_data/05_algae_metrics_with_FFM_POR.RData")


load("output_data/04_algae_gage_flow_metrics_Lag_and_Ann.RData")
# split lag and ann into separate datasets
head(algae_asci_flow_lagann)
str(algae_asci_flow_lagann)
algae_asci_flow_lagann$year.y <- as.numeric(as.character(algae_asci_flow_lagann$year.y))

## annual
algae_asci_flow_annual <- filter(algae_asci_flow_lagann, YY==year.y)
head(algae_asci_flow_annual) 
sum(is.na(algae_asci_flow_annual)) # 608, loads!!!!!

## lag 1

algae_asci_flow_lag1 <- filter(algae_asci_flow_lagann, YY==year.y+1)
head(algae_asci_flow_lag1) 
sum(is.na(algae_asci_flow_lag1)) # 416, loads!!!!!

## lag 2
algae_asci_flow_lag2 <- filter(algae_asci_flow_lagann, YY==year.y+2)
head(algae_asci_flow_lag2) 
sum(is.na(algae_asci_flow_lag2)) # 295, loads!!!!!

##save 

save(algae_asci_flow_annual, file="output_data/05_asci_flow_annual.RData")
save(algae_asci_flow_lag1, file="output_data/05_asci_flow_lag1.RData")
save(algae_asci_flow_lag2, file="output_data/05_asci_flow_lag2.RData")


load("output_data/02_paired_gages_algae_merged.RData") # sel_algae_gages - 126 algae sites, 40 gages
load("output_data/02_paired_only_gages_algae.RData") # sel_gages_algae paired gages - no algae data
#  gages paired with algae sites. needed for spatial join


head(sel_algae_gages)
names(sel_algae_gages)
sel_algae_gages <- sel_algae_gages[,c(1:16,41:46,58)]
save(sel_algae_gages, file="output_data/05_paired_gages_algaesites_merged.RData")

head(sel_gages_algae)
save(sel_gages_algae, file="output_data/05_selected_gages.RData")

