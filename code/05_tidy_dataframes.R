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

# load("data_output/05a_selected_ref_bmi_w_csci_flow_por.rda") # bmi_csci_flow_por
# load("data_output/05a_selected_ref_bmi_w_csci_flow_yrs.rda") # bmi_csci_flow_yrs
# load("data_output/05a_selected_ref_bmi_final_dat.rda") # bmi_final_dat
# load("data_output/05a_all_ref_usgs_flow_metrics_por.rda") # flow_por_wide, flow_por
# 
# # flow_by_years_bmi, # flow_by_years_bmi_wide
# load("data_output/05a_selected_ref_flow_by_years_of_bmi.rda") 
# 
# load("data_output/01_bmi_cleaned_stations_w_site_status.rda") # bmi_clean_stations_ss (site status)
# load("data_output/02_selected_bmi_and_gages_same_h12.rda") # sel_bmi_gages, sel_gages_bmi
# load("data_output/02_selected_nhd_flowlines_mainstems.rda") # mainstems_us, mainstems_ds
# load("data_output/02_selected_h12_contain_bmi_gage.rda") # all h12s w bmi and gage: sel_h12_bmi
# load("data_output/02_final_bmi_stations_dat_reference.rda") # bmi_coms_dat (all data for selected), bmi_coms_final (just coms and id)
# #bmi_coms <- read_rds("data_output/02_bmi_all_stations_comids.rds") # just bmi_coms, comids for all BMI sites
# 
# load("output_data/algae_all_stations_comids.rda") # algae_segs_df - algae sites and comids
# load("output_data/selected_h12_contain_algae_gage.rda") # sel_h12s_algae - huc 12s
load("output_data/02_selected_nhd_flowlines_mainstems.rda") # mainstems_us, mainstems_ds mainstems us/ds
# load("output_data/paired_gages_algae_merged.RData") # sel_algae_gages - 126 algae sites, 40 gages
# load("output_data/paired_only_gages_algae.RData") # sel_gages_algae paired gages - no algae data
load("output_data/03_gages_comids_algae_mets.RData") # algae_coms asci metrics, gages and comids
# load("output_data/clean_algae.RData") # algae - all data
load("output_data/04_algae_gage_flow_metrics_POR.RData") #algae_asci_flow_por - algae metrics, gages, comids and FFM for period of record


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


load("output_data/04_algae_gage_flow_metrics_POR.RData") 
names(algae_asci_flow_por)
algae_asci_flow_por <- algae_asci_flow_por[, -c(96,97, 60,61,48:58, 18:41)]
names(algae_asci_flow_por)[21:22] <- c("maxYr", "minYr")

save(algae_asci_flow_por, file="output_data/05_algae_metrics_with_FFM_POR.RData")


load("output_data/02_paired_gages_algae_merged.RData") # sel_algae_gages - 126 algae sites, 40 gages
load("output_data/02_paired_only_gages_algae.RData") # sel_gages_algae paired gages - no algae data
#  gages paired with algae sites. needed for spatial join

head(sel_algae_gages)
names(sel_algae_gages)
sel_algae_gages <- sel_algae_gages[,c(1:16,41:46,58)]
save(sel_algae_gages, file="output_data/05_paired_gages_algaesites_merged.RData")

head(sel_gages_algae)
save(sel_gages_algae, file="output_data/05_selected_gages.RData")

