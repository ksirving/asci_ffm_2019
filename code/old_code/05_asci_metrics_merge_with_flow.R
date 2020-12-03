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
install.packages("tidylog")

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
load("output_data/selected_nhd_flowlines_mainstems.rda") # mainstems_us, mainstems_ds mainstems us/ds
# load("output_data/paired_gages_algae_merged.RData") # sel_algae_gages - 126 algae sites, 40 gages
# load("output_data/paired_only_gages_algae.RData") # sel_gages_algae paired gages - no algae data
load("output_data/03_gages_comids_algae_mets.RData") # algae_coms asci metrics, gages and comids
# load("output_data/clean_algae.RData") # algae - all data
load("output_data/04_algae_gage_flow_metrics_POR.RData") #algae_asci_flow_por - algae metrics, gages, comids and FFM for period of record




# load("/Users/katieirving/Documents/git/bmi_ffm_links/data_output/02_final_bmi_stations_dat_reference.rda")

#  organise data sets

load("output_data/03_gages_comids_algae_mets.RData") # algae_coms asci metrics, gages and comids
# names(algae_coms)
# need algae_coms with ds/us info

algae_coms_red <- algae_coms[,c(1:6, 14:16, 58, 61)]
save(algae_coms_red, file= "output_data/05_gages_comids_algae.RData") # algae sites, gages, hucs and comids

# re order cols & drop duplicates
algae_coms_red <- algae_coms_red %>% 
  select(StationID, Longitude, Latitude, HUC_12, 
         h12_area_sqkm, ID:to_gage, geometry) %>% 
  distinct(StationID, ID, .keep_all=TRUE) # n=157

load("output_data/selected_nhd_flowlines_mainstems.rda")
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
head(algae_asci_flow_por)
# Get Algae comids ----------------------------------------------------------

# make non-sf: 
bmi_csci_flow_yrs <- bmi_csci_flow_yrs %>% st_drop_geometry() %>% as.data.frame


# ANNUAL: Join Algae Metrics with Flow Data -------------------------------------

# get flow POR and add to annual
flow_por_wide <- flow_por_wide %>% mutate(WY=as.factor("POR"))

# join but make year a "factor"
flow_by_years_bmi_wide <- flow_by_years_bmi_wide %>% 
  mutate(WY=as.factor(year)) %>% 
  bind_rows(., flow_por_wide) %>%
  mutate(WY=as.factor(WY))

# check distrib of years
table(flow_by_years_bmi_wide$WY)

# join with flow data by gage ID, then filter to years BMI data collected
bmi_flow_metrics_all <- left_join(bmi_metrics_df, flow_by_years_bmi_wide, by="ID") %>% 
  # fix years so Period of record shows up as 1900 and can be filtered out for annual vs. non
  mutate(WYs = as.integer(ifelse(WY=="POR", "1900", as.character(WY))))

table(bmi_flow_metrics_all$WY)
table(bmi_flow_metrics_all$WYs)

# make an annual dataset where flow data matches same year of bug data: 
bmi_flow_metrics_annual <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == WYs )#| YYYY == WYs-1 | YYYY==WYs-2)

table(bmi_flow_metrics_annual$WYs)


# LAGGED: Join Bugs with Flow Lagged Data -----------------------------------------------------

# make a lagged dataset, need to look for WYs + 1 equal to the actual bmi year:

## LAG 1
bmi_flow_metrics_lag1 <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == WYs+1)

## LAG 2
bmi_flow_metrics_lag2 <- bmi_flow_metrics_all %>% 
  dplyr::filter(YYYY == WYs+2)

# make POR dataset
bmi_flow_metrics_por <- bmi_flow_metrics_all %>% 
  dplyr::filter(WYs==1900)


# JOIN WITH CSCI & METRICS ----------------------------------------------------

# Now add CSCI: but need to regen sampleID for CSCI data (station_YMD_samplemethod_replicate)
bmi_csci_flow_por <- bmi_csci_flow_por %>% 
  mutate(SampleID=paste0(StationCode, "_", year(sampledate), sprintf("%02d", month(sampledate)), sprintf("%02d", day(sampledate)), "_", collectionmethodcode, "_", fieldreplicate)) %>% st_drop_geometry()

# annual and lagged data
bmi_csci_flow_yrs <- bmi_csci_flow_yrs %>% 
  mutate(SampleID=paste0(StationCode, "_", year(sampledate), sprintf("%02d", month(sampledate)), sprintf("%02d", day(sampledate)), "_", collectionmethodcode, "_", fieldreplicate)) 

# make a distinct SampleID list of csci
csci_only_por <- bmi_csci_flow_por %>% select(SampleID, csci, csci_percentile) %>% 
  distinct(SampleID, .keep_all = T)

# distinct for csci_only_yrs
csci_only_yrs <- bmi_csci_flow_yrs %>% select(SampleID, csci, csci_percentile) %>% 
  distinct(SampleID, .keep_all = T)

# now join
bmi_flow_metrics_por_csci <- left_join(bmi_flow_metrics_por, csci_only_por)
bmi_flow_metrics_ann_csci <- left_join(bmi_flow_metrics_annual, csci_only_yrs)
bmi_flow_metrics_lag1_csci <- left_join(bmi_flow_metrics_lag1, csci_only_yrs)
bmi_flow_metrics_lag2_csci <- left_join(bmi_flow_metrics_lag2, csci_only_yrs)

# ultimately use just: csci, Shannons, EPT, % pred, % coleoptra, % clingers, tax richness, % Intolerant
metrics_to_use <- c("csci", "Taxonomic_Richness", "Shannon_Diversity", "EPT_Percent", "Clinger_Percent", "Coleoptera_Percent", "Predator_Percent",  "Intolerant_Percent")

# Save out ----------------------------------------------------------------

save(bmi_flow_metrics_por_csci, file="data_output/06a_selected_bmi_flow_metrics_w_csci_POR.rda")
save(bmi_flow_metrics_ann_csci, file="data_output/06a_selected_bmi_flow_metrics_w_csci_ANN.rda")
save(bmi_flow_metrics_lag1_csci, file="data_output/06a_selected_bmi_flow_metrics_w_csci_LAG1.rda")
save(bmi_flow_metrics_lag2_csci, file="data_output/06a_selected_bmi_flow_metrics_w_csci_LAG2.rda")
