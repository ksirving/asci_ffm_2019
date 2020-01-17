## 06a Calc BMI Metrics and Merge with Flow datasets
## R. Peek/Katie Irving
# Calc bug metrics with CSCI and merge with flow metrics for respective datasets (annual, lag1, lag2, POR)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(CSCI)
library(BMIMetrics)
library(lubridate)
library(tidylog)

# Data --------------------------------------------------------------------

load("data_output/05a_selected_ref_bmi_w_csci_flow_por.rda") # bmi_csci_flow_por
load("data_output/05a_selected_ref_bmi_w_csci_flow_yrs.rda") # bmi_csci_flow_yrs
load("data_output/05a_selected_ref_bmi_final_dat.rda") # bmi_final_dat
load("data_output/05a_all_ref_usgs_flow_metrics_por.rda") # flow_por_wide, flow_por

# flow_by_years_bmi, # flow_by_years_bmi_wide
load("data_output/05a_selected_ref_flow_by_years_of_bmi.rda") 

load("data_output/01_bmi_cleaned_stations_w_site_status.rda") # bmi_clean_stations_ss (site status)
load("data_output/02_selected_bmi_and_gages_same_h12.rda") # sel_bmi_gages, sel_gages_bmi
load("data_output/02_selected_nhd_flowlines_mainstems.rda") # mainstems_us, mainstems_ds
load("data_output/02_selected_h12_contain_bmi_gage.rda") # all h12s w bmi and gage: sel_h12_bmi
load("data_output/02_final_bmi_stations_dat_reference.rda") # bmi_coms_dat (all data for selected), bmi_coms_final (just coms and id)
#bmi_coms <- read_rds("data_output/02_bmi_all_stations_comids.rds") # just bmi_coms, comids for all BMI sites

# re order cols & drop duplicates
bmi_coms_final <- bmi_coms_final %>% 
  select(StationCode, longitude, latitude, HUC_12, 
         h12_area_sqkm, ID:to_gage, geometry) %>% 
  distinct(StationCode, ID, .keep_all=TRUE) # n=157


# make a mainstems all file
mainstems_all <- rbind(mainstems_us, mainstems_ds)
rm(mainstems_ds, mainstems_us)

# load mapview bases
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Get BMI comids ----------------------------------------------------------

# join with status:
bmi_coms_final2 <- bmi_coms_final %>% 
  left_join(., bmi_clean_stations_ss[, c(1:2)], by="StationCode") #%>% 
#distinct(StationCode, ID, .keep_all = T) # 157 total

# make non-sf: 
bmi_csci_flow_yrs <- bmi_csci_flow_yrs %>% st_drop_geometry() %>% as.data.frame

# Calc Bug Metrics --------------------------------------------------------

# only need to do this once, don't rerun, takes a fair bit of time
# first filter to years of interest

bmi_filt <- bmi_final_dat %>% 
  filter(YYYY %in% c(1993:2019)) # should be the same but good to check

library(purrr)

# split by sampleID
bugs_split <- bmi_filt %>%
  split(.$SampleID) %>%
  map(~BMI(.x)) # make into BMI object

# now subsample
bugs_samp <- bugs_split %>%
  map(~sample(.x)) # subsample to 500 individuals

# aggregate (this takes a few secs)
bugs_agg <- bugs_samp %>%
  map(~aggregate(.x))

# Calculate metrics at SAFIT Level 1
bug_metrics <- bugs_agg %>%
  map(~BMIall(.x, effort=1))

# make clean station set (should be distinct already)
bmi_sampleids <- bmi_filt %>% distinct(SampleID, .keep_all = T)

# flatten and rejoin with station data:
bmi_metrics_df <- bug_metrics %>%
  do.call(what = rbind) %>%
  remove_rownames() %>%
  inner_join(., bmi_sampleids, by="SampleID")

# all IDs accounted for? should equal total dim rows
dim(bmi_metrics_df[!is.na(bmi_metrics_df$ID),])[1] == dim(bmi_metrics_df)[1]

# clean workspace, rm old bits
rm(bugs_agg, bugs_samp, bugs_split, bug_metrics, bmi_filt)

# SAVE IT
save(bmi_metrics_df, file="data_output/06a_selected_bmi_metrics_at_gage_sites.rda")

# ANNUAL: Join Bug Metrics with Flow Data -------------------------------------

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
