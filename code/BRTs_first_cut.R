# BRTs (Boosted Regression Trees)
## R. Peek - Katie Irving
## BRT Models of the BMI Metrics vs. Flw Metrics
## Use 4 different flow datasets:  Annual, lag1, lag2, and POR
## This is initial cut

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)
library(sf)
library(mapview)
library(gbm)


# Data --------------------------------------------------------------------

# load("data_output/06_selected_bmi_flow_metrics_w_csci_ANN.rda")
# load("data_output/06_selected_bmi_flow_metrics_w_csci_POR.rda")
# load("data_output/06_selected_bmi_flow_metrics_w_csci_LAG1.rda")
# load("data_output/06_selected_bmi_flow_metrics_w_csci_LAG2.rda")
# load("data_output/05_selected_bmi_stations_w_comids.rda")
# load("data_output/05_mainstems_us_ds_selected_gages.rda")
# load("data_output/03_selected_bmi_and_gages.rda")
# load("data_output/07_selected_bmi_nearest_usgs_stations.rda")

# Link Regions ------------------------------------------------------------

# read in fish regions:
load("data/07_umbrella_sp_regions.rda")

# spatial join gage sites with regions, adds ATTRIBUTES, retains ALL pts if left=TRUE,
sel_gages_bmi <- st_join(st_transform(sel_gages_bmi, 3310), left = TRUE, ca_sp_regions["huc_region"])

bmi_nearest <- st_join(st_transform(bmi_nearest, 3310), left = TRUE, ca_sp_regions["huc_region"])

mapview(sel_gages_bmi, col.regions="deepskyblue4", cex=7, alpha=0.7) + 
  mapview(mainstems, color="darkblue", lwd=0.5) +
  mapview(ca_sp_regions, zcol="huc_region", alpha.regions=0.3) + 
  mapview(sel_bmi_gages, col.regions="orange", cex=5, alpha=.7)

# Set up Model Vars -------------------------------------------------------

bmi.metrics<-c("Shannon_Diversity", "Simpson_Diversity", "Taxonomic_Richness", "EPT_Percent", "Tolerant_Percent", "Intolerant_Percent", "csci", "csci_percentile", "mmi", "mmi_percentile")

# source functions:
source("code/functions/My.gbm.step.R")
# source("code/functions/My.gbm.fixed.R")
# source("code/functions/My.gbm.simplify.R") 
source("code/functions/brt.functions.R") 
source("code/functions/lm_R2_equation_ggplot.R")


# Select Response Var -----------------------------------------------------

#region_sel <- bmi_nearest %>% filter(huc_region=="north_coast" | huc_region=="south_coast")

## select data and arrange
data_ann <- dplyr::select(bmi_flow_metrics_ann_csci, 1, 106, 151:152, 91:93, 96, one_of(bmi.metrics), 117:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

data_lag1 <- dplyr::select(bmi_flow_metrics_lag1_csci, 1, 106, 151:152, 91:93, 96, one_of(bmi.metrics), 117:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

data_lag2 <- dplyr::select(bmi_flow_metrics_lag2_csci, 1, 106, 151:152, 91:93, 96, one_of(bmi.metrics), 117:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

data_por <- dplyr::select(bmi_flow_metrics_por_csci, 1, 106, 151:152, 91:93, 96, one_of(bmi.metrics), 117:148) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()
