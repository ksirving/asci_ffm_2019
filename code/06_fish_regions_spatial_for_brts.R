# BRTs (Boosted Regression Trees)
## R. Peek - Katie Irving
## BRT Models of the Algae Metrics vs. Flw Metrics
## Use 4 different flow datasets:  Annual, lag1, lag2, and POR - just POR for now
## This is initial cut

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)
library(sf)
library(mapview)
library(gbm)

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

# Data --------------------------------------------------------------------

# load("data_output/06_selected_bmi_flow_metrics_w_csci_ANN.rda")
# load("data_output/06_selected_bmi_flow_metrics_w_csci_POR.rda")
# load("data_output/06_selected_bmi_flow_metrics_w_csci_LAG1.rda")
# load("data_output/06_selected_bmi_flow_metrics_w_csci_LAG2.rda")
# load("data_output/05_selected_bmi_stations_w_comids.rda")
# load("data_output/05_mainstems_us_ds_selected_gages.rda")
# load("data_output/03_selected_bmi_and_gages.rda")
# load("data_output/07_selected_bmi_nearest_usgs_stations.rda")

# load("output_data/algae_all_stations_comids.rda")
# load("output_data/clean_algae.RData")
# load("output_data/paired_gages_algae_merged.RData")
# load("output_data/selected_nhd_flowlines_mainstems.rda")
# load("output_data/selected_h12_contain_algae_gage.rda")
# load("output_data/paired_only_gages_algae.RData")
# load("output_data/paired_gages_algae_comid.RData")
# load("output_data/algae_gage_flow_metrics_POR.RData")

load("output_data/04_algae_gage_flow_metrics_POR.RData")
load("output_data/selected_nhd_flowlines_mainstems.rda")
load("output_data/05_selected_gages.RData")
load("output_data/05_paired_gages_algaesites_merged.RData")


# Link Regions ------------------------------------------------------------

# read in fish regions:
load("input_data/07_umbrella_sp_regions.rda")

# spatial join gage sites with regions, adds ATTRIBUTES, retains ALL pts if left=TRUE,
sel_gages_algae <- st_join(st_transform(sel_gages_algae, 3310), left = TRUE, ca_sp_regions["huc_region"])

algae_nearest <- st_join(st_transform(sel_algae_gages, 3310), left = TRUE, ca_sp_regions["huc_region"])

mapview(sel_gages_algae, col.regions="deepskyblue4", cex=7, alpha=0.7) + 
  mapview(mainstems_all, color="darkblue", lwd=0.5) +
  mapview(ca_sp_regions, zcol="huc_region", alpha.regions=0.3) + 
  mapview(algae_nearest, col.regions="orange", cex=5, alpha=.7)

#  don't have mainstems - use mainstems_all 
# Set up Model Vars -------------------------------------------------------

algae.metrics<-c("MMI.hybrid", "OxyRed.DO_30.richness", "prop.spp.BCG4", "prop.spp.IndicatorClass_DOC_high_raw",
                 "MMI.sba", "MMI.d")

# source functions:
source("code/functions/My.gbm.step.R")
# source("code/functions/My.gbm.fixed.R")
# source("code/functions/My.gbm.simplify.R") 
source("code/functions/brt.functions.R") 
source("code/functions/lm_R2_equation_ggplot.R")

# structure - SampleID, DD, Peak_Dur_2, Peak_Fre_2, StationCode, lon, lat, ID, algae metric, FFMs (check with Ryan)
# Select Response Var -----------------------------------------------------

#region_sel <- bmi_nearest %>% filter(huc_region=="north_coast" | huc_region=="south_coast")

data_por <- dplyr::select(algae_asci_flow_por, 1:3, 5,6,13, 16, one_of(algae.metrics), 4,63:94) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

head(data_por)
names(data_por)

# MMI Hybrid Period of Record BRT --------------------------------------------------------

# this matches BMI from same year against flow from same year
# response is SHANNON DIVERSITY

set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_mmi_hyb <- data_por[,c(8,14:ncol(data_por))]
names(dat_mmi_hyb) 
dat_mmi_hyb <- dat_mmi_hyb[, -35] # remove geometry column
dat_mmi_hyb$Rep <- as.numeric(as.character(dat_mmi_hyb$Rep))
# model with MMI hybrid
str(dat_mmi_hyb)

gbm1 <- My.gbm.step(data=dat_mmi_hyb ,
                    gbm.x = 2:ncol(dat_mmi_hyb),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.005, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode
gbm1
# get the relative % 
gbm1_RI<-as.data.frame(summary(gbm1, plotit = F)) %>% 
  mutate("Ymetric"="MMI_hybrid",
         "flowdat" = "por")
rownames(gbm1_RI) <- NULL

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for ANNUAL MATCH')),
#   colnames = c("Variables"=2, "Relative Influence"=3)) %>% 
#   formatStyle('Relative Influence', 
#               color = styleInterval(c(2,5), c('#440154FF', '#21908CFF', '#FDE725FF')),
#               backgroundColor = styleInterval(c(2,5), c('gray', 'yellow', 'forestgreen'))) %>% formatRound('Relative Influence', 3)
# 
gbm1_topn <- sum((summary(gbm1, plotit=FALSE)$rel.inf)>=5)
(gbm1_topvar <- as.character(summary(gbm1, plotit=FALSE)$var[1:gbm1_topn]))
# make df:
gbm1_ri_top <- tibble(RI=summary(gbm1, plotit=FALSE)$rel.inf[1:gbm1_topn], varnames=gbm1_topvar) %>%
  mutate(varnames=fct_reorder(as.factor(varnames), RI))


ggplot(data=gbm1_ri_top, aes(x=varnames,y=RI)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  labs(title=paste0("Period of Record: Top ",gbm1_topn," vars for Hybrid MMI"))

ggsave(filename = "figs/por/por_brt_mmi_hybrid_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)



