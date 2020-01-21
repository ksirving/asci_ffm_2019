
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

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")#

# Data --------------------------------------------------------------------

# load("output_data/05_algae_metrics_with_FFM_POR.RData") # algae_asci_flow_por
# load("output_data/05_asci_flow_annual.RData") # algae_asci_flow_annual
# load("output_data/05_asci_flow_lag1.RData") # algae_asci_flow_lag1
load("output_data/05_asci_flow_lag2.RData") # algae_asci_flow_lag2

# load("output_data/04_algae_gage_flow_metrics_POR.RData")
load("output_data/02_selected_nhd_flowlines_mainstems.rda")
load("output_data/05_selected_gages.RData")
load("output_data/05_paired_gages_algaesites_merged.RData")


# Link Regions ------------------------------------------------------------

# read in fish regions:
load("input_data/07_umbrella_sp_regions.rda")

# make mainstems_all combine ds and us
mainstems_all <- rbind(mainstems_us, mainstems_ds)
rm(mainstems_ds, mainstems_us)

# spatial join gage sites with regions, adds ATTRIBUTES, retains ALL pts if left=TRUE,
sel_gages_algae <- st_join(st_transform(sel_gages_algae, 3310), left = TRUE, ca_sp_regions["huc_region"])

algae_nearest <- st_join(st_transform(sel_algae_gages, 3310), left = TRUE, ca_sp_regions["huc_region"])

mapview(sel_gages_algae, col.regions="deepskyblue4", cex=7, alpha=0.7) + 
  mapview(mainstems_all, color="darkblue", lwd=0.5) +
  mapview(ca_sp_regions, zcol="huc_region", alpha.regions=0.3) + 
  mapview(algae_nearest, col.regions="orange", cex=5, alpha=.7)

#  don't have mainstems - use mainstems_all 
# Set up Model Vars -------------------------------------------------------
names(sel_algae_gages)
algae.metrics<-c("MMI.hybrid", "OxyRed.DO_30.richness", "prop.spp.BCG4", "Salinity.BF.richness", "prop.spp.IndicatorClass_DOC_high_raw",
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
names(algae_asci_flow_lag2)
data_lag2 <- dplyr::select(algae_asci_flow_lag2, 1:3, 5,6,14, 20, one_of(algae.metrics), 4,33:66) %>% 
  #filter(StationCode %in% region_sel$StationCode) %>% 
  as.data.frame()

head(data_lag2)
names(data_lag2)

#MMI Hybrid Lag 2 Year BRT --------------------------------------------------------

# this matches algae from same year against flow from same year
# response is MMI Hybrid
set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_mmi_hyb <- data_lag2[,c(8,15:(ncol(data_lag2)-1))]
names(dat_mmi_hyb) 
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
         "flowdat" = "lag2")
rownames(gbm1_RI) <- NULL

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for Lag 2 Year MATCH')),
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
  labs(title=paste0("Lag 2 Year: Top ",gbm1_topn," vars for Hybrid MMI"))

ggsave(filename = "figs/lag2/lag2_brt_mmi_hybrid_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

###################################################
# OxyRed.DO_30.richness Lag 2 Year BRT --------------------------------------------------------
names(data_lag2)
# this matches algae from same year against flow from same year
# response is OxyRed.DO_30.richness
set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_OxyRed <- data_lag2[,c(9,15:(ncol(data_lag2)-1))]
names(dat_OxyRed) 
dat_OxyRed$Rep <- as.numeric(as.character(dat_OxyRed$Rep))
# model with Oxy Red DO
str(dat_OxyRed)

gbm1 <- My.gbm.step(data=dat_OxyRed,
                    gbm.x = 2:ncol(dat_OxyRed),          
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
  mutate("Ymetric"="OxyRed",
         "flowdat" = "lag2")
rownames(gbm1_RI) <- NULL

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for Lag 2 Year MATCH')),
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
  labs(title=paste0("Lag 2 Year: Top ",gbm1_topn," vars for OxyRed 30"))

ggsave(filename = "figs/lag2/lag2_brt_OxyRed_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

###################################################
# prop.spp.BCG4 Lag 2 Year BRT --------------------------------------------------------
names(data_lag2)
# this matches algae from same year against flow from same year
# response is prop.spp.BCG4
set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_BCG4 <- data_lag2[,c(10,15:(ncol(data_lag2)-1))]
names(dat_BCG4) 
dat_BCG4$Rep <- as.numeric(as.character(dat_BCG4$Rep))
# model with BCG4
str(dat_OxyRed)

gbm1 <- My.gbm.step(data=dat_BCG4,
                    gbm.x = 2:ncol(dat_BCG4),          
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
  mutate("Ymetric"="BCG4",
         "flowdat" = "lag2")
rownames(gbm1_RI) <- NULL

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for Lag 2 Year MATCH')),
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
  labs(title=paste0("Lag 2 Year: Top ",gbm1_topn," vars for BCG4"))

ggsave(filename = "figs/lag2/lag2_brt_BCG4_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

###################################################
# Salinity.BF.richness Lag 2 Year BRT --------------------------------------------------------
names(data_lag2)
# this matches algae from same year against flow from same year
# response is Salinity.BF.richness
set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_Salinity <- data_lag2[,c(11,15:(ncol(data_lag2)-1))]
names(dat_Salinity) 
dat_Salinity$Rep <- as.numeric(as.character(dat_Salinity$Rep))
# model with Salinity
str(dat_Salinity)

gbm1 <- My.gbm.step(data=dat_Salinity,
                    gbm.x = 2:ncol(dat_Salinity),          
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
  mutate("Ymetric"="Salinity",
         "flowdat" = "lag2")
rownames(gbm1_RI) <- NULL

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for Lag 2 Year MATCH')),
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
  labs(title=paste0("Lag 2 Year: Top ",gbm1_topn," vars for Salinity"))

ggsave(filename = "figs/lag2/lag2_brt_Salinity_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)




###################################################
# prop.spp.IndicatorClass_DOC_high_raw Lag 2 Year BRT --------------------------------------------------------
names(data_lag2)
# this matches algae from same year against flow from same year
# response is prop.spp.IndicatorClass_DOC_high_raw
set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_doc <- data_lag2[,c(12,15:(ncol(data_lag2)-1))]
names(dat_doc) 
dat_doc$Rep <- as.numeric(as.character(dat_doc$Rep))
# model with DOC - doesn't work with same criteria
str(dat_doc)

gbm1 <- My.gbm.step(data=dat_doc,
                    gbm.x = 2:ncol(dat_doc),          
                    gbm.y = 1, 
                    family = "gaussian",   # the 'loss function', Gaussian minimizes mean square error
                    tree.complexity = 3,   # thus only models 3nd-order interactions
                    learning.rate = 0.001, # the lower the learning rate/shrinking, more likely to over-fit
                    bag.fraction = 0.75,   # recommended in Elith and in Brown as top end of reasonable window
                    n.folds = 5,           # 10 default for function; 5 was used in De'ath 2007, good for low n
                    n.minobsinnode = 3)    # I edited gbm.step code to allow modification of n.minobsinnode
gbm1
# get the relative % 
gbm1_RI<-as.data.frame(summary(gbm1, plotit = F)) %>% 
  mutate("Ymetric"="DOC",
         "flowdat" = "lag2")
rownames(gbm1_RI) <- NULL

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for Lag 2 Year MATCH')),
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
  labs(title=paste0("Lag 2 Year: Top ",gbm1_topn," vars for DOC"))

ggsave(filename = "figs/lag2/lag2_brt_DOC_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

###################################################
# MMI.sba Lag 2 Year BRT --------------------------------------------------------
names(data_lag2)
# this matches algae from same year against flow from same year
# response is MMI.sba
set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_MMI.sba <- data_lag2[,c(13,15:(ncol(data_lag2)-1))]
names(dat_MMI.sba) 
dat_MMI.sba$Rep <- as.numeric(as.character(dat_MMI.sba$Rep))
# model with MMI.sba
str(dat_MMI.sba)

gbm1 <- My.gbm.step(data=dat_MMI.sba,
                    gbm.x = 2:ncol(dat_MMI.sba),          
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
  mutate("Ymetric"="MMI.sba",
         "flowdat" = "lag2")
rownames(gbm1_RI) <- NULL

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for Lag 2 Year MATCH')),
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
  labs(title=paste0("Lag 2 Year: Top ",gbm1_topn," vars for MMI Soft Bodied Algae"))

ggsave(filename = "figs/lag2/lag2_brt_MMI_sba_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)

###################################################
# MMI.d Lag 2 Year BRT --------------------------------------------------------
names(data_lag2)
# this matches algae from same year against flow from same year
# response is MMI.d
set.seed(33)  # set seed to get repeatable model              

# select cols of interest
dat_MMI.d <- data_lag2[,c(14,15:(ncol(data_lag2)-1))]
names(dat_MMI.d) 
dat_MMI.d$Rep <- as.numeric(as.character(dat_MMI.d$Rep))
# model with MMI.d
str(dat_MMI.d)

gbm1 <- My.gbm.step(data=dat_MMI.d,
                    gbm.x = 2:ncol(dat_MMI.d),          
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
  mutate("Ymetric"="MMI.d",
         "flowdat" = "lag2")
rownames(gbm1_RI) <- NULL

# DT::datatable(gbm1_RI, caption=htmltools::tags$caption(
#   style = 'caption-side: bottom; text-align: center;',
#   htmltools::em('Table 1. '), 
#   htmltools::em('Relative Influence for Lag 2 Year MATCH')),
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
  labs(title=paste0("Lag 2 Year: Top ",gbm1_topn," vars for MMI Diatoms"))

ggsave(filename = "figs/lag2/lag2_brt_MMI_d_top_RI_barplot.png",width = 7, height = 7, units = "in", dpi = 300)
