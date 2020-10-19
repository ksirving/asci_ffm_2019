# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)
library(dplyr)
library(tidyr)

#  statewide study - asci - ffm

#  upload algae data - asci scores downloaded from https://sites.google.com/view/asci/results - Susie
#  also upload component metrics (for MMI) to merge
setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

# load(file="output_data/01_clean_algae.RData")
# head(algae) ## old data

# comp_scor <- read.csv("input_data/algae_comp_mets_mmi_jan2020.csv")
# asci_scor <- read.csv("input_data/asci.scores_dec2019.csv", header=T)
# 
# asci_scor <- read.csv("input_data/ASCI.1.csv")
# head(asci_scor)
# algae<- as_tibble(algae)
# 
# asci_scor <- asci_scor %>% 
#   select(sampleid, stationcode, sampledate,replicate, assemblage, metric, result) %>%
#   filter(metric == "ASCI", !assemblage == "SBA") %>%
#   rename(StationCode = stationcode, SampleID = sampleid, SampleDate = sampledate, Result = result) %>%
#   mutate(Index = ifelse(assemblage == "Hybrid", "H_ASCI", "D_ASCI")) %>%
#   select(-assemblage, -metric)

load("input_data/00_SOC_all_asci_sites.RData") ## algae4
algae_new <- algae4
dim(algae_new) ## 2522
head(algae_new)


algae_raw <- read.csv("input_data/algae.bug.data.10172019.csv", header = T)
head(algae_raw)
names(algae_raw)
# dim(algae_raw)# 130753     26

#  get coords
algae_sites <- algae_raw[,c(1,3,17,18)]
head(algae_sites)
dim(algae_sites)
## remove duplicates
algae_sites <- distinct(algae_sites)
dim(algae_sites) ## 2655

#  merge coords with algae data
names(algae_new)
names(algae_sites)

algae_merged <- left_join(algae_new, algae_sites, by="StationCode")
head(algae_merged)

algae_v2 <- algae_merged %>%
  dplyr::select(-SampleID_old)
  
  
save(algae_v2, file="output_data/1a_new_algae_clean.RData")
  
