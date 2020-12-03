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

## 1st algae data set
algae <- read.csv("input_data/ASCI.1.csv")
head(algae)
length(unique(algae$stationcode)) ## 1946

algae <- algae %>%
  dplyr::select(sampleid, stationcode, sampledate,replicate, assemblage, metric, result) %>%
  filter(metric == "ASCI", !assemblage == "SBA") %>%
  rename(StationCode = stationcode, SampleID = sampleid, SampleDate = sampledate,
         Result = result, Replicate = replicate) %>%
  mutate(Index = ifelse(assemblage == "Hybrid", "H_ASCI", "D_ASCI")) %>%
  dplyr::select(-assemblage, -metric)

algae$SampleDate <- as.Date(algae$SampleDate)

## take most recent sample from algae dfs
algae <- algae[ !duplicated(algae[, c("StationCode", "Index")], fromLast=T),]
head(algae)


## upload 2nd algae data set
algae2 <- read.csv("input_data/asci.scores.forRafi2.csv")
head(algae2)

algae2 <- algae2 %>%
  dplyr::select(SampleID, StationCode, SampleDate,Replicate, H_ASCI, D_ASCI) %>%
  gather(key = "Index", value = "Result", H_ASCI, D_ASCI)

algae2$SampleDate <- as.Date(algae2$SampleDate, format = "%m/%d/%y" )

## take most recent sample
algae2 <- algae2[ !duplicated(algae2[, c("StationCode", "Index")], fromLast=T),]

## sites lat and lon master

sites1 <- read.csv("/Users/katieirving/Documents/data/forKatie/GIS.1.csv")
head(sites1)

sites1 <- sites1 %>%
  dplyr::select(stationcode, new_lat, new_long) %>%
  rename(StationCode = stationcode, New_Lat = new_lat, New_Long = new_long) %>%
  distinct()
dim(sites1)

sum(sites1$StationCode %in% algae$StationCode) ##1509

sites2 <- read.csv("/Users/katieirving/Documents/data/forKatie/lustations.1.csv")
dim(sites2)

sites2 <- sites2 %>%
  dplyr::select(stationid, latitude, longitude) %>%
  rename(StationCode = stationid, New_Lat = latitude, New_Long = longitude) %>%
  distinct()

sum(is.na(sites2))
sum(sites2$StationCode %in% algae$StationCode) ##1946


### join algae data top sites
algae1_sites <- left_join(algae, sites2, by="StationCode")
head(algae1_sites)

algae2_sites <- left_join(algae2, sites2, by="StationCode")
head(algae2_sites)


## merge dataframes
algae3 <- bind_rows(algae1_sites, algae2_sites)
head(algae3)

## remove rep column and make wide
algae4 <- algae3 %>%
  dplyr::select(-Replicate) %>%
  pivot_wider(id_cols=c(SampleID, SampleDate, StationCode, New_Lat, New_Long), names_from = Index, values_from = Result) %>%
  distinct(SampleDate, StationCode, .keep_all = T) %>%
  rename(Latitude = New_Lat, Longitude = New_Long)

dim(algae4) # 2522
head(algae4)
save(algae4, file= "output_data/01a_all_asci_data_clean.RData")

sum(is.na(algae4)) # 105


## just the sites

algae_stations_distinct <- algae4 %>%
  dplyr::select(StationCode, Latitude, Longitude) %>%
  distinct()
dim(algae_stations_distinct) ## 2320
save(algae_stations_distinct, file="output_data/01a_algae_stations_distinct.RData")
  
