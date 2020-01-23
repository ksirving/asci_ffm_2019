# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)

#  statewide study - asci - ffm

#  upload algae data - asci scores downloaded from https://sites.google.com/view/asci/results - Susie

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

## upload raw data - to check sample method
algae_raw <- read.csv("input_data/algae.bug.data.10172019.csv", header = T)
head(algae_raw)
unique(algae_raw$CollectionMethodCode)

