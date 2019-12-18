######### spatial join with bmi and gauges

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

#  load clean algae data

load(file="output_data/clean_algae.RData") # algae
head(algae)


## convert to spatial for pairing - 1) with bmi (overlap), 2) ref gauges
# coords <- asci_scor_sites[, 2:3]
#  coords into numeric
algae$Latitude <- as.numeric(as.character(algae$Latitude))
algae$Longitude <- as.numeric(as.character(algae$Longitude))

#  remove NAs
sum(is.na(algae)) #50 - already NAs but not registering until a number
#  where are the NAs? 
na_ind <- which(is.na(algae)) 
algae <- na.omit(algae)

#  spatial point df
?st_as_sf
str(algae)
# coordinates(asci_scor_sites) <- c("Latitude","Longitude")
algae <- algae %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F) # define coords to make spatial

plot(algae)
save(algae, file="output_data/algae_spatial.RData")


#  component metrics for Algae? only OoverE & MMI for diatoms and soft bodied

#  bug data

load(file="/Users/katieirving/Documents/git/bmi_ffm_links/data_output/bmi_cleaned_all.rda")
head(bmi_clean)
dim(bmi_clean)

bmi_sites <- bmi_clean[,c(3,6:7)]
# dim(bmi_sites) # 310216      3
bmi_sites2 <- distinct(bmi_sites)
write.csv(bmi_sites2, "/Users/katieirving/Documents/Projects/CEFF/GIS/bmi_sites.csv") 
dim(bmi_sites2) # 5627    3

bmi_sites2 <- bmi_sites2 %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) # define coords to make spatial


#  compare algae sites with bmi sites
plot(bmi_sites2)
plot(algae)
head(algae)
head(bmi_sites2)
# write.csv(asci_scor_sites, "/Users/katieirving/Documents/Projects/CEFF/GIS/asci_sites.csv") 

#  pair sites
#  distance between sites

install.packages("spatstat")
# library(spatstat)
# 
# install.packages("geosphere")
# library(geosphere)
# 
# dists <- distHaversine(bmi_sites, asci_sites)

#  check same crs
st_crs(bmi_sites2)
st_crs(asci_scor_sites)

?st_join
?inner_join

test_join <- st_join(bmi_sites2, asci_scor_sites, join=st_within, left=F)
head(test_join)




