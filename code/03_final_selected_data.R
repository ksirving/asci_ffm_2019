# 04 Generate Final Selected Sites/Data
## R. Peek/Katie Irving
## Look at final output

## DATA OUT:
### - sel_h12_bmi (all huc12s with gage/bmi sites inside them, n=53)
### "data_output/03_selected_h12_contain_bmi_gage.rda"

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)

# Load Data ---------------------------------------------------------------

# load("data_output/01_bmi_cleaned_stations_w_site_status.rda") # don't have this one - algae ref sites?
# load("data_output/03_selected_bmi_and_gages.rda")
# load("data_output/03_selected_nhd_flowlines_mainstems.rda")
# load("data_output/03_selected_h12_contain_bmi_gage.rda")
# load("data_output/00_bmi_cleaned_all.rda")
# load("data_output/03_bmi_all_stations_comids.rda")

load("output_data/algae_all_stations_comids.rda") # algae_segs_df - algae sites and comids
load("output_data/clean_algae.RData") # algae - all data
load("output_data/paired_gages_algae_merged.RData") # sel_algae_gages - 126 algae sites, 40 gages
load("output_data/selected_nhd_flowlines_mainstems.rda") # mainstems_us, mainstems_ds mainstems us/ds
load("output_data/selected_h12_contain_algae_gage.rda") # sel_h12s_algae - huc 12s
load("output_data/paired_only_gages_algae.RData") # sel_gages_algae paired gages - no algae data

# load mapview bases
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)


# Get algae comids ----------------------------------------------------------
#  merge algae comids and gages
# head(algae_segs_df)
# head(sel_algae_gages)
# unique(sel_algae_gages$ID) # 40
# unique(sel_algae_gages$gage) # 40

algae_com_gage <- merge(algae_segs_df, sel_algae_gages, by="StationID")
# head(algae_com_gage) # 126 sites & 40 gages
dim(algae_com_gage)
algae_com_gage <- algae_com_gage %>% 
  st_as_sf(coords=c("Latitude", "Longitude"), crs=4326, remove=F) # define coords to make spatial

save(algae_com_gage, file="output_data/paired_gages_algae_comid.RData" ) # gages us and ds mets
# all stations us of gage:
algae_us_coms <- algae_com_gage %>% filter(comid %in% mainstems_us$nhdplus_comid)

# all stations 15km downstream on mainstem
algae_ds_coms <- algae_com_gage %>% filter(comid %in% mainstems_ds$nhdplus_comid)

# bmi_us_coms %>% st_drop_geometry() %>% inner_join(., bmi_ds_coms, by="StationCode") %>% tally()

# Make Map of Selected Gages and BMI Stations --------------------------

# this map of all sites selected U/S and D/S
m3 <- mapview(algae_ds_coms, cex=6, col.regions="orange", layer.name="Selected Algae D/S") +  
  mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
  mapview(algae_us_coms, cex=6, col.regions="yellow", layer.name="Selected Algae U/S") +  
  mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
  mapview(sel_gages_algae, col.regions="cyan", cex=7, layer.name="Selected USGS Gages") + 
  mapview(sel_algae_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other Algae Sites in H12") + 
  mapview(sel_h12_algae, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# Make Map of Selected Stations by Site Status  --------------------------

# # first add site status - do not have site status for algae!
# algae_ds_coms <- left_join(algae_ds_coms, algae, by="StationID")
# algae_us_coms <- left_join(algae_us_coms, algae, by="StationID")
# 
# 
# m4 <- mapview(algae_ds_coms, cex=6, zcol="SiteStatus", layer.name="Selected Algae D/S") +  
#   mapview(mainstems_ds, color="darkblue", cex=3, layer.name="NHD D/S Flowline 15km", legend=F)+
#   mapview(algae_us_coms, cex=6, zcol="SiteStatus", layer.name="Selected Algae U/S") +  
#   mapview(mainstems_us, color="slateblue", cex=3, layer.name="NHD U/S Flowline", legend=F)+
#   mapview(sel_gages_algae, zcol="stream_class",  cex=7, layer.name="Selected USGS Gages") + #col.regions="cyan",
#   mapview(sel_algae_gages, col.regions="gray", cex=3, alpha=0.5, layer.name="Other Algae Sites in H12") + 
#   mapview(sel_h12_algae, col.regions="dodgerblue", alpha.region=0.1, color="darkblue", legend=F, layer.name="HUC12")
# 
# m4@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# Combine algae US and DS ---------------------------------------------------

# combine:
algae_coms <- do.call(what = sf:::rbind.sf,
                    args = list(algae_ds_coms, algae_us_coms))
# class(algae_coms)
head(algae_coms)
dim(algae_coms)
# head(algae)
# sum(is.na(algae))
#library(DT)

save(algae_coms, file="output_data/gages_comids_algaemets.RData")

# # pull algae sites and get list of data, first join with orig full dataset:
# algae_coms_dat <- left_join(algae_coms, algae, by="StationID") #%>% 
#   # drop NAs (72 sites: is.na(bmi_coms_dat$SampleID)
#   #filter(!is.na(SampleID_old))
# head(algae_coms_dat)
# now look at how many unique samples are avail: n=98 unique samples
# algae_coms_dat %>% as.data.frame() %>% group_by(SampleID_old.x) %>% distinct(SampleID_old.x) %>% tally
# 
# # now look at how many unique stations: n=74 stations
# algae_coms_dat %>% as.data.frame() %>% group_by(StationID) %>% distinct(StationID) %>% tally


# Get algae comids ----------------------------------------------------------

# all stations us of gage:
algae_us_coms <- algae_com_gage %>% filter(comid %in% mainstems_us$nhdplus_comid)
algae_us_coms
# all stations 15km downstream on mainstem
algae_ds_coms <- algae_com_gage %>% filter(comid %in% mainstems_ds$nhdplus_comid)

# combine US and DS
algae_coms <- rbind(algae_ds_coms, algae_us_coms)

# distinct stations:
algae_coms %>% st_drop_geometry() %>% distinct(StationID, ID) %>% tally() #74
algae_coms %>% st_drop_geometry() %>% distinct(comid) %>% tally() #61
head(algae_coms)
# potential sites:
#bmi_coms %>% View()

# rm old layer:
rm(algae_ds_coms, algae_us_coms)


# Check against asci Scores -----------------------------------------------
# 
# # subset full df for only asci scores
head(algae_com_gage) # want stationid, comid, sampleid, lat, long, all metrics
names(algae_com_gage)
asci_mets <- algae_com_gage[,c(1:12)]
head(asci_mets)
dim(asci_mets) #126
asci_mets <- as.data.frame(asci_mets)
# 
# # # match against existing sites:
# algae_asci <- inner_join(algae_coms,asci_mets, by=c("StationID"))
# head(algae_asci)
# algae_asci <- left_join(algae_asci, algae[,c(3:4)], by="Stationid")
# 
# # how many unique matches?
# length(unique(bmi_csci$StationCode))
# table(bmi_csci$SiteStatus)
# 
# # look at CSCI
# hist(bmi_csci$csci_percentile)
# 
# # look at sampling timing
# hist(bmi_csci$samplemonth)
# table(bmi_csci$samplemonth)
# 
# # look at CSCI percentile by Site Status (not avail for all sites)
# ggplot() + geom_boxplot(data=bmi_csci, aes(x=SiteStatus, y=csci_percentile))
# 
# 
# # function to get data
# stat_box_data <- function(y, upper_limit = max(bmi_csci$csci_percentile)) {
#   return( 
#     data.frame(
#       y = 0.95 * upper_limit,
#       label = paste('count =', length(y), '\n',
#                     'mean =', round(mean(y), 1), '\n')
#     )
#   )
# }

# plot asci percentile
ggplot(data=filter(bmi_csci, !is.na(SiteStatus)), aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
  theme_bw()

ggplot(data=bmi_csci, aes(x=SiteStatus, y=csci_percentile)) + 
  geom_boxplot(aes(fill=SiteStatus), show.legend = F) +
  stat_summary(fun.data=stat_box_data, geom="text", hjust=1, vjust=0.9) +
  theme_bw()

