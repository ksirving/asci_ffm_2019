### functional flow metrics
#  package to 
setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(tidyverse)

# # library(devtools)
# # devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
# # api client package to extract ffm 
# library(ffcAPIClient) ## 
# 
# token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLYXRpZSIsImxhc3ROYW1lIjoiSXJ2aW5nIiwiZW1haWwiOiJrYXRpZWlAc2Njd3JwLm9yZyIsInJvbGUiOiJVU0VSIiwiaWF0IjoxNTc2NTQ0MDk2fQ.6fWAyYohV4wxHHSGR4zBeYoTKbUw4YXuIEjQyPB33lU"
# ffcAPIClient::set_token(token) 
# 
# daily_df <- get_usgs_gage_data(10295500)
# head(daily_df)
# results <- ffcAPIClient::get_ffc_results_for_usgs_gage(10295500) 
# 
# drh_plot <- ffcAPIClient::plot_drh(results)  # includes optional output_path argument to save to file automatically
# 
# head(results)
# drh_plot  # display the plot

# load(file="/Users/katieirving/Documents/git/bmi_ffm_links/data_output/selected_bmi_and_flow_metrics.rda")
# 
# load(file="/Users/katieirving/Documents/git/bmi_ffm_links/data_output/selected_usgs_flow_metrics_POR.rda")

# upload functional flow metric data - existing 

#  list gages
load(file="output_data/02b_paired_only_all_gages_algae.RData")
head(sel_gages_algae) # 226 gauges paired with algae
dim(sel_gages_algae)
gages_list <- sel_gages_algae$site_id
sum(is.na(gages_list)) 


# ffmets <- read.csv("input_data/gages_ref_223_period_record.csv")
load(file="input_data/ref_gage_annFlow_stats_long.rda")
head(dat_long)
dim(dat_long) # 258944      

# #  subset flow data to algae gages

# 
dat_long <- subset(dat_long, gage %in% gages_list)
head(dat_long)
dim(dat_long) # 64464 

length(unique(dat_long$gage)) # 42 (ref gages only)
#  for brts will need to rearrange data by metric

#  subset metric # need to add "T" to the gageID
flow_long <- dat_long %>% mutate(site_id=paste0("T", gage)) %>% 
  # filter out NA's?
  filter(!is.na(data)) %>% 
  ungroup() %>% 
  select(site_id, gage, stream_class, stat:YrRange)

# rm old dataset
rm(dat_long)

# Avg Metrics for Period of Record ----------------------------------------

# set ID vars for flow metrics
flow_idvars <- flow_long %>% group_by(ID, stat) %>% distinct(ID, stat, maxYr, minYr)
# flow_idvars
# now avg for PERIOD OF RECORD for ASCI comparison
flow_por <- flow_long %>% select(-YrRange, -gage, -year, -stream_class) %>% group_by(ID, stat) %>% 
  summarize_at(vars(data), mean, na.rm=T) %>% 
  # rejoin yr ranges
  left_join(., flow_idvars)

# Get Flow Record only in Same Year/lag as algae Sites ----------------------

# need to pull 2 years prior, 1 year prior, and same year as algae site data

load("output_data/02b_paired_only_all_gages_algae.RData") # sel_algae_gages - 
head(sel_algae_gages)
names(sel_algae_gages)
asci_mets <- sel_algae_gages[,c(1:13)] 

head(asci_mets)

asci_mets <- separate(asci_mets, col = sampledate , into=c("YYYY", "MM", "DD"), remove = F) 
head(asci_mets)

# make vector of years and algae_ids
asci_yrs <- asci_mets %>% group_by(SampleID_old) %>% pull(YYYY) %>% unique()
asci_yrs <- as.numeric(as.character(asci_yrs))
(asci_yrs_2 <- asci_yrs - 2) # set lag 2
(asci_yrs_1 <- asci_yrs - 1) # set lag 1

# now combine and order:
asci_years <- combine(asci_yrs, asci_yrs_1, asci_yrs_2) %>% sort() %>% unique() #  
asci_years # 2007-2015

# rm old files
rm(asci_yrs, asci_yrs_1, asci_yrs_2)

# now match with flow data (only goes through 2016)
flow_by_years_asci <- flow_long %>% 
  filter(year %in% asci_years) # 2007-2015

# make wide
flow_by_years_asci_wide <- flow_by_years_asci %>% 
  pivot_wider(names_from=stat, values_from=data)

flow_by_years_asci_wide %>% distinct(ID) %>% dim # should be 37 gages match same years

# save flow data out for annual match
save(flow_by_years_asci, flow_by_years_asci_wide, file="output_data/04_selected_flow_by_years_of_asci.RData")

head(flow_por)

# unique metrics? (should be 34)
length(unique(flow_por$stat))
unique(flow_por$stat)

# make wide for join
flow_por_wide <- flow_por %>% 
  pivot_wider(names_from=stat, values_from=data)
#values_fill=list(data=0))
# can fill missing values with zero if preferred: values_fill=list(data=0)

#  combine with asci

#  algae & gage data

load("output_data/02_algae_all_stations_comids.rda")
load("output_data/01_clean_algae.RData")
load("output_data/02_paired_gages_algae_merged.RData")
load("output_data/02_selected_nhd_flowlines_mainstems.rda") #mainstems_ds us
load("output_data/02_selected_h12_contain_algae_gage.rda")
load("output_data/02_paired_only_gages_algae.RData")
load("output_data/03_paired_gages_algae_comid.RData") #algae_com_gage


#  mainstem sites upstream/downstream. combine 
# all stations us of gage:
algae_us_coms <- algae_com_gage %>% filter(comid %in% mainstems_us$nhdplus_comid)

# all stations 15km downstream on mainstem
algae_ds_coms <- algae_com_gage %>% filter(comid %in% mainstems_ds$nhdplus_comid)

algae_coms <- do.call(what = sf:::rbind.sf,
                      args = list(algae_ds_coms, algae_us_coms))

unique(algae_coms$SampleID_old)
dim(algae_coms)
head(algae_coms)

# now look at how many unique samples are avail: n=74 unique samples -

# now look at how many unique stations: n=74 stations
algae_coms %>% as.data.frame() %>% group_by(StationID) %>% distinct(StationID) %>% tally


# all stations us of gage:
algae_us_coms <- algae_com_gage %>% filter(comid %in% mainstems_us$nhdplus_comid)
algae_us_coms$to_gage <- paste("us")

# all stations 15km downstream on mainstem
algae_ds_coms <- algae_com_gage %>% filter(comid %in% mainstems_ds$nhdplus_comid)
algae_ds_coms$to_gage <- paste("ds")
# combine US and DS
algae_coms <- rbind(algae_ds_coms, algae_us_coms)

# distinct stations:
algae_coms %>% st_drop_geometry() %>% distinct(StationID, ID) %>% tally() #74
algae_coms %>% st_drop_geometry() %>% distinct(comid) %>% tally() #61
head(algae_coms)
names(algae_coms)
# dim(algae_coms) # 120
# potential sites:
#bmi_coms %>% View()

# rm old layer:
rm(algae_ds_coms, algae_us_coms)

save(algae_coms, file="output_data/03_gages_comids_algae_mets.RData")
# #  combine with flow data POR
# POR 
# # Join with Flow POR ------------------------------------------------------
flow_por_wide
algae_asci_flow_por <- left_join(algae_coms, flow_por_wide, by="ID")

algae_asci_flow_porx <- separate(algae_asci_flow_por, col=sampledate, into =c("YY", "MM", "DD"), remove=F)
head(algae_asci_flow_porx)
algae_asci_flow_porx$YY <- as.numeric(as.character(algae_asci_flow_porx$YY))
# filter to sites that have data in the flow time range? # doesn't matter for POR?
# algae_asci_flow_por_overlap <- algae_asci_flow_porx %>%
#   filter(YY > minYr.x, YY< maxYr.x)

# length(unique(algae_asci_flow_por_overlap$StationID)) # 56 stations
# length(unique(algae_asci_flow_por_overlap$ID)) # 28 gages

save(algae_asci_flow_por, file="output_data/04_algae_gage_flow_metrics_POR.RData")

######## lag and annual here!!!!!!!!!!!!!
# YY = sample year
# year = FFM year 
algae_asci_flow_porx$year+2
algae_asci_flow_porx$year
names(algae_asci_flow_porx)
algae_asci_flow_porx <- algae_asci_flow_porx[,-c(21:44,50:61,63,64,65:100)] # remove FFMs as these are POR values

# JOIN with Flow by algae Lag Years ----------------------------------------


algae_asci_flow_yrs <- left_join(algae_asci_flow_porx, flow_by_years_asci_wide, by=c("ID")) 
algae_asci_flow_yrs$YY <- as.numeric(as.character(algae_asci_flow_yrs$YY))

#filter to same year as algae + 2 yr lag
algae_asci_flow_lagann <- filter(algae_asci_flow_yrs, YY == year.y | YY == year.y+1 | YY ==year.y+2)

# double check, should have 3 entries per sampleid
algae_asci_flow_lagann %>% select(StationID, SampleID_old, YY, year.y) %>% View()
dim(algae_asci_flow_lagann)

# how many NA's across records?
algae_asci_flow_lagann %>% drop_na(SP_Tim:Peak_Mag_20) %>% dim() # end up with 86 records, would drop 70% of data

save(algae_asci_flow_lagann, file="output_data/04_algae_gage_flow_metrics_Lag_and_Ann.RData")
