# Get Altered Gage FFM 

# Libraries ---------------------------------------------------------------

# MAIN
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 

ffctoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLYXRpZSIsImxhc3ROYW1lIjoiSXJ2aW5nIiwiZW1haWwiOiJrYXRpZWlAc2Njd3JwLm9yZyIsInJvbGUiOiJVU0VSIiwiaWF0IjoxNTc2NTQ0MDk2fQ.6fWAyYohV4wxHHSGR4zBeYoTKbUw4YXuIEjQyPB33lU"
ffcAPIClient::set_token(ffctoken)

# SUPPPORTING
library(tidyverse) # yes
library(sf)
library(tictoc) # timing stuff
library(furrr) # parallel processing for mapping functions/loops (purrr)
library(tidylog) # good for logging what happens

# Functions For Running Things --------------------------------------------

# Error wrapper to return NA if the gage data doesn't exist/not enough data
get_ffc_eval <- possibly(evaluate_gage_alteration, otherwise=NA_real_)

# Some gages return NA due to lack of data need a function to remove these NAs from a list
f_remove_empty <- function(x){
  if(is.list(x)) {
    x %>%
      purrr::discard(rlang::is_na) %>%
      purrr::map(f_remove_empty)
  } else {
    x
  }
}


# 01. Prep Data -----------------------------------------------------

# list of sites that we have pairs for
load("output_data/03_gages_comids_algae_mets.RData")
head(algae_coms)


# load the list of data we already have:
load("input_data/usgs/usgs_altered_ffc_list.rda")

usgs_alt_list <- names(usgs_ffc_alt) %>% as_tibble() %>% 
  rename(gage_id=value)

# Cleaning Data ----------------------------------------------------------------

# only need to do this once for first run through
# rearrange things a bit so geometry is dropped
algae_coms_final <- algae_coms %>% st_drop_geometry()
# length(unique(algae_coms_final$site_id))
# # how many distinct gages? (n=187)
algae_coms_final %>% distinct(site_id) %>% tally()

# # get a usgs_list
usgs_list_paired <- algae_coms_final %>% dplyr::distinct(site_id, .keep_all=TRUE) %>%
  select(site_id, StationID, comid, lon, lat) %>%
  #left_join(., ca_usgs_gages, by=c("ID"="site_id")) %>%
  rename(gage_id=site_id) %>%
  st_as_sf(coords=c("lon","lat"), crs=4269, remove=FALSE)

class(usgs_list_paired)

# join to see which gages we need to download still
gages_to_get <- anti_join(usgs_list_paired, usgs_alt_list, by=c("gage_id"))

summary(gages_to_get)

# GET COMIDS --------------------------------------------------------------


# get the comid
# library(nhdplusTools)
# 
# usgs_list2 <- usgs_list %>% st_transform(3310) %>% 
#   group_split(gage_id) %>%
#   set_names(., usgs_list$gage_id) %>%
#   map(~discover_nhdplus_id(.x$geometry))
# 
# usgs_list2 %>% 
#   purrr::map_lgl(~ length(.x)>1) %>% 
#   #table() # 3 are FALSE
#   .[.==TRUE] # get values that are TRUE
# 
# usgs_list2["11055801"]
# usgs_list2["11055801"] <- 2775510
# 
# # flatten into single dataframe instead of list
# usgs_comids <- usgs_list2 %>% flatten_dfc() %>% t() %>% 
#   as.data.frame() %>% 
#   rename("NHDV2_COMID"=V1) %>% rownames_to_column(var = "gage_id")
# 
# # rejoin to original dataset
# usgs_list <- usgs_list %>% 
#   left_join(., usgs_comids) %>% 
#   select(gage_id:comid2, NHDV2_COMID, station_nm:geometry)
# 
# # save out again
# save(usgs_list, file = "data_output/04_usgs_list_updated_comids.rda")

# 01. DOWNLOAD Multiple Gages IN LIST ------------------------------

#load("data_output/04_usgs_list_updated_comids.rda")

# remove sf class here
usgs_list <- gages_to_get %>% st_drop_geometry()

# RUN
tic(msg = "Finished Getting Data") # time start
g_300 <- usgs_list %>% 
  # slice(201:336) %>%  # pick a subset of rows from gage list
  split(.$gage_id) %>% # make into named list
  map(., ~{select(.x, gage_id, comid)}) %>% # pull gage ID out
  furrr::future_imap(., 
                     ~get_ffc_eval(gage_id = .x$gage_id, 
                                   token = ffctoken,
                                   comid = .x$comid,
                                   force_comid_lookup = FALSE,
                                   plot_results = FALSE), 
                     .progress = TRUE) 
beepr::beep(2) # something fun to let you know it's done
toc()

# 02. CLEAN NAs ----------------------------------------------------

# completed
g100f <- f_remove_empty(g_100)
g200f <- f_remove_empty(g_200)
g300f <- f_remove_empty(g_300)
#g600f <- f_remove_empty(g600)
#g800f <- f_remove_empty(g800)

# 03. SAVE OUT ------------------------------------------------------------

# FIRST TIME THROUGH 
# bind together multiple runs into one file
usgs_ffc_all <- append(x = g100f, values=c(g200f, g300f)) 

# save
save(usgs_ffc_all, file = "data_output/04_usgs_all_ffc_list.rda")

# If saving and rerunning/adding things subsequently
#load("data_output/usgs_all_ffc_list.rda") # load existing dataset

# update/add more data
#usgs_ffc_all <- append(x = usgs_ffc_all, values=c(g300f))
# so 209/336 = 70% of gages we could get data for

# add in alteration dataset (downloaded already)
gages_we_have <- inner_join(usgs_list_paired, usgs_alt_list, by=c("gage_id")) %>% st_drop_geometry() %>% 
  pull(gage_id)

# pull actual data now:
usgs_alt_dat <- usgs_ffc_alt[c(gages_we_have)]

# combine with full set NOT YET BECAUSE TYPES ARE DIFF!!
#usgs_ffc_all <- append(x=usgs_ffc_all, usgs_alt_dat)

# so about 75% had enough data
# 385/512


# 03. GET PERCENTILES AND FLATTEN TO DF -------------------------------

# Get all the Percentiles and put into one single large dataframe
g_all_percentiles <- map(usgs_ffc_all, ~{bind_rows(.x$ffc_percentiles,  .x$predicted_percentiles)}) %>% bind_rows(., .id = "list_id") # since gage_id already in df (just a double check here)

g_alt_percentiles <- map(usgs_alt_dat, ~{bind_rows(.x$ffc_percentiles,  .x$predicted_percentiles)}) %>% bind_rows(., .id = "list_id") %>% 
  mutate(gage_id = as.character(gage_id))

g_all_percentiles %>% distinct(gage_id) %>% tally()
g_alt_percentiles %>% distinct(gage_id) %>% tally()

# combine
g_all_percentiles <- bind_rows(g_all_percentiles, g_alt_percentiles)

# save 
save(g_all_percentiles, file = "data_output/04_usgs_all_ffc_percentiles.rda")
write_csv(g_all_percentiles, path="data_output/04_usgs_all_ffc_percentiles.csv")

# 04. GET ALL FFM AND FLATTEN TO DF -----------------------------------

# Get all the FFM and put into one single large dataframe
g_all_ffc <- map(usgs_ffc_alt, ~.x$ffc_results) %>% 
  bind_rows(., .id="gage_id") # gage_id not in this df

g_alt_ffc <- map(usgs_alt_dat, ~.x$ffc_results) %>% 
  bind_rows(., .id="gage_id") # gage_id not in this df

g_all_ffc <- bind_rows(g_all_ffc, g_alt_ffc)

# save 
save(g_all_ffc, file = "data_output/04_usgs_all_ffc_metrics.rda")
write_csv(g_all_ffc, path="data_output/04_usgs_all_ffc_metrics.csv")

# 05. GET ALL ALTERATION STATUS AND FLATTEN TO DF ---------------------

#load("data_output/usgs_all_ffc_list.rda")

# first pull all alteration dataframes out
g_all_alt <- map(usgs_ffc_all, ~.x[["alteration"]])

# then convert all logicals to text before bind rows together
g_all_alt <- rapply(g_all_alt, 
                    as.character, # function to apply
                    "logical", # class to match in df
                    how="replace") %>% # what to do w match
  bind_rows()

g_alt_alt <- map(usgs_alt_dat, ~.x[["alteration"]])

# then convert all logicals to text before bind rows together
g_alt_alt <- rapply(g_alt_alt, 
                    as.character, # function to apply
                    "logical", # class to match in df
                    how="replace") %>% # what to do w match
  bind_rows() %>% 
  mutate(gage_id=as.character(gage_id))

# bind together
g_all_alt <- bind_rows(g_all_alt, g_alt_alt)

# save 
save(g_all_alt, file = "data_output/04_usgs_all_ffc_alteration.rda")
write_csv(g_all_alt, path="data_output/04_usgs_all_ffc_alteration.csv")


# 06. GET WYT PERCENTILES -------------------------------------------------

# Get all the Percentiles and put into one single large dataframe
g_all_wyt_percentiles <- map(usgs_ffc_all, ~.x$predicted_wyt_percentiles) %>%
  bind_rows(., .id = "gage_id") # since gage_id already in df 
g_all_wyt_percentiles %>% distinct(gage_id)

g_alt_wyt_percentiles <- map(usgs_alt_dat, ~.x$predicted_wyt_percentiles) %>%
  bind_rows(., .id = "gage_id") # since gage_id already in df 
g_alt_wyt_percentiles %>% distinct(gage_id)

# bind
g_all_wyt_percentiles <- bind_rows(g_all_wyt_percentiles, g_alt_wyt_percentiles)

# save 
save(g_all_wyt_percentiles, file = "data_output/04_usgs_all_ffc_wytpercentiles.rda")
write_csv(g_all_wyt_percentiles, path="data_output/04_usgs_all_ffc_wytpercentiles.csv")
