# Running GBM (Boosted Regression Trees) with PURRR
# R. Peek 2020
# Generates models to select "final" model based on tuning criteria
# then makes final model, saved in models/

# Libraries ---------------------------------------------------------------

library(purrr) # for working with lists/loops
library(tidylog)
library(tidyverse) # all the things
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
library(gbm) # boosted regression trees
library(rsample) # sampling
library(rlang)
library(dismo)
source("code/functions/My.gbm.step.R")

set.seed(321) # reproducibility

# 01. Load Data ---------------------------------------------------------------

## If already run Step 01a, comment out these next 3 lines and start at mainstem rivers, and skip 01a

# trimmed (May-Sept) data, and sf layers w geoms for algae and usgs
# load("output_data/04_selected_algae_asci_ann_sf.rda")

# mainstem rivers
load("output_data/02_selected_nhd_mainstems_gages.rda") # mainstems_all

# ca regions
ca_sp_regions <- read_sf("input_data/spatial/umbrella_sp_regions.shp", as_tibble = T) %>% st_transform(4326)

# load updated data w HUC_regions: trim untrimmed
load(file="output_data/04a_selected_algae_ascii_ffm_ann_trim.rda")
load(file="output_data/04a_selected_algae_asci_ffm_ann.rda")

# set background basemaps/default options:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(homebutton = FALSE, basemaps=basemapsList, viewer.suppress = FALSE)

# 01a. Add Regions --------------------------------------------------

# # check crs:
st_crs(algae_asci_ffm_ann_trim)
st_crs(ca_sp_regions)

# # join with regions and add huc_region, make sure both df are in 4326
algae_asci_ffm_ann_trim <- st_join(algae_asci_ffm_ann_trim, left = TRUE, ca_sp_regions["huc_region"])
names(algae_asci_ffm_ann_trim)
# ?separate
# ## format date column to match BMI 
# # algae_asci_ffm_ann_trim <- algae_asci_ffm_ann_trim %>%
# #   separate(sampledate, c("YYYY", "MM", "DD"), remove=F) 
# 
# # algae_asci_ann_trim <- algae_asci_ann_trim %>%
# #   unite(YMD, YYYY, MM, DD, sep="-", remove=F)
# 
# # make a simpler layer for just editing:
algae_asci_sites <- algae_asci_ffm_ann_trim %>%
  dplyr::distinct(StationID, ID, .keep_all = TRUE)
length(unique(algae_asci_sites$StationID)) # 237
# 
# # view and update w mapedit
mapview(algae_asci_sites, col.regions="orange") + mapview(ca_sp_regions)
# install.packages("mapedit")
# install.packages("httpuv")
library(mapedit)
library(leafpm)
library(leaflet)
# # 
# # # use this to select features (returns a list of stationcodes)
# selectMap(
#   leaflet() %>%
#     addTiles() %>%
#     addPolygons(data=ca_sp_regions, layerId = ~huc_region, color = "orange") %>%
#     addCircleMarkers(data = algae_asci_sites, layerId = ~StationID)
# )
# 
# ## sites to add to central valley
# cvalley_add <- c("514FC1278", "514RCR001", "534DCC167")
# 
# ## sites to add to great_basin
# gbasin_add <- c("603MAM004", "630PS0005")
# 
# ## sites to add to southcoast
# scoast_add <- c("628PS1307","628PS1179","719MISSCK","719TRMDSS","719FCA001")
# 
# # Amargosa site is "609PS0053" = mojave?
# 
# 
# summary(as.factor(algae_asci_ann_trim$huc_region.x)) ## 224 NAs
# 
# # use case_when to replace
# algae_asci_ann_trim <- algae_asci_ann_trim %>%
#   mutate(huc_region = case_when(
#     StationID %in% cvalley_add ~ "central_valley",
#     StationID %in% gbasin_add ~ "great_basin",
#     StationID %in% scoast_add ~ "south_coast",
#     TRUE ~ huc_region.x))
# 
# summary(as.factor(algae_asci_ann_trim$huc_region.x))
# table(algae_asci_ann_trim$huc_region.x)
# # 
# # # map and double check:
# mapview(algae_asci_ann_trim, zcol="huc_region", layer.name="Selected Sites", viewer.suppress=FALSE) +
#   mapview(ca_sp_regions, zcol="huc_region", layer.name="HUC Regions", alpha.regions=0.1)
# # 
# # # save back out for later
# save(algae_asci_ann_trim, file = "output_data/04_selected_algae_asci_ann_trim_w_huc_region.rda")


# 02. Select a Region ---------------------------------------------------------

# make a simpler layer for mapping
algae_asci_sites <- algae_asci_ffm_ann_trim %>% 
  dplyr::distinct(StationID, ID, .keep_all = TRUE)

# if selecting by a specific region use region select
# list regions ("central_valley", "great_basin", "north_coast", "south_coast")
table(algae_asci_sites$huc_region.x)

modname <- "great_basin"   # or "all_ca_ffc_only"
Hregions <- c(modname) # set a region or regions
names(algae_asci_ffm_ann_trim)
# Hregions <- c("central_valley", "great_basin", "north_coast", "south_coast")
# now filter data to region(s) of interest
region_sel <- algae_asci_ffm_ann_trim %>% filter(huc_region.x %in% Hregions)
region_sel
mapview(region_sel, zcol="huc_region.x", viewer.suppress=FALSE)

# 03. Select algae Response Variable for GBM ------------------------------

# get metrics
# algae.metrics<-c("asci")

# PICK RESPONSE VAR FOR MODEL
hydroDat <- "ANN" # can be Annual, Lag1, Lag2, POR
algaeVar <- quote(MMI.hybrid) # select response var from list above

# 04. Setup ANN Data for Model ----------------------------------------------------------------
names(region_sel)
# need to select and spread data: 
data_ann <- region_sel %>% st_drop_geometry() %>% 
  distinct(SampleID_old.x, ID, ffm_metric, YYYY, .keep_all = TRUE) %>% 
  dplyr::select(StationID, SampleID_old.x, HUC_12, ID, comid, NHDV2_COMID,
                YYYY, MM, DD, MMI.hybrid, huc_region.x, CEFF_type, gage_id_c,
                ffm_metric, ffm_value) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = ffm_metric, values_from = ffm_value) %>%  #values_fn = list(ffm_value = length)) %>% 
  mutate(huc_region.x = as.factor(huc_region.x),
         CEFF_type = as.factor(CEFF_type)) %>% 
  as.data.frame()

# check how many NAs per col
dim(data_ann)
names(data_ann)
data_names <- names(data_ann) # save colnames out

# remove cols that have more than 70% NA
data_ann <- data_ann[, which(colMeans(!is.na(data_ann)) > 0.7)]
dim(data_ann)

# find the cols that have been dropped
setdiff(data_names, names(data_ann))

# [1] "FA_Dur"      "FA_Mag"      "FA_Tim"      "Peak_Tim_2"  "Peak_Dur_2" 
# [6] "Peak_Fre_2"  "NA"          "Peak_Tim_5"  "Peak_Dur_5"  "Peak_Fre_5" 
# [11] "Peak_Tim_10" "Peak_Dur_10" "Peak_Fre_10"

# 05. Make Train Data -------------------------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_ann), nrow(data_ann))
data_ann <- data_ann[random_index, ]

# make training dataset
data_ann_train <- data_ann %>% # use all data
  dplyr::select({{algaeVar}}, 14:ncol(.)) %>%  # use 14 if not including HUC region and CEFF_type
  dplyr::filter(!is.na({{algaeVar}})) %>% as.data.frame()

# double check cols are what we want
names(data_ann_train)

# 06. GBM.STEP MODEL  ------------------------------------------------------------

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
# hyper_grid <- hyper_grid[-c(3,12),] ## gbm did not work - data too small
hyper_grid
# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 1, # response in training data
    gbm.x = 2:ncol(data), # hydro dat
    family = "gaussian",
    data = data,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
    m_step$self.statistics$mean.null
  
}

# 07. RUN GBM.STEP WITH PURRR ---------------------------------------------
# rlang::last_error()
# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data_ann_train) # CHECK AND CHANGE!!
)
head(data_ann_train)
dim(data_ann_train)
warnings()
# 08. VIEW AND SAVE MODEL RESULTS -----------------------------------------

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# write these all out to a file for reference later
(gbm_file <- paste0("models/05_gbm_final_",tolower(as_name(algaeVar)),"_", tolower(hydroDat), "_", modname, "_hypergrid"))

# check for file and delete?
if(fs::file_exists(path = paste0(gbm_file,".csv"))){
  fs::file_delete(path = paste0(gbm_file, ".csv"))
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# save out
write_csv(hyper_grid, path = paste0(gbm_file,".csv"), append = TRUE)

# read in
hyper_grid <- readr::read_csv(file = paste0(gbm_file,".csv"), col_names = c("shrinkage", "interaction.depth", "n.minobsinnode", "bag.fraction", "dev_explained"))

# get best model solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))


# 09. RUN BEST BRT MODEL ---------------------------------------------------------------

# based on above, run final BRT and save:
gbm_final_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 1, # response in training data
    gbm.x = 2:ncol(data), # hydro dat
    family = "gaussian",
    data = data,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

# set up filename for best model outputs
(gbm_best_file <- paste0("models/05_gbm_final_",tolower(as_name(algaeVar)),"_", tolower(hydroDat), "_", modname, "_", "model_output.txt"))

# check for file and delete?
if(fs::file_exists(path = gbm_best_file)){
  fs::file_delete(path = gbm_best_file)
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data_ann_train # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write_tsv(hyper_best, path = gbm_best_file,
          col_names = TRUE, append=TRUE)

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 

# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_", as_name(algaeVar),"_",hydroDat, "_",modname)), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_", tolower(as_name(algaeVar)))))

# save to RDS
write_rds(x = get(fileToSave), path = paste0("models/05_",fileToSave, "_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("models/05_",fileToSave,"_model_data.rda")))

