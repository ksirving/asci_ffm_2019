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

# trimmed (May-Sept) data, and sf layers w geoms for bmi and usgs
# load("data_output/05_selected_bmi_csci_por_and_sf.rda")

# mainstem rivers
load("data_output/03_selected_nhd_mainstems_gages.rda") # mainstems_all

# ca regions
ca_sp_regions <- read_sf("data/spatial/umbrella_sp_regions.shp", as_tibble = T) %>% st_transform(4326)

# load updated data w HUC_regions:
load("data_output/07_selected_bmi_csci_por_trim_w_huc_region.rda")

# set background basemaps/default options:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(homebutton = FALSE, basemaps=basemapsList, viewer.suppress = FALSE)

# 01a. Add Regions --------------------------------------------------

# check crs:
# st_crs(bmi_csci_por_trim)
# st_crs(ca_sp_regions)

# join with regions and add huc_region, make sure both df are in 4326
# bmi_csci_por_trim <- st_join(bmi_csci_por_trim, left = TRUE, ca_sp_regions["huc_region"])

# make a simpler layer for just editing:
# bmi_csci_sites <- bmi_csci_por_trim %>% 
#   dplyr::distinct(StationCode, ID, .keep_all = TRUE)
# length(unique(bmi_csci_sites$StationCode))

# view and update w mapedit
# mapview(bmi_csci_sites, col.regions="orange") + mapview(ca_sp_regions)

# library(mapedit)
# library(leafpm)
# library(leaflet)
# 
# # use this to select features (returns a list of stationcodes)
# selectMap(
#   leaflet() %>%
#     addTiles() %>%
#     addPolygons(data=ca_sp_regions, layerId = ~huc_region, color = "orange") %>%
#     addCircleMarkers(data = bmi_csci_sites, layerId = ~StationCode)
#   )

## sites to add to central valley
# cvalley_add <- c("514FC1278", "514RCR001", "534DCC167")

## sites to add to great_basin
# gbasin_add <- c("603MAM004", "630PS0005")

## sites to add to southcoast
# scoast_add <- c("628PS1307","628PS1179","719MISSCK","719TRMDSS","719FCA001")

# Amargosa site is "609PS0053" = mojave?

# so 294 NA's
# summary(as.factor(bmi_csci_por_trim$huc_region))

# use case_when to replace
# bmi_csci_por_trim <- bmi_csci_por_trim %>% 
#   mutate(huc_region = case_when(
#     StationCode %in% cvalley_add ~ "central_valley",
#     StationCode %in% gbasin_add ~ "great_basin",
#     StationCode %in% scoast_add ~ "south_coast",
#     TRUE ~ huc_region))
# 
# summary(as.factor(bmi_csci_por_trim$huc_region))
# table(bmi_csci_por_trim$huc_region)
# 
# # map and double check:
# mapview(bmi_csci_por_trim, zcol="huc_region", layer.name="Selected Sites", viewer.suppress=FALSE) +
#   mapview(ca_sp_regions, zcol="huc_region", layer.name="HUC Regions", alpha.regions=0.1)
# 
# # save back out for later
# save(bmi_csci_por_trim, file = "data_output/07_selected_bmi_csci_por_trim_w_huc_region.rda")


# 02. Select a Region ---------------------------------------------------------

# make a simpler layer for mapping
bmi_csci_sites <- bmi_csci_por_trim %>% 
  dplyr::distinct(StationCode, ID, .keep_all = TRUE)

# if selecting by a specific region use region select
# list regions ("central_valley", "great_basin", "north_coast", "south_coast")
table(bmi_csci_sites$huc_region)

modname <- "central_valley"   # or "all_ca_ffc_only"
Hregions <- c(modname) # set a region or regions

# now filter data to region(s) of interest
region_sel <- bmi_csci_por_trim %>% filter(huc_region %in% Hregions)

mapview(region_sel, zcol="huc_region", viewer.suppress=FALSE)

# 03. Select BMI Response Variable for GBM ------------------------------

# get metrics
bmi.metrics<-c("csci")

# PICK RESPONSE VAR FOR MODEL
hydroDat <- "POR" # can be Annual, Lag1, Lag2, POR
bmiVar <- quote(csci) # select response var from list above

# 04. Setup POR Data for Model ----------------------------------------------------------------

# need to select and spread data: 
data_por <- region_sel %>% st_drop_geometry() %>% 
  dplyr::select(StationCode, SampleID, HUC_12, ID, comid_ffc, comid_bmi,
                YMD, YYYY, csci, huc_region, CEFF_type,
                metric, status_code 
  ) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = status_code) %>% 
  mutate(huc_region = as.factor(huc_region),
         CEFF_type = as.factor(CEFF_type)) %>% 
  as.data.frame()

# check how many NAs per col
#summary(data_por)
dim(data_por)
data_names <- names(data_por)

# remove cols that have more than 70% NA
data_por <- data_por[, which(colMeans(!is.na(data_por)) > 0.7)]
dim(data_por)

# find the cols that have been dropped
setdiff(data_names, names(data_por))

# seems SP_Dur is largely NA, only col that was dropped

# 05. Split Train/Test Data -------------------------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_por), nrow(data_por))
data_por <- data_por[random_index, ]

## Split data and specify train vs. test using rsample
data_por_split <- initial_split(data_por, prop = .9)

# make training dataset
#data_por_train <- training(data_por_split) %>% 
data_por_train <- data_por %>% # use all data
  dplyr::select({{bmiVar}}, 12:ncol(.)) %>%  # use 12 if not including HUC region and CEFF_type
  dplyr::filter(!is.na({{bmiVar}})) %>% as.data.frame()

# make testing set
data_por_test <- testing(data_por_split) %>% 
  dplyr::select({{bmiVar}}, 12:ncol(.)) %>% 
  filter(!is.na({{bmiVar}})) %>% as.data.frame()

# double check cols are what we want
names(data_por_train)

# 06. GBM.STEP MODEL  ------------------------------------------------------------

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
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

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data_por_train) # CHECK AND CHANGE!!
)

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
(gbm_file <- paste0("models/07_gbm_final_",tolower(as_name(bmiVar)),"_", tolower(hydroDat), "_", modname, "_hypergrid"))

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
(gbm_best_file <- paste0("models/07_gbm_final_",tolower(as_name(bmiVar)),"_", tolower(hydroDat), "_", modname, "_", "model_output.txt"))

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
    data = data_por_train # CHECK AND CHANGE!!
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
assign(x = tolower(paste0("gbm_final_", as_name(bmiVar),"_",hydroDat, "_",modname)), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_", tolower(as_name(bmiVar)))))

# save to RDS
write_rds(x = get(fileToSave), path = paste0("models/07_",fileToSave, "_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("models/07_",fileToSave,"_model_data.rda")))
