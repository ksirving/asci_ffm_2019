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

# load updated data:
load("output_data/05_algae_asci_por_trim_ecoreg.rda")
# rename for ease of use and drop sf
algae_asci_por_trim <- algae_asci_por_trim_ecoreg %>% st_drop_geometry()

# ecoregions:
unique(algae_asci_por_trim_ecoreg$US_L3_mod)

# mainstem rivers
load("output_data/02b_sel_gage_mainstems_all.rda") # mainstems_all

# set background basemaps/default options:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(homebutton = FALSE, basemaps=basemapsList, viewer.suppress = FALSE)


# 02. Select a Region ---------------------------------------------------------

# make a simpler layer for mapping
algae_asci_sites <- algae_asci_por_trim %>% 
  dplyr::distinct(StationCode, .keep_all = TRUE)
table(algae_asci_sites$US_L3_mod) # list of unique stations

(ecoregs <- unique(algae_asci_por_trim_ecoreg$US_L3_mod))

# if selecting by a specific region use region select
table(algae_asci_por_trim$US_L3_mod)
modname <- "so_cal"   # "so_cal" 
(Hregions <- c(ecoregs[1])) # set a region or regions

# now filter data to region(s) of interest
region_sel <- algae_asci_por_trim_ecoreg %>% filter(US_L3_mod %in% Hregions)

mapview(region_sel, zcol="US_L3_mod")

# 03. Select algae Response Variable for GBM ------------------------------

# get metrics
algae.metrics<-c("H_ASCI.x")
hydroDat <- "POR" # can be Annual, Lag1, Lag2, POR
algaeVar <- quote(H_ASCI.x) # select response var from list above

# 04. Setup POR Data for Model ----------------------------------------------------------------

# filter out indeterminate and not_enough_data
data_por <- region_sel %>% st_drop_geometry() #%>% 
#filter(status_code %in% c(-1, 1))

# need to select and spread data: 
data_por <- data_por %>% 
  dplyr::select(StationCode, SampleID, HUC_12, site_id, 
                comid_ffc, COMID_algae, CEFF_type,
                YYYY, H_ASCI.x,
                metric, status_code
  ) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = status_code) %>% 
  mutate(CEFF_type = as.factor(CEFF_type)) %>% 
  as.data.frame()

# check how many rows/cols: KEEP ALL FOR NOW
library(naniar)
dim(data_por)
gg_miss_var(data_por)

# show how many missing combos:
library(VIM)
summary(aggr(data_por, sortVar=TRUE))$combinations

# dim(data_por)
data_names <- names(data_por) # save colnames out

# remove cols that have more than 70% NA
dim(data_por) # before
dim(data_por[, which(colMeans(!is.na(data_por)) > 0.7)]) # after
#data_por <- data_por[, which(colMeans(!is.na(data_por)) > 0.7)]

# find the cols that have been dropped
#setdiff(data_names, names(data_por))

# remove rows that have more than 70% NA
dim(data_por)
dim(data_por[which(rowMeans(!is.na(data_por))>0.7),])
data_por <- data_por[which(rowMeans(!is.na(data_por))>0.7),]

# 05. Split Train/Test Data -------------------------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_por), nrow(data_por))
data_por <- data_por[random_index, ]

## USE ALL DATA
data_por_train <- data_por %>% # use all data
  dplyr::select({{algaeVar}}, 10:ncol(.)) %>%  # use 12 if not including HUC region and CEFF_type
  dplyr::filter(!is.na({{algaeVar}})) %>% as.data.frame()


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
    n.trees = 100,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE)
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  val <- if(!is.null(m_step$self.statistics$mean.null)){
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) / m_step$self.statistics$mean.null}
  else(NA_real_)
  
  return(val)
}

# 07. RUN GBM.STEP WITH PURRR ---------------------------------------------
# possibly(.f = gbm_fit_step, otherwise = NA_real_)

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <- purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data_por_train))

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
(gbm_file <- paste0("models/07_gbm_final_",tolower(as_name(algaeVar)),"_", tolower(hydroDat), "_", modname, "_hypergrid"))

# check for file and delete?
if(fs::file_exists(path = paste0(gbm_file,".csv"))){
  fs::file_delete(path = paste0(gbm_file, ".csv"))
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# save out
write_csv(hyper_grid, file = paste0(gbm_file,".csv"), append = TRUE)

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
(gbm_best_file <- paste0("models/07_gbm_final_",tolower(as_name(algaeVar)),"_", tolower(hydroDat), "_", modname, "_", "model_output.txt"))

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
write_tsv(hyper_best, file = gbm_best_file,
          col_names = TRUE, append=TRUE)

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 

# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_", as_name(algaeVar),"_",hydroDat, "_",modname)), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_", tolower(as_name(algaeVar)))))

# save to RDS
write_rds(x = get(fileToSave), file = paste0("models/07_",fileToSave, "_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("models/07_",fileToSave,"_model_data.rda")))

