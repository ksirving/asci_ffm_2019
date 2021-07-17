# Running GBM (Boosted Regression Trees) with PURRR
# R. Peek 2020
# Generates models to select "final" model based on tuning criteria
# then makes final model, saved in models/

# Libraries ---------------------------------------------------------------

library(purrr) # for working with lists/loops
library(glue)
#library(tidylog)
#options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(viridis) # colors
library(sf) # spatial operations
library(mapview) # web maps
library(gbm) # boosted regression trees
library(rsample) # sampling
library(rlang)
library(dismo)
library(tidylog)
source("code/functions/My.gbm.step.R")

set.seed(321) # reproducibility

# 01. Load Data ---------------------------------------------------------------

# load updated data:
asci_ffm<- read_rds("output_data/06_asci_por_trim_final_dataset.rds")

# ecoregions:
unique(asci_ffm$US_L3_mod)

# set background basemaps/default options:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron")

mapviewOptions(homebutton = FALSE, 
               basemaps=basemapsList, 
               viewer.suppress = FALSE,
               fgb = FALSE)

# 02. Select algae Response Variable for GBM ------------------------------

# get metrics
modname <- "all_ca_ffc_only"   # or "all_ca_ffc_only"
Hregions <- c(modname) # set a region or regions

algae.metrics<-c("H_ASCI") # response var for model
hydroDat <- "POR" # dataset, can be Annual, Lag1, Lag2, POR
algaeVar <- quote(H_ASCI) # select response var from list above

# 03. Setup POR Data for Model ----------------------------------------------------------------

# summarize
#asci_ffm %>% group_by(metric, alteration_type) %>% tally() %>% 
asci_ffm %>% st_drop_geometry() %>% 
  distinct(StationCode, metric, refgage, .keep_all = TRUE) %>% 
  select(StationCode, refgage, metric, delta_p50, H_ASCI) %>% 
  group_by(metric, refgage) %>% add_tally() %>% 
  #View()
  distinct(refgage, n, .keep_all=TRUE) %>% 
  ggplot() + geom_col(aes(x=metric, y=n, fill=refgage))

# need to select and spread data: 
data_por <- asci_ffm %>% st_drop_geometry() %>% 
  dplyr::select(StationCode, SampleID, HUC_12, site_id, 
                comid, COMID_algae, refgage,
                YYYY, H_ASCI,
                metric, delta_p50
  ) %>% 
  # need to spread the metrics wide
  pivot_wider(names_from = metric, values_from = delta_p50) %>% 
  mutate(refgage = as.factor(refgage)) %>% 
  # just non-ref sites
  filter(refgage=="Non-Ref") %>% 
  as.data.frame()
# n=521 obs

# check how many rows/cols: KEEP ALL FOR NOW
dim(data_por)
data_names <- names(data_por) # save colnames out

# remove cols that have more than 70% NA
data_por <- data_por[, which(colMeans(!is.na(data_por)) > 0.7)]
dim(data_por)

# find the cols that have been dropped
setdiff(data_names, names(data_por))
# remove rows that have more than 70% NA
data_por <- data_por[which(rowMeans(!is.na(data_por))>0.7),]
dim(data_por) # n=292

# 04. RANDOMIZE AND TIDY Data ----------------------------------------------------

# make sure data is randomized:
random_index <- sample(1:nrow(data_por), nrow(data_por))
data_por <- data_por[random_index, ]

## USE ALL DATA
data_por_train <- data_por %>% # use all data
  dplyr::select({{algaeVar}}, 10:ncol(.)) %>%  
  # use 12 if not including HUC region and CEFF_type
  dplyr::filter(!is.na({{algaeVar}})) %>% as.data.frame()

# double check cols are what we want
names(data_por_train) # should be 25 (asci + 24 metrics)

# 05. GBM.STEP MODEL  ------------------------------------------------------------

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
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# 06. RUN GBM.STEP WITH PURRR ---------------------------------------------

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

# 07. VIEW AND SAVE MODEL RESULTS -----------------------------------------

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# write these all out to a file for reference later
(gbm_file <- glue("models/07_gbm_final_{tolower(as_name(algaeVar))}_{tolower(hydroDat)}_{modname}_hypergrid"))

# check for file and delete?
if(fs::file_exists(path = paste0(gbm_file,".csv"))){
  fs::file_delete(path = paste0(gbm_file, ".csv"))
  print("File deleted, time for a fresh start!")
} else {
  print("No file saved yet")
}

# save out
write_csv(hyper_grid, file = glue("{gbm_file}.csv"), append = TRUE)

# read in
hyper_grid <- readr::read_csv(file = paste0(gbm_file,".csv"), col_names = c("shrinkage", "interaction.depth", "n.minobsinnode", "bag.fraction", "dev_explained"))

# get best model solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# 08. RUN BEST BRT MODEL ---------------------------------------------------------------

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
(gbm_best_file <- glue("models/07_gbm_final_{tolower(as_name(algaeVar))}_{tolower(hydroDat)}_{modname}_model_output.txt"))

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
assign(x = tolower(glue("gbm_final_{as_name(algaeVar)}_{hydroDat}_{modname}")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = glue("gbm_final_{tolower(as_name(algaeVar))}")))

# save to RDS
write_rds(x = get(fileToSave), file = glue("models/07_{fileToSave}_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("models/07_",fileToSave,"_model_data.rda")))
