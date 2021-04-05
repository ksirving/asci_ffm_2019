
# Libraries ---------------------------------------------------------------

library(sf)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(conflicted) # deals with conflicting functions
conflict_prefer("filter", "dplyr")
library(janitor)
library(mapview)
mapviewOptions(fgb = FALSE)

# Get Spatial Data --------------------------------------------------------

eco_revised <- read_rds("input_data/spatial/ecoregions_combined_L3.rds")
ca <- USAboundaries::us_states(states="california")

# Sites -------------------------------------------------------------------

algae_asci_por_trim <- read_rds("output_data/04_selected_asci_ffm_por_trim.rds")
length(unique(algae_asci_por_trim$site_id)) #Gages (n=199)
length(unique(algae_asci_por_trim$StationCode)) # algae Stations (n=242)

# all data: don't really need this
load("output_data/02c_selected_final_algae_dat_all.rda")

# Join Ecoregions to algae sites --------------------------------------------

# get unique algae sites only
algae_sf <- algae_final_dat %>% distinct(StationCode, .keep_all = TRUE) %>% st_transform(4326)
algae_sf <- st_join(algae_sf, left = TRUE, eco_revised["US_L3_mod"])
# st_crs(eco_revised)
# summarize algae stations by region
table(algae_sf$US_L3_mod)

mapview(algae_sf, zcol="US_L3_mod", layer.name="algae Sites") + mapview(eco_revised, zcol="US_L3_mod", cex=0.2, layer.name="Ecoregions")


# Add Ecoregions to algae FFC Status Data -----------------------------------

algae_asci_por_trim_sf <- st_as_sf(algae_asci_por_trim, coords=c("lon", "lat"), crs=4326, remove=F)
algae_asci_por_trim_ecoreg <- st_join(algae_asci_por_trim_sf, left = FALSE, eco_revised["US_L3_mod"])

table(algae_asci_por_trim_ecoreg$US_L3_mod)

save(algae_asci_por_trim_ecoreg, file="output_data/05_algae_asci_por_trim_ecoreg.rda")

summary(algae_asci_por_trim_ecoreg)

algae_asci_por_trim_ecoreg %>% st_drop_geometry() %>% group_by(metric) %>% tally(status_code)
