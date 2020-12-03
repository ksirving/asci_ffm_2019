# 03 Generate Final Selected Sites/Data
## R. Peek/Katie Irving
## Look at final output

## DATA OUT:
### - sel_h12_bmi (all huc12s with gage/bmi sites inside them, n=53)
### "data_output/03_selected_h12_contain_bmi_gage.rda"

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(lubridate)

# Load Data ---------------------------------------------------------------

# FISH REGIONS
ca_sp_regions <- read_sf("input_data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# asci
load(file="output_data/01_clean_algae.RData") ## algae
sel_algae_gages_asci <- readRDS("output_data/02_selected_algae_h12_all_gages_asci.rds")
sel_algae_gages <- readRDS("output_data/02_selected_algae_h12_all_gages.rds")
sel_gages_algae <- readRDS("output_data/02_selected_usgs_h12_all_gages.rds")
sel_h12_algae <- readRDS("output_data/02_selected_h12_all_gages.rds")
load("output_data/02_selected_nhd_mainstems_gages.rda")

load(file="output_data/02_gages_not_selected.RData") ##gages_not_selected 

load(file="output_data/02_gages_selected.RData") ## gages_selected

load(file="output_data/02_algae_sites_not_selected.RData") ## algae_not_selected

# Clean Data a Bit --------------------------------------------------------

table(sel_gages_algae$CEFF_type) # ALT=137  REF=52
table(gages_not_selected$CEFF_type) # ALT=188  REF=70

# Set up Mapview Basemap --------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Make Mapview of Selected Gages and BMI Stations ----------------------

# this map of all sites selected U/S and D/S
m3 <- mapview(sel_algae_gages, cex=6, col.regions="orange", 
              layer.name="Selected Algae comids") +  
  mapview(mainstems_all, color="steelblue", cex=3, 
          layer.name="NHD Flowlines") +
  mapview(gages_selected, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all bmi or gages in same H12 but not selected
  mapview(gages_not_selected, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(algae_not_selected, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other Algae Sites in H12") + 
  mapview(sel_h12_algae, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="HUC12")

m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


# TMAP MAPS ---------------------------------------------------------------

######## complete when fixed package issue
library(tmap)
library(USAboundaries) ## problem installing
install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")
ca<-us_counties(states="ca")
load("input_data/major_rivers_dissolved.rda")

# crop rivers to CA only and filter to rivers only
rivs_ca <- st_intersection(rivs, ca) %>% 
  filter(FEATURE_TYPE == "river")


# first make CA map with no border
(map_ca <- tm_shape(ca) + tm_polygons(border.alpha = 0.3) +
    tm_shape(rivs_ca) + tm_lines(col="darkblue", lwd = .7, alpha = 0.8) +
    tm_layout(frame=FALSE, o))


# BMI stations TMAP --------------------------------------------------------------

# get ALL bug data (disinct stations)
load("data_output/00_bmi_stations_distinct.rda")

# make a tmap
(map_bmi <- map_ca +
    tm_shape(bmi_stations_distinct) +
    tm_dots(col = "orange", shape = 21, size = 0.1, alpha=0.8) + 
    tm_compass(type = "arrow", size = 2,
               position = c(0.1,0.18)) +
    #position = c(0.15, 0.2)) +
    tm_scale_bar(breaks = c(0, 100, 200), 
                 text.size = 0.6,
                 position = c(0.12, 0.1)) +
    tm_layout(title = "BMI Sampling\nLocations \n (n=2,935)", 
              legend.show = FALSE, frame = FALSE, title.size = 0.8,
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              fontfamily = "Roboto Condensed", title.position = c(0.65, 0.7)))


# FIRST TRYPTYCH
tmap::tmap_save(tm = map_bmi, 
                filename = "figs/04_tmap_bmi_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  



# GAGES MAP ---------------------------------------------------------------

# get all gages list
load("data_output/01_usgs_all_gages.rda")
usgs_final_all <- st_intersection(usgs_final_all, ca)

# then add gage stations by ref type
(map_usgs <- map_ca + 
    tm_shape(usgs_final_all) +
    tm_symbols(col= "steelblue",  border.col = "black",
               size=0.1, border.alpha = 0.8) +
    # tm_logo(file = "figs/logo_cws_websafe.png", position = c("left","bottom"), height = 5) +
    tm_layout(title = "USGS Sites \n(n=799)", 
              fontfamily = "Roboto Condensed",title.size = 0.8,
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              title.position = c(0.65, 0.7)))


tmap::tmap_save(tm = map_usgs, 
                filename = "figs/04_tmap_usgs_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  

# FINAL SITES (SELECTED GAGES ONLY) TMAP -------------------------------------------

# make paired sites
(map_final_sites <- map_ca +
   tm_shape(gages_selected_v2) +
   tm_dots(col="#21908CFF", shape=21, size=0.3, alpha=1) +
   tm_layout(title = "Selected Sites \n(USGS Gages)",
             title.size = 0.8, frame = FALSE, 
             fontfamily = "Roboto Condensed",
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             title.position = c(0.65, 0.7)))
#tm_compass(type = "4star", position = c("right","top"))+
#tm_scale_bar(position = c("left","bottom"))

tmap::tmap_save(tm=map_final_sites, filename = "figs/04_tmap_selected_paired_sites_w_rivers.png", width = 8, height = 11, units = "in", dpi = 300)  


# Put them all together ---------------------------------------------------

final_triptych<-tmap::tmap_arrange(map_bmi, map_usgs, map_final_sites, ncol = 3, outer.margins = 0.001)
print(final_triptych)

tmap::tmap_save(tm = final_triptych, 
                filename = "figs/04_tmap_triptych_bmi_usgs_selected.png", width = 11, height = 7, units = "in", dpi = 300)  


# TMAP PALETTE EXPLORER ---------------------------------------------------


# tmaptools::palette_explorer()
# tm_shape(bmi_coms_final) +
#   tm_symbols(shape = 21, col = "h12_area_sqkm", n=5, pal="-Greens") #reverse the palette

# View Final Tally --------------------------------------------------------

# any NA's?
bmi_coms_dat %>% st_drop_geometry %>% filter(is.na(StationCode)) # nope

# now look at how many unique samples are avail: n=270 unique samples
bmi_coms_dat %>% st_drop_geometry %>% distinct(SampleID) %>% tally
# total distinct stations 270

# how many unique USGS gages? n=160 (ALT=116, REF=44)
bmi_coms_dat %>% st_drop_geometry %>% distinct(ID, .keep_all=TRUE) %>% count(CEFF_type)

