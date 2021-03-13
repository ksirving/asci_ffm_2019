# 0 Generate Final Selected Sites/Data
## R. Peek
## Look at final output

getwd()
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)
library(sf)
library(glue)
library(mapview)
library(lubridate)
library(tmap)
library(tmaptools)

# Load Data ---------------------------------------------------------------

# FISH REGIONS
ca_sp_regions <- read_sf("input_data/spatial/umbrella_sp_regions.shp", as_tibble = T)

# Selected Sites: with algae data
load("output_data/02c_selected_final_algae_dat_all.rda")

# Streamlines
load("output_data/02b_sel_gage_mainstems_all.rda")

# tidy streams
# get distinct segs that are DS only (either DD or DS)
mainstems_distinct <- mainstems_all %>% 
  filter(from_gage %in% c("DS", "DD")) %>% 
  distinct(nhdplus_comid, .keep_all=TRUE)

# Set up Mapview Basemap --------------------------------------------------

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery","Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")
mapviewOptions(basemaps=basemapsList)

# Make Mapview of Selected Gages and algae Stations ----------------------
#### do this later
# this map of all sites selected U/S and D/S
mapviewOptions(fgb = FALSE)

m3 <- mapview(algae_final_dat, cex=6, col.regions="orange", 
              layer.name="Selected algae asci") +  
  mapview(mainstems_all %>% filter(from_gage=="UM"), color="forestgreen", cex=3, 
          layer.name="NHD Flowlines US") +
  mapview(mainstems_distinct, color="steelblue", cex=3, 
          layer.name="NHD Flowlines DS") +
  mapview(gages_selected_v2, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages") + 
  # these are all algae or gages in same H12 but not selected
  mapview(gages_not_selected_v2, col.regions="slateblue", color="gray20",
          cex=3.2, layer.name="Other USGS Gages") + 
  mapview(algae_not_selected_v2, col.regions="gold", color="gray20", cex=3.2, 
          layer.name="Other algae Sites in H12") + 
  mapview(hucs_selected_v2, col.regions="orange3", alpha.region=0.1, 
          color="orange", legend=F, layer.name="Selected HUC12") +
  mapview(hucs_not_selected_v2, col.regions="dodgerblue", alpha.region=0.1, 
          color="darkblue", legend=F, layer.name="Other HUC12")


m3@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

# save out
#mapshot(m3, url = paste0(here::here(),"/figs/03_map_of_final_algae_asci_sites.html"))


# TMAP MAPS ---------------------------------------------------------------

library(tmap)
library(USAboundaries)
install_data_package()
install.packages("USAboundariesData")
ca<-us_counties(states="ca")
load("input_data/spatial/major_rivers_dissolved.rda")

# crop rivers to CA only and filter to rivers only
rivs_ca <- st_intersection(rivs, ca) %>% 
  filter(FEATURE_TYPE == "river")


# first make CA map with no border
(map_ca <- tm_shape(ca) + tm_polygons(border.alpha = 0.3) +
    tm_shape(rivs_ca) + tm_lines(col="darkblue", lwd = .7, alpha = 0.8) +
    tm_layout(frame=FALSE))

# algae stations TMAP --------------------------------------------------------------

# get ALL bug data (distinct stations)
load("data_output/01_algae_stations_distinct.rda")

# make a tmap
(map_algae <- map_ca +
    tm_shape(algae_stations_distinct) +
    tm_dots(col = "orange", shape = 21, size = 0.1, alpha=0.8) + 
    tm_compass(type = "arrow", size = 2,
               position = c(0.1,0.18)) +
    #position = c(0.15, 0.2)) +
    tm_scale_bar(breaks = c(0, 100, 200), 
                 text.size = 0.6,
                 position = c(0.12, 0.1)) +
    tm_layout(title = glue("algae Sampling\nLocations\n (n={nrow(algae_stations_distinct)})"), 
              legend.show = FALSE, frame = FALSE, title.size = 0.8,
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              fontfamily = "Roboto Condensed", title.position = c(0.65, 0.7)))


# FIRST TRYPTYCH
tmap::tmap_save(tm = map_algae, 
                filename = "figs/03_tmap_algae_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  


# GAGES MAP ---------------------------------------------------------------

# get all possible FFC DV gages
ref_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_ref_gages_list.csv") %>% mutate(CEFF_type="REF", site_id=as.character(site_id)) 
alt_gages <- read_csv("https://raw.githubusercontent.com/ryanpeek/ffm_comparison/main/output/usgs_alt_gages_list.csv") %>% mutate(CEFF_type="ALT")

usgs_gages <- bind_rows(ref_gages, alt_gages) %>% 
  select(-geometry) %>% 
  st_as_sf(coords=c("lon", "lat"), remove=FALSE, crs=4326)

# then add gage stations by ref type
(map_usgs <- map_ca + 
    tm_shape(usgs_gages) +
    tm_symbols(col= "steelblue",  border.col = "black",
               size=0.1, border.alpha = 0.8) +
    # tm_logo(file = "figs/logo_cws_websafe.png", position = c("left","bottom"), height = 5) +
    tm_layout(title = "USGS Sites \n(n=2316)", 
              fontfamily = "Roboto Condensed",title.size = 0.8,
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              title.position = c(0.65, 0.7)))


tmap::tmap_save(tm = map_usgs, 
                filename = "figs/03_tmap_usgs_sites_all.png", width = 8, height = 11, units = "in", dpi = 300)  

# FINAL SITES (SELECTED GAGES ONLY) TMAP -------------------------------------------

# make paired sites
(map_final_sites <- map_ca +
   tm_shape(gages_selected_v2) +
   tm_dots(col="#21908CFF", shape=21, size=0.3, alpha=1) +
   tm_shape(algae_final_dat) +
   tm_dots(col="orange", shape=21, size=0.1, alpha=1) +
   tm_layout(title = "Selected Sites:\nGages (n=226)\nalgae (n=275)",
             title.size = 0.8, frame = FALSE, 
             fontfamily = "Roboto Condensed",
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             title.position = c(0.65, 0.7)))
#tm_compass(type = "4star", position = c("right","top"))+
#tm_scale_bar(position = c("left","bottom"))

tmap::tmap_save(tm=map_final_sites, filename = "figs/03_tmap_selected_paired_sites_w_rivers.png", width = 8, height = 11, units = "in", dpi = 300)  


# Put them all together ---------------------------------------------------

final_triptych<-tmap::tmap_arrange(map_algae, map_usgs, map_final_sites, ncol = 3, outer.margins = 0.001)
print(final_triptych)

tmap::tmap_save(tm = final_triptych, 
                filename = "figs/03_tmap_triptych_algae_usgs_selected.png", width = 11, height = 7, units = "in", dpi = 300)  


# TMAP PALETTE EXPLORER ---------------------------------------------------


# tmaptools::palette_explorer()
# tm_shape(algae_coms_final) +
#   tm_symbols(shape = 21, col = "h12_area_sqkm", n=5, pal="-Greens") #reverse the palette

# View Final Tally --------------------------------------------------------

# how many unique samples are avail: 
algae_final_dat %>% st_drop_geometry %>% distinct(SampleID) %>% tally #n=493 samples
algae_final_dat %>% st_drop_geometry %>% distinct(StationCode) %>% tally #n=275 sites

# how many unique USGS gages? n=226 (ALT=171, REF=55)
algae_final_dat %>% st_drop_geometry %>% distinct(site_id, .keep_all=TRUE) %>% count(CEFF_type)

