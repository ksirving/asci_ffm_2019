# 12_glm_model_top_metrics


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(viridis)
library(sf)
library(glue)
library(purrr)
library(rlang)
library(ggthemes)
library(tidylog)
library(cowplot)

# Load Data ---------------------------------------------------------------

# load updated data w regions:
asci_ffm_sf<- read_rds("output_data/06_asci_por_trim_final_dataset.rds")

load("models/09_h_asci_por_all_ri_all_regions.rda")

# rename for ease of use and drop sf
asci_ffm <- asci_ffm_sf %>% st_drop_geometry()


### not sure where this comes from
# add specific ASCI vs CSCI col
asci_ri_all <- asci_ri_all %>% mutate(index="H_ASCI")
csci_ri_all <- csci_ri_all %>% mutate(index="CSCI")

# combine
ri_all <- bind_rows(asci_ri_all, csci_ri_all) %>% 
  # replace all H_ASCI.x with "asci"
  mutate(Ymetric = case_when(
    grepl("H_ASCI.x", Ymetric) ~ "asci",
    TRUE ~ Ymetric
  ))

# get names
library(readxl)
ff_defs <- readxl::read_xlsx("/Users/katieirving/Documents/git/bmi_ffm_links/docs/Functional_Flow_Metrics_List_and_Definitions_final.xlsx", range = "A1:F25", .name_repair = "universal", trim_ws = TRUE) 

# load FFC Metrics Data
ffm_dat <- read_rds("/Users/katieirving/Documents/git/bmi_ffm_links/data_output/usgs_combined_ffc_results_long.rds")
head(ffm_dat)
# get how many unique years for each gage exist
ffm_dat %>% group_by(gageid, year) %>% distinct(year) %>% 
  group_by(gageid) %>% mutate(yr_begin = first(year),
                              yr_end = last(year)) %>% 
  arrange(gageid, year) %>%
  add_tally() %>% 
  distinct(gageid, .keep_all=TRUE) %>% select(-year) #%>% View()


# Tidy RI Data ---------------------------------------------------------------

# join with the full RI table
ri_table <- left_join(ri_all, ff_defs, by=c("var"="Flow.Metric.Code"))

asci_ffm <- left_join(asci_ffm, ff_defs, by=c("metric"="Flow.Metric.Code"))


# drop unused factors in flow component:
ri_table <- ri_table %>% 
  filter(method=="mse") %>% 
  mutate(flow_component=forcats::fct_drop(flow_component),
         var = as.factor(var),
         var = fct_reorder2(var, flow_component, var),
         model=as.factor(model),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI))

# JOIN CSCI WITH FFM ANNUAL -------------------------------

csci_ffm_ann <- csci_por_trim %>% st_drop_geometry() %>% 
  distinct(StationCode, SampleID, YYYY, sampledate, site_id, .keep_all=TRUE) %>% 
  left_join(., ffm_dat, by=c("site_id"="gageid", "YYYY"="year")) %>% 
  # drop cols and get distinct records
  select(StationCode, SampleID, US_L3_mod, HUC_12, site_id, station_nm, lat, lon, date_begin, date_end, comid_gage, sampledate, YYYY, MM, DD, csci, csci_percentile, COMID_bmi, ffm_name, value)

## Check Records by Year --------------------------------

# calc records by SampleID (how many mult years)
csci_ffm_ann %>% select(SampleID, site_id, YYYY) %>% 
  distinct(.keep_all=TRUE) %>% 
  group_by(SampleID) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>1)

# JOIN ASCI WITH FFF ANNUAL ------------------------------

asci_ffm_ann <- asci_por_trim %>% st_drop_geometry() %>% 
  select(-c(ends_with(".y"), Latitude:median_in_iqr)) %>% mutate(YYYY = as.numeric(YYYY), MM=as.integer(MM), DD = as.integer(DD)) %>% 
  distinct(StationCode, SampleID, YYYY, SampleDate, site_id, .keep_all=TRUE) %>% 
  left_join(., ffm_dat, by=c("site_id"="gageid", "YYYY"="year")) %>% 
  # drop cols and get distinct records
  select(StationCode, SampleID, US_L3_mod, HUC_12, site_id, station_nm, lat, lon, date_begin, date_end, comid_gage, sampledate=SampleDate, YYYY, MM, DD, asci=H_ASCI.x, COMID_algae, ffm_name, value) 

# compare cols
janitor::compare_df_cols(csci_ffm_ann, asci_ffm_ann)
janitor::compare_df_cols_same(csci_ffm_ann, asci_ffm_ann)

## Check Records by Year --------------------------------

# calc records by SampleID (how many mult years)
asci_ffm_ann %>% select(SampleID, site_id, YYYY) %>% 
  distinct(.keep_all=TRUE) %>% 
  group_by(SampleID) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(n>1)

# JOIN LAG (archive for now) --------------------------------

## JOIN LAG 
# # years -1  -2
# lag_yrs_1 <- unique(bmi_sampleid$YYYY) - 1
# lag_yrs_2 <- unique(bmi_sampleid$YYYY) - 2
# 
# # make lag data
# ffm_lag1 <- ffm %>% filter(Year %in% lag_yrs_1) %>% 
#   mutate(year_flow = Year-1) # add for labeling purposes
# ffm_lag2 <- ffm %>% filter(Year %in% lag_yrs_2) %>% 
#   mutate(year_flow = Year-2) # add for labeling purposes
# 
# # rejoin
# bmi_ffm_lag1 <- left_join(bmi_sampleid, ffm_lag1, by=c("gage_id_c"="gage_id", "YYYY"="Year"))
# bmi_ffm_lag2 <- left_join(bmi_sampleid, ffm_lag2, by=c("gage_id_c"="gage_id", "YYYY"="Year"))



# SET UP NAMES and Condition Thresholds -----------------------

# biological stream condition thresholds (Mazor et al. 2016, Theroux et al. 2020 - See Table 8)
csci_breaks <- c(0, 0.63, 0.79, 0.92)
asci_breaks <- c(0, 0.75, 0.86, 0.94)
bio_labs <- c("Very likely altered", "Likely altered", "Possibly altered","Likely intact")

#facet_grid(cols = vars(model), labeller = labeller(model=c("all_ca"="All CA", "cent_coast"="C. Coast", "sierras"="Sierra Nevada", "north_coast"="N. Coast", "so_cal"="S. California"))) +

# CSCI: DS_Mag_50 ----------------------------------------

# set up variables
metselect <- "SP_ROC" # metric 
# SP_Mag, Wet_BFL_Mag_50, DS_Mag_50, SP_ROC
region <- "All CA" # region

biovar <- "CSCI" # data

# set title
(cust_title <- glue("{biovar} Annual ({metselect}): {region}"))

# data 
plotdat <- csci_ffm_ann %>% 
  filter(ffm_name==metselect, value>0) 

# summary
summary(plotdat$value)

# Trendline info:
# for gam use: method = "gam", formula = y ~ s(x, bs = "cs").
# for loess use method = "loess", span=1.5, (default span = 0.75), width of the moving window, or
# proportion of points in plot which influence the smooth at each value.
# each of the local regressions used to produce that curve incorporate x% of total data 
# (default is 75%)

(gg1a <- 
    ggplot() +
    # color the biological condition thresholds (Mazor et al 2016)
    # geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0, ymax=0.63), fill="maroon", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0.63, ymax=0.79), fill="orange2", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=0.5, ymin=0.79, ymax=0.92), fill="yellow2", alpha=0.2)+
    # geom_rect(aes(xmin=0.01,xmax=0.5, ymin=0.92, ymax=1.35), fill="seagreen", alpha=0.2)+
    
    # add line thresholds
    annotate(geom = "text", label="Very likely altered", color="gray50", 
             x=0.11, y=0.58, hjust=0, size=4) +
    annotate(geom = "text", label="Likely altered", color="gray50", 
             x=0.11, y=0.71, hjust=0, size=4) +
    annotate(geom = "text", label="Possibly altered", color="gray50", 
             x=0.11, y=0.85, hjust=0, size=4) +
    annotate(geom = "text", label="Likely intact", color="gray50", 
             x=0.11, y=1, hjust=0, size=4) +
    
    # data points
    geom_point(data=plotdat, aes(x=value, y=csci), fill="gray10", pch=21, size=2.5, alpha=0.85, show.legend = FALSE) +
    
    # loess smooth
    stat_smooth(data=plotdat, aes(x=value, y=csci), method = "loess", span=.9, lty=2, color="maroon",se = TRUE, fill="skyblue", show.legend = F)+
    
    # gam smooth
    stat_smooth(data=plotdat, aes(x=value, y=csci), method = "gam", formula = y ~ s(x, bs = "cs"), color="gray40", fill="gray80", show.legend = FALSE, span = 0.9)+
    
    # all the other stuff
    scale_y_continuous(breaks=csci_breaks, limits=c(0, 1.3))+
    scale_x_log10(labels=scales::comma, expand=c(0.01,0.01), 
                  limits=c(0.1, 1)) +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y=glue("{biovar} Score"), 
         x=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         subtitle = cust_title))

#ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.png"), width = 11, height = 7, dpi=300, units="in")
#ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# ASCI: DS_Mag_50 ----------------------------------------

# set up variables
biovar <- "ASCI" # data

# set title
(cust_title <- glue("{biovar} Annual ({metselect}): {region}"))

# data 
plotdatA <- asci_ffm_ann %>% 
  filter(ffm_name==metselect, value>0) 

# summary
summary(plotdat$value)

# 
(gg1b <- 
    ggplot() +
    # these are colored boxes for thresholds
    geom_rect(aes(xmin=0.1, xmax=max(plotdatA$value), ymin=0, ymax=0.75), fill="maroon", alpha=0.2) +
    geom_rect(aes(xmin=0.1, xmax=max(plotdatA$value), ymin=0.75, ymax=0.86), fill="orange2", alpha=0.2) +
    geom_rect(aes(xmin=0.1, xmax=max(plotdatA$value), ymin=0.86, ymax=0.94), fill="yellow2", alpha=0.2)+
    geom_rect(aes(xmin=0.1,xmax=max(plotdatA$value), ymin=0.94, ymax=1.3), fill="seagreen", alpha=0.2)+
    # based on Theroux et al, Table 8
    annotate(geom = "text", label="Very likely altered", color="gray50",
             x=0.11, y=0.25, hjust=0, size=4) +
    annotate(geom = "text", label="Likely altered", color="gray50",
             x=0.11, y=0.8, hjust=0, size=4) +
    annotate(geom = "text", label="Possibly altered", color="gray50",
             x=0.11, y=0.9, hjust=0, size=4) +
    annotate(geom = "text", label="Likely intact", color="gray50",
             x=0.11, y=1.1, hjust=0, size=4) +
    
    # data points
    geom_point(data=plotdatA, aes(x=value, y=asci), fill="gray10", pch=21, size=2.5, alpha=0.85, show.legend = FALSE) +
    
    # the smooth line
    stat_smooth(data=plotdatA, aes(x=value, y=asci), method = "gam", formula = y ~ s(x, bs = "cs"), color="gray40", fill="gray80", show.legend = FALSE, span = 0.9)+
    # using Table 8 from Theroux et al 2020
    scale_y_continuous(breaks=c(0, 0.75, 0.86, 0.94), limits=c(0, 1.3))+
    scale_x_log10(labels=scales::comma, expand=c(0.01,0.01), limits=c(0.1,max(plotdatA$value))) + 
    # theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y=glue("{biovar} Score"), 
         x=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         subtitle = cust_title))

#ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.png"), width = 11, height = 7, dpi=300, units="in")
#ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# CSCI: SP_ROC: ecoreg facet -------------------------

# set up variables
metselect <- "SP_ROC" # metric 
# SP_Mag, Wet_BFL_Mag_50, DS_Mag_50, SP_ROC
region <- "All CA" # region
biovar <- "CSCI" # data

# set title
(cust_title <- glue("{biovar} Annual ({metselect}): {region}"))

# data 
plotdat_gg1c <- csci_ffm_ann %>% 
  filter(ffm_name==metselect, value>0) 

# FACETED
(gg1c_faceted <- 
    ggplot() +
    annotate(geom = "text", label="Very likely altered", color="gray50", x=0.11, y=0.57, size=3.5, hjust=0) +
    annotate(geom = "text", label="Likely altered", color="gray50", x=0.11, y=0.71, size=3.5,  hjust=0) +
    annotate(geom = "text", label="Possibly altered", color="gray50", x=0.11, y=0.85, size=3.5,  hjust=0) +
    annotate(geom = "text", label="Likely intact", color="gray50", x=0.11, y=1, size=3.5,  hjust=0) +
    
    # for faceted
    geom_point(data=plotdat_gg1c %>% 
                 filter(!US_L3_mod %in% c("Central California Valley",
                                          "Mojave/Sonoran Desert", "North Coast")),
               aes(x=value, y=csci, group=US_L3_mod, shape=US_L3_mod, color=US_L3_mod),
               size=3, alpha=0.85, show.legend = FALSE) +
    
    # add smooth
    stat_smooth(data=plotdat_gg1c %>% 
                  filter(!US_L3_mod %in% c("Central California Valley",
                                           "Mojave/Sonoran Desert", "North Coast")),
                aes(x=value, y=csci, group=US_L3_mod, color=US_L3_mod),
                method = "gam", formula = y ~ s(x, bs = "cs"), 
                #method = "loess", span = 0.97, 
                show.legend = F, se = FALSE) +
    
    facet_wrap(.~US_L3_mod) +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    
    # all the other stuff
    scale_color_colorblind("EcoRegion") +
    scale_shape_discrete("EcoRegion") +
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(expand=c(0.01,0.01), limits=c(0.01, 0.5)) +
    # theme_clean(base_family = "Roboto Condensed") +
    labs(y="CSCI Score", 
         x=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)])) 
)

## save
ggsave(glue("figs/12_ffm_vs_top_ri_{tolower(biovar)}_{tolower(metselect)}_por_gam_faceted_ecoregion.png"), width = 11, height = 7, dpi=300, units="in")
# for pdf, add: device = cairo_pdf

# ASCI: SP_ROC: ecoreg facet -------------------------

# set up variables
metselect <- "SP_ROC" # metric 
# SP_Mag, Wet_BFL_Mag_50, DS_Mag_50, SP_ROC
region <- "All CA" # region

biovar <- "ASCI" # data

# set title
(cust_title <- glue("{biovar} Annual ({metselect}): {region}"))

# data 
plotdat_gg1d <- asci_ffm_ann %>% 
  filter(ffm_name==metselect, value>0) 


# FACETED
(gg1d_faceted <- 
    ggplot() +
    
    # for faceted
    geom_point(data=plotdat_gg1d %>% 
                 filter(!US_L3_mod %in% c("Central California Valley",
                                          "Mojave/Sonoran Desert", "North Coast")),
               aes(x=value, y=asci, group=US_L3_mod, shape=US_L3_mod, color=US_L3_mod),
               size=3, alpha=0.85, show.legend = FALSE) +
    
    # add smooth
    stat_smooth(data=plotdat_gg1d %>% 
                  filter(!US_L3_mod %in% c("Central California Valley",
                                           "Mojave/Sonoran Desert", "North Coast")),
                aes(x=value, y=asci, group=US_L3_mod, color=US_L3_mod),
                method = "gam", formula = y ~ s(x, bs = "cs"), 
                #method = "loess", span = 0.97, 
                show.legend = F, se = FALSE) +
    
    facet_wrap(.~US_L3_mod) +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    
    # all the other stuff
    scale_color_colorblind("EcoRegion") +
    scale_shape_discrete("EcoRegion") +
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(expand=c(0.01,0.01), limits=c(0.01, 0.5)) +
    # theme_clean(base_family = "Roboto Condensed") +
    labs(y="ASCI Score", 
         x=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)])) 
)


# save
ggsave(glue("figs/12_ffm_vs_top_ri_{tolower(biovar)}_{tolower(metselect)}_por_gam_faceted_ecoregion.png"), width = 11, height = 7, dpi=300, units="in")

# combine plots
# library(patchwork)
# gg1c_faceted + gg1d_faceted
library(cowplot)
plot_grid(gg1c_faceted, gg1d_faceted)

ggsave(glue("figs/12_ffm_combined_{tolower(metselect)}_por_gam_faceted_ecoregion.png"), width = 11, height = 7, dpi=300, units="in")


# SP_ROC: HUC REGIONS UNFACETED -------------------------------------------

(gg1b_unfaceted <- 
   ggplot() +
   # geom_rect(data = data.frame(xmin=0.01, xmax=0.5, ymin=0, ymax=0.63),
   #           aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), fill="maroon", alpha=0.2) +
   # geom_rect(data = data.frame(xmin=0.01, xmax=0.5, ymin=0.63, ymax=0.79), 
   #           aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="orange2", alpha=0.2) +
   # geom_rect(data = data.frame(xmin=0.01, xmax=0.5, ymin=0.79, ymax=0.92),
   #           aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="yellow2", alpha=0.2) +
   # geom_rect(data = data.frame(xmin=0.01,xmax=0.5, ymin=0.92, ymax=1.35), 
   #           aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="seagreen", alpha=0.2)+
   annotate(geom = "text", label="Very likely altered", color="gray50", x=0.011, y=0.57, size=3.5, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", x=0.011, y=0.71, size=3.5,  hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", x=0.011, y=0.85, size=3.5,  hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", x=0.011, y=1, size=3.5,  hjust=0) +
   
   # for all points (unfaceted)
   geom_point(data=plotdat , aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region),
              size=2.5, alpha=0.7, show.legend = FALSE) + # switch to size 3 and 0.85 for all
   stat_smooth(data=plotdat %>% filter(huc_region!="great_basin"),
               aes(x=p50, y=csci, group=huc_region, color=huc_region), method = "gam",
               formula = y ~ s(x, bs = "cs"), show.legend = T, se = FALSE) +
   
   # all the other stuff
   scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(expand=c(0.01,0.01), limits=c(0.01, 0.5)) +
   # theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank(),
         legend.position = c(0.88,0.88)) +
   
   labs(y="CSCI Score", 
        x=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# DS_MAG_90: ALL SITES -----------------------------------------------------------------

# select metric
metselect <- "DS_Mag_90"

# data 
plotdat <- bmi_ffm_por %>% 
  filter(model=="all_ca", 
         !is.na(huc_region),
         result_type=="observed",
         metric==metselect) %>% 
  st_drop_geometry() %>% 
  select(StationCode,gage_id, list_id, NHDV2_COMID, comid_bmi, csci, csci_percentile, huc_region,gage_id_c, metric:status_code, p10:p90, result_type:Flow.Metric.Name) %>% 
  distinct()

# PLOT
(gg2a <- 
    ggplot() +
    # add the CSCI biological/stream condition thresholds (Mazor et al 2016)
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0, ymax=0.63), fill="maroon", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.63, ymax=0.79), fill="orange2", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.79, ymax=0.92), fill="yellow2", alpha=0.2)+
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.92, ymax=1.35), fill="seagreen", alpha=0.2)+
    annotate(geom = "text", label="Very likely altered", color="gray50", 
             x=0.02, y=0.55, size=4.5, hjust=0) +
    annotate(geom = "text", label="Likely altered", color="gray50", 
             x=0.02, y=0.72,  size=4.5, hjust=0) +
    annotate(geom = "text", label="Possibly altered", color="gray50", 
             x=0.02, y=0.86,  size=4.5, hjust=0) +
    annotate(geom = "text", label="Likely intact", color="gray50", 
             x=0.02, y=1,  size=4.5, hjust=0) +
    
    # data points
    geom_point(data=plotdat, aes(x=p50, y=csci), fill="gray10", pch=21, size=2.5, alpha=0.85, show.legend = FALSE) +
    # gam smooth
    stat_smooth(data=plotdat, aes(x=p50, y=csci), method = "gam", formula = y ~ s(x, bs = "cs"), color="gray40", fill="gray80", show.legend = F)+
    # all the other stuff
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                  labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
    scale_color_colorblind("HUC Region")+
    # theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y="CSCI Score", 
         x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)

# DS_MAG_90: HUC REGIONS FACETED --------------------------------------------------------

# PLOT
(gg2b_faceted <- 
   ggplot() +
   annotate(geom = "text", label="Very likely altered", color="gray50", 
            x = 0.02, y=0.55, size=4, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", 
            x=0.02, y=0.72,  size=4, hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", 
            x=0.02, y=0.86,  size=4, hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", 
            x=0.02, y=1,  size=4, hjust=0) +
   
   # data points w no fill
   geom_point(data=plotdat, aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region),
              size=3, alpha=0.85, show.legend = FALSE) +
   
   # gam smooth
   stat_smooth(data=plotdat, aes(x=p50, y=csci, color=huc_region), method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = FALSE, se = FALSE ) +
   
   facet_wrap(huc_region~., labeller = labeller(huc_region=c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))) +
   
   # all the other stuff
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                 labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
   scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   # theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank())+
   labs(y="CSCI Score", 
        x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# DS_MAG_90: HUC REGIONS UNFACETED ------------------------------------------------------

# PLOT
(gg2b_unfaceted <- 
   ggplot() +
   annotate(geom = "text", label="Very likely altered", color="gray50", 
            x = 0.02, y=0.55, size=4, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", 
            x=0.02, y=0.72,  size=4, hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", 
            x=0.02, y=0.86,  size=4, hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", 
            x=0.02, y=1,  size=4, hjust=0) +
   
   # data points w no fill
   geom_point(data=plotdat, aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region),
              size=2.5, alpha=0.7, show.legend = FALSE) +
   
   # gam smooth
   stat_smooth(data=plotdat, aes(x=p50, y=csci, color=huc_region), method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = TRUE, se = FALSE ) +
   
   # all the other stuff
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                 labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
   scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   # theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank(),
         legend.position = c(0.88,0.88)) +
   labs(y="CSCI Score", 
        x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)


# WET_BFL_MAG_50: ALL SITES -----------------------------------------------------------------

# select metric
metselect <- "Wet_BFL_Mag_50"

# data 
plotdat <- bmi_ffm_por %>% 
  filter(model=="all_ca", 
         !is.na(huc_region),
         result_type=="observed",
         metric==metselect) %>% 
  st_drop_geometry() %>% 
  select(StationCode,gage_id, list_id, NHDV2_COMID, comid_bmi, csci, csci_percentile, huc_region,gage_id_c, metric:status_code, p10:p90, result_type:Flow.Metric.Name) %>% 
  distinct()

# PLOT
(gg3a <- 
    ggplot() +
    
    # add the CSCI biological/stream condition thresholds (Mazor et al 2016)
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0, ymax=0.63), fill="maroon", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.63, ymax=0.79), fill="orange2", alpha=0.2) +
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.79, ymax=0.92), fill="yellow2", alpha=0.2)+
    # geom_rect(aes(xmin=0.01, xmax=5500, ymin=0.92, ymax=1.35), fill="seagreen", alpha=0.2)+
    annotate(geom = "text", label="Very likely altered", color="gray50", 
             x=0.02, y=0.55, size=4, hjust=0) +
    annotate(geom = "text", label="Likely altered", color="gray50", 
             x=0.02, y=0.72, size=4, hjust=0) +
    annotate(geom = "text", label="Possibly altered", color="gray50", 
             x=0.02, y=0.86, size=4, hjust=0) +
    annotate(geom = "text", label="Likely intact", color="gray50", 
             x=0.02, y=1, size=4, hjust=0) +
    
    # data points
    geom_point(data=plotdat, aes(x=p50, y=csci), fill="gray10", pch=21, size=2.5, alpha=0.85, show.legend = FALSE) +
    
    # gam smooth
    stat_smooth(data=plotdat, aes(x=p50, y=csci), method = "gam", formula = y ~ s(x, bs = "cs"), color="gray40", fill="gray80", show.legend = F)+
    # all the other stuff
    scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
    scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                  labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
    scale_color_colorblind("HUC Region")+
    # theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y="CSCI Score", 
         x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
         title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
         subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)

# DS_MAG_90: HUC REGIONS FACETED --------------------------------------------------------

# PLOT
(gg3b_faceted <- 
   ggplot() +
   
   annotate(geom = "text", label="Very likely altered", color="gray50", 
            x=0.02, y=0.55, size=4, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", 
            x=0.02, y=0.72, size=4, hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", 
            x=0.02, y=0.86, size=4, hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", 
            x=0.02, y=1, size=4, hjust=0) +
   
   # data points
   geom_point(data=plotdat, aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region), size=3, alpha=0.8, show.legend = FALSE) +
   
   # gam smooth
   stat_smooth(data=plotdat, aes(x=p50, y=csci, color=huc_region), method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = FALSE, se = FALSE ) +
   
   facet_wrap(huc_region~., labeller = labeller(huc_region=c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))) +
   
   # all the other stuff
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                 labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
   scale_color_colorblind("HUC Region")+
   # theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank()) +
   labs(y="CSCI Score", 
        x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_faceted_by_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)

# DS_MAG_90: HUC REGIONS UNFACETED --------------------------------------------------------

# PLOT
(gg3b_unfaceted <- 
   ggplot() +
   
   annotate(geom = "text", label="Very likely altered", color="gray50", 
            x=0.02, y=0.55, size=4, hjust=0) +
   annotate(geom = "text", label="Likely altered", color="gray50", 
            x=0.02, y=0.72, size=4, hjust=0) +
   annotate(geom = "text", label="Possibly altered", color="gray50", 
            x=0.02, y=0.86, size=4, hjust=0) +
   annotate(geom = "text", label="Likely intact", color="gray50", 
            x=0.02, y=1, size=4, hjust=0) +
   
   # data points
   geom_point(data=plotdat, aes(x=p50, y=csci, group=huc_region, shape=huc_region, color=huc_region), size=2.5, alpha=0.7, show.legend = TRUE) +
   
   # gam smooth
   stat_smooth(data=plotdat, aes(x=p50, y=csci, color=huc_region), method = "gam", formula = y ~ s(x, bs = "cs"), show.legend = TRUE, se = FALSE ) +
   
   # all the other stuff
   scale_y_continuous(breaks=c(0, 0.63, 0.79, 0.92), limits=c(0, 1.35))+
   scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000), limits=c(0.01, 5500),
                 labels=c(0.01, 0.1, 1, 10, 100, 1000), expand=c(0.02, 0.02)) +
   
   scale_color_colorblind("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   scale_shape_discrete("HUC Region", labels = c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="North Coast", "south_coast"="South Coast"))+
   
   # theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank(),
         legend.position = c(0.88,0.88)) +
   labs(y="CSCI Score", 
        x=paste0(unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), " (cfs)"), 
        title=unique(ri_table$Flow.Metric.Name[which(ri_table$var==metselect)]), 
        subtitle = "Period of Record (50th percentile)"))

ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_by_huc_region.png"), width = 11, height = 7, dpi=300, units="in")
ggsave(paste0("figs/10_ffm_vs_top_ri_all_ca_", tolower(metselect), "_por_gam_by_huc_region.pdf"), width = 11, height = 7, dpi=300, units="in", device = cairo_pdf)



# SP_ROC w LAG ------------------------------------------------------------------


(gg3c <- ggplot(data=bmi_ffm_lag1 %>% 
                  filter(ffm_metric=="SP_ROC", !status=="not_enough_data"),
                aes(x=csci, y=ffm_value, fill=status)) + 
   #scale_fill_viridis_d("Alt. Status", direction = -1) +
   scale_y_log10() +
   geom_point(pch=21, size=4, show.legend = F) +
   geom_smooth(method = "gam", color="gray40", show.legend = FALSE) +
   labs(x="CSCI", y="log(FFM Value)", title="SP_ROC: Lag 1", subtitle="by alteration status")+
   scale_fill_colorblind("Alteration Status") +
   #scale_color_viridis_d("Alt. Status", direction = -1) +
   # theme_clean(base_family = "Roboto Condensed") +
   theme(panel.border = element_blank(),
         plot.background = element_blank()) +
   facet_grid(.~status))

(gg3d <- ggplot(data=bmi_ffm_lag2 %>% 
                  filter(ffm_metric=="SP_ROC", !status=="not_enough_data"),
                aes(x=csci, y=ffm_value, fill=status)) + 
    scale_fill_viridis_d("Alt. Status", direction = -1) +
    scale_y_log10() +
    geom_point(pch=21, size=4, show.legend = F) +
    geom_smooth(method = "gam", color="gray40", show.legend = FALSE) +
    labs(x="CSCI", y="log(FFM Value)", title="SP_ROC: Lag 2", subtitle="by alteration status")+
    scale_fill_colorblind("Alteration Status") +
    #scale_color_viridis_d("Alt. Status", direction = -1) +
    theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    facet_grid(.~status))

plot_grid(gg3b, gg3c, gg3d, ncol=3)

# FA_MAG ------------------------------------------------------------------


(gg5 <- ggplot(data=bmi_ffm_ann %>% filter(metric=="FA_Mag"), 
               aes(x=csci, y=ffm_value)) + 
   geom_point(pch=21, color="forestgreen") +
   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
               color="forestgreen")+
   #scale_x_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0,1.2)) +
   labs(x="CSCI", subtitle = "FA_Mag: Annual data")+
   theme_minimal())


(gg5b <- ggplot(data=bmi_ffm_por %>% filter(metric=="FA_Mag"), 
                aes(x=csci, y=ffm_value)) + 
    geom_point(pch=21, color="forestgreen") +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                color="forestgreen")+
    scale_x_continuous(breaks = seq(0, 1.2, 0.2)) +
    labs(x="CSCI", subtitle = "FA_Mag: POR data")+
    theme_minimal())

