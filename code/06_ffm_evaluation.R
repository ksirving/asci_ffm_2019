# FFM Evaluation
# Filter Ref gages to assess best FF metrics
# to refine metrics that need to be run
# R. Peek 2021


# Libraries ---------------------------------------------------------------

library(scales)
library(sf)
library(glue)
options(tidyverse.quiet = TRUE)
library(tidyverse) # load quietly
library(viridis) # colors
library(janitor)

# 01: GET DATA ----------------------------------------------------------------

# REF gages
gages_ref <- read_csv("input_data/usgs/gages_ref_223_period_record.csv") %>% 
  select(stream_class, gage, maxYr, minYr, YrRange) %>% 
  distinct(gage, .keep_all = TRUE) %>% 
  mutate(refgage="Ref")

# ffc data (alteration status)
ffc_alt <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds")) %>%
  mutate(gage=as.numeric(gageid),
         status=as.factor(status),
         alteration_type=as.factor(alteration_type))

# import observed FFM data for period of record
ffc_obs <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_ffc_percentiles.rds")) %>%
  mutate(gage=as.numeric(gageid))

# import predicted percentiles for period of record
ffc_pred <- read_rds(file=url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_predicted_percentiles.rds")) %>% 
  mutate(gage=as.numeric(gageid))

# 02: TIDY AND CLEAN ----------------------------------------------------------

# get distinct gages (n=959)
ffc_gages <- ffc_alt %>% distinct(gageid, .keep_all=TRUE) %>% 
  mutate(gage=as.numeric(gageid)) %>% 
  left_join(., gages_ref, by="gage") %>% 
  mutate(refgage = case_when(
    is.na(refgage) ~ "Non-Ref",
    TRUE ~ refgage)) %>% 
  select(-c(metric:median_in_iqr))

# join FFC_DAT w ref gages (FROM CEFF/FFM) data to get REF/non-ref class
ffc_alt <- left_join(ffc_alt, gages_ref, by="gage") %>% 
  mutate(refgage = case_when(
    is.na(refgage) ~ "Non-Ref",
    TRUE ~ refgage))

# tally (24 metrics x 959 gages = 23016)
ffc_alt %>% group_by(refgage, status) %>%
  tally(name="total_status") %>% # tally by status
  # tally by ref gage class
  add_tally(total_status, name="total_of_refgage_class") # so only 21912, some NA's still

summary(ffc_alt$status)
summary(ffc_alt$status_code)
## so not_enough_data = NA in status_code
summary(ffc_alt$alteration_type)


# 03: CALC PROPORTION GAGES ALTERED PER METRIC ----------------

# count total records by metric & refgage
(ffm_metric_count <- ffc_alt %>% 
   group_by(metric, refgage) %>% 
   count(name = "total_count"))

# calculate proportion of gages/total for each status class
ffm_prcnt_alt <- ffc_alt %>% 
  group_by(metric, status, refgage) %>% # for facet by refgage
  tally() %>% 
  left_join(., ffm_metric_count) %>% 
  mutate(prop_n = n/total_count) %>% 
  # create all possible combos
  complete(metric, status, refgage) %>% 
  # drop duplications here (not sure why getting dups!?)
  distinct(.keep_all = TRUE) %>% 
  # fill NAs with zero
  mutate(across(everything(), ~replace_na(.x, 0)))

# filter to just ref
ffm_prcnt_alt_ref <- ffm_prcnt_alt %>% filter(refgage=="Ref") 

## Plot: ALT STATUS -------------------------------------------------------

# plot faceted by REF/NON-REF type
ggplot() + geom_col(data=ffm_prcnt_alt, aes(x=metric, y=prop_n, fill=status)) +
  theme_classic() +
  scale_y_continuous(labels=percent) +
  scale_fill_viridis_d(direction = -1) +
  labs(title="Proportion of Gages by Alteration Status",
       subtitle="Based on 959 gages with sufficient data for FFM R Calculator",
       x="", y="Proportion of Gages Altered") +
  theme(axis.text.x = element_text(angle=270, hjust = 0.1, vjust=0.05)) +
  facet_wrap(vars(refgage))

#ggsave(filename = "figs/prop_gages_by_alt_status_faceted.png", width = 11, height = 8.5, dpi=300)

# 04: FILTERING FFM ---------------------------------------------

## STEP 1: <10% of Ref gages altered ------------------------

# Filter to only FFM with < 10% of ref gages == altered, excluding PEAK FLOW METRICS 

ffm_prcnt_alt_keep <- ffm_prcnt_alt_ref %>% 
  filter(status == "likely_altered") %>% 
  filter(prop_n < 0.1) # dropped "Peak_10" "Peak_2"  "Peak_5"

# FILTER METRIC LIST
(filt_less_than_10_perc_metrics <- ffm_prcnt_alt_keep$metric)

## STEP 2: <15% of Ref gages peak flows altered -------------

# Filter to only Peak flow metris w < 15% of ref gages == altered
# ffm_prcnt_alt_keep_peak <- ffm_prcnt_alt %>% 
#   filter(refgage == "Ref") %>% # keep only REF
#   filter(status == "likely_altered") %>% 
#   filter(grepl("^Peak", metric)) %>% 
#   filter(prop_n < 0.15)
# 
# # FILTER METRIC LIST
# filt_less_than_15_peak <- ffm_prcnt_alt_keep_peak$metric

## STEP 3: <25% of Ref gages are indeterminant -----------

# <25% of ref gages are indeterminate for that metric

ffm_prcnt_alt_keep_indet <- ffm_prcnt_alt %>% 
  filter(refgage == "Ref") %>% # keep only REF
  filter(metric %in% filt_less_than_10_perc_metrics) %>% 
  filter(status == "indeterminate") %>% 
  filter(prop_n < 0.25)

# list of metrics
filt_less_than_25_indeterm <- ffm_prcnt_alt_keep_indet$metric

# list of metrics that are being dropped:
anti_join(ffm_prcnt_alt_ref, ffm_prcnt_alt_keep_indet, by=c("metric")) %>% ungroup() %>% distinct(metric)

# 1 Peak_10       
# 2 Peak_2        
# 3 Peak_5        
# 4 Wet_BFL_Mag_10
# 5 Wet_BFL_Mag_50
# 6 Wet_Tim   
### WRITE IT OUT
# write_csv(ffm_prcnt_alt_keep_indet, file = "output_data/06_filtered_ffm_based_on_ref_prop.csv")


# 05: JOIN OBS & PRED FFM PERCENTILES -----------------------------------------

# using the metrics listed above get the obs 50 percentile and predicted 50 percentile data 

# get all alt gages to use:
alt_gages_unique <- ffc_alt %>% filter(refgage=="Non-Ref") %>% 
  select(gageid) %>% distinct()

ffc_obs_filt <- ffc_obs %>% 
  filter(gageid %in% alt_gages_unique$gageid) %>% 
  filter(metric %in% filt_less_than_25_indeterm) %>% 
  select(metric:gageid, p50_obs = p50, data_type=result_type)

ffc_pred_filt <- ffc_pred %>% 
  filter(gageid %in% alt_gages_unique$gageid) %>% 
  filter(metric %in% filt_less_than_25_indeterm) %>% 
  select(metric, comid, result_type, gageid, p50_pred = p50, result_type)

ffm_delta_hyd <- left_join(ffc_obs_filt, ffc_pred_filt)

# join with the ref/alt status data
ffm_final_dat <- left_join(ffm_delta_hyd, ffc_alt %>% 
                             select(metric:gageid, refgage))


ffm_final_dat %>% group_by(metric, refgage) %>% tally() %>% View()

# Filter and Fix ----------------------------------------------------------

# how  many total gages?
ffm_final_dat %>% distinct(gageid) %>% tally()

# drop 10 gages with missing SP metrics
gages_to_drop <- ffm_final_dat %>% 
  filter(metric== "SP_ROC" & is.na(p50_obs)) %>% 
  select(gageid) #%>% 
#View()

# filter
ffm_final_dat_v2 <- ffm_final_dat %>% 
  filter(!gageid %in% gages_to_drop$gageid)

# check how many remain
ffm_final_dat_v2 %>% distinct(gageid) %>% tally()

# summary of na
summary(ffm_final_dat_v2$p50_obs)
summary(ffm_final_dat_v2$p50_pred)

# fill FA_Dur obs to 0 if NA
ffm_final_dat_v2 <- ffm_final_dat_v2 %>% 
  mutate(p50_obs = case_when(
    metric == "FA_Dur" & is.na(p50_obs) ~ 0,
    TRUE ~ p50_obs
  ))

# Metrics missing from pred and obs gages
ffm_missing_metrics <- ffm_final_dat_v2 %>% filter(is.na(p50_pred)) %>% 
  group_by(metric) %>% tally(name = "n_pred") %>%
  full_join(., (
    ffm_final_dat_v2 %>% 
      filter(is.na(p50_obs)) %>%
      group_by(metric) %>% tally(name="n_obs"))) #%>% 
# get number of gages that are missing these metrics
#ffm_missing_metrics %>% View()

# write out:
#write_csv(ffm_missing_metrics, file = "data_output/06_ffm_gages_missing_per_metrics_pred_obs.csv")

# get number of metrics missing per gage
ffm_final_dat_v2 %>% filter(is.na(p50_pred)| is.na(p50_obs)) %>% 
  group_by(gageid, comid) %>% tally() %>% 
  View() # n=134 total

# get list of these gages for later use:
ffm_gages_missing_metrics <- ffm_final_dat_v2 %>% filter(is.na(p50_pred)| is.na(p50_obs)) %>% 
  group_by(gageid, comid) %>% tally()

write_rds(ffm_gages_missing_metrics, file = "output_data/06_ffm_gages_missing_metrics.rds")
write_csv(ffm_gages_missing_metrics, file = "output_data/06_ffm_gages_missing_metrics.csv")

# drop remaining NA's?
ffm_final_dat_v2 <- ffm_final_dat_v2 %>% 
  filter(!is.na(p50_obs) & !is.na(p50_pred))

# look 
ffm_final_dat_v2 %>% distinct(gageid) %>% tally()
ffm_final_dat_v2 %>% distinct(metric) %>% tally()
ffm_final_dat_v2 %>% group_by(gageid) %>% tally() %>% 
  View()

# Test FFC API ------------------------------------------------------------

# options(dplyr.print_max = 50)
# install.packages("ffcAPIClient")
# library(ffcAPIClient)
# # set/get the token for using the FFC
# ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", "")) 
# 
# gageNo <- "09423350"
# COMID <- 10017314
# 
# # get predicted data:
# ffcAPIClient::get_predicted_flow_metrics(comid = COMID, online = TRUE)
# 
# # return everything
# tst <- ffcAPIClient::evaluate_gage_alteration(token=ffctoken, comid = COMID, gage_id = gageNo, return_processor = TRUE, plot_results = FALSE)
# 
# # observe percentiles
# as_tibble(tst$ffc_percentiles)
# as_tibble(tst$predicted_percentiles)
# 
# # run and get obs percentiles
# ffc <- FFCProcessor$new()  # make a new object we can use to run the commands
# ffc$gage_start_date = "1979-10-01"
# ffc$set_up(gage_id=gageNo, token = ffctoken, comid = COMID)
# ffc$run()
# ffc$ffc_percentiles %>% as.data.frame()

# Save Out Data -----------------------------------------------------------

# write out
write_rds(ffm_final_dat_v2, file = "output_data/06_ffm_final_dataset.rds")
write_csv(ffm_final_dat_v2, file = "output_data/06_ffm_final_dataset.csv")



# 06: JOIN WITH asci DATA -----------------------------------------------------

# load updated data:
load("output_data/05_algae_asci_por_trim_ecoreg.rda")
load("/Users/katieirving/Documents/git/bmi_ffm_links/data_output/05_bmi_csci_por_trim_ecoreg.rda")
names(bmi_csci_por_trim_ecoreg)
# rename for ease of use 
asci_trim <- algae_asci_por_trim_ecoreg #%>% st_drop_geometry()
names(asci_trim)
# simplify
asci_trim <- asci_trim %>% select(StationCode:huc8, date_begin, date_end, comid_gage:H_ASCI.y,US_L3_mod, geometry) %>% distinct(.keep_all = TRUE)
names(asci_trim)

asci_trim <- asci_trim %>%
  select(-ends_with(".y"), -Latitude, -Longitude)

names(asci_trim) <- gsub(".x", "", names(asci_trim))

# now join
asci_mod_dat <- left_join(asci_trim, ffm_final_dat_v2, by=c("site_id"="gageid")) %>% 
  mutate(delta_p50 = p50_obs/p50_pred) %>% 
  filter(!is.na(delta_p50)) %>% 
  filter(!is.infinite(delta_p50))

summary(asci_mod_dat)
head(asci_mod_dat)

asci_mod_dat %>% st_drop_geometry %>% distinct(StationCode) %>% tally() # n=190
asci_mod_dat %>% st_drop_geometry %>% distinct(site_id) %>% tally() # n=147
asci_mod_dat %>% st_drop_geometry %>% distinct(metric) %>% tally() # n=18
asci_mod_dat %>% st_drop_geometry %>% distinct(refgage) %>% tally() # n=1
asci_mod_dat %>% st_drop_geometry %>% group_by(US_L3_mod, metric) %>% tally() %>% View()

write_rds(asci_mod_dat, file = "output_data/06_asci_por_trim_final_dataset.rds")
write_csv(asci_mod_dat, file = "output_data/06_asci_por_trim_final_dataset.csv")


# 07: ADD PLOTS/SUMMARIES -------------------------------------

## Plot asci by alt_hyd: 
ggplot() + 
  geom_point(data=asci_mod_dat, aes(x=H_ASCI, y=p50_obs/p50_pred, fill=as.factor(refgage), color=as.factor(refgage), shape=as.factor(refgage)), alpha=0.7) +
  geom_smooth(data=asci_mod_dat, method = "glm",
              aes(x=H_ASCI, y=delta_p50, color=status, fill=status), lwd=.5, show.legend = FALSE) +  theme_bw() +
  scale_shape("Gage Type", 
              guide = guide_legend(override.aes = 
                                     list(size = 3, alpha=0.9))) +
  ggthemes::scale_fill_colorblind("Gage Type") +
  ggthemes::scale_color_colorblind("Gage Type") +
  facet_wrap(.~metric, scales = "free_y")

ggsave(filename = "figs/prelim_glm_delta_p50obsexp_vs_asci.png", width = 11, height = 8, dpi=300)

ggsave(filename = "figs/prelim_gam_delta_p50obsexp_vs_asci.png", width = 11, height = 8, dpi=300)

# Calc REF/NON-REF w medianIQR ------------------------------

summary(ffc_alt$median_in_iqr)

# calculate proportion of gages/total
ffm_prcnt_alt2 <- ffc_alt %>% 
  group_by(metric, median_in_iqr, refgage) %>% # for facet by refgage
  tally() %>% 
  left_join(., ffm_metric_count) %>% 
  mutate(prop_n = n/total_count)

## Plot: by median IQR ---------------------------------------------------

# plot faceted by REF/NON-REF type
ggplot() + geom_col(data=ffm_prcnt_alt2, aes(x=metric, y=prop_n, fill=median_in_iqr)) +
  theme_classic() +
  scale_y_continuous(labels=percent) +
  scale_fill_viridis_d(direction = -1, na.value="gray50") +
  labs(title="Proportion of Gages by Median in IQR",
       subtitle="Based on 959 gages with sufficient data for FFM R Calculator",
       x="", y="Proportion of Gages Altered") +
  theme(axis.text.x = element_text(angle=270, hjust = 0.1, vjust=0.05)) +
  facet_wrap(vars(refgage))

# ggsave(filename = "figs/prop_gages_by_medIQR_faceted.png", width = 11, height = 8.5, dpi=300)

# Calc REF/NON-REF by ALT type ------------------------------

# calculate proportion of gages/total for each alt type
ffm_prcnt_alt3 <- ffc_alt %>% 
  group_by(metric, alteration_type, refgage) %>% # for facet by refgage
  tally() %>% 
  left_join(., ffm_metric_count) %>% 
  mutate(prop_n = n/total_count,
         # fix order of alt_type
         alteration_type = forcats::fct_relevel(alteration_type, "high", "low", 
                                                "early", "late", 
                                                "none_found", 
                                                "undeterminable", "unknown"))

## Plots: by Alt Type ---------------------------------------------------

# plot faceted by REF/NON-REF type
ggplot() + geom_col(data=ffm_prcnt_alt3, aes(x=metric, y=prop_n, fill=alteration_type), alpha=0.8) +
  theme_classic() +
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=c("high"="#E41A1C", "low"="#377EB8", "early"="#4DAF4A", "late"="#984EA3", "none_found"="gray80", "undeterminable"="gray40","unknown"="gray20")) +
  labs(title="Proportion of Gages by Alteration Type",
       subtitle="Based on 959 gages with sufficient data for FFM R Calculator",
       x="", y="Proportion of Gages Altered") +
  theme(axis.text.x = element_text(angle=270, hjust = 0.1, vjust=0.05)) +
  facet_wrap(vars(refgage))

# ggsave(filename = "figs/prop_gages_by_alt_type_faceted.png", width = 11, height = 8.5, dpi=300)

## more plots

csci_breaks <- c(0, 0.63, 0.79, 0.92)
asci_breaks <- c(0, 0.75, 0.86, 0.94)
bio_labs <- c("Very likely altered", "Likely altered", "Possibly altered","Likely intact")

unique(asci_mod_dat$metric)
unique(asci_mod_dat$refgage)

brt_out <- asci_mod_dat %>% 
  filter(metric == "SP_ROC") 
# Peak_Dur_2
# Wet_BFL_Dur

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
    geom_point(data=brt_out, aes(x=delta_p50, y=H_ASCI, 
                                 #shape=status,
                                 color=US_L3_mod), 
               size=2.8, alpha=0.7, show.legend = F) +
    
    #scale_shape_manual("Alteration\nStatus", labels=c("Likely Altered","Likely Unaltered"), 
    #                    values=c("-1"=23,"1"=21), guide=FALSE) +
    
    # gam smooth
    stat_smooth(data=brt_out, aes(x=delta_p50, y=H_ASCI,
                                  group=US_L3_mod), 
                #color=alteration_type),
                method = "loess", span = 0.95,
                #method = "glm", level = 0.89,
                #method = "gam", level = 0.89,
                #formula = y ~ s(x, bs = "cs"), 
                #color="gray40", 
                fill="gray80", 
                show.legend = FALSE) +
    
    # all the other stuff
    #scale_fill_colorblind("Bioindex", labels=c("CSCI", "ASCI"), guide = guide_legend(override.aes = list(shape = c(21, 21), size=4))) +
    #scale_fill_colorblind("FFM Alteration\nStatus", labels=c("Altered", "Unaltered"),
    #                     guide = guide_legend(override.aes = list(shape = c(23, 21), size=4))) +
    #scale_color_colorblind("FFM Alteration\nStatus", labels=c("Altered", "Unaltered")) +
    scale_y_continuous(breaks=asci_breaks, limits=c(0, 1.3)) +
    scale_x_log10(
      labels=scales::comma,
      expand=c(0.01,0.01))+
    #limits=c(0.1, 1)) 
    #max(plotdat$value)) + 
    # theme_clean(base_family = "Roboto Condensed") +
    theme(panel.border = element_blank(),
          plot.background = element_blank()) +
    labs(y="ASCI Score",
         x=unique(brt_out$metric),
         title=unique(brt_out$metric))) +
  facet_wrap(.~US_L3_mod)



# Boxplots ----------------------------------------------------------------


csci_breaks <- c(0, 0.63, 0.79, 0.92)
asci_breaks <- c(0, 0.75, 0.86, 0.94)

unique(asci_mod_dat$metric)
unique(asci_mod_dat$refgage)

brt_out <- asci_mod_dat %>% 
  filter(metric == "SP_ROC") 

ggplot() + 
  
  # add line thresholds
  annotate(geom = "text", label="Very likely altered", color="gray50", 
           x=0.11, y=0.58, hjust=0, size=4) +
  annotate(geom = "text", label="Likely altered", color="gray50",
           x=0.11, y=0.71, hjust=0, size=4) +
  annotate(geom = "text", label="Possibly altered", color="gray50", 
           x=0.11, y=0.85, hjust=0, size=4) +
  annotate(geom = "text", label="Likely intact", color="gray50",
           x=0.11, y=1, hjust=0, size=4) +
  
  geom_boxplot(data=brt_out, aes(x=delta_p50, y=H_ASCI, 
                                 #shape=status, 
                                 group=alteration_type,
                                 fill=alteration_type), 
               alpha=0.7, show.legend = TRUE, 
               #varwidth = TRUE, 
               notch = F) +
  
  scale_x_log10(
    labels=scales::comma)+
  
  # theme_clean(base_family = "Roboto Condensed") +
  theme(panel.border = element_blank(),
        plot.background = element_blank()) +
  labs(y="ASCI Score",
       x=unique(brt_out$metric),
       title=unique(brt_out$metric))


