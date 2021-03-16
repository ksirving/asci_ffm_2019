# 04 Merge algae asci Data with Flow Data for Period of Record
## R. Peek

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)
library(tidylog)


# Gages -------------------------------------------------------------------

# all gages
#gagelist <- read_csv("data/usgs/final_gage_ffc_list.csv")

# ref gages only
gages_ref <- read_csv("input_data/usgs/gages_ref_223_period_record.csv")

# ffc gage data (alteration)
ffc_dat <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds")) #%>% 

# keep unique gages only (n=959)
ffc_gages <- ffc_dat %>% select(gageid, comid, alteration_type) %>% distinct(gageid, .keep_all=TRUE) %>% 
  mutate(gage=as.numeric(gageid))
table(ffc_gages$alteration_type)

# how many gages have data past 1996? (n=101)
gages_ref_p1996 <- gages_ref %>% filter(maxYr>1996)

# gages that are suspect or that switch from ref to alt (data pre 1996)? (n=122)
gages_ref_to_alt <- gages_ref %>% filter(maxYr<1996)

# Find matches? n=114
gages_ref_match <- inner_join(ffc_gages, gages_ref_to_alt, by=c("gage"))


# LOAD DATA ---------------------------------------------------------------


# algae data:
### algae_final_dat (all data)
### algae_final_dat_trim (all data for selected site pairs btwn Jun-Sep)

algae_final_trim <- read_rds("output_data/02c_selected_final_algae_asci_dat_trim.rds") 

# ALL GAGES W FFC DATA
# read from ffm_comparison repo: https://github.com/ryanpeek/ffm_comparison

ffc_dat <- read_rds(file = url("https://github.com/ryanpeek/ffm_comparison/raw/main/output/ffc_combined/usgs_combined_alteration.rds")) #%>% 

# trim data to the USGS stations with algae asci data
ffc_trim <- ffc_dat %>% filter(gageid %in% algae_final_trim$site_id)

# how many gages distinct?
ffc_trim %>% distinct(gageid, .keep_all=TRUE) %>% tally()# n=214


# JOIN DATA ---------------------------------------------------------------

# join together selected asci data with ffm alteration status 
algae_asci_por_trim <-  inner_join(st_drop_geometry(algae_final_trim), ffc_trim,
                                 by=c("site_id"="gageid")) %>%   # n=16080
  distinct(StationCode, SampleID, metric, site_id, .keep_all=TRUE) %>%
  rename(comid_gage=comid.x, comid_ffc=comid.y)
# n=15936

# see how many distinct sites
length(unique(algae_asci_por_trim$site_id)) #Gages (n=214)
length(unique(algae_asci_por_trim$StationCode)) # algae Stations (n=238)

# gages that may be ref to alt? (25)
sum(unique(algae_asci_por_trim$site_id) %in% as.character(gages_ref_to_alt$gage))
usgs_ref_to_alt <- algae_asci_por_trim %>% 
  filter(site_id %in% as.character(gages_ref_to_alt$gage)) %>% 
  distinct(site_id)
write_csv(usgs_ref_to_alt, file="output_data/04_usgs_ref_to_alt_gages_followup.csv")


# how many of each gage type
algae_asci_por_trim %>%
  dplyr::distinct(site_id, .keep_all=TRUE) %>% 
  group_by(CEFF_type) %>%  tally() 
# ALT = 165, REF = 49, same as originally

# VISUALIZE ---------------------------------------------------------------

library(ggthemes)
library(lubridate)
names(algae_asci_por_trim)
hist(month(algae_asci_por_trim$SampleDate))

# function to get data
stat_box_data <- function(y, upper_limit = max(algae_asci_por_trim$asci, na.rm = TRUE)) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'median =', round(median(y), 2), '\n')
    )
  )
}

# install.packages("extrafont")
library(extrafont)
extrafont::font_import()

# plot asci w/ NAs
# quartz()
(g1 <- ggplot(data=algae_asci_por_trim %>% 
                filter(status!="not_enough_data"), 
              aes(x=CEFF_type, y=H_ASCI.y)) + 
    geom_boxplot(aes(fill=status), show.legend = F, notch=TRUE) +
    stat_summary(fun.data=stat_box_data, geom="text", cex=3, hjust=1, vjust=0.9) +
    labs(y="ASCI", x="CEFF Gage Type", subtitle="asci Score by FFC Alteration Status & Gage Type"))
    # theme_bw(base_family = "Roboto Condensed") + facet_grid(.~status) +
    # scale_fill_colorblind())

ggsave(filename = "figs/04_asci_scores_by_ffc_alteration_status_gage_type.png", height = 8, width = 11, units = "in", dpi=300)
# dev.off()

# plot asci w/ NAs
(g2 <- ggplot(data=algae_asci_por_trim %>% 
                filter(status!="not_enough_data"), 
              aes(x=status, y=H_ASCI.y)) + 
    geom_boxplot(aes(fill=status), show.legend = F, notch = TRUE) +
    stat_summary(fun.data=stat_box_data, geom="text", cex=3, hjust=1, vjust=0.9) +
    labs(y="asci", x="Alteration Status", subtitle="asci Score by FFC Alteration Status"))#+
    # theme_bw(base_family = "Roboto Condensed") + 
    #facet_grid(.~status) +
    # scale_fill_colorblind())

ggsave(filename = "figs/04_asci_scores_by_ffc_alteration_status.png", height = 8, width = 11, units = "in", dpi=300)

#library(cowplot)
#plot_grid(g1, g2, ncol = 2, rel_widths = c(1.5, 0.8), labels="AUTO")

# Export Joined POR Data ---------------------------------------------

# save the algae_asci_por_trim data
write_rds(algae_asci_por_trim, file = "output_data/04_selected_asci_ffm_por_trim.rds")
