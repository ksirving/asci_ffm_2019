# Identify top RI Flow Metrics
# summarize data from all GBMs

# Libraries ---------------------------------------------------------------

library(gt)
library(tidyverse)
library(sf)
library(viridis) # colors
library(rlang)
library(purrr)

# Load Data ---------------------------------------------------------------

# load updated data w HUC_regions:
load("output_data/04a_selected_algae_ascii_ffm_ann_trim.rda")
load("output_data/04a_selected_algae_asci_ffm_ann.rda")

names(algae_asci_ffm_ann_trim)
# load("models/07_all_ri_all_regions_asci.rda")
hydroDat <- "ANN"
plotname <- "All Site Pairs"  #"Central Valley" #"All Site Pairs"

## ONLY IF YOU NEED MODEL NAMES/DATA
## "all_ca_ffc_only", "central_valley", "great_basin", "north_coast", "south_coast", 
modname <- "central_valley" # model name 
algaeVar <- quote(MMI.hybrid) # select response var
# make pathnames
(plot_savename <- tolower(paste0("07_gbm_", as_name(algaeVar), "_",hydroDat, "_",modname)))

# make regional Ri --------------------------------------------------------

## ONLY IF YOU NEED MODEL NAMES/DATA
# "all_ca_ffc_only", "central_valley", "great_basin", "north_coast", "south_coast",
modname <- "central_valley" # model name
algaeVar <- quote(MMI.hybrid) # select response var

# make pathnames
(mod_savename <- tolower(paste0("05_gbm_", as_name(algaeVar), "_",hydroDat, "_",modname)))
paste0("^", mod_savename,"_RI_combined",".*\\.rds$")
# get the gbm model:
(top_ri <- list.files(path="models/", pattern = paste0("^", mod_savename,"_RI_combined",".*\\.rds$")))

top_ris <- read_rds(path=paste0("models/", top_ri))
top_ris
top_ri
# "05_gbm_mmi.hybrid_por_south_coast_RI_combined.rds"
# make sep and combine
ri_all_ca <- top_ris %>% mutate(model="all_ca")
ri_cvalley <- top_ris %>% mutate(model="central_valley")
ri_gbasin <- top_ris %>% mutate(model="great_basin")
ri_ncoast <- top_ris %>% mutate(model="north_coast")
ri_scoast <- top_ris %>% mutate(model="south_coast")

str(ri_all_ca)
ri_cvalley
ri_gbasin
ri_ncoast
ri_scoast

# # bind
ri_all_regions <- bind_rows(ri_all_ca, ri_gbasin, ri_cvalley, ri_ncoast, ri_scoast)
head(ri_all_regions)
unique(ri_all_regions$model)
# #save out for later
save(ri_all_regions, file = "models/07_all_ri_all_regions_asci.rda")


# Make a Table of RI's ----------------------------------------------------

load(file="models/07_all_ri_all_regions_asci.rda")
library(readxl)
ff_defs <- readxl::read_xlsx("docs/Functional_Flow_Metrics_List_and_Definitions_final.xlsx", range = "A1:F25", .name_repair = "universal", trim_ws = TRUE) 

# join with the full RI table
ri_table <- left_join(ri_all_regions, ff_defs, by=c("var"="Flow.Metric.Code"))

# drop unused factors in flow component:
ri_table <- ri_table %>% 
  mutate(flow_component=forcats::fct_drop(flow_component),
         var = as.factor(var),
         var = fct_reorder2(var, flow_component, var),
         model=as.factor(model),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) 

levels(ri_table$var)
levels(ri_table$flow_component)
summary(ri_table)

# generate order by CA wide RI for flow metrics:
forder <- ri_table %>% 
  filter(model=="all_ca", 
         method=="mse") %>% 
  mutate(model=as.factor(model),
         Flow.Metric.Name = as.factor(Flow.Metric.Name),
         Flow.Metric.Name = forcats::fct_reorder2(Flow.Metric.Name, model, RI)) %>% 
  group_by(model) %>% 
  arrange(desc(model, RI)) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% arrange(id) %>% 
  select(Flow.Metric.Name, id) # get the just the names for ordering things




# Plot & Summarize All RI Combined ----------------------------------------

# most common hydrometric by flowdata type?
ri_table %>% group_by(flowdat, var) %>% 
  summarize(meanRI = mean(RI),
            sumRI = sum(RI)) %>% 
  top_n(5) %>% 
  arrange(flowdat, desc(meanRI))

# so best hydrometric across model?
ri_table %>% group_by(model, var) %>% 
  summarize(meanRI = mean(RI),
            medianRI = median(RI),
            maxRI = max(RI),
            SD = sd(RI)) %>% 
  top_n(5) %>% group_by(var) %>% tally() %>% arrange(desc(n))

# top metrics based on frequency across regions
# Wet_BFL_Mag_50     5
# DS_Mag_50          3
# DS_Tim             3
# Peak_5             3
# SP_ROC             3
# DS_Dur_WS          2


# Summary Table ALL CA -----------------------------------------------------------

library(glue)

model_name <- "All CA"
## ALL CA Table
# Create a gt table based on preprocessed table
ri_table %>%
  dplyr::filter(model=="all_ca", method=="mse") %>%
  dplyr::select(-c(Ymetric, flowdat, flow_component, method, model, Flow.Characteristic)) %>%
  dplyr::select(Flow.Component, var, Flow.Metric.Name:Flow.Metric.Description, RI) %>%
  arrange(Flow.Component, var) %>% #View() 
  gt() %>%
  tab_header(
    title = "Relative Influence of Functional Flow Metrics on asci",
    subtitle = glue::glue("Model {model_name}")
  ) %>%
  fmt_number(
    columns = vars(RI), decimals = 1, 
    drop_trailing_zeros = T
  )

# Summary Table Regional -----------------------------------------------------------

model_name <- "HUC Regions"

# Create a gt table based on preprocessed table
ri_table %>%
  dplyr::filter(!model=="all_ca", method=="mse") %>%
  dplyr::select(-c(Ymetric, flowdat, flow_component, method, Flow.Characteristic)) %>%
  pivot_wider(names_from = model, values_from = RI) %>% #View()
  dplyr::select(Flow.Component, var, Flow.Metric.Name:Unit, great_basin:south_coast) %>%
  arrange(Flow.Component, var) %>% #View() 
  gt() %>%
  tab_header(
    title = "Relative Influence of Functional Flow Metrics on asci",
    subtitle = glue::glue("Model {model_name}")
  ) %>%
  fmt_number(
    columns = vars(central_valley, north_coast, south_coast, great_basin), decimals = 1, 
    drop_trailing_zeros = T
  )

# Summary Plot ALL CA ------------------------------------------------------------

# color palette 
flowcomponent_colors <- c("Fall pulse flow" = "#F0E442", "Wet-season baseflow" = "#56B4E9",
                          "Peak flow" = "#0072B2", "Spring recession flow" = "#009E73", 
                          "Dry-season baseflow" = "#D55E00")

flowcomponent_colors <- c("Fall pulse flow" = "#F0E442", "Wet-season baseflow" = "#56B4E9",
                          "Peak flow" = "#404788FF", "Spring recession flow" = "#009E73", 
                          "Dry-season baseflow" = "#D55E00")
flowcomponent_colors
# Faceted by hydrodat and flow metrics:

quartz()
unique(ri_table$flow_component)
# 
# loadfonts(dev="win")
# windowsFonts()

ri_table %>% 
  filter(model=="all_ca", 
         method=="mse") %>% 
  ggplot() +
  #geom_hline(yintercept = 5, color="gray40", lwd=0.8, lty=2, alpha=0.5)+
  geom_linerange(aes(x=reorder(Flow.Metric.Name, RI), ymax=RI, ymin=0, group=model, color=flow_component), 
                 lwd=.5, show.legend = F, alpha=0.7, lty=1)+
  geom_point(aes(x=Flow.Metric.Name, y=RI, group=model, fill=flow_component, size=RI), 
             #size=4, 
             show.legend = TRUE, pch=21) +
  scale_fill_manual("Flow Component", values=flowcomponent_colors) +
  scale_color_manual("Flow Component", values=flowcomponent_colors) +
  scale_size_area("", guide=FALSE) +
  #scale_shape_manual("Method", values=c("mse"=16, "permtest"=17))+
  coord_flip() +
  ylim(c(0,25))+
  labs(title = plotname,
       x="", y="Relative Influence (%)") +
  guides(fill = guide_legend(override.aes = list(size = 4))) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(text=element_text(family="Helvetica", size=14))+
  theme(legend.position = c(0.8, 0.3),
        legend.background = element_rect(color="white"))

# save out
ggsave(filename=tolower(paste0("models/", plot_savename, "_all_ri_sized_points_w_lines_ranked.png")), width = 9, height = 7, units = "in", dpi = 300)

# Summary Plot Regions ------------------------------------------------------------
vars(model)

ri_table <- filter(ri_table, !flow_component == "General") ## general = huc region - ok to remove
# now plot w facets (but use same ordering for ALL CA)
ri_table %>% 
  filter(model!="all_ca", 
         method=="mse") %>% 
  left_join(., forder, by="Flow.Metric.Name") %>% 
  arrange(id) %>% #View() 
  ggplot() +
  facet_grid(cols = vars(model), labeller = labeller(model=c("all_ca"="All CA", "central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="N. Coast", "south_coast"="S. Coast"))) +
  geom_linerange(aes(x=reorder(Flow.Metric.Name, desc(id)), ymax=RI, ymin=0, color=flow_component, group=model), 
                 lwd=.5, show.legend = F, alpha=0.7, lty=1)+
  geom_point(aes(x=reorder(Flow.Metric.Name, desc(id)), y=RI, fill=flow_component, size=RI, group=model), 
             #size=4, 
             show.legend = TRUE, pch=21) +
  #scale_x_continuous(breaks=forder$id, labels=forder$Flow.Metric.Name) +
  scale_fill_manual("Flow Component", values=flowcomponent_colors) +
  scale_color_manual("Flow Component", values=flowcomponent_colors) +
  scale_size_binned("", guide=FALSE, range=c(0.5, 6.5)) +
  coord_flip() +
  ylim(c(0,30))+
  labs(subtitle = "asci Models",
       x="", y="Relative Influence (%)") +
  #theme_bw(base_family = "Roboto Condensed")
  theme_minimal(base_family = "Roboto Condensed") +
  theme(text=element_text(family="Helvetica", size=14))+
  theme(legend.position = "bottom", legend.box = "horizontal")+
  guides(fill = guide_legend(override.aes = list(size = 4), 
                             title.position = "top"))
#facet_grid(cols = vars(model), labeller = labeller(model=c("central_valley"="Central Valley", "great_basin"="Great Basin", "north_coast"="N. Coast", "south_coast"="S. Coast")))

# save out
plot_savename2 <- "07_gbm_asci_por_regional"
ggsave(filename=tolower(paste0("models/", plot_savename2, "_ri_points_w_lines_ranked.png")), width = 9, height = 7, units = "in", dpi = 300)


# all 5
ggsave(filename=tolower(paste0("models/", plot_savename2, "_all_regions_ri_points_w_lines_ranked.png")), width = 9, height = 7, units = "in", dpi = 300)


# Other Plots -------------------------------------------------------------



# ggsave(filename = "figs/09_faceted_RI_by_flowcomp_hydrodat.png", width = 9, height = 6, units = "in", dpi = 300)
# 
# # Faceted by algae metrics and flow components:
# ri_table %>% group_by(flowdat, var, Ymetric, flow_component) %>%
#   summarize(meanRI = mean(RI)) %>%
#   #top_n(6) %>%
#   arrange(desc(meanRI)) %>%
#   filter(flow_component!="General", flowdat=="Annual") %>%
#   ggplot(.) +
#   geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
#   coord_flip() +
#   scale_fill_viridis_d("Flow Components")+
#   labs(x="", y="Mean Relative Inf (%)", subtitle="ANNUAL: Top Flow Metrics across algae Metrics") +
#   theme_classic(base_family = "Roboto Condensed") +
#   facet_grid(.~Ymetric)
# 
# ggsave(filename = "figs/faceted_RI_by_flowcomp_algae_ANNUAL.png", width = 9, height = 6, units = "in", dpi = 300)

# Faceted by algae metrics and flow components:
# ri_table %>% group_by(flowdat, var, Ymetric, flow_component) %>%
#   summarize(meanRI = mean(RI)) %>%
#  # top_n(6) %>%
#   arrange(desc(meanRI)) %>%
#   filter(flow_component!="General", flowdat=="Lag1") %>%
#   ggplot(.) +
#   geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
#   coord_flip() +
#   scale_fill_viridis_d("Flow Components")+
#   labs(x="", y="Mean Relative Inf (%)", subtitle="LAG-1: Top Flow Metrics across algae Metrics") +
#   theme_classic(base_family = "Roboto Condensed") +
#   facet_grid(.~Ymetric)
#
# ggsave(filename = "figs/faceted_RI_by_flowcomp_algae_LAG1.png", width = 9, height = 6, units = "in", dpi = 300)
#
# # Faceted by algae metrics and flow components:
# ri_table %>% group_by(flowdat, var, Ymetric, flow_component) %>%
#   summarize(meanRI = mean(RI)) %>%
#   # top_n(6) %>%
#   arrange(desc(meanRI)) %>%
#   filter(flow_component!="General", flowdat=="Lag2") %>%
#   ggplot(.) +
#   geom_col(aes(x=var, y=meanRI, fill=flow_component), color="gray20", lwd=.1, position="dodge") +
#   coord_flip() +
#   scale_fill_viridis_d("Flow Components")+
#   labs(x="", y="Mean Relative Inf (%)", subtitle="LAG-2: Top Flow Metrics across algae Metrics") +
#   theme_classic(base_family = "Roboto Condensed") +
#   facet_grid(.~Ymetric)
#
# ggsave(filename = "figs/faceted_RI_by_flowcomp_algae_LAG2.png", width = 9, height = 6, units = "in", dpi = 300)
#
#
