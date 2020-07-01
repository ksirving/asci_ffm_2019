# 08 Extract and Plot TOP Flow variables from GBM
# R. Peek

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
library(viridis) # colors
library(cowplot)
library(gbm) # boosted regression trees
library(dismo)
library(pdp)
library(rlang)
# install.packages("pdp")
# install.packages("cowplot")
#extrafont::loadfonts(quiet=TRUE)

# GBM evaluation of single points of data
#library(DALEX)
#library(ingredients)

# load updated data w HUC_regions:
load("output_data/04_selected_algae_asci_por_trim_w_huc_region.rda")
load("output_data/04_selected_algae_asci_por_w_huc_region.rda")

# simple just sites:
algae_asci_sites <- algae_asci_por_trim %>% 
  dplyr::distinct(StationID, .keep_all = TRUE)

#length(unique(algae_asci_sites$StationID)) # n=238
#length(unique(algae_asci_sites$ID)) # n=139

# Load Data --------------------------------------------------------------------

## VARIABLES:
# "all_ca_ffc_only"
# "central_valley", "great_basin", "north_coast", "south_coast", 

hydroDat <- "POR" # can be Annual, Lag1, Lag2, POR
modname <- "all_ca_ffc_only" # model name 
plotname <- "All Site Pairs"  #"Central Valley" #"All Site Pairs"
algaeVar <- quote(MMI.hybrid) # select response var

# make pathnames
(mod_pathname <- paste0("05_gbm_final_", tolower(algaeVar), "_",tolower(hydroDat), "_",modname))
(mod_savename <- tolower(paste0("06_gbm_", as_name(algaeVar), "_",hydroDat, "_",modname)))

# get the gbm model:
(brt <- list.files(path="models/", pattern = paste0("^", mod_pathname,".*\\.rds$")))

gbm_final <- read_rds(path=paste0("models/", brt))
class(gbm_final)

# get model datasets (for PDPs)
load(paste0("models/",mod_pathname, "_model_data.rda"))

# rename datasets for plotting:
gbm_out_train <- data_por_train # NEED TO CHANGE THESE

# % percent explained
(gbm_final$self.statistics$mean.null - gbm_final$cv.statistics$deviance.mean) / gbm_final$self.statistics$mean.null 

# 01A. RELATIVE INFLUENCE PLOTS (MSE) ALL VARS -------------------------------------

## RI: improvement by MSE (split criterion), vars with largest avg decrease in MSE are important
gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) %>% 
  mutate("Ymetric"= as_name(algaeVar),
         "flowdat" = hydroDat,
         "method" = "mse")

# add flow components for plotting purposes
gbm_fin_RI <- gbm_fin_RI %>% 
  mutate(flow_component = case_when(
    grepl("DS_", var) ~ "Dry-season baseflow",
    grepl("SP_", var) ~ "Spring recession flow",
    grepl("Peak_", var) ~ "Peak flow",
    grepl("Wet_", var) ~ "Wet-season baseflow",
    grepl("FA_", var) ~ "Fall pulse flow",
    TRUE ~ "General"
  ),
  flow_component = factor(flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow", "General")),
  var = as.factor(var),
  var = fct_reorder2(var, flow_component, var)) %>% 
  rename(RI = rel.inf)

## Now Plot ALL
quartz
plotname
dev.off()
head(fin_ri)
fin_ri <- gbm_fin_RI %>% 
    arrange(desc(RI)) %>% 
    #filter(RI > 3) %>% #View()
    filter(flow_component!="General") # %>% 
    
ggplot(data=fin_ri) +
    geom_col(aes(x=var,
                 y=RI, fill=flow_component), color="gray20", lwd=.1,
             position="dodge") +
    coord_flip() +
    geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8) +
    ylim(c(0,30)) +
    # # theme(text=element_text(family="Garamond", size=14))+
    scale_fill_viridis_d("Flow Component") #+
    # labs(title = plotname,
    #      title=paste0(hydroDat, " (", toupper(as_label(algaeVar)),") Metrics: ", modname),
    #      subtitle="MSE Criterion",
    #      x="", y="Relative Influence (%)") #+
    # theme_classic(base_family = "Roboto Condensed")

# # save out
ggsave(filename=tolower(paste0("models/", mod_savename, "_all_RI_mse.png")), width = 9, height = 7, units = "in", dpi = 300)

# 01B. RELATIVE INFLUENCE PLOTS (MSE) TOP VARS -------------------------------------

# and plot top vars only
fin_ri_top <- gbm_fin_RI %>% 
   arrange(desc(RI)) %>% 
   filter(flow_component!="General", RI>5) #%>% 
   
ggplot(fin_ri_top) +
   geom_col(aes(x=reorder(var, RI),
                y=RI, fill=flow_component), color="gray20", lwd=.1,
            position="dodge") +
   coord_flip() +
   geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8)+
   ylim(c(0,30))+
   scale_fill_viridis_d("Flow Component")
   # labs(title=paste0(hydroDat, " (", toupper(as_label(algaeVar)),") Top Metrics: ", modname),
   #      x="", y="Relative Influence (%)", subtitle="MSE Criterion") +
   # theme_classic(base_family = "Roboto Condensed")) 

# save out
ggsave(filename=tolower(paste0("models/", mod_savename, "_top_RI_mse.png")), width = 9, height = 7, units = "in", dpi = 300)


# 02A. RI PERMUTATION TEST PLOTS ALL VARS ------------------------------------------------

## PT: permutation test, decrease in accuracy is averaged and vars with largest avg decrease in accuracy are import

gbm_fin_PT<-as.data.frame(summary(gbm_final, plotit = F, method=permutation.test.gbm)) %>% 
  mutate("Ymetric"= as_name(algaeVar),
         "flowdat" = hydroDat,
         "method" = "permtest")
gbm_fin_PT <- gbm_fin_PT %>% 
  mutate(flow_component = case_when(
    grepl("DS_", var) ~ "Dry-season baseflow",
    grepl("SP_", var) ~ "Spring recession flow",
    grepl("Peak_", var) ~ "Peak flow",
    grepl("Wet_", var) ~ "Wet-season baseflow",
    grepl("FA_", var) ~ "Fall pulse flow",
    TRUE ~ "General"
  ),
  flow_component = factor(flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow", "General")),
  var = as.factor(var),
  var = fct_reorder2(var, flow_component, var)) %>% 
  rename(RI = rel.inf)

## Now Plot ALL (Most Accurate)
(fin_pt <- gbm_fin_PT %>% 
    arrange(desc(RI)) %>% 
    filter(flow_component!="General") %>% 
    ggplot(.) +
    geom_col(aes(x=var,
                 y=RI, fill=flow_component), color="gray20", lwd=.1,
             position="dodge") +
    coord_flip() +
    geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8)+
    ylim(c(0,50))+
    scale_fill_viridis_d("Flow Component"))
    # labs(title=paste0(hydroDat, " (", toupper(as_label(algaeVar)),") Metrics: ", modname),
    #      y="Relative Influence (%) (perm test)", x="",
    #      subtitle = "Permutation Test") +
    # theme_classic(base_family = "Roboto Condensed")) 

# save out
ggsave(filename=tolower(paste0("models/", mod_savename, "_all_RI_permtest.png")), width = 9, height = 7, units = "in", dpi = 300)


# 02B. RI PERMUTATION TEST PLOTS TOP VARS ------------------------------------------------

# and plot top vars only
(fin_pt_top <- gbm_fin_PT %>% 
   arrange(desc(RI)) %>% 
   filter(flow_component!="General", RI>5) %>% 
   ggplot(.) +
   geom_col(aes(x=reorder(var, RI),
                y=RI, fill=flow_component), color="gray20", lwd=.1,
            position="dodge") +
   coord_flip() +
   geom_hline(yintercept = 5, color="gray40", lwd=1, lty=2, alpha=0.8)+
   ylim(c(0,50))+
   scale_fill_viridis_d("Flow Component"))
   # labs(title=paste0(hydroDat, " (", toupper(as_label(algaeVar)),") Top Metrics: ", modname),
   #      y="Relative Influence (%) (perm test)", x="",
   #      subtitle = "Permutation Test") +
   # theme_classic(base_family = "Roboto Condensed")) 

# save out
ggsave(filename=tolower(paste0("models/", mod_savename, "_top_RI_permtest.png")), width = 9, height = 7, units = "in", dpi = 300)

# Plot Side by Side -------------------------------------------------------

# library(cowplot)

# (pg1 <- plot_grid(fin_ri_top, fin_pt_top, align = "h", labels=c("A","B")))
# 
# cowplot::save_plot(pg1, filename = tolower(paste0("models/08_gbm_", as_name(algaeVar), "_", hydroDat,"_top_RI_both", ".png")), base_width = 11, units = "in", dpi = 300)


# 03. COMBINE RIs AND SAVE -----------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0(as_name(algaeVar),"_",hydroDat,"_RI")), value=bind_rows(gbm_fin_PT, gbm_fin_RI))

filepattern <- ls(pattern = paste0("^",tolower(as_name(algaeVar))))

write_rds(x = get(filepattern), path = paste0("models/", mod_savename, "_RI_combined.rds"))

# 04. MARGINAL FX Plots ----------------------------------------------

# MARGINAL FX:: partial dependency or marginal effect plots (ALL)

pdf(file=paste0("models/", mod_savename,"_partial_depend_plots.pdf"),
    width = 11, height = 8)

gbm.plot(gbm_final, rug = T, n.plots = 8, show.contrib = T, 
         smooth=T, write.title = F, common.scale = T,
         y.label = as_name(algaeVar), plot.layout = c(2,4))
title(main=paste0("Flow Data: ", hydroDat, "_", modname), outer = T, line = -1.5)
dev.off()

# 05. ICE PLOTS -----------------------------------------

## displays avg change in predicted Y VAR as we vary an X VAR while holding everything else constant

library(pdp)

# get top var
(bestHydro_ri <- gbm_fin_RI %>% top_n(n = 3, RI))
(bestHydro_pt <- gbm_fin_PT %>% top_n(n = 3, RI))

## ICE (Individual conditional expectation) plots: rather than plot the average marginal effect on the response variable, we plot the change in the predicted response variable for each observation as we vary each predictor variable.

## The equivalent to a PDP for individual data instances is called individual conditional expectation (ICE) plot (Goldstein et al. 2017). An ICE plot visualizes the dependence of the prediction on a feature for each instance separately, resulting in one line per instance, compared to one line overall in partial dependence plots.

# When the curves have a wide range of intercepts and are consequently “stacked” on each other, heterogeneity in the response variable values due to marginal changes in the predictor variable of interest can be difficult to discern, thus centering can help

varNo <- 3 # single number makes single plot

# RI
(ice_ri <- gbm_final %>%
    partial(
      pred.var = as.character(bestHydro_ri$var[varNo]), 
      n.trees = gbm_final$n.trees, train=gbm_out_train,
      grid.resolution = 100,
      ice = TRUE
    ) %>%
    autoplot(rug = TRUE, train = gbm_out_train, alpha = .1, center = TRUE))# +
    # labs(subtitle = paste0("ICE Centered (RI): ", bestHydro_ri$var[varNo], " for ", hydroDat),
    #      y=paste0("Predicted ", as_name(algaeVar))) +
    # ggdark::dark_theme_classic(base_family = "Roboto Condensed"))

# RI save:
ggsave(filename=paste0("models/", mod_savename, "_pdp_ice_",
                       as.character(bestHydro_ri$var[varNo]),
                       ".png"), width = 11, height = 7, units = "in", dpi=300)







# Z-archive this chunk: USE DALEX ---------------------------------------------------------------

gbm_explain <- explain(gbm_final, data=gbm_out_train[,-1], y=gbm_out_train$asci)
gbm_explain_tst <- explain(gbm_final, data=gbm_out_test[,-1], y=gbm_out_test$asci)

# feature importance
gbm_feat <- feature_importance(gbm_explain)
plot(gbm_feat, max_vars=12)

gbm_feat_tst <- feature_importance(gbm_explain_tst)
plot(gbm_feat_tst, max_vars=12)

# look at explained variance for a specific observation(s)
library(iBreakDown)
gbm_breakdown <- break_down(gbm_explain, new_observation = gm_out_test[,-1], keep_distributions=TRUE)
gbm_shap <- shap(gbm_explain, gbm_out_test[,-1])
plot(gbm_breakdown, baseline=0)
plot(gbm_shap)

# describe a bit more
describe(gbm_breakdown)
describe(gbm_shap)


# partial dependency using ingredients (only works w/ no NAs)
# gbm_pd <- partial_dependence(gbm_explain_tst, N=50, variables=c("SP_ROC","SP_Dur"))
# plot(gbm_pd)
# 
# # ceteris_paribus (only works w/ data w/out NAs)
# gbm_cp_pg <- ceteris_paribus(gbm_explain_tst, 
#                               new_observation = gm_out_test[1,],
#                               variables="SP_Dur")
# plot(gbm_cp_pg)

# predict values for test data
pred <- predict(gbm_final, n.trees = gbm_final$n.trees, gm_out_test)

# results
caret::RMSE(pred, gm_out_train[,1])
