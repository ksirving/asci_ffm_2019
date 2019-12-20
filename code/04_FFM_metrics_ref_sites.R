### functional flow metrics
#  package to 
setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

# library(devtools)
# devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
# api client package to extract ffm 
library(ffcAPIClient) ## 

token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLYXRpZSIsImxhc3ROYW1lIjoiSXJ2aW5nIiwiZW1haWwiOiJrYXRpZWlAc2Njd3JwLm9yZyIsInJvbGUiOiJVU0VSIiwiaWF0IjoxNTc2NTQ0MDk2fQ.6fWAyYohV4wxHHSGR4zBeYoTKbUw4YXuIEjQyPB33lU"
ffcAPIClient::set_token(token) 

#  list gages
load(file="output_data/paired_gages_algae.RData")
head(sel_gages_algae) # 39 gauges paired with algae


daily_df <- get_usgs_gage_data(10295500)
head(daily_df)
results <- ffcAPIClient::get_ffc_results_for_usgs_gage(10295500) 


drh_plot <- ffcAPIClient::plot_drh(results)  # includes optional output_path argument to save to file automatically

head(results)
drh_plot  # display the plot

# load(file="/Users/katieirving/Documents/git/bmi_ffm_links/data_output/selected_bmi_and_flow_metrics.rda")
# 
# load(file="/Users/katieirving/Documents/git/bmi_ffm_links/data_output/selected_usgs_flow_metrics_POR.rda")

# upload functional flow metric data - existing 

# ffmets <- read.csv("input_data/gages_ref_223_period_record.csv")
# load(file="input_data/ref_gage_annFlow_stats_long.rda")

