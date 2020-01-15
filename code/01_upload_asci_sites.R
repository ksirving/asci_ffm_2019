
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)

#  statewide study - asci - ffm

#  upload algae data - asci scores downloaded from https://sites.google.com/view/asci/results - Susie
#  also upload component metrics (for MMI) to merge
setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

comp_scor <- read.csv("input_data/algae_comp_mets_mmi_jan2020.csv")
asci_scor <- read.csv("input_data/asci.scores_dec2019.csv", header=T)
head(asci_scor)
head(comp_scor)
dim(asci_scor) # 2588
dim(comp_scor) # 2579
#  use MMI (d=diatom, sha=soft algae, hybrid = both)

# change sample id name
colnames(asci_scor)[1] <- "SampleID"
colnames(comp_scor)[1] <- "SampleID"

#  merge datasets

asci_comp <- merge(asci_scor, comp_scor, by="SampleID", all=T)
# head(asci_comp)
# dim(asci_comp) # 2588
asci_scor <- asci_comp
#  upload site details - for coordinates

algae_raw <- read.csv("input_data/algae.bug.data.10172019.csv", header = T)
# head(algae_raw)
# names(algae_raw)
# dim(algae_raw)# 130753     26

#  get coords
algae_sites <- algae_raw[,c(1,17,18)]
head(algae_sites)
dim(algae_sites)


#  merge coords with asci scores
head(asci_scor)
dim(asci_scor)  # 2588    2
str(asci_scor)


asci_scor_sites <- merge(algae_sites, asci_scor, by.x="SampleID_old", by.y="SampleID")
# head(asci_scor_sites)
# dim(asci_scor_sites) # 128764      4
# str(algae_sites)
#  lose ~2K Samples

#  remove duplicates
asci_scor_sites <- asci_scor_sites[!duplicated(asci_scor_sites),]
dim(asci_scor_sites) #2625    4 - more sites here than the asci scores df as some samples have 2x reps
#  remove NAs
sum(is.na(asci_scor_sites)) # 969 some NAs from missing data in 0overE - keep MMIs from these sites? progress without but can change
names(asci_scor_sites)
asci_scor_sites <- asci_scor_sites[,-c(4,6,8,11)] # keep 11 to keep salinity
asci_scor_sites <- na.omit(asci_scor_sites)
# dim(asci_scor_sites) # 2270


# lose some sites if keep in OoverE metrics and salinity - 2260 (365 lost)
# for now continue with most sites possible - just MMI and DO metrics. 
# Ask ryan re salinity metric site loss

write.csv(asci_scor_sites, "output_data/asci_scores_coords.csv")
str(asci_scor_sites)
# change from factor to character
asci_scor_sites$SampleID_old <- as.character(asci_scor_sites$SampleID_old)

#  split date - station IDs etc

# use lubridate/tidyr to fix dates to a consistent format
algae <- separate(asci_scor_sites, col = SampleID_old , into=c("StationID", "MM", "DD", "YY", "Rep", "Rep2"), remove = F) 
head(algae)


# SampleID: recommended format is "stationcode_sampledate_collectionmethodcode_fieldreplicate" - already set this way in algae

# Warning message:
  # Expected 6 pieces. Missing pieces filled with `NA` in 2234 rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...]. 
# message is about REP2 column - all good - sorted below

# 1679, 1680, 1685, 1686, 1687, 1700, 2199, 2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207, 2208, 2209, 2210, 2211, 2212, ...].
## 26 have extra elements to split - need to be kept for StationID

#  issue here!! number below no longer match - need a universal solution!! Continue tomorrow
nas <- which(!is.na(algae$Rep2))
#  all station IDs with the extra element
weird_station_IDs <- unique(algae[nas, 2])
weird_station_IDs
# vector with index positions
wsid <- algae$StationID %in% weird_station_IDs
# sum(wsid) #26
#  change values using index
algae[wsid,2] <- paste(algae$StationID[wsid], "_", algae$MM[wsid], sep="")
algae[wsid,3] <- paste(algae$DD[wsid])
algae[wsid,4] <- paste(algae$YY[wsid])
algae[wsid,5] <- paste(algae$Rep[wsid])
algae[wsid,6] <- paste(algae$Rep2[wsid])

head(algae)
# algae[which(is.na(algae$sampledate)),]
# which(is.na(algae$sampledate))
 # add sample date
algae$sampledate = ymd(paste0(algae$YY,"-", algae$MM, "-",algae$DD))

#  remove Rep 2 column 

algae$Rep2 <- NULL
sum(is.na(algae)) # 0 

#  look at oldest date - to match with FFM
sort(unique(as.Date(algae$sampledate)))[1] # 2007-06-05

dim(algae) ## 2270
head(algae)

# how many missing SampleID's?: 
sum(is.na(algae$SampleID_old))

# how many missing Sampledates?
sum(is.na(algae$sampledate))

##############
## some sites have reps 1 & 2 with different values- some sites are duplicated with
# the same values

# remove duplicated same values - differ only due to coords dp

#  remove duplicates
algae <- algae[!duplicated(algae$SampleID_old),]
dim(algae) # 2223
head(algae)

##### below code regarding reps - average?????? to be finished

#  create new station id column with id, date but no rep number
# algae$SampleID_new <- paste(algae$StationID, "_", algae$MM, "/", algae$DD, "/", algae$YY, sep="")
# 
# #  count number of repeated stationids
# SampleID_freq <- as.data.frame(table(algae$SampleID_new))
# #  extract stations with more than 1 rep 
# str(SampleID_freq)
# SampleID_freq$Freq <- as.numeric(as.character(SampleID_freq$Freq))
# station_reps <- subset(stationID_freq, Freq > 1)
# dim(station_reps)
# station_reps
# 
# #  list stations with more than 1 rep
# station_repsx <- station_reps$Var1
# length(station_repsx) #187
# 
# #  subset asci data by stations with reps
# algae_reps <- subset(algae, SampleID_new %in% station_repsx)
# head(algae_reps)
# dim(algae_reps) # 382
# 
# #  look at station with 3 x reps
# ind <- algae_reps$SampleID_new %in% paste("405BH2Axx_06/16/10")
# algae_reps[ind,]

#  reps have different values - mean? 
# i=1
# 
# ind <- algae_reps$SampleID_new %in% paste(station_repsx[i])
# ind
# 
# algae_reps[ind,]


#             SampleID_old StationID MM DD YY Rep    Latitude
# 81  102PS0177_08/28/12_1 102PS0177 08 28 12   1 41.92939101
# 107 102PS0177_08/28/12_2 102PS0177 08 28 12   2 41.92939101
#       Longitude  OoverE.d    MMI.d OoverE.sba   MMI.sba
# 81  -123.5725438 0.9580512 1.139708  1.0853745 1.0883831
# 107 -123.5725438 0.7715050 1.126726  0.7384906 0.9756199
# OoverE.hybrid MMI.hybrid sampledate       SampleID_new
# 81      0.8719727   1.040264 2012-08-28 102PS0177_08/28/12
# 107     0.7571862   1.083255 2012-08-28 102PS0177_08/28/12

#  remiove unwated columns and save

algae <- algae[, -c(3:6)] # may change with rep decision
head(algae)
save(algae, file="output_data/clean_algae.RData") # data has mmi and DO, not salinity. can add salinity easily but will lose some sites

