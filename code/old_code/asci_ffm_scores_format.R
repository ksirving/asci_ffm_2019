#  statewide study - asci - ffm

library("raster")
# git test
#  upload algae data - asci scores downloaded from https://sites.google.com/view/asci/results - Susie

setwd("/Users/katieirving/Documents/git/asci_ffm_2019")

asci_scor <- read.csv("input_data/asci.scores_dec2019.csv", header=T)
head(asci_scor)
#  use MMI (d=diatom, sha=soft algae, hybrid = both)

# subset to only sites and hybrid MMI score
asci_scor <- asci_scor[,c(1,7)]
colnames(asci_scor)[1] <- "site_id"

#  upload site details - for coordinates

algae_raw <- read.csv("input_data/algae.bug.data.10172019.csv", header = T)
head(algae_raw)
names(algae_raw)
dim(algae_raw)# 130753     26

#  get coords
algae_sites <- algae_raw[,c(1,17,18)]
head(algae_sites)
dim(algae_sites)

#  merge coords with asci scores
head(asci_scor)
dim(asci_scor)  # 2588    2


matches <- asci_scor[1] %in% algae_sites[1]
head(matches)
matches
asci_scor[1] %in% algae_sites[1]
asci_scor[1]
algae_sites[1]

asci_scor_sites <- merge(algae_sites, asci_scor, by.x="SampleID_old", by.y="site_id")
head(asci_scor_sites)
dim(asci_scor_sites) # 128764      4
#  lose ~2K Samples

#  remove duplicates

asci_scor_sites <- asci_scor_sites[!duplicated(asci_scor_sites),]
dim(asci_scor_sites) #2625    4 - more sites here than the asci scores df as some samples have 2x reps
#  remmove NAs
sum(is.na(asci_scor_sites))
asci_scor_sites <- na.omit(asci_scor_sites)

head(asci_scor_sites)
write.csv(asci_scor_sites, "output_data/asci_scores_coords.csv")

coords <- asci_scor_sites[, 2:3]
#  coords into numeric
asci_scor_sites$Latitude <- as.numeric(as.character(asci_scor_sites$Latitude))
asci_scor_sites$Longitude <- as.numeric(as.character(asci_scor_sites$Longitude))

#  remove NAs
sum(is.na(asci_scor_sites)) #50
asci_scor_sites <- na.omit(asci_scor_sites)

#  spatial point df
coordinates(asci_scor_sites) <- c("Latitude","Longitude")
     plot(asci_scor_sites)
     