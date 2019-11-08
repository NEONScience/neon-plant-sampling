library(knitr)
library(plyr)
library(dplyr)
library(stringr)

# exifPath <- "/Volumes/neon/DSF/CI Dropbox/Domain 17/TOS/LAI_RAW_images/2016"
exifPath <- "/Volumes/neon/DSF/CI Dropbox/Domain 04/TOS/LAI Image Storage/2017"
setwd(exifPath)
system('exiftool -T -r -common -csv -G4 GUAN08 > /Users/cmeier/Documents/neonScienceDocs/gitRepositories/neonPlantSampling/dhpPhotoCheck/dhp_exifdata_D04_2017_GUAN08.csv')
system('exiftool -T -r -common -csv -G4 LAJA07 > /Users/cmeier/Documents/neonScienceDocs/gitRepositories/neonPlantSampling/dhpPhotoCheck/dhp_exifdata_D04_2017_LAJA07.csv')



setwd("~/Documents/neonScienceDocs/gitRepositories/neonPlantSampling/dhpPhotoCheck")

dhpGuan <- read.csv("dhp_exifdata_D04_2017_GUAN08.csv", header=T, stringsAsFactors = F)
dhpLaja <- read.csv("dhp_exifdata_D04_2017_LAJA07.csv", header=T, stringsAsFactors = F)
