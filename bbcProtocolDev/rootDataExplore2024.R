####  Download and analysis of NEON Plant Belowground Biomass data for 2024 pre-season seminar

### Setup
#   Load required libraries
library(dplyr)
library(ggplot2)
library(neonUtilities)
library(stringr)
library(tidyr)



### Retrieve BBC data from Portal for all sites and years
bbc <- neonUtilities::loadByProduct(dpID = "DP1.10067.001",
                                    site = "all",
                                    tabl = "all",
                                    check.size = FALSE,
                                    include.provisional = TRUE,
                                    token = Sys.getenv("NEON_TOKEN"))

##  Identify most recent eventID by site and retain bbc_percore samples from latest eventIDs with select columns
bbc_percore <- bbc$bbc_percore

bbcLatest <- bbc_percore %>%
  dplyr::mutate(year = as.numeric(stringr::str_extract(string = eventID,
                                                       pattern = "20[0-9]{2}"))) %>%
  dplyr::group_by(siteID) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(eventID)

bbc_percore <- bbc_percore %>%
  dplyr::filter(eventID %in% bbcLatest$eventID,
                samplingImpractical == "OK") %>%
  dplyr::select(domainID,
                siteID,
                plotID,
                subplotID,
                clipID,
                coreID,
                eventID,
                samplingImpractical,
                sampleID,
                rootSamplingMethod,
                rootSampleArea,
                rootSampleDepth)


##  Retain bbc_rootmass sampleIDs in most recent eventIDs and remove qaDryMass == "Y"
bbc_rootmass <- bbc$bbc_rootmass

bbc_rootmass <- bbc_rootmass %>%
  dplyr::filter(sampleID %in% bbc_percore$sampleID,
                qaDryMass == "N") %>%
  dplyr::select(domainID,
                siteID,
                plotID,
                collectDate,
                sampleID,
                subsampleID,
                sizeCategory,
                rootStatus,
                dryMass)

#   Determine root dryMass by sizeCategory, combining live/dead rootStatus in older data
bbc_rootmass <- bbc_rootmass %>%
  dplyr::group_by(domainID,
                  siteID,
                  plotID,
                  sampleID,
                  subsampleID,
                  sizeCategory) %>%
  dplyr::summarise(dryMass = sum(dryMass),
                   .groups = "drop")



### Rework bbc_chemistryPooling table to create one row per subsampleID
bbc_chempool <- bbc$bbc_chemistryPooling

bbc_chempool <- bbc_chempool %>%
  dplyr::select(domainID,
                siteID,
                plotID,
                subsampleIDList,
                poolSampleID,
                cnSampleID)


##  Expand subsampleIDList if "|" exists in string; pivot_longer() approach preserves all columns in input df
bbc_chempool <- bbc_chempool %>%
  #   tempSub1: If subsampleIDList contains pipe, extract everything before pipe
  #   tempSub2: If subsamleIDList contains pipe, extract everything after pipe
  dplyr::mutate(tempSub1 = dplyr::case_when(grepl("\\|", subsampleIDList) ~ stringr::str_extract(subsampleIDList,
                                                                                                 pattern = "^.*?(?=\\|)"),
                                            TRUE ~ subsampleIDList),
                tempSub2 = dplyr::case_when(grepl("\\|", subsampleIDList) ~ stringr::str_extract(subsampleIDList,
                                                                                                 pattern = "[^\\|]*$"),
                                            TRUE ~ NA)) %>%
  tidyr::pivot_longer(cols = c(tempSub1, tempSub2),
                      names_to = NULL,
                      values_to = "subsampleID") %>%
  dplyr::relocate(subsampleID,
                  .before = poolSampleID) %>%
  dplyr::filter(!is.na(subsampleID)) %>%
  dplyr::select(-subsampleIDList)







#--> Will need to combine 0-0.5 and 0.5-1 old sizeCategories as final step after calculating chemistry

### Root Mass Data ################################################################
### Goal: Calculate fragment mass from dilution sampling per unit soil volume as
###        percent of total belowground biomass per unit soil volume.


### Calculate dilution sampling mass per unit soil volume
##  Retrieve data using loadByProduct() function to pipe to R session
#   Download data as list for all sites
allRoot <- loadByProduct(dpID = 'DP1.10067.001', site = 'all', package = 'basic', check.size = FALSE)

#   Get dilution data and write to .csv for loading later
dil1 <- allRoot[["bbc_dilution"]]
write.csv(dil1, file = paste(wdir, "bbc_dilution.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

#   Calculate dryMass for fragments < 1 cm length for each dilutionSubsampleID; filter outliers > 99 percentile
#   for whole dataset; filter fragMass < 0 g
dil1 %>%
  mutate(fragMass = round(dryMass*(sampleVolume/dilutionSubsampleVolume), digits = 4)) %>%
  filter(fragMass < quantile(fragMass, probs = 0.99), fragMass >= 0) -> dil2

#   Calculate mean dryMass of fragments < 1 cm length for each sampleID
dil2 %>%
  group_by(domainID, siteID, plotID, sampleID) %>%
  summarise(dryMass = mean(fragMass), sdDM = sd(fragMass), nDM = n()) -> dil3

dil3$sizeCategory <- "frag"

#   Write summary dilution data table
write.csv(dil3, file = paste(wdir, "bbc_summaryDil.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")


##  Calculate root mass per unit volume by sizeCategory, including dilution fragments
#   Extract and save core data
core1 <- allRoot[["bbc_percore"]]
write.csv(core1, file = paste(wdir, "bbc_percore.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

#   Extract and save root mass data
mass1 <- allRoot[["bbc_rootmass"]]
write.csv(mass1, file = paste(wdir, "bbc_rootmass.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

#   Remove QA records from dataset and calculate dryMass per sizeCategory
rMass %>%
  filter(qaDryMass == 'N') %>%
  group_by(domainID, siteID, plotID, sampleID, sizeCategory) %>%
  summarise(dM = sum(dryMass, na.rm = TRUE)) %>%
  rename(dryMass = dM) -> sumRoot

##  Bind rows of dilution data with root mass data, keep only records that have both dilution and root mass data
#   Before binding, check for missing sampleIDs in sumDil and sumRoot data frames
sumDil[sumDil=='NA'] <- NA
sumDil %>% filter(is.na(sampleID)) -> missDilSamp

sumRoot[sumRoot=='NA'] <- NA
sumRoot %>% filter(is.na(sampleID)) -> missRootSamp

#   Remove sampleIDs from sumRoot with no match in sumDil
sumRoot %>%
  filter(sampleID %in% unique(sumDil$sampleID)) -> sumRoot2

#   Remove unneeded columns from sumDil, then rowbind to sumRoot and write to .csv
sumDil %>% select(-sdDM, -nDM) %>% bind_rows(sumRoot2) %>% arrange(sampleID) -> rootDF
write.csv(rootDF, file = paste(wdir, "bbc_totalRootMass.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

#   Make ggplot of sizeCategory vs. dryMass with siteID facet wrap
rootP4 <- ggplot(rootDF, aes(x = sizeCategory, y = dryMass)) +
  geom_boxplot(width = 0.7) +
  ylab("Root Dry Mass (g)") +
  xlab("Root size category (mm diameter)") +
  facet_wrap(~siteID, ncol = 3, scales = "free_y") +
  theme_bw()

ggsave(filename = paste(wdir, "plot_rootDryMass_sizeCat.pdf", sep = "/"),
       plot = rootP4, width = 6.5, units = 'in', device = 'pdf')






### Root chemistry data #################################################################

##  Retrieve data using loadByProduct() function to pipe to R session
args(loadByProduct)
# function (dpID, site = "all", package = "basic", avg = "all", 
#          check.size = TRUE)

#   Download data as list of lists: One list per site, first list item is 'bbc_rootChemistry' table
rootChem <- lapply(X = sites, FUN = loadByProduct, dpID = "DP1.10102.001", package = "basic", check.size = FALSE)

#   Create single dataframe from list data
chemDF <- ldply(c(rootChem[[1]][1], rootChem[[2]][1], rootChem[[3]][1], rootChem[[4]][1],
                  rootChem[[5]][1], rootChem[[6]][1], rootChem[[7]][1], rootChem[[8]][1],
                  rootChem[[9]][1], rootChem[[10]][1], rootChem[[11]][1]))

#   Remove megapit root data; these records do not have a poolSampleID
chemDF %>% filter(poolSampleID != "NA") -> chemDF

#   Write out chemDF so source data are easily re-usable
write.csv(chemDF, file = paste(wdir, "rootChem_2016-2017.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

#   Calculate mean chemistry values for analytical replicates
chemDF %>%
  group_by(poolSampleID) %>%
  summarise_all(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) -> chemDF

#   Create needed variables for plotting
chemDF %>%
  mutate(sizeCategory = str_sub(poolSampleID, start = 25, end = -6)) -> chemDF
chemDF$sizeCategory <- recode(chemDF$sizeCategory, '0-05' = '0-0.5', '05-1' = '0.5-1')

### Create plots and save
#   Create C:N Ratio by sizeCategory plot
rootP1 <- ggplot(chemDF, aes(x = sizeCategory, y = CNratio)) +
  geom_boxplot(width = 0.7) +
  ylab("Root C:N ratio") +
  xlab("Root size category (mm diameter)") +
  facet_wrap(~siteID, ncol = 4, scales = "free_x") +
  theme_bw()

ggsave(filename = paste(wdir, "plot_CNratio_sizeCat.pdf", sep = "/"),
       plot = rootP1, width = 6.5, units = 'in', device = 'pdf')

#   Create carbonPercent by sizeCategory plot
rootP2 <- ggplot(chemDF, aes(x = sizeCategory, y = carbonPercent)) +
  geom_boxplot(width = 0.7) +
  ylab("Root % Carbon") +
  xlab("Root size category (mm diameter)") +
  facet_wrap(~siteID, ncol = 4, scales = "free_x") +
  theme_bw()

ggsave(filename = paste(wdir, "plot_cPercent_sizeCat.pdf", sep = "/"),
       plot = rootP2, width = 6.5, units = 'in', device = 'pdf')

#   Create a nitrogenPercent by sizeCategory plot
rootP3 <- ggplot(chemDF, aes(x = sizeCategory, y = nitrogenPercent)) +
  geom_boxplot(width = 0.7) +
  ylab("Root % Nitrogen") +
  xlab("Root size category (mm diameter)") +
  facet_wrap(~siteID, ncol = 4, scales = "free_x") +
  theme_bw()

ggsave(filename = paste(wdir, "plot_nPercent_sizeCat.pdf", sep = "/"),
       plot = rootP3, width = 6.5, units = 'in', device = 'pdf')

