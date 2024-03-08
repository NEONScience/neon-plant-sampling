####  Download and analysis of NEON Plant Belowground Biomass data for 2024 pre-season seminar ####

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




####  Root chemistry data exploration ###################################################################################
### Join bbc_chemistryPooling with bbc_rootChemistry to enable join with bbc_rootmass
##  Rework bbc_chemistryPooling table to create one row per subsampleID
bbc_chempool <- bbc$bbc_chemistryPooling

bbc_chempool <- bbc_chempool %>%
  dplyr::select(domainID,
                siteID,
                plotID,
                subsampleIDList,
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
                  .before = cnSampleID) %>%
  dplyr::filter(!is.na(subsampleID)) %>%
  dplyr::select(-subsampleIDList)


##  Join bbc_rootChemistry with bbc_chempool
bbc_rootchem <- bbc$bbc_rootChemistry

#   Calculate mean for analytical replicates
bbc_rootchem <- bbc_rootchem %>%
  dplyr::group_by(cnSampleID) %>%
  dplyr::summarise(nitrogenPercent = mean(nitrogenPercent),
                   carbonPercent = mean(carbonPercent),
                   CNratio = mean(CNratio),
                   .groups = "drop")

#   Join with bbc_chempool to get subsampleID, which enables join with bbc_rootmass to get sizeCategory;
#   filter out 'dead' roots with no chemistry --> presents some difficulties comparing older chemistry 
#   values with new ones derived from material that is a mix of live/dead
bbc_rootchem <- bbc_chempool %>%
  dplyr::left_join(bbc_rootchem,
                   by = "cnSampleID")

bbc_masschem <- bbc_rootmass %>%
  dplyr::left_join(bbc_rootchem %>%
                     dplyr::select(subsampleID,
                                   nitrogenPercent,
                                   carbonPercent,
                                   CNratio),
                   by = "subsampleID") %>%
  dplyr::filter(rootStatus == "live" | is.na(rootStatus))



### Sum mass across older 0-05 and 05-1 sizeCategories, calculate new 0-1 weighted chemistry means
#   Sum older categories to new 0-1mm category
olderFineMass <- bbc_masschem %>%
  dplyr::filter(sizeCategory == "0-05" | sizeCategory == "05-1",
                !is.na(nitrogenPercent) & !is.na(carbonPercent) & !is.na(CNratio)) %>%
  dplyr::group_by(sampleID) %>%
  dplyr::summarise(dryMass = sum(dryMass)) %>%
  dplyr::mutate(subsampleID = paste(sampleID, "0-1.LIVE", sep = "."),
                sizeCategory = "0-1",
                .after = sampleID)

#   Add data from olderFineMass to older fine root chemistry, then group_by() to calculate weighted mean chem
olderFine <- bbc_masschem %>%
  dplyr::filter(sizeCategory == "0-05" | sizeCategory == "05-1",
                !is.na(nitrogenPercent) & !is.na(carbonPercent) & !is.na(CNratio)) %>%
  dplyr::left_join(olderFineMass %>%
                     dplyr::select(sampleID,
                                   dryMass) %>%
                     dplyr::rename(dryMass01 = dryMass),
                   by = "sampleID") %>%
  dplyr::mutate(partialN = (dryMass/dryMass01)*nitrogenPercent,
                partialC = (dryMass/dryMass01)*carbonPercent,
                partialCN = (dryMass/dryMass01)*CNratio) %>%
  dplyr::group_by(domainID,
                  siteID,
                  plotID,
                  sampleID) %>%
  dplyr::summarise(nitrogenPercent = sum(partialN, na.rm = TRUE),
                   carbonPercent = sum(partialC, na.rm = TRUE),
                   CNratio = sum(partialCN, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(subsampleID = paste(sampleID, "0-1.LIVE", sep = "."),
                sizeCategory = "0-1",
                .after = sampleID)


##  Bind olderFine data with root data sorted according to newer sizeCategories
#   Get newer sizeCategory data
newerFine <- bbc_masschem %>%
  dplyr::filter(sizeCategory %in% c("0-1", "1-2", "2-10"),
                !is.na(nitrogenPercent) & !is.na(carbonPercent) & !is.na(CNratio)) %>%
  dplyr::select(domainID,
                siteID,
                plotID,
                sampleID,
                subsampleID,
                sizeCategory,
                nitrogenPercent,
                carbonPercent,
                CNratio) %>%
  dplyr::arrange(domainID,
                 siteID,
                 sampleID,
                 sizeCategory)

#   Combine newer and older root data
bbc_masschem <- rbind(newerFine,
                      olderFine)

#   Add clipID from bbc_percore and calculate final mean per clipID x sizeCategory;
#   for most clipIDs, North/South cores have same chemistry, but for older data the
#   way weighted means were calculated by subsampleID means North/South differ somewhat
#   for constructed 0-1 sizeCategory for these samples.
bbc_masschem <- bbc_masschem %>%
  dplyr::left_join(bbc_percore %>%
                     dplyr::select(sampleID,
                                   clipID),
                   by = "sampleID") %>%
  dplyr::relocate(clipID,
                  .after = plotID) %>%
  dplyr::group_by(domainID,
                  siteID,
                  plotID,
                  clipID,
                  sizeCategory) %>%
  dplyr::summarise(nitrogenPercent = mean(nitrogenPercent, na.rm = TRUE),
                   carbonPercent = mean(carbonPercent, na.rm = TRUE),
                   CNratio = mean(CNratio, na.rm = TRUE),
                   .groups = "drop")



### Construct chemistry ggplots by sizeCategory with siteID panels
#   Create CN Ratio by sizeCategory plot
cnPlot <- ggplot2::ggplot(bbc_masschem, 
                          ggplot2::aes(x = sizeCategory, 
                                       y = CNratio)) +
  ggplot2::geom_boxplot(width = 0.7) +
  ggplot2::ylab("Root C:N ratio") +
  ggplot2::xlab("Size category (mm diameter)") +
  ggplot2::facet_wrap(~domainID, 
                      ncol = 6, 
                      scales = "free_x") 




  

####  Root mass data exploration ####################################################################################

### Calculate root mass by sizeCategory in bbc_rootmass
#   Combine live/dead root mass in older data into one total mass like newer data
bbc_rootmass <- bbc_rootmass %>%
  dplyr::group_by(domainID,
                  siteID,
                  plotID,
                  collectDate,
                  sampleID,
                  sizeCategory) %>%
  dplyr::summarise(dryMass = sum(dryMass, na.rm = TRUE),
                   .groups = "drop")

#   Combine older 0-0.5mm and 0.5-1mm sizeCategories into newer 0-1mm sizeCategory
olderFineMass <- bbc_rootmass %>%
  dplyr::filter(sizeCategory %in% c("0-05", "05-1")) %>%
  dplyr::group_by(domainID,
                  siteID,
                  plotID,
                  collectDate,
                  sampleID) %>%
  dplyr::summarise(dryMass = sum(dryMass, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(sizeCategory = "0-1",
                .after = sampleID)

bbc_newmass <- bbc_rootmass %>%
  dplyr::filter(!sizeCategory %in% c("0-05", "05-1")) %>%
  rbind(olderFineMass) %>%
  dplyr::arrange(domainID,
                 siteID,
                 plotID,
                 collectDate,
                 sampleID,
                 sizeCategory)

#   Join with bbc_percore to get sampling area, calculate g per meters squared
bbc_newmass <- bbc_newmass %>%
  dplyr::left_join(bbc_percore %>%
                     dplyr::select(sampleID,
                                   rootSampleArea),
                   by = "sampleID") %>%
  dplyr::mutate(rootMassArea = round(dryMass/rootSampleArea,
                                     digits = 2))



### Calculate root fragment mass from bbc_dilution data
bbc_dilution <- bbc$bbc_dilution

#   Remove NAs, calculate fragment dryMass and remove outliers (very small masses can be difficult to weigh);
#   inner-join with bbc_percore means that only those samples from most recent eventIDs per site are considered
#   since bbc_percore was already filtered this way.
bbc_fragmass <- bbc_dilution %>%
  dplyr::filter(!is.na(dryMass)) %>%
  dplyr::mutate(fragMass = round(dryMass*(sampleVolume/dilutionSubsampleVolume),
                                 digits = 4),
                .before = dryMass) %>%
  dplyr::filter(fragMass < quantile(fragMass, probs = 0.99),
                fragMass >= 0) %>%
  dplyr::group_by(domainID,
                  siteID,
                  plotID,
                  collectDate,
                  sampleID) %>%
  dplyr::summarise(dryMass = mean(fragMass, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::inner_join(bbc_percore %>%
                     dplyr::select(sampleID,
                                   rootSampleArea),
                   by = "sampleID") %>%
  dplyr::mutate(rootMassArea = round(dryMass/rootSampleArea,
                                     digits = 2)) %>%
  dplyr::mutate(sizeCategory = "frag",
                .before = dryMass)



### Combine mass from sorted sizeCategories with fragment mass and plot
#   Bind bbc_newmass and bbc_fragmass
bbc_allmass <- rbind(bbc_newmass,
              bbc_fragmass) %>%
  dplyr::arrange(domainID,
                 siteID,
                 plotID,
                 sampleID,
                 sizeCategory)

#   Create boxplot by sizeCategory for each domainID
massPlot <- ggplot2::ggplot(bbc_allmass,
                            aes(x = sizeCategory,
                                y = rootMassArea)) +
  ggplot2::geom_boxplot(width = 0.7) +
  ggplot2::ylab("Root Mass (g/m2)") +
  ggplot2::xlab("Size Category (mm diameter)") +
  ggplot2::facet_wrap(~domainID,
                      ncol = 6,
                      scales = "free")

#   Mass on average by sizeCategory
bbc_massSummary <- bbc_allmass %>%
  dplyr::group_by(sizeCategory) %>%
  dplyr::summarise(meanMass = round(mean(dryMass, na.rm = TRUE),
                                    digits = 2),
                   sdMass = round(sd(dryMass, na.rm = TRUE),
                                  digits = 2),
                   count = n())

