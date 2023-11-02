### BBC subplotID string update to remove MMDD portion of date now that cores can be 
### collected on multiple days

#   Background: When cores are collected on > 1 day, app is currently taking earliest collectDate
#   to create the poolSampleID

library(dplyr)
library(neonUtilities)
library(stringr)


### Step 1: Identify list of samples shipped to Biorepository that originate from cores
### collected on different days

### Retrieve all BBC data for all time
bbc <- neonUtilities::loadByProduct(dpID = "DP1.10067.001",
                                    site = "all",
                                    release = "LATEST",
                                    tabl = "all",
                                    check.size = FALSE,
                                    include.provisional = TRUE,
                                    token = Sys.getenv("NEON_PAT"))

bbc_percore <- bbc$bbc_percore
bbc_rootmass <- bbc$bbc_rootmass
bbc_chemistryPooling <- bbc$bbc_chemistryPooling
bbc_rootChemistry <- bbc$bbc_rootChemistry
bbc_dilution <- bbc$bbc_dilution



### Identify samples shipped to Biorepository
biorepoDF <- bbc_chemistryPooling %>%
  dplyr::filter(!is.na(bgcArchiveID)) %>%
  dplyr::select(domainID, 
                siteID,
                plotID,
                subsampleIDList,
                poolSampleID,
                bgcArchiveID)



### Split subsampleIDList column into 2 columns
#   Remove rows with no value for 'bSubsampleID' as impossible for pool sample to have > 1 collectDate if no bSubsampleID
biorepoDF <- biorepoDF %>%
  dplyr::mutate(aSubsampleID = stringr::word(subsampleIDList, 1, sep = "\\|"),
                bSubsampleID = dplyr::case_when(
                  grepl("\\|", subsampleIDList) ~ stringr::word(subsampleIDList, -1, sep = "\\|"),
                  TRUE ~ NA)) %>%
  dplyr::filter(!is.na(bSubsampleID))

#   Create data frame with subsampleIDs as rows
biorepoDF <- biorepoDF %>%
  dplyr::select(domainID,
                siteID,
                plotID,
                poolSampleID,
                aSubsampleID,
                bSubsampleID) %>%
  tidyr::pivot_longer(cols = aSubsampleID:bSubsampleID,
                      names_to = "poolInput",
                      values_to = "subsampleID")



### Identify sampleIDs with > 1 collectDate
#   Use join to obtain clipID for cores that were pooled for shipping to Biorepository
#   and construct an eventID
bbc_rootmass <- bbc_rootmass %>%
  dplyr::filter(subsampleID %in% biorepoDF$subsampleID) %>%
  dplyr::left_join(bbc_percore %>%
                     dplyr::select(sampleID,
                                   clipID),
                   by = "sampleID") %>%
  dplyr::mutate(eventID = paste("BBC",
                                siteID,
                                lubridate::year(collectDate),
                                sep = "."),
                .before = collectDate)

#   Summarize to quantify number of dates sampled
multiDatePoolInputs <- bbc_rootmass %>%
  dplyr::group_by(domainID, siteID, eventID, plotID, clipID) %>%
  dplyr::summarise(dateCount = length(unique(collectDate))) %>%
  dplyr::filter(dateCount > 1) %>%
  dplyr::mutate(eventClip = paste(eventID,
                                  clipID,
                                  sep = "_"),
                .before = dateCount)



### Filter bbc_rootmass to eventClips with > 1 collectDate
multiDatePoolSamples <- bbc_rootmass %>%
  dplyr::mutate(eventClip = paste(eventID,
                                  clipID,
                                  sep = "_")) %>%
  dplyr::filter(eventClip %in% multiDatePoolInputs$eventClip) %>%
  dplyr::arrange(domainID, siteID, eventClip)



### Filter biorepoDF to records created by pooling samples collected on multiple dates
relabelCandidates <- biorepoDF %>%
  dplyr::filter(subsampleID %in% multiDatePoolSamples$subsampleID) %>%
  dplyr::distinct(domainID, siteID, plotID, poolSampleID)

  #--> 193 samples at Biorepository














