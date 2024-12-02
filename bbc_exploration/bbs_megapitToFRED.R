### Reformatting BBS Megapit data for FRED 4.0 inclusion ####
library(neonUtilities)
library(tidyverse)


mpr <- neonUtilities::loadByProduct(dpID = "DP1.10066.001",
                                    include.provisional = TRUE,
                                    check.size = FALSE,
                                    token = Sys.getenv("NEON_TOKEN"))


#   Extract individual data frames
cnDF <- mpr$mpr_carbonNitrogen
incremDF <- mpr$mpr_perdepthincrement
profileDF <- mpr$mpr_perpitprofile
sampleDF <- mpr$mpr_perrootsample


##  Prepare chem data: Take only first analytical replicate
cnDF <- cnDF %>%
  dplyr::group_by(cnSampleID) %>%
  dplyr::arrange(analyticalRepNumber) %>%
  dplyr::slice_head() %>%
  dplyr::ungroup() %>%
  dplyr::select(cnSampleID,
                d15N,
                d13C,
                nitrogenPercent,
                carbonPercent,
                CNratio,
                cnIsotopeQF,
                cnPercentQF)


##  Prepare mass data and join with chem data
sampleDF <- sampleDF %>%
  dplyr::select(sampleID, 
                depthIncrementID,
                rootStatus,
                sizeCategory,
                rootDryMass) %>%
  dplyr::mutate(sizeCategory = dplyr::case_when(sizeCategory == "<=2mm" ~ "0-2",
                                                sizeCategory == ">2mm" ~ "2-10",
                                                sizeCategory == "<=4mm" ~ "0-4",
                                                sizeCategory == ">4mm" ~ "4-10",
                                                TRUE ~ sizeCategory))

massChemDF <- dplyr::left_join(sampleDF,
                               cnDF,
                               by = c("sampleID" = "cnSampleID"))


##  Join mass and chem data with increment data
incremDF <- incremDF %>%
  dplyr::select(pitProfileID,
                depthIncrementID,
                topDepth,
                bottomDepth,
                depthIncrementVolume,
                sampleMethod)

incrMassChemDF <- dplyr::right_join(incremDF,
                                    massChemDF,
                                    by = "depthIncrementID")

#   Calculate sampleArea to generate field needed for FRED
incrMassChemDF <- incrMassChemDF %>%
  dplyr::mutate(sampleArea = depthIncrementVolume / ((bottomDepth - topDepth) / 100),
                .after = bottomDepth) %>%
  dplyr::select(-depthIncrementVolume)


##  Join increment, mass, and chem data with profile data to create final megapit root dataset
profileDF <- profileDF %>%
  dplyr::select(domainID,
                siteID,
                nlcdClass,
                endDate,
                elevation,
                decimalLatitude,
                decimalLongitude,
                pitProfileID) %>%
  dplyr::rename(date = endDate) %>%
  dplyr::group_by(pitProfileID) %>%
  dplyr::arrange(pitProfileID) %>%
  dplyr::slice_head() %>%
  dplyr::ungroup()

mprDF <- dplyr::right_join(profileDF,
                           incrMassChemDF,
                           by = "pitProfileID") %>%
  dplyr::arrange(domainID,
                 siteID,
                 pitProfileID,
                 depthIncrementID) %>%
  dplyr::select(-sampleMethod)








