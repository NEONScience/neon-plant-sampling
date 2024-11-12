###  Download and analysis of NEON Plant Belowground Biomass data for 2024 pre-season seminar ####

### Setup
#   Load required libraries
library(neonUtilities)
library(tidyverse)



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
                wst10cmDist,
                wst1cmDist,
                bareGround,
                litterDepth,
                rootSamplingMethod,
                rootSampleArea,
                rootSampleDepth)


##  Retrieve nlcdClass data for each plotID from Fulcrum and join
spatialTable <- "eb10727b-bb4d-4778-9a74-8ee03465417e"
spatialColumns <- "plotid, nlcdclass"

spatialQuery <- URLencode(glue::glue('SELECT {spatialColumns} FROM "{spatialTable}" WHERE subtype=', "'basePlot' AND plottype='tower' ORDER BY _record_id", .sep = " "))

spatialDF <- neonOSTools::get_fulcrum_data(api_token = Sys.getenv("FULCRUM_PAT"),
                                           sql = spatialQuery)

spatialDF <- spatialDF %>%
  dplyr::rename(plotID = plotid,
                nlcdClass = nlcdclass) %>%
  dplyr::arrange(plotID, 
                 nlcdClass)

bbc_percore <- bbc_percore %>%
  dplyr::left_join(spatialDF,
                   by = "plotID") %>%
  dplyr::relocate(nlcdClass, .after = plotID)


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
                                   nlcdClass,
                                   rootSampleArea),
                   by = "sampleID") %>%
  dplyr::mutate(rootMassArea = round(dryMass/rootSampleArea,
                                     digits = 2)) %>%
  dplyr::relocate(nlcdClass, .after = plotID)



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
                                   nlcdClass,
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

#   Create jitter plot by sizeCategory for each domainID
massPlot <- ggplot2::ggplot(bbc_allmass %>%
                              dplyr::filter(!is.na(nlcdClass)),
                            aes(x = rootMassArea,
                                y = sizeCategory)) +
  ggplot2::geom_jitter() +
  ggplot2::ylab("Size Category (mm diameter)") +
  ggplot2::xlab("Root Mass (g/m2)") +
  ggplot2::facet_wrap(~nlcdClass,
                      ncol = 3,
                      scales = "free")



### Plot average mass within sizeCategory by siteID against MAP, MAT
#   Calculate average mass within sizeCategory by siteID
bbc_summarymass <- bbc_allmass %>%
  dplyr::group_by(siteID,
                  #nlcdClass,
                  sizeCategory) %>%
  dplyr::summarise(repNum = n(),
                   meanDryMass = round(mean(rootMassArea, na.rm = TRUE),
                                   digits = 2),
                   seDryMass = round(sd(rootMassArea, na.rm = TRUE)/sqrt(repNum),
                                     digits = 2),
                   .groups = "drop")

#   Retrieve MAP, MAT and join with summary mass data
neonClimateDF <- read.csv(file = "NEON_Site_Metadata_20240725.csv",
                          header = TRUE) %>%
  dplyr::rename(siteID = field_site_id,
                meanAnnualTemp = field_mean_annual_temperature_C,
                meanAnnualPrecip = field_mean_annual_precipitation_mm) %>%
  dplyr::select(siteID,
                meanAnnualTemp,
                meanAnnualPrecip) %>%
  dplyr::mutate(meanAnnualTemp = dplyr::case_when(meanAnnualTemp == "" ~ NA,
                                                  TRUE ~ stringr::str_extract(meanAnnualTemp, pattern = "^[^°]*")),
                meanAnnualTemp = as.numeric(meanAnnualTemp))

#   Join climate data to bbc_summarymass
bbc_summarymass <- bbc_summarymass %>%
  dplyr::left_join(neonClimateDF,
                   by = "siteID")

#   Plot mass vs MAT by sizeCategory 
massMAT <- ggplot2::ggplot(bbc_summarymass %>%
                             dplyr::filter(sizeCategory != "frag"),
                           aes(x = meanAnnualTemp,
                               y = meanDryMass)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm",
                       se = TRUE,
                       color = "blue",
                       formula = y ~ x) +
  ggplot2::facet_wrap(~sizeCategory,
                      ncol = 3,
                      scales = "fixed") +
  ggplot2::labs(x = "Mean annual temp (°C)",
                y = "Mean dry mass (g/m2)")

#   Plot mass vs MAP by sizeCategory
massMAP <- ggplot2::ggplot(bbc_summarymass %>%
                             dplyr::filter(sizeCategory != "frag"),
                           aes(x = meanAnnualPrecip,
                               y = meanDryMass)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm",
                       se = TRUE,
                       color = "blue",
                       formula = y ~ x) +
  ggplot2::facet_wrap(~sizeCategory,
                      ncol = 3,
                      scales = "fixed") +
  ggplot2::labs(x = "Mean annual precip (mm)",
                y = "Mean dry mass (g/m2)")








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
  dplyr::summarise(nitrogenPercent = round(mean(nitrogenPercent, na.rm = TRUE),
                                           digits = 2),
                   carbonPercent = round(mean(carbonPercent, na.rm = TRUE),
                                         digits = 2),
                   CNratio = round(mean(CNratio, na.rm = TRUE),
                                   digits = 1),
                   .groups = "drop")

#   Join with bbc_chempool to get subsampleID, which enables join with bbc_rootmass to get sizeCategory;
#   filter out 'dead' roots with no chemistry --> presents some difficulties comparing older chemistry 
#   values with new ones derived from material that is a mix of live/dead
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
  dplyr::summarise(nitrogenPercent = round(sum(partialN, na.rm = TRUE),
                                           digits = 2),
                   carbonPercent = round(sum(partialC, na.rm = TRUE),
                                         digits = 2),
                   CNratio = round(sum(partialCN, na.rm = TRUE),
                                   digits = 1),
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
                     dplyr::select(nlcdClass,
                                   sampleID,
                                   clipID),
                   by = "sampleID") %>%
  dplyr::relocate(nlcdClass,
                  clipID,
                  .after = plotID) %>%
  dplyr::group_by(domainID,
                  siteID,
                  plotID,
                  nlcdClass,
                  clipID,
                  sizeCategory) %>%
  dplyr::summarise(nitrogenPercent = round(mean(nitrogenPercent, na.rm = TRUE),
                                           digits = 2),
                   carbonPercent = round(mean(carbonPercent, na.rm = TRUE),
                                         digits = 2),
                   CNratio = round(mean(CNratio, na.rm = TRUE),
                                   digits = 1),
                   .groups = "drop")

#   Join with site-specific MAT, MAP
bbc_masschem <- bbc_masschem %>%
  dplyr::left_join(neonClimateDF,
                   by = "siteID")



### Construct chemistry ggplots by sizeCategory with siteID panels
#   Create CN Ratio by sizeCategory plot
cnPlot <- ggplot2::ggplot(bbc_masschem, 
                          ggplot2::aes(x = CNratio,
                                       y = sizeCategory)) +
  ggplot2::geom_jitter() +
  ggplot2::ylab("Size category (mm diameter)") +
  ggplot2::xlab("Root C:N ratio") +
  ggplot2::facet_wrap(~nlcdClass, 
                      ncol = 3, 
                      scales = "free") 

#   Create summarychem table with mean CN ratio by sizeCategory by site
bbc_summarychem <- bbc_masschem %>%
  dplyr::group_by(siteID,
                  meanAnnualPrecip,
                  meanAnnualTemp,
                  sizeCategory) %>%
  dplyr::summarise(repNum = n(),
                   meanCN = round(mean(CNratio, na.rm = TRUE),
                                       digits = 1),
                   seCN = round(sd(CNratio, na.rm = TRUE)/sqrt(repNum),
                                     digits = 2),
                   .groups = "drop")

#   Plot CNratio vs MAT by sizeCategory 
cnMAT <- ggplot2::ggplot(bbc_summarychem,
                           aes(x = meanAnnualTemp,
                               y = meanCN)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm",
                       se = TRUE,
                       color = "blue",
                       formula = y ~ x) +
  ggplot2::facet_wrap(~sizeCategory,
                      ncol = 3,
                      scales = "fixed") +
  ggplot2::labs(x = "Mean annual temp (°C)",
                y = "Mean C:N ratio")

#   Plot CNratio vs MAP by sizeCategory
cnMAP <- ggplot2::ggplot(bbc_summarychem,
                         aes(x = meanAnnualPrecip,
                             y = meanCN)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm",
                       se = TRUE,
                       color = "blue",
                       formula = y ~ x) +
  ggplot2::facet_wrap(~sizeCategory,
                      ncol = 3,
                      scales = "fixed") +
  ggplot2::labs(x = "Mean annual precip (mm)",
                y = "Mean C:N ratio")

  
  