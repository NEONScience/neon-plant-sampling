library(neonUtilities)
library(tidyverse)


### Retrieve 2021 and 2022 VST data for TEAK ####
vst <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                    site = "TEAK",
                                    startdate = "2020-01",
                                    enddate = "2023-12",
                                    tabl = "all",
                                    check.size = FALSE,
                                    token = Sys.getenv("NEON_TOKEN"))


aiDF <- vst$vst_apparentindividual
mtDF <- vst$vst_mappingandtagging
pmDF <- vst$vst_perplotperyear


### Clean M&T data to retain most recent record per individualID
mtDF <- mtDF %>%
  dplyr::group_by(individualID) %>%
  dplyr::arrange(date) %>%
  dplyr::slice_tail()



### Filter aiDF to eventIDs of interest then join aiDF and mtDF to get taxonID
aiDF <- aiDF %>%
  dplyr::filter(eventID %in% c("vst_TEAK_2021", "vst_TEAK_2022")) %>%
  dplyr::left_join(mtDF %>%
                     dplyr::select(individualID, taxonID),
                   by = "individualID") %>%
  dplyr::select(siteID,
                plotID,
                subplotID,
                eventID,
                individualID,
                taxonID,
                growthForm,
                plantStatus,
                tagStatus,
                stemDiameter)



### Join aiDF with pmDF to obtain 'plotType', 'totalSampledAreaTrees', and 'totalSampledAreaShrubSapling' by plotID x eventID
aiDF <- aiDF %>%
  dplyr::left_join(pmDF %>%
                     dplyr::select(plotID, 
                                   eventID,
                                   eventType,
                                   dataCollected,
                                   plotType, 
                                   totalSampledAreaTrees,
                                   totalSampledAreaShrubSapling),
                   by = c("plotID", "eventID")) %>%
  dplyr::relocate("eventType",
                  "dataCollected",
                  "plotType",
                  "totalSampledAreaTrees",
                  "totalSampledAreaShrubSapling",
                  .before = "individualID")



### Calculate "ABH_cmm2" (cm2/m2) for individuals with growthForm == 'single bole tree', 'multi-bole tree', 'small tree', 
### and 'single shrub', as this is comparable to calculation made for Site Characterization Report; filter out 
### dataCollected == "dendrometerOnly" because data not collected from all trees in plot when this condition is true.
abhDF <- aiDF %>%
  dplyr::filter(growthForm %in% c("single bole tree", "multi-bole tree", "small tree", "single shrub"),
                dataCollected != "dendrometerOnly") %>%
  dplyr::mutate(ABH_cmm2 = dplyr::case_when(growthForm %in% c("single bole tree", "multi-bole tree") ~
                                              round((pi * (stemDiameter / 2) ^ 2) / totalSampledAreaTrees,
                                                    digits = 3),
                                            growthForm %in% c("small tree", "single shrub") ~
                                              round((pi * (stemDiameter / 2) ^ 2) / totalSampledAreaShrubSapling,
                                                    digits = 3)))



### Summarize "ABH_cmm2" by siteID, plotID, plotType, eventID, and taxonID
abhPlotEventDF <- abhDF %>%
  dplyr::group_by(siteID,
                  plotID,
                  plotType,
                  eventID,
                  taxonID) %>%
  dplyr::summarise(ABH_cmm2 = round(sum(ABH_cmm2, na.rm = TRUE),
                                    digits = 1),
                   .groups = "drop") %>%
  dplyr::arrange(siteID,
                 eventID,
                 plotType,
                 plotID,
                 taxonID)



### Calculate mean "ABH_cmm2" for taxonIDs by plotType and eventID
abhSummaryDF <- abhPlotEventDF %>%
  dplyr::group_by(siteID,
                  plotType,
                  taxonID,
                  eventID) %>%
  dplyr::summarise(meanABH_cmm2 = round(mean(ABH_cmm2, na.rm = TRUE),
                                        digits = 1),
                   plotNum = n(),
                   .groups = "drop")

#--> Note that compared to the Tower plot dataset used for the D17 TEAK Site Characterization Report (n = 20 plots), the
#--> number of Tower plots with complete sampling in 2021 and 2022 at TEAK is much smaller (n = 5 plots). This reduced sample size 
#--> likely explains the slightly different numbers for ABMA, ABCO, and PICO.








