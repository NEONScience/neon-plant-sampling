### Goal: Determine whether largest VST particles in MOAB Tower plots would qualify for CDW Tally
### if they were downed

library(neonUtilities)
library(tidyverse)



### Retrieve MOAB VST data
vst <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                    site = "MOAB",
                                    tabl = "all",
                                    check.size = FALSE,
                                    include.provisional = TRUE,
                                    token = Sys.getenv("NEON_TOKEN"))

pmdDF <- vst$vst_perplotperyear
aiDF <- vst$vst_apparentindividual

#   Identify latest year with data from all Tower plots
events <- dplyr::distinct(pmdDF,
                          eventID,
                          eventType)

#--> 2021 was last allTowerPlot year

vstDF <- dplyr::right_join(pmdDF %>%
                             dplyr::select(plotID, eventID, plotType, dataCollected),
                           aiDF,
                           by = c("plotID", "eventID"))

summaryDF <- vstDF %>%
  dplyr::filter(eventID == "vst_MOAB_2021",
                plotType == "tower") %>%
  dplyr::group_by(plotID) %>%
  dplyr::summarise(maxDiam = max(stemDiameter, na.rm = TRUE),
                   maxBasalDiam = max(basalStemDiameter, na.rm = TRUE))

#--> no MOAB Tower plots have any individuals with a stemDiameter (all have DBH < 1 cm).
#--> CDW Tally should continue to be scheduled as "assess"