### Estimate savings from Dilution Sample optimization in 2023

library(dplyr)
library(neonUtilities)


### Retrieve BBC data from the Portal for 2023
bbc <- neonUtilities::loadByProduct(dpID = "DP1.10067.001",
                                    site = "all",
                                    startdate = "2023-01",
                                    enddate = "2024-05",
                                    check.size = FALSE,
                                    include.provisional = TRUE,
                                    token = Sys.getenv("NEON_TOKEN"))

#   Extract bbc_dilution table and bbc_percore for eventID
coreDF <- bbc$bbc_percore
dilDF <- bbc$bbc_dilution

dilDF <- dilDF %>%
  dplyr::left_join(coreDF %>%
                     dplyr::select(sampleID,
                                   eventID),
                   by = "sampleID") %>%
  dplyr::relocate(eventID,
                  .after = siteID)

#   Find records from 2023
dilDF <- dilDF %>%
  dplyr::filter(grepl("2023$", eventID))

dilNumber <- length(unique(dilDF$sampleID))

priorLaborHours <- dilNumber * 10 * 0.25
priorLaborCost <- priorLaborHours * 35

currLaborHours <- dilNumber * 3 * 0.25
currLaborCost <- currLaborHours * 35

