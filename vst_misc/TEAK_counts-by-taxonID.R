library(tidyverse)
library(neonUtilities)

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


### Clean M&T data
mtDF <- mtDF %>%
  dplyr::group_by(individualID) %>%
  dplyr::arrange(date) %>%
  dplyr::slice_tail()



### Join aiDF and mtDF to get taxonID
aiDF <- aiDF %>%
  dplyr::filter(eventID %in% c("vst_TEAK_2021", "vst_TEAK_2022")) %>%
  dplyr::left_join(mtDF %>%
                     dplyr::select(individualID, taxonID),
                   by = "individualID") %>%
  dplyr::select(siteID,
                plotID,
                individualID,
                taxonID,
                growthForm,
                plantStatus,
                tagStatus)

temp <- aiDF %>%
  dplyr::filter(is.na(taxonID))
