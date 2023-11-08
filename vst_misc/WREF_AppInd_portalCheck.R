library(dplyr)
library(neonUtilities)


wrefAI <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = "WREF",
                                       startdate = "2021-01",
                                       release = "LATEST",
                                       tabl = "vst_apparentindividual",
                                       check.size = FALSE,
                                       include.provisional = TRUE,
                                       token = Sys.getenv("NEON_TOKEN"))

wrefAI <- wrefAI$vst_apparentindividual

temp <- wrefAI %>%
  dplyr::filter(eventID == "vst_WREF_2022") %>%
  dplyr::distinct(plotID)
