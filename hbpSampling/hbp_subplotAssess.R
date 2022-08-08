### HBP: Evaluating geoNEON location calculation accuracy
### Verify which subplotIDs are reported in Portal data

library(dplyr)
library(neonUtilities)

#   Retrieve all Portal data for hbp_perbout table
hbp_perbout <- neonUtilities::loadByProduct(
  dpID = "DP1.10023.001",
  site = "all",
  package = "basic",
  release = "current",
  tabl = "hbp_perbout",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
)

hbp_perbout <- hbp_perbout$hbp_perbout

#   Retrieve TOS Spatial Data to bring in plotType and mixedsite data
hbpQuery <-
  URLencode(
    glue::glue(
      'SELECT domainid, siteid, plotid, plottype, subtype, plotsize, mixedsite FROM "TOS Spatial Data" WHERE subtype =',
      "'basePlot' ORDER BY _record_id",
      .sep = " "
    )
  )

spatial <- get_fulcrum_pageLim(
  apiToken = Sys.getenv('FULCRUM_PAT'),
  sql = hbpQuery
) %>%
  dplyr::arrange(domainid, siteid, plotid)



subplotSumm <- hbp_perbout %>%
  dplyr::left_join(
    spatial %>% dplyr::select(-plotsize, -subtype),
    by = c(
      "domainID" = "domainid",
      "siteID" = "siteid",
      "plotID" = "plotid"
    )
  ) %>%
  dplyr::group_by(plottype, mixedsite, subplotID) %>%
  dplyr::summarise(
    count = n()
  )