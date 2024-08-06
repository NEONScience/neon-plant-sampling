### Find individualIDs in VST plots with incorrect subplotIDs - i.e., those inconsistent with plotType from TOS Spatial Data

#   Retrieve all VST: Mapping and Tagging [PROD] records
mtQuery <- URLencode(glue::glue('SELECT _record_id, startdate, plotid, subplotid, nestedsubplotid, individualid, tagid, taxonid, pointid, recordtype, stemazimuth, stemdistance FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae" ORDER BY _record_id', .sep = " "))

#   Retrieve Fulcrum data and keep latest
mtData <- get_fulcrum_pageLim(apiToken = fulcrumToken, 
                              sql = mtQuery) %>%
  dplyr::group_by(individualid) %>%
  dplyr::filter(startdate == max(startdate)) %>%
  dplyr::ungroup()

#   Join with plotSpatial
mtData <- mtData %>%
  dplyr::left_join(plotSpatial %>%
                     dplyr::select(domainID,
                                   plotID,
                                   plotType),
                   by = c("plotid" = "plotID"))

#   Identify individuals in large-stature Tower plots with incorrect subplotIDs
largePlotMT <- mtData %>%
  dplyr::filter(plotType == "lgTower",
                subplotid %in% c("41_100", "40_100", "32_100", "31_100") | is.na(subplotid),
                !grepl("^TEMP", individualid)) %>%
  dplyr::arrange(domainID,
                 plotid, 
                 subplotid, 
                 individualid)

#   Identify individuals in small-stature Tower and Distributed plots with incorrect subplotIDs
smallPlotMT <- mtData %>%
  dplyr::filter(plotType == "smTower" | plotType == "distributed",
                !grepl("^TEMP", individualid),
                subplotid %in% c("21_400", "23_400", "39_400", "41_400") | is.na(subplotid)) %>%
  dplyr::arrange(domainID,
                 plotid,
                 subplotid,
                 individualid)

#   Summarize all affected plotIDs
affectedPlots <- largePlotMT %>%
  dplyr::bind_rows(smallPlotMT) %>%
  dplyr::group_by(domainID,
                  plotid,
                  plotType) %>%
  dplyr::summarise(count = n())

#   Affected individuals
affectedIndiv <- largePlotMT %>%
  dplyr::bind_rows(smallPlotMT) %>%
  dplyr::arrange(domainID,
                 plotid,
                 subplotid,
                 individualid) %>%
  dplyr::relocate(domainID,
                  .before = plotid) %>%
  dplyr::relocate(plotType,
                  .after = plotid)


openxlsx::write.xlsx(affectedPlots, 
                     file = "VST_mapTag_errorSubplotID_plotList.xlsx", 
                     colNames = TRUE, 
                     keepNA = TRUE, 
                     na.string = "NA", 
                     firstRow = TRUE)

openxlsx::write.xlsx(affectedIndiv,
                     file = "VST_mapTag_errorSubplotID_indivList.xlsx",
                     colNames = TRUE,
                     keepNA = TRUE,
                     na.string = "NA",
                     firstRow = TRUE)
