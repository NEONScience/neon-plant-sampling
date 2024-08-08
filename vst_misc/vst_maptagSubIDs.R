### Find individualIDs in VST: Mapping and Tagging [PROD] with incorrect subplotIDs 
### - i.e., those inconsistent with plotType from TOS Spatial Data. No need to correct L0 as subplotID is not
### published in vst_mappingandtagging

library(dplyr)
library(neonOSTools)


##  Retrieve all VST: Mapping and Tagging [PROD] records
#   Define MT query
mtQuery <- URLencode(glue::glue('SELECT _record_id, startdate, plotid, subplotid, nestedsubplotid, individualid, tagid, taxonid, pointid, recordtype, stemazimuth, stemdistance FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae" WHERE', "(cfconlytag IS NULL OR cfconlytag='N') ORDER BY _record_id", .sep = " "))

#   Retrieve Fulcrum data and keep latest
mtData <- neonOSTools::get_fulcrum_data(api_token = Sys.getenv("FULCRUM_TOKEN"), 
                                        sql = mtQuery) %>%
  dplyr::group_by(individualid) %>%
  dplyr::filter(startdate == max(startdate)) %>%
  dplyr::ungroup()


##  Retrieve TOS Spatial Data from Fulcrum
#   Define query
spatialQuery <- URLencode(glue::glue('SELECT domainid, siteid, plotid, plottype, plotsize, mixedsite, randomsubplota, randomsubplotb FROM "eb10727b-bb4d-4778-9a74-8ee03465417e" WHERE', "subtype='basePlot' ORDER BY _record_id", .sep = " "))

#   Retrieve TOS Spatial Data
plotSpatial <- neonOSTools::get_fulcrum_data(api_token = Sys.getenv("FULCRUM_TOKEN"),
                                             sql = spatialQuery) %>%
  dplyr::arrange(domainid, 
                 siteid,
                 plottype,
                 plotid) %>%
  dplyr::mutate(plottype = case_when(plottype == "distributed" ~ "distributed",
                                     plottype == "tower" & mixedsite == TRUE ~ "smTower",
                                     plottype == "tower" & mixedsite == FALSE ~ "lgTower",
                                     TRUE ~ plottype)) %>%
  dplyr::rename(domainID = domainid,
                siteID = siteid,
                plotID = plotid,
                plotType = plottype,
                mixedSite = mixedsite,
                randomSubplotA = randomsubplota,
                randomSubplotB = randomsubplotb)


##  Identify issues and summarize
#   Join with plotSpatial
mtData <- mtData %>%
  dplyr::left_join(plotSpatial %>%
                     dplyr::select(domainID,
                                   plotID,
                                   plotType,
                                   randomSubplotA,
                                   randomSubplotB),
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


##  Summarize all affected plotIDs
affectedPlots <- largePlotMT %>%
  dplyr::bind_rows(smallPlotMT) %>%
  dplyr::group_by(domainID,
                  plotid,
                  plotType,
                  subplotid) %>%
  dplyr::summarise(count = n())


##  Affected individuals
#   Bind large and small
affectedIndiv <- largePlotMT %>%
  dplyr::bind_rows(smallPlotMT) %>%
  dplyr::arrange(domainID,
                 plotid,
                 subplotid,
                 individualid) %>%
  dplyr::relocate(domainID,
                  .before = plotid) %>%
  dplyr::relocate(plotType,
                  randomSubplotA,
                  randomSubplotB,
                  .after = plotid)


##  Affected plotType and subplotID combos
affectedCombos <- affectedIndiv %>%
  dplyr::group_by(plotType,
                  subplotid) %>%
  dplyr::summarise(count = n())


##  Explore data issues
temp <- affectedIndiv %>%
  dplyr::filter(subplotid == "31_100" & plotType == "lgTower")
#--> All but 3 are from DEJU and have randomSubplotA = "21_400"; map data spot check suggests 21_400 is likely correct
#--> others can be assigned to randomSubplotA, no other logic makes sense and no info in vst_apparentindividual

temp <- affectedIndiv %>%
  dplyr::filter(subplotid == "32_100" & plotType == "lgTower")
#--> most have "23_400" as one of the randomSubplots, a few do not; for those that don't, set to NA

temp <- affectedIndiv %>%
  dplyr::filter(subplotid == "40_100" & plotType == "lgTower")
#--> All are tagOnly records from UKFS_043; most of these do not exist in vst_apparentindividual or have unknown subplotID
#--> subplotid = "40_100" is illogical, so set to NA


#--> Most of errors are when subplotid == "41_100" (> 8000)
#   Check whether randomSubplotB == "41_400" when subplotid == "41_100" for these records
temp <- affectedIndiv %>%
  dplyr::filter(subplotid == "41_100" & randomSubplotB == "41_400") #--> majority, 7736 records
#--> For those that are mapped, a spot check revealed that subplotID is correct in vst_apparentindividual, just not in Fulcrum Mapping & Tagging, assign to sublotid = "41_400"

temp <- affectedIndiv %>%
  dplyr::filter(subplotid == "41_100" & randomSubplotB != "41_400") #--> 305 records
#--> All are from HARV 2014 (305 records), and majority of these are "tag only" records
#--> Spot checking multiple records from each plot, none exist in vst_apparentindividual, so set to NA

temp <- affectedIndiv %>%
  dplyr::filter(is.na(subplotid) & plotType == "lgTower") #--> 208 records
#--> Create INC for Field Science to fix? Ignore if individual cannot be found?

  
##  Create proposed fix
affectedIndiv <- affectedIndiv %>%
  dplyr::mutate(fixSubplotID = dplyr::case_when(plotType == "distributed" | plotType == "smTower" ~ "31_400",
                                                plotType == "lgTower" & subplotid == "31_100" & randomSubplotA == "21_400" ~ "21_400",
                                                plotType == "lgTower" & subplotid == "31_100" & randomSubplotA != "21_400" ~ NA,
                                                plotType == "lgTower" & subplotid == "32_100" & 
                                                  (randomSubplotA == "23_400" | randomSubplotB == "23_400") ~ "23_400",
                                                plotType == "lgTower" & subplotid == "32_100" & randomSubplotA != "23_400" &
                                                  randomSubplotB != "23_400" ~ NA,
                                                plotType == "lgTower" & subplotid == "40_100" ~ NA,
                                                plotType == "lgTower" & subplotid == "41_100" & randomSubplotB == "41_400" ~ "41_400",
                                                plotType == "lgTower" & subplotid == "41_100" & randomSubplotB != "41_400" ~ NA,
                                                TRUE ~ NA),
                .after = subplotid)





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
