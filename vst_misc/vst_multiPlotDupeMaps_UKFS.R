### UKFS: Identify mapped individuals for which duplicate tags were removed with no traceability

#   Load required libraries
library(DBI)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(glue)
library(openxlsx)
library(pool)
library(restR2)
library(RPostgres)

#   Use straight quotes
options(useFancyQuotes = FALSE)



### Setup ####
### Connect to vstqaqc database
conPool <- pool::dbPool(drv = RPostgres::Postgres(),
                        dbname = "vstqaqc",
                        host = Sys.getenv("DB_HOST"),
                        port = 5432,
                        user = "shiny_vstqaqc_ro",
                        password = Sys.getenv("DB_TOKEN"),
                        onCreate = \(con) DBI::dbExecute(con, 'SET search_path TO vstqaqc;'))



### Radians function: Convert azimuth degrees to radians
radians <- function(degrees) {
  rad = (degrees*pi)/180
  return(rad) 	
}



### Read in pointSpatial data from Peregrine
pointSpatial <- readRDS("../shiny-vst-qaqc/app/src/data/pointSpatialData_20241029.RDS")



### UKFS_049 map with 2021 and 2016 data ####
### Get Fulcrum Mapping and Tagging data: Marian temporarily restored older UKFS records
mtQuery <- glue::glue('SELECT _record_id, startdate, plotid, individualid, tagid, taxonid, pointid, recordtype, stemazimuth, stemdistance FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae" WHERE plotid =', "'UKFS_049' ORDER BY _record_id", .sep = " ")

mt049 <- restR2::get.fulcrum.sql(apiToken = Sys.getenv("FULCRUM_TOKEN"),
                                 sql = mtQuery,
                                 urlEncode = TRUE)

#   Retain most recent record by individualID, then keep only those individuals with mapping data
mt049 <- mt049 %>%
  dplyr::group_by(individualid) %>%
  dplyr::arrange(startdate) %>%
  dplyr::slice_tail() %>%
  dplyr::ungroup()

mt049 <- mt049 %>%
  dplyr::filter(!dplyr::if_all(c(pointid, stemazimuth, stemdistance), is.na))

#   Create plotPointID then join with pointSpatial data
mt049 <- mt049 %>%
  dplyr::mutate(plotPointID = paste(plotid, pointid, sep = "_"),
                .after = plotid)

mt049 <- dplyr::left_join(mt049,
                          pointSpatial %>%
                            dplyr::select(plotPointID, pointEasting, pointNorthing),
                          by = "plotPointID")

#   Calculate stem easting and northing data
mt049 <- mt049 %>%
  dplyr::mutate(stemEasting = round(pointEasting + stemdistance*sin(radians(stemazimuth)), 
                                    digits = 2),
                stemNorthing = round(pointNorthing + stemdistance*cos(radians(stemazimuth)), 
                                     digits = 2))



### Get apparentindividual data from 2016 and 2021
#   Construct vstqaqc.apparentindividual query
aiQuery <- glue::glue("SELECT plotid, subplotid, eventid, individualid, tempstemid, tagstatus, growthform, plantstatus, stemdiameter, height FROM vstqaqc.apparentindividual WHERE plotid='UKFS_049' AND eventid IN ('vst_UKFS_2021', 'vst_UKFS_2016') ORDER BY eventid, subplotid, individualid;")

#   Retrieve vstqaqc.apparentindividual data for 2016 and 2021
ai049 <- RPostgres::dbGetQuery(conn = conPool,
                               statement = aiQuery)

#   Join with Fulcrum M&T data
map049 <- dplyr::left_join(ai049,
                           mt049 %>%
                             dplyr::select(individualid, tagid, taxonid, stemEasting, stemNorthing),
                           by = "individualid") %>%
  dplyr::filter(!dplyr::if_all(c(stemEasting, stemNorthing), is.na))



### Generate base map using subplot points from expected random subplots (21_400, 39_400)
cornerPoints <- c(21,23,39,41,57,59)

mapPoints <- pointSpatial %>%
  dplyr::filter(plotID == "UKFS_049",
                pointID %in% cornerPoints)

ggMap049 <- ggplot(mapPoints, 
                aes(pointEasting, 
                    pointNorthing)) +
  
  #   Set the aspect ratio using the mapPoint data
  coord_fixed() +
  
  #   Add axis labels
  labs(x = "Easting (meters)",
       y = "Northing (meters)",
       title = "UKFS_049") +
  
  #   Remove axis and grid lines from panel
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12, 
                                  face = "bold")) +
  theme(legend.text = element_text(size = 10)) +
  theme(title = element_text(size = 14)) +
  
  #   Set axis ticks to begin at minimum easting and northing values in mapPoints, and space every 5 meters
  scale_x_continuous(breaks = seq(round(min(mapPoints$pointEasting)), 
                                  max(mapPoints$pointEasting), 
                                  5)) +
  scale_y_continuous(breaks = seq(round(min(mapPoints$pointNorthing)), 
                                  max(mapPoints$pointNorthing),
                                  5)) +
  
  geom_label(aes(label = pointID), 
             fill = "red", 
             colour="white", 
             size = 2, 
             show.legend = FALSE) +
  
  #   Add points from 2021 with stemDiameter
  geom_point(data = map049 %>%
               dplyr::filter(eventid == "vst_UKFS_2021",
                             growthform %in% c("single bole tree", "multi-bole tree")), 
             aes(x = stemEasting, 
                 y = stemNorthing, 
                 color = taxonid, 
                 size = stemdiameter),
             shape = 21, 
             stroke = 0.5, 
             show.legend = TRUE) +
  
  #   Add points from 2016 as solid filled circles
  geom_point(data = map049 %>%
               dplyr::filter(eventid == "vst_UKFS_2016",
                             growthform %in% c("single bole tree", "multi-bole tree")),
             aes(x = stemEasting,
                 y = stemNorthing,
                 color = taxonid,
                 size = stemdiameter/2),
             shape = 19,
             show.legend = FALSE) +
  
  #   Add tagID labels for all individuals
  ggrepel::geom_text_repel(data = map049 %>%
                             dplyr::filter(growthform %in% c("single bole tree", "multi-bole tree")), 
                           aes(x = stemEasting, 
                               y = stemNorthing, 
                               label = tagid), 
                           size = 2,
                           nudge_x = 0.3, 
                           nudge_y = 0.3,
                           max.overlaps = 20)



### UKFS_053 map with 2021 and 2016 data ####
### Get Fulcrum Mapping and Tagging data: Marian temporarily restored older UKFS records
mtQuery <- glue::glue('SELECT _record_id, startdate, plotid, individualid, tagid, taxonid, pointid, recordtype, stemazimuth, stemdistance FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae" WHERE plotid =', "'UKFS_053' ORDER BY _record_id", .sep = " ")

mt053 <- restR2::get.fulcrum.sql(apiToken = Sys.getenv("FULCRUM_TOKEN"),
                                 sql = mtQuery,
                                 urlEncode = TRUE)

#   Retain most recent record by individualID, then keep only those individuals with mapping data
mt053 <- mt053 %>%
  dplyr::group_by(individualid) %>%
  dplyr::arrange(startdate) %>%
  dplyr::slice_tail() %>%
  dplyr::ungroup()

mt053 <- mt053 %>%
  dplyr::filter(!dplyr::if_all(c(pointid, stemazimuth, stemdistance), is.na))

#   Create plotPointID then join with pointSpatial data
mt053 <- mt053 %>%
  dplyr::mutate(plotPointID = paste(plotid, pointid, sep = "_"),
                .after = plotid)

mt053 <- dplyr::left_join(mt053,
                          pointSpatial %>%
                            dplyr::select(plotPointID, pointEasting, pointNorthing),
                          by = "plotPointID")

#   Calculate stem easting and northing data
mt053 <- mt053 %>%
  dplyr::mutate(stemEasting = round(pointEasting + stemdistance*sin(radians(stemazimuth)), 
                                    digits = 2),
                stemNorthing = round(pointNorthing + stemdistance*cos(radians(stemazimuth)), 
                                     digits = 2))



### Get apparentindividual data from 2016 and 2021
#   Construct vstqaqc.apparentindividual query
aiQuery <- glue::glue("SELECT plotid, subplotid, eventid, individualid, tempstemid, tagstatus, growthform, plantstatus, stemdiameter, height FROM vstqaqc.apparentindividual WHERE plotid='UKFS_053' AND eventid IN ('vst_UKFS_2021', 'vst_UKFS_2016') ORDER BY eventid, subplotid, individualid;")

#   Retrieve vstqaqc.apparentindividual data for 2016 and 2021
ai053 <- RPostgres::dbGetQuery(conn = conPool,
                               statement = aiQuery)

#   Join with Fulcrum M&T data
map053 <- dplyr::left_join(ai053,
                           mt053 %>%
                             dplyr::select(individualid, tagid, taxonid, stemEasting, stemNorthing),
                           by = "individualid") %>%
  dplyr::filter(!dplyr::if_all(c(stemEasting, stemNorthing), is.na))



### Generate base map using subplot points from expected random subplots (23_400, 41_400)
cornerPoints <- c(23,25,41,43,59,61)

mapPoints <- pointSpatial %>%
  dplyr::filter(plotID == "UKFS_053",
                pointID %in% cornerPoints)

ggMap053 <- ggplot(mapPoints, 
                   aes(pointEasting, 
                       pointNorthing)) +
  
  #   Set the aspect ratio using the mapPoint data
  coord_fixed() +
  
  #   Add axis labels
  labs(x = "Easting (meters)",
       y = "Northing (meters)",
       title = "UKFS_053") +
  
  #   Remove axis and grid lines from panel
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12, 
                                  face = "bold")) +
  theme(legend.text = element_text(size = 10)) +
  theme(title = element_text(size = 14)) +
  
  #   Set axis ticks to begin at minimum easting and northing values in mapPoints, and space every 5 meters
  scale_x_continuous(breaks = seq(round(min(mapPoints$pointEasting)), 
                                  max(mapPoints$pointEasting), 
                                  5)) +
  scale_y_continuous(breaks = seq(round(min(mapPoints$pointNorthing)), 
                                  max(mapPoints$pointNorthing),
                                  5)) +
  
  geom_label(aes(label = pointID), 
             fill = "red", 
             colour="white", 
             size = 2, 
             show.legend = FALSE) +
  
  #   Add points from 2021 with stemDiameter
  geom_point(data = map053 %>%
               dplyr::filter(eventid == "vst_UKFS_2021",
                             growthform %in% c("single bole tree", "multi-bole tree")), 
             aes(x = stemEasting, 
                 y = stemNorthing, 
                 color = taxonid, 
                 size = stemdiameter),
             shape = 21, 
             stroke = 0.5, 
             show.legend = TRUE) +
  
  #   Add points from 2016 as solid filled circles
  geom_point(data = map053 %>%
               dplyr::filter(eventid == "vst_UKFS_2016",
                             growthform %in% c("single bole tree", "multi-bole tree")),
             aes(x = stemEasting,
                 y = stemNorthing,
                 color = taxonid,
                 size = stemdiameter/2),
             shape = 19,
             show.legend = FALSE) +
  
  #   Add tagID labels for all individuals
  ggrepel::geom_text_repel(data = map053 %>%
                             dplyr::filter(growthform %in% c("single bole tree", "multi-bole tree")), 
                           aes(x = stemEasting, 
                               y = stemNorthing, 
                               label = tagid), 
                           size = 2,
                           nudge_x = 0.3, 
                           nudge_y = 0.3,
                           max.overlaps = 20)



### UKFS_059 map with 2021 and 2017 data ####
### Get Fulcrum Mapping and Tagging data: Marian temporarily restored older UKFS records
mtQuery <- glue::glue('SELECT _record_id, startdate, plotid, individualid, tagid, taxonid, pointid, recordtype, stemazimuth, stemdistance FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae" WHERE plotid =', "'UKFS_059' ORDER BY _record_id", .sep = " ")

mt059 <- restR2::get.fulcrum.sql(apiToken = Sys.getenv("FULCRUM_TOKEN"),
                                 sql = mtQuery,
                                 urlEncode = TRUE)

#   Retain most recent record by individualID, then keep only those individuals with mapping data
mt059 <- mt059 %>%
  dplyr::group_by(individualid) %>%
  dplyr::arrange(startdate) %>%
  dplyr::slice_tail() %>%
  dplyr::ungroup()

mt059 <- mt059 %>%
  dplyr::filter(!dplyr::if_all(c(pointid, stemazimuth, stemdistance), is.na))

#   Create plotPointID then join with pointSpatial data
mt059 <- mt059 %>%
  dplyr::mutate(plotPointID = paste(plotid, pointid, sep = "_"),
                .after = plotid)

mt059 <- dplyr::left_join(mt059,
                          pointSpatial %>%
                            dplyr::select(plotPointID, pointEasting, pointNorthing),
                          by = "plotPointID")

#   Calculate stem easting and northing data
mt059 <- mt059 %>%
  dplyr::mutate(stemEasting = round(pointEasting + stemdistance*sin(radians(stemazimuth)), 
                                    digits = 2),
                stemNorthing = round(pointNorthing + stemdistance*cos(radians(stemazimuth)), 
                                     digits = 2))



### Get apparentindividual data from 2017 and 2021
#   Construct vstqaqc.apparentindividual query
aiQuery <- glue::glue("SELECT plotid, subplotid, eventid, individualid, tempstemid, tagstatus, growthform, plantstatus, stemdiameter, height FROM vstqaqc.apparentindividual WHERE plotid='UKFS_059' AND eventid IN ('vst_UKFS_2021', 'vst_UKFS_2017') ORDER BY eventid, subplotid, individualid;")

#   Retrieve vstqaqc.apparentindividual data for 2017 and 2021
ai059 <- RPostgres::dbGetQuery(conn = conPool,
                               statement = aiQuery)

#   Join with Fulcrum M&T data
map059 <- dplyr::left_join(ai059,
                           mt059 %>%
                             dplyr::select(individualid, tagid, taxonid, stemEasting, stemNorthing),
                           by = "individualid") %>%
  dplyr::filter(!dplyr::if_all(c(stemEasting, stemNorthing), is.na))



### Generate base map using subplot points from expected random subplots (21_400, 23_400)
cornerPoints <- c(21,23,39,41,25,43)

mapPoints <- pointSpatial %>%
  dplyr::filter(plotID == "UKFS_059",
                pointID %in% cornerPoints)

ggMap059 <- ggplot(mapPoints, 
                   aes(pointEasting, 
                       pointNorthing)) +
  
  #   Set the aspect ratio using the mapPoint data
  coord_fixed() +
  
  #   Add axis labels
  labs(x = "Easting (meters)",
       y = "Northing (meters)",
       title = "UKFS_059") +
  
  #   Remove axis and grid lines from panel
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12, 
                                  face = "bold")) +
  theme(legend.text = element_text(size = 10)) +
  theme(title = element_text(size = 14)) +
  
  #   Set axis ticks to begin at minimum easting and northing values in mapPoints, and space every 5 meters
  scale_x_continuous(breaks = seq(round(min(mapPoints$pointEasting)), 
                                  max(mapPoints$pointEasting), 
                                  5)) +
  scale_y_continuous(breaks = seq(round(min(mapPoints$pointNorthing)), 
                                  max(mapPoints$pointNorthing),
                                  5)) +
  
  geom_label(aes(label = pointID), 
             fill = "red", 
             colour="white", 
             size = 2, 
             show.legend = FALSE) +
  
  #   Add points from 2021 with stemDiameter
  geom_point(data = map059 %>%
               dplyr::filter(eventid == "vst_UKFS_2021",
                             growthform %in% c("single bole tree", "multi-bole tree")), 
             aes(x = stemEasting, 
                 y = stemNorthing, 
                 color = taxonid, 
                 size = stemdiameter),
             shape = 21, 
             stroke = 0.5, 
             show.legend = TRUE) +
  
  #   Add points from 2017 as solid filled circles
  geom_point(data = map059 %>%
               dplyr::filter(eventid == "vst_UKFS_2017",
                             growthform %in% c("single bole tree", "multi-bole tree")),
             aes(x = stemEasting,
                 y = stemNorthing,
                 color = taxonid,
                 size = stemdiameter/2),
             shape = 19,
             show.legend = FALSE) +
  
  #   Add tagID labels for all individuals
  ggrepel::geom_text_repel(data = map059 %>%
                             dplyr::filter(growthform %in% c("single bole tree", "multi-bole tree")), 
                           aes(x = stemEasting, 
                               y = stemNorthing, 
                               label = tagid), 
                           size = 2,
                           nudge_x = 0.3, 
                           nudge_y = 0.3,
                           max.overlaps = 20)



### UKFS_059 map with 2021 and 2016 data #### --> don't use this one, use map with 2021 and 2016 data
### Get Fulcrum Mapping and Tagging data: Marian temporarily restored older UKFS records
mtQuery <- glue::glue('SELECT _record_id, startdate, plotid, individualid, tagid, taxonid, pointid, recordtype, stemazimuth, stemdistance FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae" WHERE plotid =', "'UKFS_059' ORDER BY _record_id", .sep = " ")

mt059 <- restR2::get.fulcrum.sql(apiToken = Sys.getenv("FULCRUM_TOKEN"),
                                 sql = mtQuery,
                                 urlEncode = TRUE)

#   Retain most recent record by individualID, then keep only those individuals with mapping data
mt059 <- mt059 %>%
  dplyr::group_by(individualid) %>%
  dplyr::arrange(startdate) %>%
  dplyr::slice_tail() %>%
  dplyr::ungroup()

mt059 <- mt059 %>%
  dplyr::filter(!dplyr::if_all(c(pointid, stemazimuth, stemdistance), is.na))

#   Create plotPointID then join with pointSpatial data
mt059 <- mt059 %>%
  dplyr::mutate(plotPointID = paste(plotid, pointid, sep = "_"),
                .after = plotid)

mt059 <- dplyr::left_join(mt059,
                          pointSpatial %>%
                            dplyr::select(plotPointID, pointEasting, pointNorthing),
                          by = "plotPointID")

#   Calculate stem easting and northing data
mt059 <- mt059 %>%
  dplyr::mutate(stemEasting = round(pointEasting + stemdistance*sin(radians(stemazimuth)), 
                                    digits = 2),
                stemNorthing = round(pointNorthing + stemdistance*cos(radians(stemazimuth)), 
                                     digits = 2))



### Get apparentindividual data from 2016 and 2021
#   Construct vstqaqc.apparentindividual query
aiQuery <- glue::glue("SELECT plotid, subplotid, eventid, individualid, tempstemid, tagstatus, growthform, plantstatus, stemdiameter, height FROM vstqaqc.apparentindividual WHERE plotid='UKFS_059' AND eventid IN ('vst_UKFS_2021', 'vst_UKFS_2016') ORDER BY eventid, subplotid, individualid;")

#   Retrieve vstqaqc.apparentindividual data for 2016 and 2021
ai059 <- RPostgres::dbGetQuery(conn = conPool,
                               statement = aiQuery)

#   Join with Fulcrum M&T data
map059 <- dplyr::left_join(ai059,
                           mt059 %>%
                             dplyr::select(individualid, tagid, taxonid, stemEasting, stemNorthing),
                           by = "individualid") %>%
  dplyr::filter(!dplyr::if_all(c(stemEasting, stemNorthing), is.na))



### Generate base map using subplot points from expected random subplots (21_400, 23_400)
cornerPoints <- c(21,23,39,41,25,43)

mapPoints <- pointSpatial %>%
  dplyr::filter(plotID == "UKFS_059",
                pointID %in% cornerPoints)

ggMap059_2016 <- ggplot(mapPoints, 
                   aes(pointEasting, 
                       pointNorthing)) +
  
  #   Set the aspect ratio using the mapPoint data
  coord_fixed() +
  
  #   Add axis labels
  labs(x = "Easting (meters)",
       y = "Northing (meters)",
       title = "UKFS_059") +
  
  #   Remove axis and grid lines from panel
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12, 
                                  face = "bold")) +
  theme(legend.text = element_text(size = 10)) +
  theme(title = element_text(size = 14)) +
  
  #   Set axis ticks to begin at minimum easting and northing values in mapPoints, and space every 5 meters
  scale_x_continuous(breaks = seq(round(min(mapPoints$pointEasting)), 
                                  max(mapPoints$pointEasting), 
                                  5)) +
  scale_y_continuous(breaks = seq(round(min(mapPoints$pointNorthing)), 
                                  max(mapPoints$pointNorthing),
                                  5)) +
  
  geom_label(aes(label = pointID), 
             fill = "red", 
             colour="white", 
             size = 2, 
             show.legend = FALSE) +
  
  #   Add points from 2021 with stemDiameter
  geom_point(data = map059 %>%
               dplyr::filter(eventid == "vst_UKFS_2021",
                             growthform %in% c("single bole tree", "multi-bole tree")), 
             aes(x = stemEasting, 
                 y = stemNorthing, 
                 color = taxonid, 
                 size = stemdiameter),
             shape = 21, 
             stroke = 0.5, 
             show.legend = TRUE) +
  
  #   Add points from 2016 as solid filled circles
  geom_point(data = map059 %>%
               dplyr::filter(eventid == "vst_UKFS_2016",
                             growthform %in% c("single bole tree", "multi-bole tree")),
             aes(x = stemEasting,
                 y = stemNorthing,
                 color = taxonid,
                 size = stemdiameter/2),
             shape = 19,
             show.legend = FALSE) +
  
  #   Add tagID labels for all individuals
  ggrepel::geom_text_repel(data = map059 %>%
                             dplyr::filter(growthform %in% c("single bole tree", "multi-bole tree")), 
                           aes(x = stemEasting, 
                               y = stemNorthing, 
                               label = tagid), 
                           size = 2,
                           nudge_x = 0.3, 
                           nudge_y = 0.3,
                           max.overlaps = 20)
  

  
  