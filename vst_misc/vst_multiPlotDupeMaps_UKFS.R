### UKFS: Identify mapped individuals for which duplicate tags were removed with no traceability

#   Load required libraries
library(DBI)
library(dplyr)
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



### Create map of UKFS_049 with 2021 and 2016 data ####
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
subplotPoints <- c(21,23,31,39,41,49,57,59)

cornerPoints <- setdiff(subplotPoints, c(31,33,49,51))

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
             fill = "#FF6C5E", 
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
                 size = stemdiameter/2),
             shape = 19,
             color = "grey50",
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
  

  
  