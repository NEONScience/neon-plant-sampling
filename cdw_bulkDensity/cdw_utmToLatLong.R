### CDW bulk density: Convert log locations from UTM to decimal degrees ####
library(glue)
library(httr)
library(neonUtilities)
library(openxlsx)
library(sf)
library(tidyverse)



### Input variables
##  List sites
theSites <- c("TREE", "UNDE")
theSites <- "BONA"
theSites <- "NIWO"

##  Define UTM zone for sites
utmSites <- 16 #--> D05 TREE, UNDE
utmSites <- 6 #--> D19 BONA
utmSites <- 13 #--> D13 NIWO



### Function: Degrees to radians
radians <- function(degrees) {
  rad = (degrees*pi)/180
  return(rad) 	
}



### Load Portal data: Retrieve CDW BD log-level data for sites of interest
cdwbd <- neonUtilities::loadByProduct(dpID = "DP1.10014.001",
                                      site = theSites,
                                      tabl = "cdw_densitylog",
                                      check.size = FALSE,
                                      token = Sys.getenv("NEON_TOKEN"))

logDF <- cdwbd$cdw_densitylog



### Data prep: Calculate positions of logs with mappingMethod == "Relative"
##  Retrieve pointSpatialData from Github
pointsURL <- "https://raw.githubusercontent.com/NEONScience/NEON-OS-spatial-data/main/TOS/data/pointSpatialData.csv"

response <- httr::GET(pointsURL, 
                      httr::authenticate(Sys.getenv("GITHUB_PAT"), ''))

pointsDF <- httr::content(response, 
                          type = "text/csv", 
                          encoding = "UTF-8")

#   Filter to base plots for joining with logDF data
pointsDF <- pointsDF %>%
  dplyr::rename(pointEasting = easting,
                pointNorthing = northing) %>%
  dplyr::filter(subtype == "basePlot")


##  Join logDF and pointsDF and filter to mapped logs
logDF <- dplyr::left_join(logDF,
                          pointsDF %>%
                            dplyr::select(plotID,
                                          pointID,
                                          pointEasting,
                                          pointNorthing),
                          by = c("plotID", "pointID"))


##  Filter to logs with mapping data
logDF <- logDF %>%
  dplyr::filter(mappingMethod %in% c("Relative", "GPS"))

#   Calculate 'correctedAzimuth' to account for mapping direction from log back to pointID
#   then calculate mapped log location
logDF <- logDF %>%
  dplyr::mutate(correctedAzimuth = logAzimuth + 180,
                sampleEasting = dplyr::case_when(mappingMethod == "Relative" ~ 
                                                   round(pointEasting + logDistance*sin(radians(correctedAzimuth)),
                                                         digits = 2),
                                                 TRUE ~ sampleEasting),
                sampleNorthing = dplyr::case_when(mappingMethod == "Relative" ~ 
                                                    round(pointNorthing + logDistance*cos(radians(correctedAzimuth)),
                                                          digits = 2),
                                                  TRUE ~ sampleNorthing))

#   Filter records missing easting/northing
logDF <- logDF %>%
  dplyr::filter(!if_all(c(sampleEasting, sampleNorthing), is.na))

#   D05 specific: Remove sampleNorthing data entry errors
logDF <- logDF %>%
  dplyr::filter(sampleNorthing > 5000000)



### Convert log data frame to 'sf' object
logSF <- sf::st_as_sf(x = logDF,
                      coords = c("sampleEasting", "sampleNorthing"),
                      crs = glue::glue("+proj=utm +zone={utmSites}"))

logSF <- sf::st_transform(logSF,
                          crs = "+proj=longlat +datum=WGS84")

logSF <- logSF %>%
  dplyr::arrange(plotID,
                 logID) %>%
  dplyr::select(-uid,
                -namedLocation,
                -decimalLatitude,
                -decimalLongitude,
                -coordinateUncertainty,
                -publicationDate,
                -release)



### Extract coordinates from 'geometry' and write out
logSF <- logSF %>%
  dplyr::mutate(logLon = sf::st_coordinates(.)[,1],
                logLat = sf::st_coordinates(.)[,2])

#   D05 file output
# fileOut <- "~/Desktop/D05_CDW_BD_latLong.xlsx"

#   D13 file output
fileOut <- "~/Desktop/D13_NIWO_CDW_BD_latLong.xlsx"

#   D19 file output
# fileOut <- "~/Desktop/D19_BONA_CDW_BD_latLong.xlsx"

#   Write out spreadsheet
openxlsx::write.xlsx(logSF %>%
                       as.data.frame() %>%
                       dplyr::select(-geometry),
                     file = fileOut,
                     colNames = TRUE,
                     rowNames = FALSE,
                     firstRow = TRUE)



### Create plots of mapped logs
# ##  D05 log maps
# #   Plot TREE logs
# plot(logSF %>%
#        dplyr::filter(grepl("TREE", plotID)) %>%
#        dplyr::select(plotID,
#                      geometry),
#      main = "TREE CDW BD mapped logs")
# 
# #   Plot UNDE logs
# plot(logSF %>%
#        dplyr::filter(grepl("UNDE", plotID)) %>%
#        dplyr::select(plotID,
#                      geometry),
#      main = "UNDE CDW BD mapped logs")


# ##  D19 log map
# plot(logSF %>%
#        dplyr::select(plotID,
#                      geometry),
#      main = "BONA CDW BD mapped logs")


##  D13 log map
plot(logSF %>%
       dplyr::select(plotID,
                     geometry),
     main = "NIWO CDW BD mapped logs")

