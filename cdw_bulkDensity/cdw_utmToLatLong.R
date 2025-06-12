### CDW bulk density: Convert log locations from UTM to decimal degrees ####
library(glue)
library(httr)
library(neonUtilities)
library(sf)
library(tidyverse)



### Input variables
#   List sites
theSites <- c("TREE", "UNDE")

#   Define UTM zone for sites
utmSites <- 16



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
#   Retrieve pointSpatialData from Github
pointsURL <- "https://raw.githubusercontent.com/gitbarnitt/NEON-OS-spatial-data/main/TOS/data/pointSpatialData.csv"

response <- httr::GET(pointsURL, httr::authenticate(Sys.getenv("GITHUB_PAT"), ''))

pointsDF <- httr::content(response, type = 'text/csv', encoding = 'UTF-8')

#   Filter to CDW points for joining with logDF data
pointsDF <- pointsDF %>%
  dplyr::filter(grepl("cdw", applicableModules),
                plotStatus == "current") %>%
  dplyr::rename(pointEasting = easting,
                pointNorthing = northing)

#   Join logDF and pointsDF and filter to mapped logs
logDF <- dplyr::left_join(logDF,
                         pointsDF %>%
                           dplyr::select(plotID,
                                         pointID,
                                         pointEasting,
                                         pointNorthing),
                         by = c("plotID", "pointID")) %>%
  dplyr::filter(mappingMethod %in% c("Relative", "GPS"))

#   Calculate 'correctedAzimuth' to account for mapping direction from log back to pointID
#   then calculate mapped log location, filter records missing easting/northing, and remove 
#   records with 'sampleNorthing' data entry errors
logDF <- logDF %>%
  dplyr::mutate(correctedAzimuth = logAzimuth + 180,
                sampleEasting = dplyr::case_when(mappingMethod == "Relative" ~ 
                                                   round(pointEasting + logDistance*sin(radians(correctedAzimuth)),
                                                         digits = 2),
                                                 TRUE ~ sampleEasting),
                sampleNorthing = dplyr::case_when(mappingMethod == "Relative" ~ 
                                                    round(pointNorthing + logDistance*cos(radians(correctedAzimuth)),
                                                          digits = 2),
                                                  TRUE ~ sampleNorthing)) %>%
  dplyr::filter(!if_all(c(sampleEasting, sampleNorthing), is.na),
                sampleNorthing > 5000000)



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

write.csv(logSF %>%
            as.data.frame() %>%
            dplyr::select(-geometry),
          file = "~/Desktop/D05_CDW_BD_latLong.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")



### Create plots of mapped logs
#   Plot TREE logs
plot(logSF %>%
       dplyr::filter(grepl("TREE", plotID)) %>%
       dplyr::select(plotID,
                     geometry),
     main = "TREE CDW BD mapped logs")

#   Plot UNDE logs
plot(logSF %>%
       dplyr::filter(grepl("UNDE", plotID)) %>%
       dplyr::select(plotID,
                     geometry),
     main = "UNDE CDW BD mapped logs")



