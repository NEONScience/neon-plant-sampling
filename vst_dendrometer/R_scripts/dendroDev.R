### Development code to use for retrieving latest dataCollected = "allGrowthForms" bout for each plotID ####
library(pool)
library(DBI)
library(RPostgres)
library(tidyverse)


options(useFancyQuotes = FALSE)

### Setup querying to obtain data from Fulcrum and NEON Portal APIs
#   Define api-tokens
dbHost <- "prod-commondb.gcp.neoninternal.org"
dbPwd <- "nD0BEMA8Cb6efs1dniFkfOIPeBE09HIy"
fulcrumToken <- Sys.getenv("FULCRUM_TOKEN")

#   Set up vstqaqc db connection pool
conPool <- pool::dbPool(drv = RPostgres::Postgres(),
                        dbname = "vstqaqc",
                        host = dbHost,
                        port = 5432,
                        user = "shiny_vstqaqc_ro",
                        password = dbPwd,
                        onCreate = \(con) DBI::dbExecute(con, 'SET search_path TO vstqaqc;'))


### Define sites to query
siteList <- c("ABBY", "WREF")
siteList <- sQuote(siteList)
siteList <- paste(siteList, collapse = ",")


###  Retrieve woody data from 'vstqaqc' PostgreSQL data base
#    Define vstqaqc.apparentindividual query
aiQuery <- glue::glue("SELECT inv.domainid, inv.siteid, inv.plotid, inv.eventid, inv.date, inv.individualid, inv.tempstemid, inv.growthform, inv.plantstatus, inv.tagstatus, inv.shape, inv.stemdiameter, inv.measurementheight, inv.dendrometerinstallationdate, inv.dendrometergap, inv.dendrometercondition, inv.remarks
FROM vstqaqc.apparentindividual AS inv
JOIN (
  SELECT DISTINCT pmevent.plotid, pmevent.eventid
  FROM vstqaqc.perplotperyear AS pmevent
  JOIN (
    SELECT plotid, MAX(date) AS latestdate
    FROM vstqaqc.perplotperyear
    WHERE siteid IN ({siteList}) AND (samplingimpractical='OK' OR samplingimpractical IS NULL) AND 
    (datacollected IN ('allGrowthForms') OR datacollected IS NULL)
    GROUP BY plotid
  ) AS pmlatest
  ON pmevent.plotid=pmlatest.plotid AND pmevent.date=pmlatest.latestdate
) AS latest
ON inv.plotid=latest.plotid AND inv.eventid=latest.eventid
ORDER BY inv.eventid, inv.plotid, inv.individualid;")

#    Get vstqaqc.apparentindividual data
aiDF <- RPostgres::dbGetQuery(conn = conPool,
                              statement = aiQuery)



### Retrieve M&T data from PostgreSQL data base to associate taxonID, etc. with AI data
mtQuery <- glue::glue('SELECT m1.date, m1.subplotid, m1.nestedsubplotid, UPPER(m1.individualid) AS individualid, m1.tagid, m1.taxonid, m1.recordtype, m1.pointid, m1.stemazimuth, m1.stemdistance
FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae" AS m1
JOIN (
  SELECT UPPER(individualid) AS individualid, MAX(date) AS date
  FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae"
  WHERE', "siteid IN ({siteList})
  GROUP BY UPPER(individualid)
) AS m2
ON UPPER(m1.individualid) = m2.individualid AND m1.date = m2.date
ORDER BY UPPER(m1.individualid)", .sep = " ")

#    Get Fulcrum Mapping and Tagging data
mtDF <- restR2::get.fulcrum.sql(apiToken = fulcrumToken,
                                sql = mtQuery,
                                urlEncode = TRUE)

#    Standardize data types and column names
mtDF <- mtDF %>%
  dplyr::rename(fixedsubplotid = subplotid,
                nestedcorner = nestedsubplotid) %>%
  dplyr::mutate(nestedcorner = as.integer(nestedcorner),
                recordtype = dplyr::case_when(recordtype %in% c("map and tag", "mapping") ~ "mapAndTag",
                                              recordtype %in% c("tag only", "tagging") ~ "tagOnly",
                                              TRUE ~ recordtype),
                pointid = as.integer(pointid)) %>%
  dplyr::select(-"date")

