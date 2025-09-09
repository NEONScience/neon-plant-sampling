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


