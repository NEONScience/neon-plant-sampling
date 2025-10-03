### Development code to use for retrieving latest dataCollected = "allGrowthForms" bout for each plotID ####
library(pool)
library(DBI)
library(RPostgres)
library(tidyverse)


options(useFancyQuotes = FALSE)

### Setup querying to obtain data from Fulcrum and NEON Portal APIs
#   Define api-tokens
dbHost <- Sys.getenv("VST_HOST")
dbPwd <- Sys.getenv("VST_PWD")
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

siteList <- c("RMNP", "YELL", "NIWO", 
              "WREF", "SJER", "SOAP",
              "TEAK", "BONA", "DEJU", "HEAL")
options("useFancyQuotes" = FALSE)
siteList <- sQuote(siteList)
siteList <- paste(siteList, collapse = ",")


###  Retrieve woody data from 'vstqaqc' PostgreSQL data base
#    Define vstqaqc.apparentindividual query
aiQuery <- glue::glue("SELECT inv.domainid, inv.siteid, inv.plotid, inv.subplotid, inv.eventid, inv.date, inv.individualid, inv.tempstemid, inv.growthform, inv.plantstatus, inv.tagstatus, inv.shape, inv.stemdiameter, inv.measurementheight, inv.remarks
FROM vstqaqc.apparentindividual AS inv
JOIN (
  SELECT DISTINCT pmevent.plotid, pmevent.eventid
  FROM vstqaqc.perplotperyear AS pmevent
  JOIN (
    SELECT plotid, MAX(date) AS latestdate
    FROM vstqaqc.perplotperyear
    WHERE siteid IN ({siteList}) AND (samplingimpractical='OK' OR samplingimpractical IS NULL) AND 
    (datacollected IN ('allGrowthForms') OR datacollected IS NULL) AND plottype='tower'
    GROUP BY plotid
  ) AS pmlatest
  ON pmevent.plotid=pmlatest.plotid AND pmevent.date=pmlatest.latestdate
) AS latest
ON inv.plotid=latest.plotid AND inv.eventid=latest.eventid
ORDER BY inv.eventid, inv.plotid, inv.individualid;")

aiQuery <- glue::glue("SELECT * 
FROM vstqaqc.apparentindividual AS inv
JOIN (
  SELECT DISTINCT pmevent.plotid, pmevent.eventid
  FROM vstqaqc.perplotperyear AS pmevent
  JOIN (
    SELECT plotid, MAX(date) AS latestdate
    FROM vstqaqc.perplotperyear
    WHERE siteid IN ({siteList}) AND (samplingimpractical='OK' OR samplingimpractical IS NULL) AND 
    (datacollected IN ('allGrowthForms') OR datacollected IS NULL) AND plottype='tower'
    GROUP BY plotid
  ) AS pmlatest
  ON pmevent.plotid=pmlatest.plotid AND pmevent.date=pmlatest.latestdate
) AS latest
ON inv.plotid=latest.plotid AND inv.eventid=latest.eventid
ORDER BY inv.eventid, inv.plotid, inv.individualid;")

#    Get vstqaqc.apparentindividual data
aiDF <- RPostgres::dbGetQuery(conn = conPool,
                              statement = aiQuery)

df_sub <- aiDF%>%
  filter(plantstatus%in%c("1", "4", "5", "6", "7", "9", 
                          "Live", "Live, disease damaged", "Live, insect damaged", "Live, other damage", "Live, physically damaged") & 
           growthform%in%c("single bole tree", "sbt", "multi-bole tree", "mbt", "small tree", "smt"))%>%
  arrange(plotid, desc(eventid))

#fixing 2 yellowstone trees with invalid stem diams
#to_fix <- which(df_sub$individualid %in% c("NEON.PLA.D12.YELL.01603B", "NEON.PLA.D12.YELL.01603"))
to_fix <- which(is.na(df_sub$stemdiameter))
#diameters from past data

WREF_a = 29.7
WREF_a_meas = 130
WREF_b = 23.6
WREF_b_meas = 130
YELL_a = 50.3
YELL_a_meas = 196
YELL_b = 45.6
YELL_b_meas = 194
#fix diameters
df_sub[to_fix[6],'stemdiameter'] = WREF_a
df_sub[to_fix[6],'measurementheight'] = WREF_a_meas
df_sub[to_fix[7],'stemdiameter'] = WREF_b
df_sub[to_fix[7],'measurementheight'] = WREF_b_meas
df_sub[to_fix[8],'stemdiameter'] = YELL_a
df_sub[to_fix[8],'measurementheight'] = YELL_a_meas
df_sub[to_fix[9],'stemdiameter'] = YELL_b
df_sub[to_fix[9],'measurementheight'] = YELL_b_meas

### Retrieve M&T data from PostgreSQL data base to associate taxonID, etc. with AI data
mtQuery <- glue::glue('SELECT m1.date, m1.subplotid, m1.nestedsubplotid, UPPER(m1.individualid) AS individualid, m1.tagid, m1.taxonid, m1.recordtype, m1.pointid, m1.stemazimuth, m1.stemdistance, m1.initialbandstemdiameter
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


map <- mtDF%>%
  arrange(individualid)%>%
  filter(individualid%in%df_sub$individualid)%>%
  distinct(individualid, .keep_all = TRUE)%>%
  select(-fixedsubplotid, -nestedcorner)

df_out <- df_sub%>%
  left_join(map)%>%
  select(domainid, siteid, plotid, subplotid, pointid, stemazimuth, stemdistance, taxonid, individualid, tempstemid, growthform, stemdiameter, initialbandstemdiameter)%>%
  arrange(domainid, plotid, subplotid, desc(stemdiameter))%>%
  filter(!is.na(stemdiameter))

wdir <- "C:/Users/lea/Documents/GitHub/neon-plant-sampling/vst_dendrometer"
year <- '2025'
my_dpid <- "DP1.10098.001"

saveRDS(df_sub, paste(wdir, year, 'sourceData', 'vst_ai_data.rds', sep='/'))

## Mapping & Tagging
saveRDS(mtDF, paste(wdir, year, 'sourceData', 'vst_mt_data.rds', sep='/'))

## Merged
saveRDS(df_out, paste(wdir, year, 'sourceData', 'vst_merged_data.rds', sep='/'))