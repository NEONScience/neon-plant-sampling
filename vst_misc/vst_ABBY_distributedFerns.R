library(DBI)
library(dplyr)
library(pool)
library(RPostgres)


conPool <- pool::dbPool(drv = RPostgres::Postgres(),
                        dbname = "vstqaqc",
                        host = Sys.getenv("DB_HOST"),
                        port = 5432,
                        user = "shiny_vstqaqc_ro",
                        password = Sys.getenv("DB_TOKEN"),
                        onCreate = \(con) DBI::dbExecute(con, 'SET search_path TO vstqaqc;'))

#   Query db to find fern records from distributed plots
dbQuery <- glue::glue("SELECT nw.date, nw.eventid, nw.siteid, nw.plotid, pm.plottype, nw.individualid, nw.taxonid, nw.growthform FROM vstqaqc.nonwoody AS nw LEFT JOIN vstqaqc.perplotperyear AS pm ON nw.plotid=pm.plotid WHERE nw.siteid='ABBY' AND pm.plottype='distributed' AND nw.growthform='fern' ORDER BY nw.eventid, nw.plotid, nw.individualid;", .sep = " ")

dbNW <- RPostgres::dbGetQuery(conn = conPool,
                              statement = dbQuery)

abbyDistFerns <- dbNW %>%
  dplyr::group_by(eventid,
                  plotid,
                  taxonid) %>%
  dplyr::summarise(fernCount = n())
