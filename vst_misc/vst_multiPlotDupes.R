### Identify VST multi-plot duplicates for Field Science resolution ####

#   Load required libraries
library(DBI)
library(dplyr)
library(glue)
library(openxlsx)
library(pool)
library(RPostgres)



### Setup
#   Establish vstqaqc db connection pool
conPool <- pool::dbPool(drv = RPostgres::Postgres(),
                        dbname = "vstqaqc",
                        host = Sys.getenv("DB_HOST"),
                        port = 5432,
                        user = "shiny_vstqaqc_ro",
                        password = Sys.getenv("DB_TOKEN"),
                        onCreate = \(con) DBI::dbExecute(con, 'SET search_path TO vstqaqc;'))

#   Use straight quotes
options(useFancyQuotes = FALSE)



### Identify individuals occurring in > 1 plotid in all vst_apparentindividual data
#   Construct summary query
aiSummaryQuery <- glue::glue("WITH indivplotcount AS (SELECT individualid, plotid FROM vstqaqc.apparentindividual GROUP BY individualid, plotid) SELECT individualid FROM indivplotcount GROUP BY individualid HAVING count(plotid) > 1 ORDER BY individualid;")

#   Retrieve individualids from apparentindividual table
multiplotID <- RPostgres::dbGetQuery(conn = conPool,
                                     statement = aiSummaryQuery)



### Retrieve apparentindiviual data for individualids in > 1 plotid
#   Create list of individuals for query
queryIndividuals <- sQuote(sort(unique(multiplotID$individualid)))
queryIndividuals <- paste(queryIndividuals, collapse = ",")

#   Construct apparentindividual query
aiQuery <- glue::glue("SELECT domainid, siteid, date, eventid, individualid, plotid, growthform, tagstatus, plantstatus, stemdiameter, remarks FROM vstqaqc.apparentindividual WHERE individualid IN ({queryIndividuals}) ORDER BY domainid, siteid, individualid, date;")

#   Retrieve apparentindividual data for individualids in > 1 plotid
multiplotDF <- RPostgres::dbGetQuery(conn = conPool,
                                     statement = aiQuery)

#   Summary count by domain and site
summaryDF <- multiplotDF %>%
  dplyr::group_by(domainid, siteid) %>%
  dplyr::distinct(domainid, siteid, individualid) %>%
  dplyr::summarise(count = n())

#   Write out summary table
write.csv(summaryDF,
          file = "vst_multiPlotDupe_summaryCount.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")



### Retrieve and join data from mappingandtagging for more context
#   Construct mappingandtagging query
mtQuery <- glue::glue("SELECT date, eventid, plotid, subplotid, nestedsubplotid, taxonid, recordtype, individualid, previouslytaggedas FROM vstqaqc.mappingandtagging WHERE individualid IN ({queryIndividuals}) ORDER BY domainid, siteid, individualid, date;")

#   Retrieve mappingandtagging data for individualids in > 1 plotid
mtDF <- RPostgres::dbGetQuery(conn = conPool,
                              statement = mtQuery)

#   Filter to latest records to remove intentional dupes
mtDF <- mtDF %>%
  dplyr::group_by(individualid) %>%
  dplyr::filter(date == max(date))

#   Rename fields and join with apparentindividual data
multiplotDF <- multiplotDF %>%
  dplyr::left_join(mtDF %>%
                     dplyr::rename(mtdate = date,
                                   mteventid = eventid,
                                   mtplotid = plotid),
                   by = "individualid")

#   Write out .xlsx by domain
theDomains <- unique(multiplotDF$domainid)

for (i in 1:length(theDomains)) {
  
  tempDF <- multiplotDF %>%
    dplyr::filter(domainid == theDomains[i])
  
  openxlsx::write.xlsx(tempDF,
            file = glue::glue("vst_multiPlotDupe_{theDomains[i]}.xlsx"),
            row.names = FALSE,
            firstRow = TRUE,
            colWidths = "auto")
  
}



### Close connection pool
pool::poolClose(conPool)
rm(conPool)


