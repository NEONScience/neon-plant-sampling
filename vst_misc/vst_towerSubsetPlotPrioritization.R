### VST QAQC: Identify all cases since May 2020 when towerSubset sampling has not been 
### consistent with the Plot Prioritization list

#   Load libraries
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

#   Source get.github.xlsx function
source(file = "plant_tools/get.github.xlsx.R")



### Create lookup of expected plotIDs for towerSubset eventType
priorityURL <- "https://raw.githubusercontent.com/NEONScience/NEON-OS-spatial-data/master/TOS/data/UniquePlotIDsSamplingModulesPriorityLists.xlsx"

#   Read in Plot Prioritization Excel file, get desired columns, and focus on 'vst'
priorityDF <- get.github.xlsx(xlsxURL = priorityURL, 
                              xlsxSheet = "plotPriorityLists", 
                              gitToken = Sys.getenv("GITHUB_PAT"))

priorityDF <- priorityDF %>%
  dplyr::select(siteID, 
                plotID, 
                plotType, 
                nlcdClass, 
                specificModule, 
                specificModuleSamplingPriority) %>%
  dplyr::rename(priority = specificModuleSamplingPriority) %>%
  dplyr::filter(specificModule=="vst")

#   Assign eventTypes, then summarise to create plot lists by site and eventType
vstExpPlots <- priorityDF %>%
  dplyr::filter(!siteID %in% c("MOAB", "SRER", "JORN", "ONAQ")) %>%
  dplyr::filter(plotType == "distributed" | (plotType == "tower" & priority %in% 1:5)) %>%
  dplyr::mutate(eventType = "distributedAndTowerSubset")

tSub <- priorityDF %>%
  dplyr::filter(!siteID %in% c("MOAB", "SRER", "JORN", "ONAQ")) %>%
  dplyr::filter(plotType == "tower" & priority %in% 1:5) %>%
  dplyr::mutate(eventType = "towerSubset")

tAll <- priorityDF %>%
  dplyr::filter(plotType == "tower") %>%
  dplyr::mutate(eventType = "allTowerPlots")

dOnly <- priorityDF %>%
  dplyr::filter(siteID %in% c("MOAB", "SRER", "JORN", "ONAQ")) %>%
  dplyr::filter(plotType == "distributed") %>%
  dplyr::mutate(eventType = "distributedOnly")

vstExpPlots <- vstExpPlots %>%
  dplyr::bind_rows(tSub, tAll, dOnly) %>%
  dplyr::group_by(siteID, eventType) %>%
  dplyr::summarise(
    plotPriorityList = paste(plotID, collapse = "|")
  )

rm(tSub, tAll, dOnly)



### From May 2020: Check vst_perplotperyear for correct Tower plots sampled when eventType includes towerSubset
#   Retrieve vst_perplotperyear data from PostgreSQL database
ppyQuery <- glue::glue("SELECT date, domainid, siteid, plotid, plottype, eventid, eventtype, datacollected, samplingimpractical, remarks FROM vstqaqc.perplotperyear WHERE date >= '2020-05-01' AND plottype='tower' AND eventtype IN ('towerSubset', 'distributedAndTowerSubset') ORDER BY domainid, siteid, eventid, plotid;")

ppyDF <- RPostgres::dbGetQuery(conn = conPool,
                               statement = ppyQuery)

#   Identify plots sampled not in Plot Prioritization list from all eventIDs since 2020
ppyDF <- ppyDF %>%
  dplyr::left_join(vstExpPlots,
                   by = c("siteid" = "siteID", 
                          "eventtype" = "eventType")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(plotQF = dplyr::case_when(datacollected == "dendrometerOnly" ~ "OK",
                                          !grepl(plotid, plotList) ~ "plotPriority error",
                                          TRUE ~ "OK")) %>%
  dplyr::ungroup()

#   Identify sites where an incorrect plotID was sampled at least once
theSites <- ppyDF %>%
  dplyr::filter(plotQF == "plotPriority error") %>%
  dplyr::distinct(siteid)

#   Filter to all records for sites with at least one "plotID error" (retain all context)
flagsDF <- ppyDF %>%
  dplyr::filter(siteid %in% theSites$siteid)



### For FieldSci follow-up: Write out xlsx files to share via INC
theDomains <- unique(flagsDF$domainid)

for (i in 1:length(theDomains)) {
  
  tempDF <- flagsDF %>%
    dplyr::filter(domainid == theDomains[i])
  
  openxlsx::write.xlsx(tempDF,
                       file = glue::glue("vst_plotPriorityError_{theDomains[i]}.xlsx"),
                       colNames = TRUE,
                       rowNames = FALSE,
                       firstRow = TRUE,
                       colWidths = "auto")
  
}

