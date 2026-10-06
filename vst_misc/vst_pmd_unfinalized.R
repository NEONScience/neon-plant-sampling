### Review VST PMD records for unfinalized status ####
library(tidyverse)
library(restR2)

options(useFancyQuotes = FALSE)


pmQuery <- glue::glue('SELECT domainid, siteid, eventid, eventtype, finalize, load_status, COUNT(*)
                      FROM "de2da940-190b-415b-96dc-e67852bb96d3" 
                      GROUP BY domainid, siteid, eventid, eventtype, finalize, load_status 
                      ORDER BY domainid, siteid, eventid, eventtype, finalize, load_status',
                      .sep = "")

finalizeDF <- restR2::get.fulcrum.sql(apiToken = Sys.getenv("FULCRUM_TOKEN"),
                                      sql = pmQuery,
                                      urlEncode = TRUE)

finalizeDF <- finalizeDF %>%
  dplyr::filter(grepl("2025$", eventid))


