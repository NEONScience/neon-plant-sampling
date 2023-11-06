### D08 DELA and LENO sites not sampling correct Tower subset (1 incorrect plotID per site)
#   Goal: Assess species rank abundance by growthForm for each plot
#   Goal: Assess stemDiameter distribution by growthForm for each plot

#   Load required libraries
library(DBI)
library(dplyr)
library(glue)
library(plotly)
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


### DELA analyses ####
### Prepare data --> 2021 last bout when both plots measured
#   Find vstqaqc.apparentindividual data for DELA plots
delaQuery <- glue::glue("SELECT siteid, plotid, individualid, tempstemid, tagstatus, growthform, plantstatus, stemdiameter, height, basalstemdiameter FROM vstqaqc.apparentindividual WHERE plotid IN ('DELA_039', 'DELA_042') AND eventid='vst_DELA_2021' AND tagstatus<>'tagRemoved' ORDER BY plotid, individualid, tempstemid;")

dela <- RPostgres::dbGetQuery(conn = conPool,
                              statement = delaQuery)

#   Retrieve vstqaqc.mappingandtagging data for taxonIDs
delaIndividuals <- sQuote(sort(unique(dela$individualid)))
delaIndividuals <- paste(delaIndividuals, collapse = ",")

mtDelaQuery <- glue::glue("SELECT date, individualid, taxonid FROM vstqaqc.mappingandtagging WHERE individualid IN ({delaIndividuals}) ORDER BY individualid, date;")
mtDela <- RPostgres::dbGetQuery(conn = conPool,
                                statement = mtDelaQuery)

mtDela <- mtDela %>%
  dplyr::group_by(individualid) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup()

#   Add taxonID data from mappingandtagging to apparentindividual
dela <- dela %>%
  dplyr::full_join(mtDela %>%
                     dplyr::select(individualid, taxonid),
                   by = "individualid")



### Create DELA plot comparison graphs
#   Stem diameter by growthform
delaStemDiamByGF <- ggplot2::ggplot(data = dela %>%
                                      dplyr::filter(!is.na(stemdiameter),
                                                    growthform != "sapling"),
                                    mapping = aes(x = stemdiameter,
                                                  color = plotid)) + 
  ggplot2::geom_density() +
  ggplot2::labs(title = "DELA stemDiameter distributions by growthForm") +
  ggplot2::facet_wrap(~growthform, ncol = 2, scales = "free")

#   Basal stem diameter for saplings and small shrubs
delaBasalDiamByGF <- ggplot2::ggplot(data = dela %>%
                                       dplyr::filter(!is.na(basalstemdiameter),
                                                     growthform == "sapling"),
                                     mapping = aes(x = basalstemdiameter,
                                                   color = plotid)) + 
  ggplot2::geom_density() +
  ggplot2::labs(title = "DELA basalStemDiameter distribution: saplings")


#   Species rank abundance
delaTaxonSumm <- dela %>%
  dplyr::filter(!is.na(growthform)) %>%
  dplyr::group_by(growthform, plotid, taxonid) %>%
  dplyr::summarise(taxonCount = n()) %>%
  dplyr::ungroup()

delaTaxonByGF <- ggplot2::ggplot(data = delaTaxonSumm,
                                 mapping = aes(x = taxonCount,
                                               y = reorder(taxonid, taxonCount),
                                               fill = plotid)) +
  ggplot2::geom_bar(position = "dodge",
                    stat = "identity") +
  ggplot2::labs(title = "DELA species abundance by growthForm",
                y = "taxonID") +
  ggplot2::facet_wrap(facets = ~growthform, scales = "free") +
  ggplot2::theme(legend.position = "bottom")





###  LENO analyses ####
### Prepare data --> 2018 last bout when both plots measured
#   Find vstqaqc.apparentindividual data for LENO plots
lenoQuery <- glue::glue("SELECT siteid, plotid, individualid, tempstemid, tagstatus, growthform, plantstatus, stemdiameter, height, basalstemdiameter FROM vstqaqc.apparentindividual WHERE plotid IN ('LENO_065', 'LENO_067') AND eventid='vst_LENO_2018' AND tagstatus<>'removed' ORDER BY plotid, individualid, tempstemid;")

leno <- RPostgres::dbGetQuery(conn = conPool,
                              statement = lenoQuery)

#   Retrieve vstqaqc.mappingandtagging data for taxonIDs
lenoIndividuals <- sQuote(sort(unique(leno$individualid)))
lenoIndividuals <- paste(lenoIndividuals, collapse = ",")

mtLenoQuery <- glue::glue("SELECT date, individualid, taxonid FROM vstqaqc.mappingandtagging WHERE individualid IN ({lenoIndividuals}) ORDER BY individualid, date;")
mtLeno <- RPostgres::dbGetQuery(conn = conPool,
                                statement = mtLenoQuery)

mtLeno <- mtLeno %>%
  dplyr::group_by(individualid) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup()

#   Add taxonID data from mappingandtagging to apparentindividual
leno <- leno %>%
  dplyr::full_join(mtLeno %>%
                     dplyr::select(individualid, taxonid),
                   by = "individualid")



### Create LENO plot comparison graphs
#   Stem diameter by growthform
lenoStemDiamByGF <- ggplot2::ggplot(data = leno %>%
                                      dplyr::filter(!is.na(stemdiameter),
                                                    growthform != "sapling"),
                                    mapping = aes(x = stemdiameter,
                                                  color = plotid)) + 
  ggplot2::geom_density() +
  ggplot2::labs(title = "LENO stemDiameter distributions by growthForm") +
  ggplot2::facet_wrap(~growthform, ncol = 2, scales = "free")

#   Basal stem diameter for saplings and small shrubs
lenoBasalDiamByGF <- ggplot2::ggplot(data = leno %>%
                                       dplyr::filter(!is.na(basalstemdiameter),
                                                     growthform == "sapling"),
                                     mapping = aes(x = basalstemdiameter,
                                                   color = plotid)) + 
  ggplot2::geom_density() +
  ggplot2::labs(title = "LENO basalStemDiameter distribution: saplings")


#   Species rank abundance
lenoTaxonSumm <- leno %>%
  dplyr::filter(!is.na(growthform)) %>%
  dplyr::group_by(growthform, plotid, taxonid) %>%
  dplyr::summarise(taxonCount = n()) %>%
  dplyr::ungroup()

lenoTaxonByGF <- ggplot2::ggplot(data = lenoTaxonSumm,
                                 mapping = aes(x = taxonCount,
                                               y = reorder(taxonid, taxonCount),
                                               fill = plotid)) +
  ggplot2::geom_bar(position = "dodge",
                    stat = "identity") +
  ggplot2::labs(title = "LENO species abundance by growthForm",
                y = "taxonID") +
  ggplot2::facet_wrap(facets = ~growthform, scales = "free", ncol = 4) +
  ggplot2::theme(legend.position = "bottom")



