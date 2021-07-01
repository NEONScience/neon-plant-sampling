### NEON Plant Productivity TWG: Overview of R tools for working with NEON data products ####
### Courtney Meier (cmeier@BattelleEcology.org), 2021-07-01

#   Load packages
library(dplyr)
library(neonUtilities)

#   Setup authentication: Paste and save 'NEON_PAT=yourNEONAPItoken'; generate token via neonscience.org account; token allows for faster download speeds
usethis::edit_r_environ()



### Retrieve Veg Structure data using neonUtilities
#   Define product
prodID <- "DP1.10098.001"

#   Determine which data are available by site and month
vstInfo <- neonUtilities::getProductInfo(dpID = prodID, token = Sys.getenv('NEON_PAT'))
vstSiteInfo <- vstInfo$siteCodes


##  Retrieve data from all tables for BART site for specified months
bartVst2019 <- neonUtilities::loadByProduct(
  dpID = prodID,
  site = "BART",
  startdate = "2019-07",
  enddate = "2019-10",
  check.size = FALSE,
  release = "current", # includes data from latest release and provisional data
  token = Sys.getenv('NEON_PAT')
)

#   Extract basic information about data table variables and organization
vstVariables <- bartVst2019$variables_10098

#   Get 'Per Plot Per Year' sampling meta-data, required for scaling 'Apparent Individual' data to plot level
pmdBart2019 <- bartVst2019$vst_perplotperyear

#   Get 'Mapping and Tagging' data, includes all individuals ever tagged/mapped up to latest user-specified date;
#   duplicates are possible if mapping data or taxonID were updated at any point after original publication.
mtBart2019 <- bartVst2019$vst_mappingandtagging
mtBart2019 <- mtBart2019 %>%
  dplyr::group_by(individualID) %>%
  dplyr::filter(date==max(date)) %>%
  dplyr::ungroup()

#   Get 'Apparent Individuals' data
aiBart2019 <- bartVst2019$vst_apparentindividual


##  Retrieve data from a specified Veg Structure table for BART and HARV sites for specified months
aiD012018 <- neonUtilities::loadByProduct(
  dpID = prodID,
  site = c("BART", "HARV"),
  startdate = "2018-01",
  enddate = "2018-12",
  tabl = "vst_apparentindividual",
  check.size = FALSE,
  release = "current",
  token = Sys.getenv('NEON_PAT')
)
aiD012018 <- aiD012018$vst_apparentindividual


