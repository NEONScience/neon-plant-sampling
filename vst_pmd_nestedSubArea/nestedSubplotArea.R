### Purpose: Identify most recent nestedSubplotArea values by growthForm for all NEON plots ####
### Author: Courtney Meier - cmeier@BattelleEcology.org

library(dplyr)
library(neonUtilities)

### Retrieve all NEON Portal vst_perplotperyear data for all sites
pmdDF <- neonUtilities::loadByProduct(
  dpID = "DP1.10098.001",
  site = "all",
  package = "basic",
  release = "current",
  tabl = "vst_perplotperyear",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
)

pmdDF <- pmdDF$vst_perplotperyear



### Determine most recently recorded value of nestedSubplotAreaShrubSapling, nestedSubplotAreaLiana, and nestedSubplotAreaOther
### Note: Check values of nestedSubplotAreaOther=='noneSelected' to make sure most recent value for Tower plots comes from
### a bout where ALL Tower plots were measured.

