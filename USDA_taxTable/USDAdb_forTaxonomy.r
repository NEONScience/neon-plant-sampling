library(plyr)
library(zoo) # For na.locf


#sinSyn = without synonymns or hybrids = 35010 rows
#Includes genus level TaxonIDs
getPlantDb <- function(){
  plantDbFile <- 'c:/Users/kjones/Documents/R/USDA_db_sinSyn.csv'
  if (!file.exists(plantDbFile)){
    query <- 'http://plants.usda.gov/java/AdvancedSearchServlet?pfa=l48&pfa=ak&pfa=hi&pfa=pr&
    dsp_symbol=on&dsp_vernacular=on&dsp_genus=on&dsp_family=on&dsp_orders=on&dsp_class=on&
    dsp_division=on&dsp_kingdom=on&dsp_itis_tsn=on&dsp_dur=on&dsp_grwhabt=on&dsp_nativestatuscode=on&
    dsp_fed_nox_status_ind=on&dsp_state_nox_status=on&dsp_invasive_pubs=on&dsp_fed_te_status=on&
    dsp_state_te_status=on&dsp_nat_wet_ind=on&dsp_wet_region=on&includeAuthors=on&Synonyms=none&
    hybrids=nonhybrids&dsp_authorname_separate=on&viewby=sciname ame&download=on'
    ### something needs to be done here to remove the extra lines. the download from USDA may not have the correct
    ## line endings 
    download.file(query, plantDbFile)
  }
  df <- read.csv((plantDbFile), na.strings="", stringsAsFactors=FALSE)
  # Fill NAs in taxonomic columns from appropriate rows
  # Shouldn't affect Synonym.Symbol b/c the NA row is first
  # This is concise, but sloowwww - there are almost 50k accepted symbols
  # When the formatting has settled, we should consider storing a modified
  # version on disk
  ddply(df, 'Accepted.Symbol', na.locf)
}

# Load this because it's big and we only want to do it once per session
plantDb <- getPlantDb()

#######################################

# includes synonyms, not hybrids = 71,958 rows
getPlantDb <- function(){
  plantDbFile <- 'c:/Users/kjones/Documents/R/USDA_db_conSyn.csv'
  if (!file.exists(plantDbFile)){
    query <- 'http://plants.usda.gov/java/AdvancedSearchServlet?pfa=l48&pfa=ak&pfa=hi&pfa=pr&
    dsp_symbol=on&dsp_vernacular=on&dsp_genus=on&dsp_family=on&dsp_orders=on&dsp_class=on&
    dsp_division=on&dsp_kingdom=on&dsp_itis_tsn=on&dsp_dur=on&dsp_grwhabt=on&dsp_nativestatuscode=on&
    dsp_fed_nox_status_ind=on&dsp_state_nox_status=on&dsp_invasive_pubs=on&dsp_fed_te_status=on&
    dsp_state_te_status=on&dsp_nat_wet_ind=on&dsp_wet_region=on&includeAuthors=on&Synonyms=none&
    dsp_synonyms=on&hybrids=nonhybrids&dsp_authorname_separate=on&viewby=sciname ame&download=on'
    download.file(query, plantDbFile)
  }
  df <- read.csv((plantDbFile), na.strings="", stringsAsFactors=FALSE)
  # Fill NAs in taxonomic columns from appropriate rows
  # Shouldn't affect Synonym.Symbol b/c the NA row is first
  # This is concise, but sloowwww - there are almost 50k accepted symbols
  # When the formatting has settled, we should consider storing a modified
  # version on disk
  ddply(df, 'Accepted.Symbol', na.locf)
}

# Load this because it's big and we only want to do it once per session
plantDb <- getPlantDb()

###########################################

#Step 1-field names
#   
#
#
#
#