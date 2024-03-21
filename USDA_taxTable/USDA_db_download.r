library(plyr)
library(zoo) # For na.locf

getPlantDb <- function(){
  plantDbFile <- 'c:/Users/kjones/Documents/R/USDA_db.csv'
  if (!file.exists(plantDbFile)){
    query <- 'http://plants.usda.gov/java/AdvancedSearchServlet?dsp_symbol=on&dsp_vernacular=on&
    dsp_category=on&dsp_genus=on&dsp_family=on&dsp_familySym=on&dsp_orders=on&dsp_subclass=on&
    dsp_class=on&dsp_Subdivision=on&dsp_division=on&dsp_superdivision=on&dsp_subkingdom=on&
    dsp_kingdom=on&dsp_dur=on&dsp_grwhabt=on&dsp_invasive_pubs=on&Synonyms=all&dsp_synonyms=on&
    viewby=scin ame&download=on'
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
