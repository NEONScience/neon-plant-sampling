library(restR2)
library(tidyverse)

#load plant taxon table
#plant_tax <- get.taxon.table.by.type(stack = 'prod', type = "PLANT")
saveRDS(plant_tax, 'H:/Phenology/Species selection/plantTaxonTable_all.rds')

#load dioecies species list (from Wikipedia - https://en.wikipedia.org/wiki/Category:Dioecious_plants)
d_list <- read.csv('H:/Phenology/Species selection/dioeciousSpecis_wikipedia.csv', header = T)

#working df
out <- d_list

# add 'sp.' to genus level records
out$scientificName <- ifelse(grepl(" ", out$scientificName)==FALSE, 
                             paste(out$scientificName, "sp.", sep=' '),
                             out$scientificName)

plant_tax$g_sp <- paste(plant_tax$genus, plant_tax$specificEpithet)

inBoth <- intersect(out$scientificName, plant_tax$g_sp)

#assign genus level taxonID and taxonRank
for(i in inBoth){
  out$acceptedTaxonID[out$scientificName==i] <- plant_tax$acceptedTaxonID[plant_tax$g_sp==i]
  out$taxonID[out$scientificName==i] <- plant_tax$taxonID[plant_tax$g_sp==i]
  out$taxonRank[out$scientificName==i] <- plant_tax$taxonRank[plant_tax$g_sp==i]
}

table(out$taxonRank, useNA = "always")
## as far as I can tell all the NAs are not in our taxonlist. Not US species?
## filter them out

out <- filter(out, !is.na(taxonRank))

## then add species from tax list for family and genera only rows 
for (i in out$scientificName[out$taxonRank=='family']){
  fam <- str_split(i," ")[[1]][1]
  temp <- plant_tax%>%
    filter(family==fam)%>%
    select("scientificName","acceptedTaxonID","taxonRank","taxonID")
  out <- bind_rows(out, temp)
}

for (i in out$scientificName[out$taxonRank=='genus']){
  gen <- str_split(i," ")[[1]][1]
  temp <- plant_tax%>%
    filter(genus==gen)%>%
    select("scientificName","acceptedTaxonID","taxonRank","taxonID")
  out <- bind_rows(out, temp)
}

# remove duplicates and alphabetize
out <- out%>%
  distinct(taxonID, .keep_all = TRUE)%>%
  arrange(scientificName)

# re-assign sciName to match neon taxon table
for (i in out$taxonID){
  out$scientificName[out$taxonID==i] <- plant_tax$scientificName[plant_tax$taxonID==i]
}

write.csv(out, "H:/Phenology/Species selection/dioeciousSpeciesList.csv", row.names=F)
