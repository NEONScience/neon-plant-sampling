library(tidyverse)

gsheet <- read.csv('H:/Phenology/Species selection/NPN-NEON species_20180807.csv', stringsAsFactors = F)
neonList <- read.csv('H:/Phenology/Species selection/2018_phe_speciesList.csv', stringsAsFactors = F)

names(gsheet)
names(neonList)

setdiff(neonList$acceptedtaxonid, gsheet$taxonID)

newSpecies <- neonList%>%
  select(taxonid=taxonid_pull, growthform)%>%
  filter(!taxonid%in%gsheet$taxonID)

sciNames <- neonList%>%
  filter(scientificname!='')%>%
  select(taxonid=taxonid_pull, scientificname)

out <- left_join(newSpecies, sciNames)
out <- filter(out, !taxonid%in%c('ALPE4', 'BOHI2'))

write.csv(out, 'H:/Phenology/Species selection/newSpecies_2018.csv', row.names=F)
