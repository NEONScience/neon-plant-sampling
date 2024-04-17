library(tidyverse)
library(restR)

gsheet <- read.csv('H:/Phenology/Species selection/NPN-NEON species_20210208.csv', stringsAsFactors = F)

neonList <- phe_perindividual%>%
  select(NEON.taxonID=taxonID, NEON.scientificName=scientificName)%>%
  distinct()

names(gsheet)
names(neonList)

setdiff(neonList$taxonid, gsheet$NEON.taxonID)

updateTax <- gsheet%>%
  filter(NEON.taxonID=="")%>%
  select(-NEON.taxonID)

updateTax <- left_join(updateTax, neonList)

out <- filter(gsheet, NEON.taxonID!="")

out <- bind_rows(out, updateTax)

toAdd <- phe_perindividual%>%
  filter(!taxonID%in%out$NEON.taxonID)%>%
  select(NEON.taxonID=taxonID, NEON.scientificName=scientificName, NEONsuggests=growthForm)%>%
  distinct()

out <- bind_rows(out, toAdd)


write.csv(out, 'H:/Phenology/Species selection/allSpecies_20210208.csv', row.names=F, na="")



neonList <- phe_perindividual%>%
  select(taxonID, scientificName, taxonRank, NEONsuggests=growthForm)%>%
  distinct()

update <- filter(gsheet, NEON.taxonID=="")


out <- gsheet%>%
  bind_rows(toAdd)%>%
  arrange()
