library(tidyverse)
library(restR2)
library(neonUtilities)
library(neonOS)

gsheet <- read.csv('H:/Phenology/Species selection/NPN-NEON species_202208.csv', stringsAsFactors = F)

# npnList <- read.csv('H:/Phenology/Species selection/USA-NPN_NN_species_list_2022.csv', stringsAsFactors = F)
#per ED: don't omit existing npn species, want to flag all neon species in db

dat <- loadByProduct(dpID="DP1.10055.001",
                     tabl="phe_perindividual",
                     package = "basic",
                     check.size = FALSE, 
                     token = Sys.getenv('NEON_KEY'))

phe_perindividual <- dat$phe_perindividual

phe_ind <- neonOS::removeDups(data=phe_perindividual,
                              variables=dat$variables_10055,
                              table='phe_perindividual') 

neonList <- phe_ind%>%
  select(NEON.taxonID=taxonID, NEON.scientificName=scientificName, growthForm, taxonRank)%>%
  filter(taxonRank!="genus")%>%
  distinct()


names(gsheet)
names(neonList)

new <- setdiff(neonList$NEON.taxonID, gsheet$NEON.taxonID)

toAdd <- neonList%>%
  filter(!NEON.taxonID%in%gsheet$NEON.taxonID & !NEON.taxonID%in%alreadyAdded)%>%
           #npnList$plants.usda.gov.Symbol)%>%
  select(NEON.taxonID, NEON.scientificName, NEONsuggests=growthForm, taxonRank)%>%
  distinct()%>%
  arrange(NEON.taxonID)

# intersect(toAdd$NEON.taxonID, npnList$plants.usda.gov.Symbol)
# setdiff(toAdd$NEON.taxonID, npnList$plants.usda.gov.Symbol)

#out <- bind_rows(out, toAdd)


write.csv(toAdd, 'H:/Phenology/Species selection/new_Neon_Species_202210.csv', row.names=F, na="")




###check fulcrum ###
api_token = Sys.getenv('FULCRUM_KEY')

db_req <-get.fulcrum.data(api_token = Sys.getenv('FULCRUM_KEY'),
                         appName="Phenology DB", 
                         fulcFields_oi = c('siteid', 'taxonid'),
                         queryField = 'selectionstatus',
                         queryValues=c('accepted - Phase I', 'accepted - Phase II'))

setdiff(toAdd$NEON.taxonID, db_req$taxonid)

look <- filter(db_req, c(db_req$siteid, db_req$taxonid)%in%c(phe_ind$siteID, phe_ind$taxonID))


ful_df <- restR2::get.fulcrum.data(api_token = Sys.getenv('FULCRUM_KEY'),
                           appName="PHE: Field Setup [PROD]", 
                           repeatable='phe_pertag',
                           fulcFields_oi = c('siteid', 'taxonid_pull', 'growthform'),
                           createdDateStart = '2017-01-01',
                           createdDateEnd ='2022-12-01')

ful_df <- select(ful_df, -fulcrum_id, -`_child_record_id`, -siteid)
ful_df <- distinct(ful_df)

phe_ind <- phe_ind%>%
  filter(taxonRank!="genus")%>%
  select(taxonID, taxonRank, scientificName, growthForm)%>%
  distinct()

missingFromFulcrum<- setdiff(phe_ind$taxonID, ful_df$taxonid_pull)

look <- phe_perindividual[phe_perindividual$taxonID%in%missingFromFulcrum,]


newRow <- data.frame(taxonid_pull = "ASRE7",
                     growthform = "DBL", 
                     scientificName = find.scientific.name("ASRE7", type='PLANT'))

ful_df <- bind_rows(ful_df, newRow)
ful_df <- arrange(ful_df, scientificName)

write.csv(ful_df, 'H:/Phenology/Species selection/allNEONspecies_202110.csv', row.names=F, na="")

sp_in <- read.csv('H:/Phenology/coordination/NPN data integration/allNEONspecies_202110_notes.csv', stringsAsFactors = F)

sp <- select(sp_in, tax=taxonid_pull, growthform)

out <- get.taxon.table.by.taxon.list(taxonID_list = sp$tax, 
                                       type="PLANT",
                                       stack="prod")

out <- out%>%
  select(taxonID, acceptedTaxonID, scientificName)%>%
  distinct()

allAcceptedTax <- left_join(sp, out, by=c("tax" = "taxonID"))
allAcceptedTax <- allAcceptedTax%>%
  arrange(acceptedTaxonID)%>%
  select(acceptedTaxonID, growthform)

for(i in allAcceptedTax$acceptedTaxonID){
  allAcceptedTax$scientificName[allAcceptedTax$acceptedTaxonID==i] <- find.scientific.name(i, type="PLANT")
}

write.csv(allAcceptedTax, 'H:/Phenology/Species selection/allNEONspecies_20211012_acceptedIDs.csv')


##### Summary for FS

phe_ind$d_site <- paste(phe_ind$domainID, phe_ind$siteID, sep='_')
phe_ind$present <- "X"

neonSummary <- phe_ind%>%
  select(d_site, taxonID, scientificName, growthForm, present)%>%
  distinct()

out <- neonSummary%>%
  pivot_wider(names_from = d_site, values_from = present, values_fill = '' )%>%
  arrange(taxonID)

gf_conflict <- out$taxonID[duplicated(out$taxonID)]

fix_gf <- out[out$taxonID%in%gf_conflict,]


install.packages("reactable")
library(reactable)
reactable(out, defaultSorted = c("taxonID", "growthForm"))

library(xlsx)

write.csv(out, paste(getwd(), "taxBySite_20230223.csv", sep='/'),
          row.names=FALSE)
