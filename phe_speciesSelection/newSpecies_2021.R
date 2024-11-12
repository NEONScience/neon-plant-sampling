library(tidyverse)
library(restR)
library(neonUtilities)
library(neonOS)

gsheet <- read.csv('H:/Phenology/Species selection/NPN-NEON species_20210208.csv', stringsAsFactors = F)

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

setdiff(neonList$NEON.taxonID, gsheet$NEON.taxonID)

toAdd <- neonList%>%
  filter(!NEON.taxonID%in%gsheet$NEON.taxonID)%>%
  select(NEON.taxonID, NEON.scientificName, NEONsuggests=growthForm, taxonRank)%>%
  distinct()%>%
  arrange(NEON.taxonID)

#out <- bind_rows(out, toAdd)


write.csv(toAdd, 'H:/Phenology/Species selection/new_Neon_Species_202109.csv', row.names=F, na="")



neonList <- phe_perindividual%>%
  select(taxonID, scientificName, taxonRank, NEONsuggests=growthForm)%>%
  distinct()



update <- filter(gsheet, NEON.taxonID=="")


out <- gsheet%>%
  bind_rows(toAdd)%>%
  arrange()


###check fulcrum ###
api_token = Sys.getenv('FULCRUM_KEY')

source("C:/Users/kjones/Documents/GitHub/fulcrumDev/admin_tools_scripts/magpie_requests/get_fulcrum_data.R")

dbQuery <- paste(URLencode('SELECT siteid, taxonid, FROM "Phenology DB"'),
                  URLencode(paste0("WHERE selectionstatus IN('accepted - Phase I', 'accepted - Phase II')")),
                  sep = "%20")

db_req <- get_fulcrum_data(api_token = api_token, sql = dbQuery)

setdiff(toAdd$NEON.taxonID, db_req$taxonid)

look <- filter(db_req, c(db_req$siteid, db_req$taxonid)%in%c(phe_ind$siteID, phe_ind$taxonID))


ful_df <- restR2::get.fulcrum.data(api_token = Sys.getenv('FULCRUM_KEY'),
                           appName="PHE: Field Setup [PROD]", 
                           repeatable='phe_pertag',
                           fulcFields_oi = c('siteid', 'taxonid_pull', 'growthform'),
                           createdDateStart = '2017-01-01',
                           createdDateEnd ='2021-12-01')

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
