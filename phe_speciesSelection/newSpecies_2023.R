library(tidyverse)
library(restR2)
library(neonUtilities)
library(neonOS)
library(restR)

gsheet <- read.csv('H:/Phenology/Species selection/NPN-NEONspecies_20230919.csv', stringsAsFactors = F)

# npnList <- read.csv('H:/Phenology/Species selection/USA-NPN_NN_species_list_2022.csv', stringsAsFactors = F)
#per ED: don't omit existing npn species, want to flag all neon species in db

dat <- loadByProduct(dpID="DP1.10055.001",
                     tabl="phe_perindividual",
                     package = "basic",
                     check.size = FALSE, 
                     token = Sys.getenv('NEON_KEY'))

l0_ind <- read.csv('H:/Phenology/Species selection/L0_phe_perindividual_in_20230919.csv', sep='\t', header = TRUE)
##too big to download through restR2  
#get.os.l0.data(stack='prod',
                         # dpID = 'DP0.10002.001', 
                         # ingestTable='phe_perindividual_in',
                         # format_for_L0_editor=TRUE)

l1_ind <- read.csv('H:/Phenology/Species selection/L1__phe_perindividual_pub_20230919.txt', sep='\t', header = TRUE)

phe_perindividual <- dat$phe_perindividual

phe_ind <- neonOS::removeDups(data=l0_ind,  #phe_perindividual,
                              variables=dat$variables_10055,
                              table='phe_perindividual') 

neonList <- phe_ind%>%
  select(NEON.taxonID=taxonID, NEON.scientificName=scientificName, growthForm, taxonRank)%>%
  filter(taxonRank!="genus")%>%
  distinct()




neonList_l0 <- l0_ind%>%
  select(NEON.taxonID=taxonID,  growthForm)%>%
  #filter(taxonRank!="genus")%>%
  distinct()

lovLookup <- get.lov(lovName='phe.growthForm')

for(i in 1:nrow(neonList_l0)){
  neonList_l0$growthForm[i] <- lovLookup$lovElementName[lovLookup$lovElementCode==neonList_l0$growthForm[i]]
}

taxTable <- data.frame()
for(i in unique(neonList_l0$NEON.taxonID)){
  temp <- find.taxon.info(type="PLANT", taxonID=i)
  taxTable <- rbind(taxTable, temp)
}

taxTable <- select(taxTable, NEON.taxonID=taxonID, NEON.scientificName=scientificName, taxonRank)

neonList_l0 <- left_join(neonList_l0, taxTable)

neonList <- bind_rows(neonList, neonList_l0)%>%
  filter(!taxonRank=='genus')%>%
  distinct()%>%
  arrange(NEON.taxonID)

names(gsheet)
names(neonList)

new <- setdiff(neonList$NEON.taxonID, gsheet$NEON.taxonID)

toAdd <- neonList%>%
  filter(!NEON.taxonID%in%gsheet$NEON.taxonID)%>%  #& !NEON.taxonID%in%alreadyAdded)
           #npnList$plants.usda.gov.Symbol)%>%
  select(NEON.taxonID, NEON.scientificName, NEONsuggests=growthForm, taxonRank)%>%
  distinct()%>%
  arrange(NEON.taxonID)


alreadyAdded <- c('LIBOL2', 'PICOL2', 'TRBOL', 'JUCO6', 'QUMA6')

toAdd <- toAdd[!toAdd$NEON.taxonID%in%alreadyAdded,]

write.csv(toAdd, 'H:/Phenology/Species selection/new_Neon_Species_20230919.csv', row.names=F, na="")


###check fulcrum ###
api_token = Sys.getenv('FULCRUM_KEY')

db_req <-get.fulcrum.data(api_token = Sys.getenv('FULCRUM_KEY'),
                         appName="Phenology DB", 
                         fulcFields_oi = c('siteid', 'taxonid', 'scientificname', 'selectionstatus'),
                         queryField = 'selectionstatus',
                         queryValues=c('accepted - Phase I', 'accepted - Phase II'))

for(i in 1:nrow(db_req)){
  db_req$scientificName[i] <- restR::find.scientific.name(db_req$taxonid[i], type="PLANT")
  db_req$genus[i] <- str_split(db_req$scientificName[i], " ", n = 3)[[1]][1]
  db_req$species[i] <- str_split(db_req$scientificName[i], " ", n = 3)[[1]][2]
  # db_req$protocolLabel[i] <- paste0(neonSummary$genus[i], " ", 
  #                                        neonSummary$species[i], " - ",
  #                                        neonSummary$gf[i])
}

setdiff(toAdd$NEON.taxonID, db_req$taxonid)

look <- filter(db_req, c(db_req$siteid, db_req$taxonid)%in%c(phe_ind$siteID, phe_ind$taxonID))


ful_df <- restR2::get.fulcrum.data(api_token = Sys.getenv('FULCRUM_KEY'),
                           appName="PHE: Field Setup [PROD]", 
                           repeatable='phe_pertag',
                           fulcFields_oi = c('siteid', 'taxonid_pull', 'growthform'),
                           createdDateStart = '2017-01-01',
                           createdDateEnd ='2023-12-01')

ful_df <- select(ful_df, -fulcrum_id, -`_child_record_id`)
ful_df <- distinct(ful_df)

for(i in 1:nrow(ful_df)){
  ful_df$scientificName[i] <- restR::find.scientific.name(ful_df$taxonid_pull[i], type="PLANT")
  ful_df$genus[i] <- str_split(ful_df$scientificName[i], " ", n = 3)[[1]][1]
  ful_df$species[i] <- str_split(ful_df$scientificName[i], " ", n = 3)[[1]][2]
  ful_df$gf[i] <- lovLookup$lovElementName[lovLookup$lovElementCode==ful_df$growthform[i]]
  ful_df$protocolLabel[i] <- paste0(ful_df$genus[i], " ", 
                                    ful_df$species[i], " - ",
                                    ful_df$gf[i])
}

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

write.csv(ful_df, paste(getwd(), "taxBySite_fulcrumFS_20230731.csv", sep='/'),
          row.names=FALSE)

#write.csv(ful_df, 'H:/Phenology/Species selection/allNEONspecies_202110.csv', row.names=F, na="")

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
phe_ind <- l0_ind 

for(i in 1:nrow(phe_ind)){
  phe_ind$scientificName[i] <- restR::find.scientific.name(phe_ind$taxonID[i], type="PLANT")
  phe_ind$genus[i] <- str_split(phe_ind$scientificName[i], " ", n = 3)[[1]][1]
  phe_ind$species[i] <- str_split(phe_ind$scientificName[i], " ", n = 3)[[1]][2]
}

phe_ind$siteID <- substr(phe_ind$individualID, 14, 17)
phe_ind$domainID <- substr(phe_ind$individualID, 10, 12)

phe_ind$d_site <- paste(phe_ind$domainID, phe_ind$siteID, sep='_')
phe_ind$present <- "X"
d_site <- unique(neonSummary$d_site)

neonSummary <- phe_ind%>%
  select(d_site, taxonID, genus, species, growthForm, present)%>%
  distinct()

lovLookup <- get.lov(lovName='phe.growthForm')
df$phenophaseIntensity[!df$phenophaseIntensity%in%lovLookup$lovElementCode]


for(i in 1:nrow(neonSummary)){
    neonSummary$gf[i] <- lovLookup$lovElementName[lovLookup$lovElementCode==neonSummary$growthForm[i]]
    neonSummary$protocolLabel[i] <- paste0(neonSummary$genus[i], " ", 
                                          neonSummary$species[i], " - ",
                                          neonSummary$gf[i])
    
}

neonSummary <- arrange(neonSummary, d_site)

out <- neonSummary%>%
  pivot_wider(names_from = d_site, values_from = present, values_fill = '' )%>%
  arrange(protocolLabel)

gf_conflict <- out$taxonID[duplicated(out$taxonID)]

fix_gf <- out[out$taxonID%in%gf_conflict,]


install.packages("reactable")
library(reactable)
reactable(out, defaultSorted = c("taxonID", "growthForm"))

write.csv(out, paste(getwd(), "taxBySite_20230731.csv", sep='/'),
          row.names=FALSE)

