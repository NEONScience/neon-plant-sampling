library(tidyverse)
library(restR2)
library(neonUtilities)
library(neonOS)
library(restR2)

# if edits are necessary

l0Box <- 'C:/Users/kjones/Box/L0dataEditing'

newFolder <- 'phe_2025releaseClean'

###
#crosswalk file - download from 
#(https://docs.google.com/spreadsheets/d/1siKL8Oi3ArzNQXLWdaQuADo6Q0BcB5BFN2GvkPUxxCw/edit?gid=1061307607#gid=1061307607)

gsheet <- read.csv('C:/Users/kjones/Downloads/NPN-NEONspecies_20240918.csv', stringsAsFactors = F)

# Download all tags

dat <- loadByProduct(dpID="DP1.10055.001",
                     tabl="phe_perindividual",
                     package = "basic",
                     check.size = FALSE, 
                     token = Sys.getenv('NEON_KEY'))

l1_ind <- dat$phe_perindividual


# L0 -------------------------------------------------
l0_ind <- par.get.os.l0.data(stack='prod',
                   dpID = 'DP0.10002.001',
                   startDate = '2013-01-01',
                   endDate = '2025-05-01',
                   ingestTable='phe_perindividual_in',
                   format_for_L0_editor=TRUE)

#####

neonList <- l1_ind%>%
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

#alreadyAdded <- c('JUCOD')

#toAdd <- toAdd[!toAdd$NEON.taxonID%in%alreadyAdded,]

write.csv(toAdd, 'H:/Phenology/Species selection/new_Neon_Species_20240918.csv', row.names=F, na="")


###check fulcrum ###
api_token = Sys.getenv('FULCRUM_KEY')

db_req <-get.fulcrum.data(apiToken = Sys.getenv('FULCRUM_KEY'),
                         appName="Phenology DB", 
                         fulcrumFields = c('siteid', 'taxonid', 'scientificname', 'selectionstatus'),
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


ful_df <- restR2::get.fulcrum.data(apiToken = Sys.getenv('FULCRUM_KEY'),
                           appName="PHE: Per Individual INGEST [PROD]", 
                           #repeatableName='phe_pertag',
                           fulcrumFields = c('siteid', 'taxonid', 'growthform'), #'siteid', restR2 can't handle fields from parent & child tables
                           startCreatedDate = '2017-01-01',
                           endCreatedDate ='2025-05-01')

ful_df <- ful_df%>%
  select(, -`_record_id`, siteID = siteid, taxonID = taxonid, 
         growthForm = growthform)%>%  
  filter(!siteID%in%c('KONA', 'NOGP', 'STEI', 'STER'))%>% # deprecated sites
  distinct()

lovLookup <- get.lov(lovName='phe.growthForm')

for(i in 1:nrow(ful_df)){
  if(!is.na(ful_df$taxonID[i])){
    ful_df$scientificName[i] <- restR::find.scientific.name(ful_df$taxonID[i], type="PLANT")
    ful_df$genus[i] <- str_split(ful_df$scientificName[i], " ", n = 3)[[1]][1]
    ful_df$species[i] <- str_split(ful_df$scientificName[i], " ", n = 3)[[1]][2]
    ful_df$gf[i] <- lovLookup$lovElementName[lovLookup$lovElementCode==ful_df$growthForm[i]]
    ful_df$protocolLabel[i] <- paste0(ful_df$genus[i], " ", 
                                      ful_df$species[i], " - ",
                                      ful_df$gf[i])
  }
}

write.csv(ful_df, 'H:/Phenology/Species selection/allNEONspecies_202409.csv', row.names=F, na="")


##### Summary for FS
phe_ind <- read.csv('H:/Phenology/Species selection/allNEONspecies_202409.csv', header=T, na="")

siteList <- read.csv('C:/Users/kjones/Documents/R/NEON_Field_Site_Metadata_20240918.csv', header=T)

siteList <- siteList%>%
  select(domainID=field_domain_id, siteID=field_site_id)

phe_ind <- left_join(phe_ind, siteList)
  

phe_ind$d_site <- paste(phe_ind$domainID, phe_ind$siteID, sep='_')
phe_ind$present <- "X"
d_site <- unique(phe_ind$d_site)

neonSummary <- phe_ind%>%
  select(d_site, taxonID, genus, species, growthForm, present)%>%
  distinct()


for(i in 1:nrow(neonSummary)){
    #neonSummary$gf[i] <- lovLookup$lovElementName[lovLookup$lovElementCode==neonSummary$growthForm[i]]
    neonSummary$protocolLabel[i] <- paste0(neonSummary$genus[i], " ", 
                                          neonSummary$species[i], " - ",
                                          neonSummary$gf[i])
    
}

neonSummary <- arrange(neonSummary, d_site)

out <- neonSummary%>%
  pivot_wider(names_from = d_site, values_from = present, values_fill = '' )%>%
  arrange(protocolLabel)%>%
  select(-growthForm)%>%
  mutate(siteCount = rowSums(sapply(out, grepl, pattern = "X")))%>%
  relocate(siteCount, .after=protocolLabel)

gf_conflict <- out$taxonID[duplicated(out$taxonID)]

fix_gf <- out[out$taxonID%in%gf_conflict,]

#install.packages("reactable")
library(reactable)
reactable(out, defaultSorted = c("taxonID", "growthForm"))

write.csv(out, paste('H:/Phenology/Species selection', "taxBySite_20240919.csv", sep='/'),
          row.names=FALSE)

