##############################################################################################
#' @title Add new Nox List species to phe db

#' @author
#' Katie Jones \email{kjones@battelleecology.org} \cr

#' @description compares nox list of relevant invasive species to existing species on phe db, adds missing options

#' @return OUTPUT DESCRIPTION

# changelog and author contributions / copyrights
#   YOUR NAME (2021-08-23)
#     original creation
##############################################################################################

#load libraries
library(devtools)
library(tidyverse)
library(utils)
library(restR)

api_token = Sys.getenv('FULCRUM_KEY')

#set filepath
if (file.exists(
  'C:/Users/kjones')){
  myPathToFulcrumDev <- "C:/Users/kjones/Documents/GitHub/fulcrumDev/admin_tools_scripts/magpie_requests" 
  myPathToFulcrumDevNew <- "C:/Users/kjones/Documents/GitHub/fulcrumDev/admin_tools_scripts/fulcrumAPI/R"
}

#source useful functions
source(paste(myPathToFulcrumDev, 'get_fulcrum_data.R', sep ="/"))

#pull full pheDB
query <- URLencode('SELECT * FROM "Phenology DB"')
pheDb <- get_fulcrum_data(api_token = api_token, sql = query)

#load nox list
noxList <- read.csv('noxiousWeeds_wfh17/final/nw_additions_allsites.csv', 
                    stringsAsFactors = F)

#explore
names(noxList)
intersect(names(noxList), names(pheDb))

# subset nox list to additions requested by FS
addToDb <- filter(noxList, accept_addition.y.n.== "y")

#add temp field to both dfs
addToDb$temp <- paste(addToDb$siteid, addToDb$taxonid, sep='-')
pheDb$temp <- paste(pheDb$siteid, pheDb$taxonid, sep='-')

#identify addition requests not already present in pheDb
missingFromDb <- setdiff(addToDb$temp, pheDb$temp)

#create new records to be added to pheDb

out_nox <- data.frame()

for(i in missingFromDb){
  add <- addToDb[addToDb$temp==i,] #sub addToDb df
  temp <- data.frame(`_status`= 'unedited', 
                    domainid= add$domainid,
                    siteid=add$site,
                    taxonid = add$taxon,
                    scientificname = add$scientificname,
                    preferredstatus = add$preferredstatus, 
                    remarks = 'state listed noxious weed added by request of FS')
  ifelse(nrow(temp)>1, print(i), NA)
  out_nox <- rbind(out_nox, temp)
  rm(add)
  rm(temp)
}

write.csv(out_nox, 'noxiousWeeds_wfh17/final/add_pheDb_20210823.csv', 
          row.names=F)
