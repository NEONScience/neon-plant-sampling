##############################################################################################
#' @title Phenology DB updates based on SN requests

#' @author
#' kjones \email{EMAIL@battelleecology.org} \cr

#' @description BRIEF DESCRIPTION

#' @return OUTPUT DESCRIPTION

# changelog and author contributions / copyrights
#   YOUR NAME (YYYY-MM-DD)
#     original creation
##############################################################################################

library(devtools)
library(tidyverse)
library(utils)
library(restR2)
library(restR)
library(fulcrumAPI)

api_token = Sys.getenv('FULCRUM_KEY')

pheDb <- get.fulcrum.data(api_token = api_token,
                          appName = "Phenology DB",
                          createdDateStart = "2015-01-01",
                          return_format = "dataframe")

###############################################################################################


##create a new record/s

inc0077445_new <- data.frame(`_status`= 'unedited', 
                           project= 'D12',
                             domainid= 'D12',
                           siteid='YELL',
                           taxonid = 'SHCA',
                           scientificname = find.scientific.name(taxonID="SHCA", 
                                                                 type = "PLANT"),
                           preferredstatus = "", 
                           remarks = 'added 2024-08-19 by request of FS staff')


write.csv(inc0077445_new, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/yell_inc0077445_new.csv', 
          row.names=F)





INC0074170 <- data.frame()

add <- c("POAR7", "JUCO6", "PICO", "SHCA", "BERE", "RISE2", "ARCO9")

for(i in add){
  print(i)
  new <- data.frame(`_status`= 'unedited', 
                    domainid= 'D12',
                    siteid='YELL',
                    taxonid = i,
                    scientificname = find.scientific.name(taxonID=i, 
                                                                 type = "PLANT"),
                    preferredstatus = "", 
                    remarks = 'added 2024-04-15 by request of FS staff')
  INC0074170 <- bind_rows(INC0074170, new)
}


write.csv(INC0074170, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/yell_INC0074170.csv', 
          row.names=F)


ITASK0026884 <- data.frame(`_status`= 'unedited', 
                         domainid= 'D19',
                         siteid='DEJU',
                         taxonid = 'SABE2',
                         scientificname = find.scientific.name(taxonID="SABE2", 
                                                               type = "PLANT"),
                         preferredstatus = "", 
                         remarks = 'added 2023-08-25 by request of FS staff')


write.csv(ITASK0026884, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/deju_ITASK0026884.csv', 
          row.names=F)



INC0067119 <- data.frame(`_status`= 'unedited', 
                         domainid= 'D09',
                         siteid='NOGP',
                         taxonid = 'SHAR',
                         scientificname = find.scientific.name(taxonID="SHAR", 
                                                               type = "PLANT"),
                         preferredstatus = "", 
                         remarks = 'added 2023-08-02 by request of FS staff')


write.csv(INC0067119, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/nogp_INC0067119.csv', 
          row.names=F)


########## update a record

inc0077445_update <- pheDb%>%
  filter(siteid=="YELL" & taxonid%in%c("ANTEN", "PHLOX"))%>%
  select(fulcrum_id, taxonid, scientificname, remarks)

inc0077445_update$taxonid[1] <- "PHMU3"
inc0077445_update$scientificname[1] <- find.scientific.name("PHMU3", type="PLANT")
inc0077445_update$remarks[1] <- 'changed taxonID to PHMU3 from PHLOX, per FS feedback, inc0077445. 2024-08-19'

inc0077445_update$taxonid[2] <- "ANRO2"
inc0077445_update$scientificname[2] <- find.scientific.name("ANRO2", type="PLANT")
inc0077445_update$remarks[2] <- 'changed taxonID to ANRO2 from ANTEN, per FS feedback, inc0077445. 2024-08-19'

write.csv(inc0077445_update, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/yell_inc0077445_update.csv', 
          row.names=F)



itask0021932_update <- pheDb%>%
  filter(siteid=="OAES" & taxonid=="HECO26")%>%
  select(fulcrum_id, taxonid, scientificname, remarks)

itask0021932_update$taxonid <- "HECA8"
itask0021932_update$scientificname <- find.scientific.name("HECA8", type="PLANT")
itask0021932_update$remarks <- 'changed taxonID to HECA8 from HECO26, per FS feedback, itask0021932. 2022-02-01'

write.csv(itask0021932_update, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/oaes_itask0021932_update.csv', 
          row.names=F)


ritm0039292_update <- pheDb%>%
  filter(siteid=='JORN'&taxonid=='ARPU9')%>%
  select(`_record_id`, taxonid, scientificname, remarks)

ritm0039292_update$taxonid <- "ARPUP9"
ritm0039292_update$scientificname <- find.scientific.name("ARPUP9", type="PLANT")
ritm0039292_update$remarks <- 'changed taxonID to ARPUP9 from ARPU9, per FS feedback, RITM0039292. 2021-10-19'

write.csv(ritm0039292_update, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/jorn_ritm0039292_update.csv', 
          row.names=F)

ritm0022073 <- pheDb%>%
  filter(siteid=='SRER'&taxonid=='STREPTANTHUS SP.')%>%
  select(`_record_id`, taxonid, scientificname, remarks)

ritm0022073$taxonid <- 'STCAA'
ritm0022073$scientificname <- 'Streptanthus carinatus C. Wright ex A. Gray ssp. arizonicus'
ritm0022073$remarks <- 'changed taxonID to STCAA from STREPTANTHUS SP., per FS feedback, RITM0022073. 2020-02-13'

write.csv(ritm0022073, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/srer_ritm0022073.csv', 
          row.names=F)

##create a new record
INC0061242 <- data.frame(`_status`= 'unedited', 
                          domainid= 'D03',
                          siteid='JERC',
                          taxonid = 'VADA',
                          scientificname = find.scientific.name(taxonID="VADA", 
                                                               type = "PLANT"),
                          preferredstatus = "", 
                          remarks = 'recently identified species at site, added 2022-12-21 by request of FS staff')


write.csv(INC0061242, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/jerc_INC0061242.csv', 
          row.names=F)


ritm0039292 <- data.frame()

for (i in c("POHA5", "DECO2")){
  temp <- data.frame(`_status`= 'unedited', 
                          domainid= 'D14',
                          siteid='JORN',
                          taxonid = i,
                          scientificname = find.scientific.name(taxonID = i, type= "PLANT"),
                          preferredstatus = "", 
                          remarks = 'not recorded during site characterization, added 2021-10-19 by request of FS staff"')
  ritm0039292 <- bind_rows(ritm0039292, temp)
}

write.csv(ritm0039292, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/jorn_ritm0039292.csv', 
          row.names=F)

ritm0021763 <- data.frame(`_status`= 'unedited', 
                          domainid= 'D01',
                          siteid='HARV',
                          taxonid = 'BETH',
                          scientificname = 'Berberis thunbergii DC.',
                          preferredstatus = "D01 - StateNW", 
                          remarks = 'not recorded during site characterization, added 2020-02-12 by request of FS staff"')

write.csv(ritm0021763, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/harv_ritm0021763.csv', 
          row.names=F)


ritm0028298 <- data.frame(`_status`= 'unedited', 
                          domainid= 'D14',
                          siteid='SRER',
                          taxonid = 'CAGI10',
                          scientificname = find.scientific.name(type='PLANT', taxonID='CAGI10'),
                          preferredstatus = "USA-NPN; Flowers for Bats", 
                          remarks = 'not recorded during site characterization, added 2020-10-21 by request of FS staff"')

write.csv(ritm0028298, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/srer_ritm0028298.csv', 
          row.names=F)

tax_CAGI <- find.taxon.info(
  type='PLANT', 
  taxonID = 'CAGI10',
  encoding = 'UTF-8'
)

my_taxon_id <- find.accepted.taxon.id(
  scientificName = 'Carnegiea gigantea',
  type = 'PLANT')


find.accepted.taxon.id(
  scientificName = regex("^Achillea millefolium\w"),
  type = 'PLANT')


find.accepted.taxon.id(
  scientificName ='Achillea millefolium L.')
  
  
allTax <-   getTaxonTable(
    taxonType = "PLANT",
    recordReturnLimit = NA,
    stream = "true",
    token = Sys.getenv('NEON_KEY')
  )
  
  
###############################################################################################

#update multiple records 

reqLitst <- c("ACNE2")

itask0013252_ABBY <- pheDb%>%
  filter(siteid=='ABBY'& taxonid%in%c('LOCR', 'CHANA2',
                                      'IRIS', 'HIERA',
                                      'VERON', 'GAAP2', 'HYSC2', 'SPDOD', 'CISU',
                                      'TRBOL', 'TROVO2','CLSIS', 'CIRSI', 'SENEC',
                                      'ACGLD4'))%>%
  select(`_record_id`, taxonid, scientificname, remarks)

find.scientific.name(type='PLANT', taxonID='LOCR')

abby_out <- itask0013252_ABBY


for(i in 1:nrow(abby_out)){
  if(abby_out$taxonid[i]=='LOCR'){ 
  ot <- 'LOCR'
  nt <- 'LOAB'
  abby_out$taxonid[i] <- nt
  abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
  abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                             paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0013252. 2020-02-26", sep=' '))
  }else{
    if(abby_out$taxonid[i]=='CHANA2'){
      ot <- 'CHANA2'
      nt <- 'CHAN9'
      abby_out$taxonid[i] <- nt
      abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
      abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                    paste("changed taxonID to", nt, "from", ot, 
                                          "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                    paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                          "per FS feedback, itask0013252. 2020-02-26", sep=' '))
    }else{
      if(abby_out$taxonid[i]=='IRIS'){
        ot <- 'IRIS'
        nt <- 'IRTE'
        abby_out$taxonid[i] <- nt
        abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
        abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                      paste("changed taxonID to", nt, "from", ot, 
                                            "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                      paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                            "per FS feedback, itask0013252. 2020-02-26", sep=' '))
      }else{
        if(abby_out$taxonid[i]=='VERON'){
          ot <- 'VERON'
          nt <- 'VEOF2'
          abby_out$taxonid[i] <- nt
          abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
          abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                        paste("changed taxonID to", nt, "from", ot, 
                                              "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                        paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                              "per FS feedback, itask0013252. 2020-02-26", sep=' '))
        }else{
          if(abby_out$taxonid[i]=='GAAP2'){
            ot <- 'GAAP2'
            nt <- 'GATR3'
            abby_out$taxonid[i] <- nt
            abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
            abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                          paste("changed taxonID to", nt, "from", ot, 
                                                "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                          paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                "per FS feedback, itask0013252. 2020-02-26", sep=' '))
          }else{
            if(abby_out$taxonid[i]=='HYSC2'){
              ot <- 'HYSC2'
              nt <- 'HYPE'
              abby_out$taxonid[i] <- nt
              abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
              abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                            paste("changed taxonID to", nt, "from", ot, 
                                                  "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                            paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                  "per FS feedback, itask0013252. 2020-02-26", sep=' '))
            }else{
              if(abby_out$taxonid[i]=='SPDOD'){
                ot <- 'SPDOD'
                nt <- 'SPDO'
                abby_out$taxonid[i] <- nt
                abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                              paste("changed taxonID to", nt, "from", ot, 
                                                    "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                              paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                    "per FS feedback, itask0013252. 2020-02-26", sep=' '))
              }else{
                if(abby_out$taxonid[i]=='CISU'){
                  ot <- 'CISU'
                  nt <- 'CIVU'
                  abby_out$taxonid[i] <- nt
                  abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                  abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                                paste("changed taxonID to", nt, "from", ot, 
                                                      "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                                paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                      "per FS feedback, itask0013252. 2020-02-26", sep=' '))
                }else{
                  if(abby_out$taxonid[i]=='TRBOL'){
                    ot <- 'TRBOL'
                    nt <- 'TRBO2'
                    abby_out$taxonid[i] <- nt
                    abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                    abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                                  paste("changed taxonID to", nt, "from", ot, 
                                                        "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                                  paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                        "per FS feedback, itask0013252. 2020-02-26", sep=' '))
                  }else{
                    if(abby_out$taxonid[i]=='TROVO2'){
                      ot <- 'TROVO2'
                      nt <- 'TROV2'
                      abby_out$taxonid[i] <- nt
                      abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                      abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                                    paste("changed taxonID to", nt, "from", ot, 
                                                          "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                                    paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                          "per FS feedback, itask0013252. 2020-02-26", sep=' '))
                    }else{
                      if(abby_out$taxonid[i]=='CLSIS'){
                        ot <- 'CLSIS'
                        nt <- 'CLSI2'
                        abby_out$taxonid[i] <- nt
                        abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                        abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                                      paste("changed taxonID to", nt, "from", ot, 
                                                            "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                                      paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                            "per FS feedback, itask0013252. 2020-02-26", sep=' '))
                      }else{
                        if(abby_out$taxonid[i]=='CIRSI'){
                          ot <- 'CIRSI'
                          nt <- 'CIAR4'
                          abby_out$taxonid[i] <- nt
                          abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                          abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                                        paste("changed taxonID to", nt, "from", ot, 
                                                              "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                                        paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                              "per FS feedback, itask0013252. 2020-02-26", sep=' '))
                        }else{
                          if(abby_out$taxonid[i]=='ACGLD4'){
                            ot <- 'ACGLD4'
                            nt <- 'ACCI'
                            abby_out$taxonid[i] <- nt
                            abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                            abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                                          paste("changed taxonID to", nt, "from", ot, 
                                                                "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                                          paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                                "per FS feedback, itask0013252. 2020-02-26", sep=' '))
                          }else{
                            if(abby_out$taxonid[i]=='HIERA'){
                              ot <- 'HIERA'
                              nt <- 'HIAL2'
                              abby_out$taxonid[i] <- nt
                              abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                              abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                                            paste("changed taxonID to", nt, "from", ot, 
                                                                  "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                                            paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                                  "per FS feedback, itask0013252. 2020-02-26", sep=' '))
                            }else{
                              if(abby_out$taxonid[i]=='SENEC'){
                                ot <- 'SENEC'
                                nt <- 'SESY'
                                abby_out$taxonid[i] <- nt
                                abby_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
                                abby_out$remarks[i] <- ifelse(is.na(abby_out$remarks[i]), 
                                                              paste("changed taxonID to", nt, "from", ot, 
                                                                    "per FS feedback, itask0013252. 2020-02-26", sep=' '),
                                                              paste(abby_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                                                    "per FS feedback, itask0013252. 2020-02-26", sep=' '))
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

write.csv(abby_out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0013252_ABBY_edit.csv', 
          row.names=F)

itask0013252_ABBY_add <- data.frame(`_status`= 'unedited', 
                          domainid= 'D16',
                          siteid='ABBY',
                          taxonid = 'SANI4',
                          scientificname =  find.scientific.name(type='PLANT', 'SANI4'),
                          rank = pheDb$rank[pheDb$siteid=='ABBY' & pheDb$taxonid=='SARA2'],
                          remarks = 'added 2020-02-26 by request of FS staff')

b <- data.frame(`_status`= 'unedited', 
                domainid= 'D16',
                siteid='ABBY',
                taxonid = 'HISC2',
                scientificname =  find.scientific.name(type='PLANT', 'HISC2'),
                rank = pheDb$rank[pheDb$siteid=='ABBY' & pheDb$taxonid=='HIERA'],
                remarks = 'added 2020-02-26 by request of FS staff')


itask0013252_ABBY_add <- rbind(itask0013252_ABBY_add, b)

c <- data.frame(`_status`= 'unedited', 
                domainid= 'D16',
                siteid='ABBY',
                taxonid = 'SEJA',
                scientificname =  find.scientific.name(type='PLANT', 'SEJA'),
                rank = pheDb$rank[pheDb$siteid=='ABBY' & pheDb$taxonid=='SENEC'],
                remarks = 'added 2020-02-26 by request of FS staff')

itask0013252_ABBY_add <- rbind(itask0013252_ABBY_add, c)

write.csv(itask0013252_ABBY_add, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0013252_ABBY_add.csv', 
          row.names=F)

###############################################################################################

itask0013252_WREF <- pheDb%>%
  filter(siteid=='WREF'& taxonid%in%c('HISCA', 'CORAL5', 'STREP3'))%>%
  select(`_record_id`, taxonid, scientificname, remarks)

itask0013252_WREF_add <- data.frame()

req <- c('ACRU2', 'OSBE', 'MADI', 'MARA7')

for(i in 1:length(req)){
  temp <- data.frame(
  `_status`= 'unedited', 
  domainid= 'D16',
  siteid='WREF',
  taxonid = req[i],
  scientificname =  find.scientific.name(type='PLANT', req[i]),
  remarks = 'added 2020-03-03 by request of FS staff')
  itask0013252_WREF_add <- rbind(itask0013252_WREF_add, temp)
} 

write.csv(itask0013252_WREF_add, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0013252_WREF_add.csv', 
          row.names=F)

###############################################################################################

itask0015043_DEJU <- pheDb%>%
  filter(siteid=='DEJU'& taxonid%in%c('LEPAD', 'POACE'))%>%
  select(`_record_id`, taxonid, scientificname, remarks)

deju_out <- itask0015043_DEJU


for(i in 1:nrow(deju_out)){
  if(deju_out$taxonid[i]=='COCA13'){ 
    ot <- 'COCA13'
    nt <- 'COUN'
    deju_out$taxonid[i] <- nt
    deju_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    deju_out$remarks[i] <- ifelse(is.na(deju_out$remarks[i]), 
                                  paste("changed taxonID to", nt, "from", ot, 
                                        "per FS feedback, itask0015043. 2020-03-03", sep=' '),
                                  paste(deju_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                        "per FS feedback, itask0015043. 2020-03-03", sep=' '))
  }
}

for(i in 1:nrow(deju_out)){
  if(deju_out$taxonid[i]=='LEPAD'){ 
    ot <- 'LEPAD'
    nt <- 'LEPA11'
    deju_out$taxonid[i] <- nt
    deju_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    deju_out$remarks[i] <- ifelse(is.na(deju_out$remarks[i]), 
                                  paste("changed taxonID to", nt, "from", ot, 
                                        "per FS feedback, itask0015043. 2021-02-15", sep=' '),
                                  paste(deju_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                        "per FS feedback, itask0015043. 2020-03-03", sep=' '))
  }else if(deju_out$taxonid[i]=='POACE'){  
    ot <- 'POACE'
    nt <- 'FEAL'
    deju_out$taxonid[i] <- nt
    deju_out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    deju_out$remarks[i] <- ifelse(is.na(deju_out$remarks[i]), 
                                  paste("changed taxonID to", nt, "from", ot, 
                                        "per FS feedback, itask0015043. 2021-02-15", sep=' '),
                                  paste(deju_out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                        "per FS feedback, itask0015043. 2020-03-03", sep=' '))
  }
}


write.csv(deju_out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0015043_DEJU_edit_2.csv', 
          row.names=F)

###############################################################################################


req_list <- c('COCA13')

itask0015044_BONA <- pheDb%>%
  filter(siteid=='BONA'& taxonid%in%req_list)%>%
  select(`_record_id`, taxonid, scientificname, remarks)

out <- itask0015044_BONA

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='COCA13'){ 
    ot <- 'COCA13'
    nt <- 'COUN'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                                  paste("changed taxonID to", nt, "from", ot, 
                                        "per FS feedback, itask0015043. 2020-03-03", sep=' '),
                                  paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                        "per FS feedback, itask0015043. 2020-03-03", sep=' '))
  }
}

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0015044_BONA_edit.csv', 
          row.names=F)

###############################################################################################

req_list <- c('CAREX')

itask0015045_TOOL <- pheDb%>%
  filter(siteid=='TOOL'& taxonid%in%req_list)%>%
  select(`_record_id`, taxonid, scientificname, remarks)

out <- itask0015045_TOOL

#find.taxon.id(type="PLANT", 'Carex bigelowii Torr. ex Schwein.')

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='CAREX'){ 
    ot <- 'CAREX'
    nt <- 'CABI5'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0015045. 2020-03-04", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0015045. 2020-03-04", sep=' '))
  }
}

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0015045_TOOL_edit.csv', 
          row.names=F)

###############################################################################################

req_list <- c('ERVA4', 'STCR')

itask0015046_BARR <- pheDb%>%
  filter(siteid=='BARR'& taxonid%in%req_list)%>%
  select(`_record_id`, taxonid, scientificname, remarks)

out <- itask0015046_BARR

#find.taxon.id(type="PLANT", 'Carex bigelowii', stack='PROD')

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='ERVA4'){ 
    ot <- 'ERVA4'
    nt <- 'ERRU2'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0015046. 2020-03-04", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0015046. 2020-03-04", sep=' '))
  }else if(out$taxonid[i]=='STCR'){
    ot <- 'STCR'
    nt <- 'STLO2'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0015046. 2020-03-04", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0015046. 2020-03-04", sep=' '))
  }
}

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0015046_BARR_edit.csv', 
          row.names=F)


itask0015046_BARR_add <- data.frame(`_status`= 'unedited', 
                                    domainid= 'D18',
                                    siteid='BARR',
                                    taxonid = 'POAR2',
                                    scientificname =  find.scientific.name(type='PLANT', 'POAR2'),
                                    #rank = pheDb$rank[pheDb$siteid=='ABBY' & pheDb$taxonid=='SARA2'],
                                    remarks = 'added 2020-03-05 by request of FS staff. Species is common but not reflected in characterization data.')

b <- data.frame(`_status`= 'unedited', 
                domainid= 'D18',
                siteid='BARR',
                taxonid = 'LUCO5',
                scientificname =  find.scientific.name(type='PLANT', 'LUCO5'),
                #rank = pheDb$rank[pheDb$siteid=='ABBY' & pheDb$taxonid=='HIERA'],
                remarks = 'added 2020-03-05 by request of FS staff. Species is common but not reflected in characterization data.')


itask0015046_BARR_add <- rbind(itask0015046_BARR_add, b)

c <- data.frame(`_status`= 'unedited', 
                domainid= 'D18',
                siteid='BARR',
                taxonid = 'CAPA5',
                scientificname =  find.scientific.name(type='PLANT', 'CAPA5'),
                #rank = pheDb$rank[pheDb$siteid=='ABBY' & pheDb$taxonid=='SENEC'],
                remarks = 'added 2020-03-05 by request of FS staff. Species is common but not reflected in characterization data.')

itask0015046_BARR_add <- rbind(itask0015046_BARR_add, c)

write.csv(itask0015046_BARR_add, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0015046_BARR_add.csv', 
          row.names=F)

###############################################################################################

req_list <- c('MEVI')

itask0015222_MLBS <- pheDb%>%
  filter(siteid=='MLBS'& taxonid%in%req_list)%>%
  select(`_record_id`, taxonid, scientificname, remarks)

out <- itask0015222_MLBS

#find.taxon.id(type="PLANT", 'Carex bigelowii Torr. ex Schwein.')

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='MEVI'){ 
    ot <- 'MEVI'
    nt <- 'TRBO2'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0015222. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0015222. 2020-03-05", sep=' '))
  }
}

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0015222_MLBS_edit.csv', 
          row.names=F)

###############################################################################################


req_list <- c('QUMA3', 'CILUC', 'DUIN', 'POVI2', 'BOVI', 'POAR6', 'BODI2', 'OPVU', 'POPE10')

itask0014893_SERC <- pheDb%>%
  filter(siteid=='SERC'& taxonid%in%req_list)%>%
  select(`_record_id`, taxonid, scientificname, remarks)

out <- itask0014893_SERC

#find.taxon.id(type="PLANT", 'Carex bigelowii Torr. ex Schwein.')

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='QUMA3'){ 
    ot <- 'QUMA3'
    nt <- 'QUFA'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='CILUC'){
    ot <- 'CILUC'
    nt <- 'CILU'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='DUIN'){
    ot <- 'DUIN'
    nt <- 'POIN5'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='POVI2'){
    ot <- 'POVI2'
    nt <- 'PEVI13'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='BOVI'){
    ot <- 'BOVI'
    nt <- 'BOVI8'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='POAR6'){
    ot <- 'POAR6'
    nt <- 'PEAR13'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='BODI2'){
    ot <- 'BODI2'
    nt <- 'SCDI8'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='OPVU'){
    ot <- 'OPVU'
    nt <- 'OPPY3'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='POPE10'){
    ot <- 'POPE10'
    nt <- 'PEPE35'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014893. 2020-03-05", sep=' '))
  }
}

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0014893_SERC_edit.csv', 
          row.names=F)

############### 

req_list <- c('CILUC', 'ANAR', 'DUIN', 'POVI2')

itask0014892_BLAN <- pheDb%>%
  filter(siteid=='BLAN'& taxonid%in%req_list)%>%
  select(`_record_id`, taxonid, scientificname, remarks)

out <- itask0014892_BLAN

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='ANAR'){ 
    ot <- 'ANAR'
    nt <- 'LYAR6'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='CILUC'){
    ot <- 'CILUC'
    nt <- 'CILU'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='DUIN'){
    ot <- 'DUIN'
    nt <- 'POIN5'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='POVI2'){
    ot <- 'POVI2'
    nt <- 'PEVI13'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }
}

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0014892_BLAN_edit.csv', 
          row.names=F)

###############################################################################################

req_list <- c('POVI2', 'CILUC', 'SMTA2', 'POPE10', 'BOVI', 'POCE4', 'VITR2', 'DIQU', 
              'DUIN', 'RUAR2', 'CAAL27', 'HULU', 'SANIC4')

itask0014891_SCBI <- pheDb%>%
  filter(siteid=='SCBI'& taxonid%in%req_list)%>%
  select(`_record_id`, taxonid, scientificname, remarks)

out <- itask0014891_SCBI

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='POVI2'){ 
    ot <- 'POVI2'
    nt <- 'PEVI13'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014891. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014891. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='CILUC'){
    ot <- 'CILUC'
    nt <- 'CILU'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='SMTA2'){
    ot <- 'SMTA2'
    nt <- 'SMHI'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='POPE10'){
    ot <- 'POPE10'
    nt <- 'PEPE35'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='BOVI'){
    ot <- 'BOVI'
    nt <- 'BOVI8'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='POCE4'){
    ot <- 'POCE4'
    nt <- 'PELO10'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='VITR2'){
    ot <- 'VITR2'
    nt <- 'VIPA3'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='DIQU'){
    ot <- 'DIQU'
    nt <- 'DIVI4'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='DUIN'){
    ot <- 'DUIN'
    nt <- 'POIN5'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='RUAR2'){
    ot <- 'RUAR2'
    nt <- 'RUPE3'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='CAAL27'){
    ot <- 'CAAL27'
    nt <- 'CATO6'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='HULU'){
    ot <- 'HULU'
    nt <- 'HUJA'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }else if(out$taxonid[i]=='SANIC4'){
    ot <- 'SANIC4'
    nt <- 'SACA12'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0014892. 2020-03-05", sep=' '))
  }
}

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0014891_SCBI_edit.csv', 
          row.names=F)

############# 2021-01-14 ################

ritm0029275_new <- data.frame(`_status`= 'unedited', 
                          domainid= 'D01',
                          siteid='HARV',
                          taxonid = 'MARAR',
                          scientificname = find.scientific.name(type='PLANT', taxonID='MARAR'),
                          preferredstatus = "", 
                          remarks = 'not recorded during site characterization, added 2021-01-14 by request of FS staff"')

b <- data.frame(`_status`= 'unedited', 
                domainid= 'D01',
                siteid='HARV',
                taxonid = 'VIAC',
                scientificname = find.scientific.name(type='PLANT', taxonID='VIAC'),
                preferredstatus = "USA-NPN; Shady Invaders", 
                remarks = 'not recorded during site characterization, added 2021-01-14 by request of FS staff"')

ritm0029275_new <- bind_rows(ritm0029275_new, b)

write.csv(ritm0029275_new, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/ritm0029275_new.csv', 
          row.names=F)

req_list <- c('ACSA3', 'TRBO2', 'PRSE2')
ritm0029275_harv <- pheDb%>%
  filter(siteid%in%c('HARV') & taxonid%in%req_list )%>%
  select(`_record_id`, taxonid, scientificname, remarks)

out <- ritm0029275_harv

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='ACSA3'){ 
    ot <- 'ACSA3'
    nt <- 'ACSAS'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                           paste("changed taxonID to", nt, "from", ot, 
                                 "per FS feedback, ritm0029275. 2021-01-14", sep=' '),
                           paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                 "per FS feedback, ritm0029275. 2021-01-14", sep=' '))
  }else if(out$taxonid[i]=='PRSE2'){
  ot <- 'PRSE2'
  nt <- 'PRSES'
  out$taxonid[i] <- nt
  out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
  out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                           paste("changed taxonID to", nt, "from", ot, 
                                 "per FS feedback, ritm0029275. 2021-01-14", sep=' '),
                           paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                 "per FS feedback, ritm0029275. 2021-01-14", sep=' '))
  }else if(out$taxonid[i]=='TRBO2'){
    ot <- 'TRBO2'
    nt <- 'TRBOB'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, ritm0029275. 2021-01-14", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, ritm0029275. 2021-01-14", sep=' '))
  }
}

ritm0029275_bart <- pheDb%>%
    filter(siteid%in%c('BART') & taxonid=='TRBO2' )%>%
    select(`_record_id`, taxonid, scientificname, remarks)
  
out_2 <- ritm0029275_bart

ot <- 'TRBO2'
nt <- 'TRBOB'
out_2$taxonid <- nt
out_2$scientificname <- find.scientific.name(type='PLANT', nt)
out_2$remarks <- ifelse(is.na(out_2$remarks), 
                         paste("changed taxonID to", nt, "from", ot, 
                               "per FS feedback, ritm0029275. 2021-01-14", sep=' '),
                         paste(out_2$remarks, "|", "changed taxonID to", nt, "from", ot, 
                               "per FS feedback, ritm0029275. 2021-01-14", sep=' '))

out <- rbind(out, out_2)

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/ritm0029275_d01_edit.csv', 
          row.names=F)
    

# RITM0029961
# D04-LAJA add RUTU, AMSP, CYDA, BOPE2

a <- data.frame(`_status`= 'unedited', 
                domainid= 'D04',
                siteid='LAJA',
                taxonid = 'RUTU',
                scientificname = find.scientific.name(type='PLANT', taxonID='RUTU'),
                preferredstatus = '', 
                remarks = 'not recorded during site characterization, added 2021-01-15 by request of FS staff"')

b <- data.frame(`_status`= 'unedited', 
                domainid= 'D04',
                siteid='LAJA',
                taxonid = 'AMSP',
                scientificname = find.scientific.name(type='PLANT', taxonID='AMSP'),
                preferredstatus = "", 
                remarks = 'not recorded during site characterization, added 2021-01-15 by request of FS staff"')

c <- data.frame(`_status`= 'unedited', 
                domainid= 'D04',
                siteid='LAJA',
                taxonid = 'CYDA',
                scientificname = find.scientific.name(type='PLANT', taxonID='CYDA'),
                preferredstatus = "", 
                remarks = 'not recorded during site characterization, added 2021-01-15 by request of FS staff"')

d <- data.frame(`_status`= 'unedited', 
                domainid= 'D04',
                siteid='LAJA',
                taxonid = 'BOPE2',
                scientificname = find.scientific.name(type='PLANT', taxonID='BOPE2'),
                preferredstatus = "", 
                remarks = 'not recorded during site characterization, added 2021-01-15 by request of FS staff"')

out <- bind_rows(a, b, c, d)

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/ritm0029961_d04_add.csv', 
          row.names=F)


## redo phe - YELL list - RITM0023370
## 
query <- URLencode('SELECT * FROM "Phenology DB"')
pheDb <- get_fulcrum_data(api_token = api_token, sql = query)

source(paste(myPathToFulcrumDevNew, 'delete_record.R', sep="/"))

db_y <- filter(pheDb, siteid=="YELL")

write.csv(db_y, "H:/Phenology/Species selection/deleted_yell_dbRecs_20210211.csv",
          row.names=FALSE)

record_list = db_y$`_record_id`


# Delete old db records
sapply(X = record_list, FUN = delete_record, 
       api_token = api_token)

y_in <- read.csv('H:/Phenology/Species selection/CSP_analyses/task_1b_YELL_ranks_and_weights_2019.csv', stringsAsFactors=FALSE)

intersect(names(pheDb), tolower(names(y_in)))

sort(setdiff(names(pheDb)), tolower(names(y_in)))

library(restR)

for(i in 1:nrow(y_in)){
  y_in$scientificName[i] <- find.scientific.name(taxonID=y_in$taxonID[i], type="PLANT", stack="prod")
}

stat_update <- select(db_y, taxonID=taxonid, `_status`, selectionstatus, remarks)

y_out <- left_join(y_in, stat_update)
y_out$`_status`[is.na(y_out$`_status`)] <- "unedited"

write.csv(y_out, "C:/Users/kjones/Desktop/temp/yell_pheDB_upload.csv", row.names=FALSE,
          na="")


## itask0018483 D05

db05 <- pheDb%>%
  filter(`_record_id`%in%c("d4f0112c-645f-4897-9b6d-61f105a7b3c1", "dcabb621-45ce-4b51-bfdd-1f897f12d862"))%>%
 select(`_record_id`, taxonid, scientificname, remarks)

out <- db05

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='CACO7'){ 
    ot <- 'CACO7'
    nt <- 'CAAR3'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to",nt,
                                   "from", ot, 
                                   "per FS feedback, itask0018483. 2021-02-22", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0018483. 2021-02-22", sep=' '))
  }else if(out$taxonid[i]=='CAPE6'){
    ot <- 'CAPE6'
    nt <- 'CAPE4'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0018483. 2021-02-22", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0018483. 2021-02-22", sep=' '))
  }
}
    
write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0018483_d05_edit.csv', 
          row.names=F)


### ITASK0018487
out <- pheDb%>%
  filter(siteid=="GRSM" & taxonid=="PRENA")%>%
  select(`_record_id`, taxonid, scientificname, remarks)

for(i in 1:nrow(out)){
  if(out$taxonid[i]=='PRENA'){ 
    ot <- 'PRENA'
    nt <- 'PRAL3'
    out$taxonid[i] <- nt
    out$scientificname[i] <- find.scientific.name(type='PLANT', nt)
    out$remarks[i] <- ifelse(is.na(out$remarks[i]), 
                             paste("changed taxonID to",nt,
                                   "from", ot, 
                                   "per FS feedback, itask0018497. 2021-02-26", sep=' '),
                             paste(out$remarks[i], "|", "changed taxonID to", nt, "from", ot, 
                                   "per FS feedback, itask0018497. 2021-02-26", sep=' '))
  }
}

write.csv(out, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/itask0018497_d07_edit.csv', 
          row.names=F)

##create a new record
INC0050913 <- data.frame(`_status`= 'unedited', 
                     domainid= 'D03',
                     siteid='DSNY',
                     taxonid = "CRCO28", 
                     scientificname = find.scientific.name(taxonID = "CRCO28", type= "PLANT"),
                     preferredstatus = "", 
                     remarks = 'not recorded during site characterization, added 2021-12-16 by request of FS staff, inc0050913')

write.csv(INC0050913, 'C:/Users/kjones/Desktop/temp/phe_db_updateRequests/dsny_INC0050913.csv', 
          row.names=F)
