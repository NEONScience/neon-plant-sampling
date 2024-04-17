setwd("~/Downloads")
library(car)
library(dplyr)



Ca_Nox_Weeds <- read_csv("Downloads/CA_Nox_Weeds.csv")
Copyphenology_db <- read_csv("Downloads/Copyphenology_db.csv")
#Load in your state's list and the overall plant list


Nox_Weeds_List_Wa <- Reduce(intersect, list(Wa_Nox_Weeds$scientific_name,Copyphenology_db$scientificname))
Nox_Weeds_List_Wa
#gives you the noxious weeds that actually occur in the site
Nox_Weeds_List_Ca <- Reduce(intersect, list(Ca_Nox_Weeds$scientific_name,Copyphenology_db$scientificname))
Nox_Weeds_List_Ca
#the same but for california


WaPlants <- subset(Copyphenology_db, Copyphenology_db$domainid=='D16')
CaPlants <- subset(Copyphenology_db, Copyphenology_db$domainid=='D17')
#Limits the phenology database to plants previously seen in the domain of interest


matchplantsWa <- match(Wa_Nox_Weeds$scientific_name , WaPlants$scientificname)
matchplantsWa
matchplantsCa <- match(Ca_Nox_Weeds$scientific_name , CaPlants$scientificname)
matchplantsCa
#returns the location (in the data table) of plants that are on both lists. Not the best way to a solution but it does reduce work?
#Hi Katie Jones! This is where I'm having trouble, I want to create a new column where this automatically fills in Noxious Weed Status in a sort of Presence/Absence Way. 
#I was thinking of using and ifelse function but R doesn't like the lists being different lengths... Maybe a for loop or dplyr function could help?


#Work below here is in order to fill in USDA codes for the noxious weed species that haven't been seen in a domain... yet


Ca_Nox_Weeds$Scientific_Name <- Ca_Nox_Weeds$scientific_name
#small difference in column names came back to bite me here

CaUSDACodes <- semi_join(Copyca_allspp_usda, Ca_Nox_Weeds, by = c("Scientific_Name"))
#brings all of the columns from the usda database but limits them by what is on the noxious weeds list for the state

