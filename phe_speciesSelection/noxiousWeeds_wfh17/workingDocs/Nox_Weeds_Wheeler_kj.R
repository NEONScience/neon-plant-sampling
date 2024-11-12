# KJ: I made filepaths conditional so this script will still work for either of us, if you want 
# to contribute this to the field science collaboration git repo, I can help you format generalized
# paths that should work for anyone working within the R project.

if (file.exists(
  'C:/Users/kjones')){
  setwd("H:/Phenology/Species selection/noxiousWeeds_wfh17") 
}else{
  setwd("~/Downloads")
}

library(car) # is this packaged used?
#library(dplyr)
##KJ: you call read_csv below which is part of the readr package tidyverse will load all Hadley Wickam tidy packages including dplyr
library(tidyverse)

##BW load
Ca_Nox_Weeds <- read_csv("Downloads/CA_Nox_Weeds.csv")
Copyphenology_db <- read_csv("Downloads/Copyphenology_db.csv")
#Load in your state's list and the overall plant list

##kj load
Ca_Nox_Weeds <- read_csv("CA_Nox_Weeds.csv")
Copyphenology_db <- read_csv("Copyphenology_db.csv")
#KJ this file was an xlsx, converted to csv
Wa_Nox_Weeds <- read_csv("Wa_Nox_Weeds.csv")
# KJ: remove extra column, rename sciname
Wa_Nox_Weeds <- select(Wa_Nox_Weeds, common="Common Name", scientific_name="Scientific Name", Rank )
# KJ: this file was not loaded and there appear to be extra columns
Copyca_allspp_usda <- read_csv("Copyca_allspp_usda.csv")



## KJ: I'm unclear what the Reduce funcion is doing in this step. Suggest simplying to:
# Nox_Weeds_List_Wa <-  intersect(Wa_Nox_Weeds$scientific_name, Copyphenology_db$scientificname)
Nox_Weeds_List_Wa <- Reduce(intersect, list(Wa_Nox_Weeds$scientific_name,Copyphenology_db$scientificname))
Nox_Weeds_List_Wa

# ditto here, simplify. Tidyverse can do it all!
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

## try this ifelse

# I created a dup dataframe so I don't mess with your original
df <- Copyphenology_db

df$nox <- ifelse(df$domainid=='D16' & df$scientificname%in%Nox_Weeds_List_Wa, 
                 "State_NW - WA",
                 ifelse(df$domainid=='D17' & df$scientificname%in%Nox_Weeds_List_Ca, 
                        "State_NW - CA", 
                        NA))


#Work below here is in order to fill in USDA codes for the noxious weed species that haven't been seen in a domain... yet


#Ca_Nox_Weeds$Scientific_Name <- Ca_Nox_Weeds$scientific_name
#small difference in column names came back to bite me here

# KJ: try renaming, the code above creates a new column
Ca_Nox_Weeds <- rename(Ca_Nox_Weeds, Scientific_Name=scientific_name)

#
CaUSDACodes <- semi_join(Copyca_allspp_usda, Ca_Nox_Weeds, by = c("Scientific_Name"))
#brings all of the columns from the usda database but limits them by what is on the noxious weeds list for the state

# KJ: this will give you the list of CA listed noxious species not already in the phenology db
# but how will you determine which are relevant by site? (personally, I'd look to the div data...)
newTax <- unique(CaUSDACodes$Symbol)

