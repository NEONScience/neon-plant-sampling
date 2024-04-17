# KJ: I made filepaths conditional so this script will still work for either of us, if you want 
# to contribute this to the field science collaboration git repo, I can help you format generalized
# paths that should work for anyone working within the R project.

if (file.exists(
  'C:/Users/kjones')){
  setwd("H:/Phenology/Species selection/noxiousWeeds_wfh17") 
}else{
  setwd("~/Downloads")
}

library(tidyverse)
library(traits)

##BW load
Ca_Nox_Weeds <- read_csv("Downloads/CA_Nox_Weeds.csv")
Copyphenology_db <- read_csv("Downloads/Copyphenology_db.csv")
#Load in your state's list and the overall plant list

##kj load
Ca_Nox_Weeds <- read_csv("CA_Nox_Weeds.csv")
Copyphenology_db <- read_csv("Copyphenology_db.csv")
Wa_Nox_Weeds <- read_csv("Wa_Nox_Weeds.csv")

# KJ: remove extra column, rename sciname
Wa_Nox_Weeds <- select(Wa_Nox_Weeds, common_names="Common Name", scientific_name="Scientific Name", Rank )

# create a dup dataframe so you don't mess with the original
df <- Copyphenology_db

# assign nw status if present in both the weed list and the phenology db
# note, this will miss synonyms, exact matches only

df$nox <- ifelse(df$domainid=='D16' & df$scientificname%in%Nox_Weeds_List_Wa, 
                        "State_NW - WA",
                 ifelse(df$domainid=='D17' & df$scientificname%in%Nox_Weeds_List_Ca, 
                               "State_NW - CA", 
                        NA))
################
# Get taxonIDs from USDA for a list of scientific names
################


#names(Ca_Nox_Weeds)
#names(Wa_Nox_Weeds)

#let's align the column names
Ca_Nox_Weeds <- rename(Ca_Nox_Weeds, Rank=rating)
Ca_Nox_Weeds$state <- "CA"

Wa_Nox_Weeds$state <- "WA"

#check that they match
setdiff(names(Ca_Nox_Weeds), names(Wa_Nox_Weeds))

all_nox <- bind_rows(Ca_Nox_Weeds, Wa_Nox_Weeds) #this function will bind as many dataframes as you need

#separate sciname to separate fields for genus and species 
all_nox$Genus <- str_split(all_nox$scientific_name, "\\s+", simplify=T)[,1]
all_nox$Species <- str_split(all_nox$scientific_name, "\\s+", simplify=T)[,2]

#initiate blank dataframe
usda_taxList <- data.frame()

for(i in 1:nrow(all_nox)){
  test <- try(tr_usda(query = list(genus = all_nox$Genus[i], 
                                   species = all_nox$Species[i]), 
                      fields = c('Symbol', 'Genus', 'Species'))$data, silent=TRUE)
  if(class(test) %in% 'try-error') ## Error handling - if request to usda fails, skip and move to the next row.
    {next} 
  else {temp <- tr_usda(query = list(genus = all_nox$Genus[i], 
                                     species = all_nox$Species[i]), 
                                     fields = c('Accepted_Symbol_x', 'Genus', 'Species'), 
                        limit=1)$data} 
  usda_taxList <- rbind(usda_taxList, temp)
  print(i)
}

# get rid of duplicate rows
usda_taxList <- distinct(usda_taxList)

# append taxonid to list of species. 
all_nox <- left_join(all_nox, usda_taxList)
