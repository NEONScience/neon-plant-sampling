##############################################################################################
#' @title 

#' @author
#' YOUR NAME \email{EMAIL@battelleecology.org} \cr

#' @description BRIEF DESCRIPTION

#' @return OUTPUT DESCRIPTION

# changelog and author contributions / copyrights
#   YOUR NAME (YYYY-MM-DD)
#     original creation
##############################################################################################

library(tidyverse)
library(fulcrumAPI)
library(restR)

df <- get.fulcrum.data(api_token = Sys.getenv('FULCRUM_PAT'),
                       appName = 'TOS: Vegetation Cover Assessment [PROD]',
                       createdDateStart = '2021-01-01')

soap <- filter(df, siteid=="SOAP")

table(soap$height_over_2m)
nrow(soap[soap$height_over_2m=="Y",])/nrow(soap)
#68
nrow(soap[soap$tree_growth_form_dbh_10cm=="Y" & soap$height_over_2m=="Y",])/nrow(soap)
#39

