# Here’s an example of doing an API call to Fulcrum for several domains worth of data. When there are too many records to pull the entire data table, the Fulcrum server request time out (causing your R function to fail).

# This example is geared towards grabbing and joining Veg Structure – Apparent Individuals data (LOTS of child records):
  
## libraries
library(httr) # talk to the api
library(jsonlite)
library(plyr) # munge lists
library(dplyr) # filter and arrange

## Define a function that fulfills your processing needs
##  Here we call the SQL API and convert data from JSON to a data frame
get_child_domain <- function(parent, child, api_token, domainid){
  ## regular request
  sql = paste(URLencode(paste0('SELECT * FROM "',parent,'" AS parent')),
              URLencode(paste0('JOIN "',child,'" AS child')),
              URLencode(paste0('ON (parent._record_id = child._record_id)')),
              URLencode(paste0("WHERE domainid LIKE '",domainid, "'")),
              sep = "%20")
  url =  paste0("https://api.fulcrumapp.com/api/v2/query?token=", api_token, "&format=json", "&q=", sql, "&headers=true")
  request = httr::GET(url, add_headers("X-ApiToken" = api_token, Accept = "application/json"))
  content = jsonlite::fromJSON(httr::content(request, as = "text"))
  out = content$rows
  
  ## error handling: if there are no data for a domain, return nothing
  if (class(out) == "list"){
    ## gimme nothin'
    return()
  } else {
    ## gimme somethin'
    return(dplyr::select(out,-`_geometry`, -`_created_geometry`, -`_updated_geometry`))
  }
}


## Setup indexing variable you want to 'loop' over
## Note that D02 doesn't have data here, hence the error handling above
doms <- c("D01", "D02", "D03", "D04", "D05")

## apply a function to the loop – this vectorized data grab will automatically create a list where each list member is a data frame for each domain specified in ‘doms’
v_data <- sapply(X = doms, FUN = get_child_domain, parent = "(TOS) VST: Apparent Individuals [PROD]", child = "(TOS) VST: Apparent Individuals [PROD]/non_woody_stems", api_token=api_token)

## sapply returns a list, convert the list to a data frame with a nifty function
merged_data <- plyr::ldply(v_data)