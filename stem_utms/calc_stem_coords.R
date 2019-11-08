########### SUMMARY
### Open this script by double-clicking on the R Project file, rather than loading the script directly. The R Project file will set the working directory relative paths in order to load supplemental data. 

### This script pulls TOS Vegetation Structure (VST) Mapping and Tagging data directly from the Fulcrum cloud...this is the most up to date representation of our data. 

### Specify which domains you would like data from in the 'doms' variable, where you want data exported with 'file_export_path', then run the entire script. 

### Script takes current TOS spatial data and attempts to estimate coordinates. Our spatial data (plotSpatial, pointSpatial) have changed over time as plots are retired, moved, or added on...missing spatial data for these types of plots can be found by contacting the TOS Geospatial team (Dave Barnett, Rachel Krauss). 

###################################################### PROCESSING CONSTANTS ###################################################### 
## define which domains you would like data from
doms <- c("D07")
## If you want to export data as a .csv, indicate TRUE here
write_file <- FALSE 
## Indicate exported file destination
## NOTE YOU NEED TO USE FORWARD SLASHES, AND NAME THE FILE WITH A .CSV extension
file_export_path <- "C:/path/to/my/export/name.csv"


#################### FUNCTIONS #####################
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)

# access code 
api_passcode <- "8141de55fde62012e73c7cf12edda28550172d07f0760eb9141b71bfee1938e5dec475beca94e9ff"

# get fulcrum data
get_parent_domain <- function(parent, api_token, domainid){
  ## regular request
  sql = paste(URLencode(paste0('SELECT * FROM "', parent,'" AS parent')),
              URLencode(paste0("WHERE domainid LIKE '", domainid, "'")),
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

### Define functions needed globally
# Define function for converting azimuth degrees to radians
radians = function(degrees) {
  rad = (degrees*pi)/180
  return(rad) 	
}

#################### FUNCTIONS <end>  #####################

#################### DATA #####################
## vectorized data pull -- pull each domain into a list element
field_data <- lapply(X = doms, FUN = get_parent_domain, parent = "(TOS) VST: Mapping and Tagging [PROD]", api_token=api_passcode)

## convert list to data frame
field_data <- do.call(rbind, field_data)


###  Spatial data input
## Read in plot spatial data, filter to applicableModules with 'vst', select desired columns, and rename
# fields to match Fulcrum data. Plot spatial data needed for 'plotSize' variable, and assigning of 
# new plottype variable to account for different types of Tower Plots - smStat, lgStat
plotSpatial <- read.csv("data/plotSpatialData_20161206.csv", stringsAsFactors = F, header = T)
plotSpatial %>%
  dplyr::filter(grepl('vst', applicableModules)) %>%
  dplyr::select(siteID, plotID, plotType, plotSize) %>%
  dplyr::rename(siteid=siteID, plotid=plotID, plottype=plotType, plotsize=plotSize) %>%
  dplyr::arrange(siteid, plotid) -> plotSpatial

## In 'plotSpatial' data, create new 'plottype' variable that accounts for type of Tower Plot
plotSpatial$newType <- ""

# Assign 'newType = distributed' for all 'distributed' plots
plotSpatial[plotSpatial$plottype=="distributed",][,"newType"] <- "distributed"


# Assign 'newType = smStatTower | lgStatTower' based on plotsize
sites <- unique(plotSpatial$siteid)
for (i in 1:length(sites)){
  sitePlots <- filter(plotSpatial, siteid==sites[i])
  if(400 %in% sitePlots$plotsize){
    plotSpatial[plotSpatial$siteid==sites[i] & plotSpatial$plottype=="tower",][,"newType"] <- "smTower"
  } else {
    plotSpatial[plotSpatial$siteid==sites[i] & plotSpatial$plottype=="tower",][,"newType"] <- "lgTower"
  }
}

# Remove original 'plottype', 'siteid', and 'plotsize', and rename 'newType -> plottype'
plotSpatial %>% select(-plottype, -siteid, -plotsize) %>% rename(plottype=newType) -> plotSpatial


## Read in point spatial data, select desired columns, filter to applicableModule with 'vst', and 
## rename fields to match Fulcrum data. Then create 'plotpointid' for joining with 'vstInput'
pointSpatial <- read.csv("data/pointSpatialData_20161206.csv", stringsAsFactors = F, header = T)
pointSpatial %>% 
  filter(grepl('vst', applicableModules)) %>%
  select(plotID, pointID, decimalLatitude, decimalLongitude, easting, northing) %>%
  rename(plotid=plotID, pointid=pointID, latitude=decimalLatitude, longitude=decimalLongitude, 
         pointeasting=easting, pointnorthing=northing) %>%
  arrange(plotid, pointid) %>%
  mutate(plotpointid = paste(plotid, pointid, sep = "_")) -> pointSpatial

pointSpatial$pointid <- as.integer(pointSpatial$pointid)

### Define additional parameters needed to work with site VST data in server.R
# Define expected pointid values for Distributed/smTower Plots, and lgTower Plots
expSmall <- c(31,33,41,49,51,21,25,57,61)
expLarge <- c(expSmall,23,39,43,59)

#################### DATA <end> #####################

#################### PROCESSING <end> #####################
## 
sd <- field_data %>% 
## use this select statement if data are coming directly from Fulcrum; a lot of these fields are fulcrum-only
select(nestedshrubsapling, nestedliana, nestedother, bouttype, plotid, siteid, taxonid,
       subplotid, nestedsubplotid, tagid, supportingstemtagid, pointid, stemdistance, stemazimuth,
       `_record_id`, load_status)

# Join with 'plotSpatial', and only keep records with matching plotid
sd <- inner_join(sd, plotSpatial, by = "plotid")

sd %>% 
  # Some subplotIDs were assigned at the 10m x 10m level: Assign new `subplotid` based on plotType
  mutate(newsubplotid = ifelse(plottype!="lgTower", 31,
                               ifelse(subplotid %in% c(21,22,30,31), 21,
                                      ifelse(subplotid %in% c(23,24,32,33), 23,
                                             ifelse(subplotid %in% c(39,40,48,49), 39,
                                                    ifelse(subplotid %in% c(41,42,50,51), 41,
                                                           "NA")))))) %>%
  select(-subplotid) %>%
  rename(subplotid=newsubplotid) %>%
  
  # Remove instances where `subplotid` is 'NA'
  filter(!subplotid=="NA") %>%
  
  # Create 'plotsubplotid' to pipe to plotChoice drop-down: e.g., "(T) BART_050: 21"
  mutate(plotsubplotid = ifelse(plottype=="distributed", paste0("(D) ", plotid),
                                ifelse(plottype=="smTower", paste0("(T) ", plotid),
                                       paste0("(T) ", plotid, ": ", subplotid)))) %>%
  # Create `pointstatus` variable to detect invalid pointid values based on plottype
  mutate(pointstatus = ifelse(is.na(pointid), "notMapped",
                              ifelse(plottype=="lgTower" & pointid %in% expLarge, "validPointID",
                                     ifelse(pointid %in% expSmall, "validPointID",
                                            "errorPointID")))) %>%
  # Create 'plotpointid' variable for valid pointids only, to enable join with 'pointSpatial' data table
  mutate(plotpointid = ifelse(is.na(pointid), "NA", 
                              ifelse(pointstatus=="validPointID", paste(plotid, pointid, sep="_"),
                                     "NA"))) -> sd

# Join to create `pointeasting` and `pointnorthing`
sd$pointid <- as.integer(sd$pointid)

# sd$fkey <- paste0(sd$plotid,".",sd$pointid)
# pointSpatial$fkey <- paste0(pointSpatial$plotid,".",pointSpatial$pointid)

sd <- left_join(sd, pointSpatial, by=c("plotpointid"))

# Calculate UTMx='stemeasting' and UTMy='stemnorthing', and sort final dataset
sd %>%
  mutate(UTMx = round(pointeasting + stemdistance*sin(radians(stemazimuth)), digits = 2)) %>%
  mutate(UTMy = round(pointnorthing + stemdistance*cos(radians(stemazimuth)), digits = 2)) %>%
  arrange(plotid.x, subplotid) -> sd

## View snippets of data
head(sd)
tail(sd) ## missing data are either not mapped or might have old spatial data

######################## EXPORT DATA #########################
if(write_file==TRUE){
  write.csv(sd, file = file_export_path, na="")
}
