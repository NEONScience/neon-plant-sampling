###   Determine which sites have CDW tally data that can be used to create ranked DST lists.
###   Accurate as of 2018-05-23

## Define function for retrieving Fulcrum data
get_Fulcrum_data <- function(api_token, sql){
  require(httr)
  url = paste0("https://api.fulcrumapp.com/api/v2/query?token=", 
               api_token, "&format=json", "&q=", sql, "&headers=true")
  request <- httr::GET(url, add_headers("X-ApiToken" = api_token, 
                                        Accept = "application/json"))
  content <- jsonlite::fromJSON(httr::content(request, as = "text"))
  return(content$rows)
}

api_token = "3ab235047ec293b27f06f6819e81b291435f9c61282345ff1de9624f744034b4233a6fcd1b87c3c2"

# Define CDW Fulcrum query for domain
domain <- "D19"
cdwQuery = paste(URLencode('SELECT * FROM "(TOS) Coarse Downed Wood: Tally [PROD]" AS parent
                           JOIN "(TOS) Coarse Downed Wood: Tally [PROD]/per_plot_azimuth_log" AS child'),
                 URLencode(paste0("ON (parent._record_id = child._parent_id)
                                  WHERE domainid LIKE'", domain, "'")), sep = "%20")

# Get CDW data from Fulcrum
cdw <- get_Fulcrum_data(api_token = api_token, sql = cdwQuery)
cdw %>% count(siteid) %>% kable

# D20
"PUUM not yet sampled."

# D19
"HEAL sampling suspended due to lack of particles."
"Tally data exist for DEJU and BONA --> BONA DST complete; DEJU DST complete."

# D18
"No CDW in Arctic tundra."

# D17
"SJER DST list complete; SOAP and TEAK not yet sampled."

# D16
"ABBY DST list complete; WREF not yet sampled."

# D15
"Sampling suspended in 2018 due to lack of CDW."

# D14
"Sampling suspended in 2018 due to lack of CDW."

# D13
"NIWO has tally data --> complete; MOAB not yet sampled."

# D12
"YELL not yet sampled."

# D11
"CLBJ has tally data --> DST analysis complete; OAES no tally data, no CDW sampling."

# D10
"No sites with tally data."

# D09
"NA"

# D08
"Three sites have tally data --> TALL incomplete due to missing **diameterClass** data from 2015 tally."

# D07
"Three sites have tally data --> DST analysis needed for MLBS; dataset appears incomplete for MLBS."

# D06
length(unique(cdw$plotid_parent))
"KONZ has tally data --> 1 particle tallied in all 50 plots --> suspend sampling."

# D05
"Three sites have tally data --> DST analysis needed at UNDE; cannot use UNDE 2015 data due to incompleteness."

# D04
"No CDW data collected to date."

# D03
"Three sites have tally data --> DST complete"

# D02
"Three sites have tally data --> DST complete"

# D01
"Two sites have tally data --> DST complete"

