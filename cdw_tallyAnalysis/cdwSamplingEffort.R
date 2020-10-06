### Define required API token and functions ####
#   Define Fulcrum query function
get_fulcrum_pageLim <- function(apiToken, sql, pageLim = 2000, timeLim = 120, urlEncode = FALSE){
  require(httr)
  require(jsonlite)
  
  if(urlEncode){
    sql <- URLencode(sql)
  }
  
  # Initialize parameters used by while loop to obtain paginated data
  dat <- data.frame()
  pageNum <- 1
  queryDat <- data.frame()
  
  ##  Nested while loops to retrieve paginated data and prevent dropped queries from stalling data retrieval
  #   Sys.time() used to prevent infinite loop with unresponsive server
  time1 <- Sys.time()
  time2 <- Sys.time()
  
  #   Loop condition: Executes as long as queryDat object returned from API contains data
  while(!is.null(nrow(queryDat))){
    # Define URL to send to API
    url  <-  paste0("https://api.fulcrumapp.com/api/v2/query?token=", apiToken, 
                    "&format=json", "&q=", sql, "&headers=true", "&per_page=", pageLim, "&page=", pageNum)
    
    ##  Request JSON from API using 'url'
    #   Loop condition: Executes as long as time-out has not been reached and server response is NULL
    temp <- NULL
    while(is.null(temp) & (difftime(time2, time1, units = "secs") < timeLim)){
      time2 <- Sys.time()
      request <- httr::GET(url, add_headers("X-ApiToken" = fulcrumToken, 
                                            Accept = "application/json"))
      content <- httr::content(request, as = "text")
      
      if(grepl("error", content) | !200 %in% request$status_code){
        temp <- NULL
      } else {
        temp <- jsonlite::fromJSON(content)
        temp <- temp$rows
      }
    } # End second while loop
    
    #   Continue querying pages if time-out limit has not been reached
    if(difftime(time2, time1, units = "secs") < timeLim){
      queryDat <- temp
      pageNum <- pageNum + 1
      dat <- rbind(dat, queryDat)
    } else {
      queryDat <- NULL
      dat <- "Fulcrum query time-out, please try again"
    }
    
  } # End first while loop
  
  return(dat)
  
} # End



### Define API token
fulcrumToken = "3ab235047ec293b27f06f6819e81b291435f9c61282345ff1de9624f744034b4233a6fcd1b87c3c2"



### Define CDW Summary Function ####
get_cdwPlots_Fulcrum <- function(eventID){
  require(dplyr)
  
  # CDW Tally query for distinct plotIDs within user-specified eventID
  cdwPlotQuery <- paste(URLencode('SELECT DISTINCT plotid_parent FROM "4487b90c-51b3-495a-873b-16f6b6d0abf7"'),
                        URLencode(paste0("WHERE eventid = '", eventID, "'")), 
                        sep = "%20")
  
  # Get data from Fulcrum
  cdwPlotList <- get_fulcrum_pageLim(apiToken = fulcrumToken, sql = cdwPlotQuery)
  cdwPlotList <- cdwPlotList %>% arrange(plotid_parent)
  cdwPlotNum <- nrow(cdwPlotList)
  
  # Create output
  output <- list(cdwPlotList = cdwPlotList, cdwPlotNum = cdwPlotNum)
  return(output)
  
}
