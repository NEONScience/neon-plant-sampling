get_fulcrum_vstMT <- function(apiToken, pageLim = 5000, timeLim = 120, urlEncode = TRUE){
  require(httr)
  require(jsonlite)
  
  sql <- vstMapQuery
  
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
      request <- httr::GET(url, add_headers("X-ApiToken" = api_token, 
                                            Accept = "application/json"))
      content <- httr::content(request, as = "text")
      
      if(grepl("query took too long to execute", content) | !200 %in% request$status_code){
        temp <- NULL
        print(paste0("Dropped query for page ", pageNum, ", trying again"))
      } else {
        temp <- jsonlite::fromJSON(content)
        temp <- temp$rows
        print(paste0("Successfully queried page: ", pageNum))
      }
    } # End second while loop
    
    #   Continue querying pages if time-out limit has not been reached
    if(difftime(time2, time1, units = "secs") < timeLim){
      queryDat <- temp
      pageNum <- pageNum + 1
      dat <- rbind(dat, queryDat)
    } else {
      queryDat <- NULL
      print("Query time-out")
    }
    
    # Determine if query is complete
    if(is.null(nrow(queryDat)) & is.list(queryDat)){
      print(paste("VST MapTag query complete", sep = " "))
    }
    
  } # End first while loop
  
  return(dat)
  
} # End
