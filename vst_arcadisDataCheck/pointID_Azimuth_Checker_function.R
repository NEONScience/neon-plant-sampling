# E) Point ID and Azimuth checking function - works with FOPS data, where plotTypeFile has two columns: Plot_ID, Type (distributed/tower)
# The code then appends these "keys" to the data.frame and uses ifelse statements from there to check whether the pointID is correct and
# then whether the azimuth is correct
pointChecker.func <- function(input, ply = llply){
  ply(input, function(x){
    t <- read.csv(x, header=T, sep = ",",stringsAsFactors=FALSE) # "x" = the file name here, read it into memory, assign it to object "t"
    out <- t
    out$Type <- plotTypeFile$Type[match(out$plotID, plotTypeFile$Plot_ID, nomatch = NA)]# append plot type by matching ID's
    out$pointIDQF <- ifelse(out$Type == "Distributed", 
                            ifelse((out$pointID %in% pointIDList.distributed$pointID),1,ifelse(is.na(out$pointID),1,-9999)), # conditions if distributed, use is.na(field) to assign value, not field == "NA" else this won't work
                            ifelse((out$pointID %in% pointIDList.tower$pointID),1,ifelse(is.na(out$pointID),1,-9999)))
    # match and append azimuth values, these columns are temporary
    out$aAzimuth.ch <- azCh$aAzimuth[match(out$pointID, azCh$pointID, nomatch = NA)]
    out$bAzimuth.ch <- azCh$bAzimuth[match(out$pointID, azCh$pointID, nomatch = NA)]
    # check if stemAzimuth is in that range
    out$azimuthQF <- ifelse(out$stemAzimuth >= out$aAzimuth.ch & out$stemAzimuth <= out$bAzimuth.ch, 1, -9999)
    out[c("aAzimuth.ch", "bAzimuth.ch","Field1")] <- list(NULL) # clean-up, remove the extra columns
    name1 <- unlist(strsplit(x, '_D')) # this splits a string,  then this will unlist a list, which is what strsplit() returns
    name2 <- do.call(rbind, strsplit(name1, '.csv')) # re-combine as rows
    indexer <- seq(from = 0, to = length(name2),by = 2) # grab every other element, i.e. the site ID
    siteID.names <- name2[indexer] # store the site IDs
    out$siteID <- siteID.names
    return(out)
  })
}
