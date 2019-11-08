plotCount.func = function(input, ply = ldply){
  ply(input, function(x){
    t <- read.csv(x, stringsAsFactors=FALSE, sep=",") # "x" = the file name here, read it into memory, assign it to object "t"
    out <- unique(t$plotID)
    out2 <- unlist(out)
    out3 <- out[which(out2 != "")] # using [-which(out == ""] does NOT work
    out4 <-length(unique(out3))
    return(out4)
    #return(col2) # for some reason this won't unlist to a single vector inside lapply()
  })
}

plotCount = plotCount.func(fileFeed, ldply)
plotCount
