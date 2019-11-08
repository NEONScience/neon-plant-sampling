library(plyr)
library(dplyr)
library(httr)

token <- 'f68fd4db05c32933b94e1760104cfdaa01eda4956c883943ab9fe55f2fb99313'

# the file list here should point to "fileGrab1" for this specific script
multiCombine <- function(input, ply = llply){
    ply(input, function(x){
        t <- read.csv(x, header=TRUE, sep=",",stringsAsFactors = FALSE) # read the csv
        t1 <- plyr::rbind.fill(t) # rbind it to a temporary variable
        return(t1) # return the full variable
    }
    )
}

get_parent_simple <- function(parent, api_token){
    ## regular request
    sql = paste(URLencode(paste0('SELECT * FROM "', parent,'" AS parent')),
                sep = "%20")
    url =  paste0("https://api.fulcrumapp.com/api/v2/query?token=", api_token, "&format=json", "&q=", sql, "&headers=true")
    request = httr::GET(url, add_headers("X-ApiToken" = api_token, Accept = "application/json"))
    content = jsonlite::fromJSON(httr::content(request, as = "text")) 
    out = content$rows
    
    ## error handling: if there are no data for a domain, return nothing
    if (class(out) == "list" || is.null(out)){
        ## gimme nothin'
        return()
    } else {
        ## gimme somethin'
        return(dplyr::select(out,-`_geometry`, -`_created_geometry`, -`_updated_geometry`))
    }
}

flist <- list.files()
flist <- flist[grepl(pattern = ".csv", x = flist)]
flist <- flist[grepl(pattern = "_lidsList", x = flist)]

dat <- multiCombine(flist, ply = ddply)
dat2 <- do.call(rbind.fill, dat)

head(dat2)

dat2$remarks <- NULL
dat2$siteID <- stringr::str_sub(string = dat2$plotID, 1, 4)
dat2$lidsConcat <- paste0(dat2$lidsAngle1,"|",dat2$lidsAngle2,"|", dat2$lidsAngle3)
dat2$namedLoc <- paste0(dat2$plotID, ".basePlot")

## get TOS spatial data in Fulcrum
spatial <- get_parent_simple(parent = "TOS Spatial Data", api_token = token)
spatial$namedLoc <- paste0(spatial$plotid, ".", spatial$subtype)

## leave out the mosquitoes, bird grids etc.
spatial_cdw <- dplyr::filter(spatial, subtype == 'basePlot')

## merge data
spatial_merge <- merge(x = dat2, y = spatial, by.x = 'namedLoc', by.y = 'namedLoc')

table(duplicated(spatial_merge$namedLoc))

out_toFulc <- dplyr::select(spatial_merge, `_record_id`, lidsConcat) %>% dplyr::rename(cdw_lidsList=lidsConcat)
write.csv(out_toFulc, "lidsLists_to_fulcrum_spatial.csv", row.names=FALSE)
