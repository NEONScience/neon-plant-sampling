library(dplyr)
library(stringr)

# read in fulcrumAPI functions
fx_path <- '../../../../fulcrumDev/admin_tools_scripts/fulcrumAPI/R'
fulcrumAPI_files <- list.files(fx_path, full.names = TRUE)
lapply(fulcrumAPI_files, FUN = source)

vst <- read.csv('vst_mt_fulc_records.csv', stringsAsFactors = FALSE)

vst$year <- str_sub(string = vst$vstid, -4, -1)
vst$siteID <- str_sub(vst$plotid, 1, 4)
vst$creationDate <- str_sub(vst$X_created_at, 1, 10)

vst$duplicate_individualid <- duplicated(vst$individualid)
table(vst$duplicate_individualid)

## this hinges on all fulcrum records having a vstid...make sure those values are all populated
table(is.na(vst$vstid))
table(vst$vstid == '')
table(vst$vstid == ' ')

## records to keep -- arrange by latest creation date, keep that one
no_dupes <- vst %>% 
  group_by(individualid) %>% 
  arrange(desc(creationDate)) %>% 
  slice(1)

## records to delete 
delete_vst <- vst[!(vst$vstid %in% no_dupes$vstid),]

## double-check
table(delete_vst$individualid %in% no_dupes$individualid) 
table(delete_vst$vstid %in% no_dupes$vstid) # none of these should be present in the de-duplicated data.frame

## most dupes are from early on, before 2018
table(delete_vst$year)

# for each dupe, grab the record and stash it for posterity
keep_records <- delete_vst$X_record_id

xout <- lapply(keep_records, query_app_filter, parent ='VST: Mapping and Tagging [PROD]', api_token = Sys.getenv('FULCRUM_API_NEON'), column = '_record_id')

## for some reason this creates a duplicate row of each record
z <- data.table::rbindlist(xout)

## these are weird lists nested within the returned data.frame that muck up operations
z[,14] <- NULL
z[,24] <- NULL
z[,29] <- NULL

z$dupe <- duplicated(z)

zout <- dplyr::filter(z, dupe == FALSE)

write.csv(zout, 'vst_mapping_tagging_record_dupes_deleted.csv', row.names=FALSAE)

### then delete those records -- DELETE RECORDS AFTER ALL HAVE BEEN STASHED
# lapply(delete_vst$X_record_id, delete_record, api_token = Sys.getenv('FULCRUM_API_NEON'))

