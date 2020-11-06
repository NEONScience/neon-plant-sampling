library(dplyr)
library(stringr)

# read in fulcrumAPI functions
fx_path <- '../../fulcrumDev/admin_tools_scripts/fulcrumAPI/R'
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

### grab records from fulcrum because I only pulled 9 columns from Fulcrum originally -- get ALL columns
xout <- lapply(keep_records, query_app_filter, parent ='VST: Mapping and Tagging [PROD]', api_token = Sys.getenv('FULCRUM_API_NEON'), column = '_record_id')

xout2 <- xout

for (df in 1:length(xout)){

  xout2[[df]][,22] <- NULL

}
## for some reason this creates a duplicate row of each record
z <- data.table::rbindlist(xout2, fill=TRUE)

# z$dupe <- duplicated(z)
#
# zout <- dplyr::filter(z, dupe == FALSE)

z2 <- apply(X = z, MARGIN = 2, as.character)

write.csv(z2, 'vst_mapping_tagging_record_dupes_deleted.csv', row.names=FALSE)

names(delete_vst) %in% names(z2)
### then delete those records -- DELETE RECORDS AFTER ALL HAVE BEEN STASHED
lapply(delete_vst$X_record_id, delete_record, api_token = Sys.getenv('FULCRUM_API_NEON'))

### grab records from fulcrum because I only pulled 9 columns from Fulcrum originally -- get ALL columns
double_check <- lapply(delete_vst$individualid, query_app_filter, parent ='VST: Mapping and Tagging [PROD]', api_token = Sys.getenv('FULCRUM_API_NEON'), column = 'individualid')

### some API calls returned nothing, which inserts a NULL into the list. as.character on NULL causes a fail
double_check2 <- lapply(1:length(double_check), FUN = function(x) {
  #print(x)
  if (is.null(double_check[[x]]) == FALSE){
    data.frame(apply(double_check[[x]], c(1,2), as.character))
  }
  })

### having to do all this munging suggests a function that pulls Fulcrum records should dump the geometry fields whenever they exist
double_check2 <- data.table::rbindlist(double_check2, fill=TRUE)

### looks like there are still a few dupes (15 total), but they share the same vstID (so the above code wouldn't have caught these). vstID dupes are indeterminate via code, unless we assume the latest date is the correct record. 
double_check2 %>% group_by(individualid, vstid) %>% count %>% arrange(desc(n))
