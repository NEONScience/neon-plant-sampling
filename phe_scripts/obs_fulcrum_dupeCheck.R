##############################################################################################
#' @title Phe Observation INGEST duplicate finalization

#' @author
#' katie jones \email{kjones@battelleecology.org} \cr

#' @description Queries PHE: Observation INGEST [PROD] fulcrum application for duplicate 
#' finalizations from upstream application, based on the source_record_id field

#' @return This script generates 3 vectors, flag, delete_pdr, delete_fulcrum and provides 
#' steps for automatically deleting or flagging records in fulcrum

# changelog and author contributions / copyrights
#   YOUR NAME (YYYY-MM-DD)
#     original creation
##############################################################################################

library(fulcrumAPI)
library(restR)


api_token = Sys.getenv('FULCRUM_KEY')



# Custom Functions --------------------------------------------------------

set_status <- function (record_id, api_token, load_val = "", verbose = FALSE, 
                        status = "recordError") {
  require(httr)
  in_json <- get_record(record_id, api_token)
  in_json$form_values$LOAD_STATUS <- ifelse(in_json$form_values$LOAD_STATUS%in%c('NONE', 'PARSE_FAIL'), "SKIP", 
                                            in_json$form_values$LOAD_STATUS)
  in_json$status = ifelse(status == "", in_json$status, status)
  url_in = paste0("https://api.fulcrumapp.com/api/v2/records/", 
                  record_id, ".json")
  request <- httr::PUT(url = url_in, config = add_headers(`X-ApiToken` = api_token, 
                                                          Accept = "application/json", `Content-Type` = "application/json"), 
                       body = in_json, encode = "json")
  if (verbose == TRUE) {
    print(url_in)
    print(content(request))
  }
}

# enhancement option - add errorRemark = "duplicate source record id" if set to skip


obs_ingest_dupes <- function(df=NA, stack="CERT", run_fulcrum_delete=FALSE){
  del_fulc <- vector() # only needs to be deleted from fulcrum (either dupe already deleted from pdr or all fulc dupes unloaded)
  del_pdr <- vector() # delete from both fulcrum and PDR
  flag <- vector()
  keep <- vector()
  for(i in unique(df$source_record_id)){
    temp <- df[df$source_record_id==i,]
    id_field <- ifelse(stack=="PROD", "temp$`_record_id`", "temp$individualbarcode")
    id_dt <- ifelse(stack=="PROD", "temp$`_server_created_at`", "temp$`_server_updated_at`")
    dt <- eval(parse(text=id_dt))
    fulcID <- unique(eval(parse(text=id_field)))
    # All recs not loaded
    if(all(temp$load_status%in%c("NONE", "PARSE_FAIL"))){
      a <- if(stack=="PROD"){
        temp$`_record_id`[temp$`_server_created_at`!=max(dt)]
      } else {
        temp$individualbarcode[temp$`_server_updated_at`!=max(dt)]
      } 
      b <- fulcID[!fulcID%in%a]
      del_fulc <- c(del_fulc, a)
      keep <- c(keep, b)
    }else{
      # All recs loaded
      if(all(temp$load_status=="LOADED")){
        l0 <- get.os.l0.by.query(stack='prod',
                                 tab = 'DP0.10002.001:phe_statusintensity_in',
                                 fieldDataSearchStrings = fulcID,
                                 fieldName="fulcrumID")
        # All loaded recs present in PDR
        if(length(unique(l0$fulcrumID))==length(fulcID)){
          check_count <- l0%>%
            group_by(fulcrumID)%>%
            summarize(count=n())
          # All duplicates have same number of records
          if(length(unique(check_count$count))==1){
            c <- unique(l0$fulcrumID[!l0$createdDate==max(l0$createdDate)])
            d <- fulcID[!fulcID%in%c]
            del_pdr <- c(del_pdr, c)
            keep <- c(keep, d)
          }
        }else{
          # All recs loaded, dupes deleted from PDR
          e <- setdiff(fulcID, unique(l0$fulcrumID))
          f <- intersect(fulcID, unique(l0$fulcrumID))
          del_fulc <- c(del_fulc, e)
          keep <- c(keep, f)
        }
      }else{
        flag <- c(flag, fulcID)
      }
    }
    rm(temp)
    rm(fulcID)
    rm(a,b,c,d,e,f)
    if(exists("l0")){
      rm(l0)
    }
  }
# delete del_fulc recs  
  if(run_fulcrum_delete==TRUE){
    sapply(X = if(stack=="PROD"){
      del_fulc
    }else{
      fulDat$`_record_id`[fulDat$individualbarcode%in%del_fulc]
    }, 
      FUN = fulcrumAPI::delete_record,
      api_token = Sys.getenv('FULCRUM_KEY'))
  }
# flag flag recs - consider adding error_recs = "duplicate source_record_id"
  for(j in flag){
    set_status(record_id = if(stack=="PROD"){
    j
  } else{
    fulDat$`_record_id`[fulDat$individualbarcode==j]
  },
    api_token=Sys.getenv('FULCRUM_KEY'),
    status = "recordError",
    verbose = FALSE)
  }
return(list(del_fulc=del_fulc, 
              del_pdr=del_pdr, 
              flag=flag, 
              keep=keep))
}



# run dupe check ----------------------------------------------------------


# Change to [PROD] / [CERT] as needed
fulDat <- fulcrumAPI::get_fulcrum_data(api_token = Sys.getenv('FULCRUM_KEY'), 
sql=URLencode('SELECT * FROM "PHE: Observations INGEST [PROD]" 
WHERE source_record_id IN(
  SELECT source_record_id
  FROM "PHE: Observations INGEST [PROD]"s
  WHERE load_status <> \'SKIP\'
  GROUP BY source_record_id
  HAVING COUNT(*) >1
  );')
)

#for CERT testing
#fulDat <- fulDat[fulDat$`_created_at`<"2024-01-01T00:00:00.000Z",]
#

out <- obs_ingest_dupes(df=fulDat, stack="PROD", run_fulcrum_delete = FALSE)


# manual delete
sapply(X = out$del_fulc,
       FUN = fulcrumAPI::delete_record,
       api_token = Sys.getenv('FULCRUM_KEY'))


## out$del_fulc - automatically deleted from fulcrum if run_fulcrum_delete = TRUE

## out$del_pdr - set fulcrum status to delete_this, create magpie update request

## out$flag - set status = "record error",  if load_status = "PARSE_FAIL" or "NONE", set load_status=SKIP,
                      
## out$keep - do nothing, this is a verification                      
                      
                      