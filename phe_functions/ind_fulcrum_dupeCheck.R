

library(fulcrumAPI)
library(restR2)


api_token = Sys.getenv('FULCRUM_KEY')



fulDat <- fulcrumAPI::get_fulcrum_data(api_token = Sys.getenv('FULCRUM_KEY'), 
                                       sql=URLencode('SELECT * FROM "PHE: Per Individual INGEST [PROD]" 
WHERE individualid IN(
  SELECT individualid
  FROM "PHE: Per Individual INGEST [PROD]"s
  GROUP BY individualid
  HAVING COUNT(*) >1
  );')
)

tags <- unique(fulDat$individualid)

del_fulIDs <- vector()

for (i in tags){
  temp_l0 <-  get.os.l0.data(stack='prod',
                          dpID = 'DP0.10002.001', 
                          ingestTable='phe_perindividual_in',
                          sampleTag=i,
                          format_for_L0_editor=TRUE)
  temp_ful <- fulDat[fulDat$individualid==i,]
  extra <- temp_ful$`_record_id`[!temp_ful$`_record_id`%in%temp_l0]
  del_fulIDs <- c(del_fulIDs, extra)
}


# Delete erroneous records
sapply(X = del_fulIDs, FUN = delete_record, 
       api_token = api_token)


