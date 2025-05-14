library(restR)
library(restR2)
library(tidyverse)

l0Box <- 'C:/Users/kjones/Box/L0dataEditing'

newFolder <- 'phe_dbComparison_20250305'
dir.create(file.path(l0Box, newFolder), recursive = TRUE)

newSubs <- c('originalL0download', 'editedL0upload', 'editingActivities', 'comparisonResults')

for(i in newSubs){
  dir.create(path=paste(l0Box, newFolder, i, sep='/'), recursive = TRUE)
}

## get L0 data
#ltr dpID="DP0.10033.001"
#phe dpID= "DP0.10002.001"
#vst dpID= "DP0.10098.001"


# parallel
df_phe <- par.get.os.l0.data(stack='prod',
                           dpID = 'DP0.10002.001', 
                           ingestTable='phe_perindividualperyear_in',
                          startDate = "2023-01-12",
                          endDate = "2025-03-01",
                           format_for_L0_editor=TRUE)

look <- df_phe[is.na(df_phe$measuredBy) | is.na(df_phe$recordedBy),]


phe_fid <- unique(look$fulcrumID)

appName <- 'PHE: Annual Measurements [PROD]'
repName <- 'children_pertag_annual'


fulRec <- restR::get.fulcrum.data(api_token = fulcToken, 
                                  appName = appName,
                                  repeatable = repName,
                                  #fulcrumIDs = fulcID)
                                  fulcIDs = phe_fid)


p <- restR::get.fulcrum.data(api_token = Sys.getenv("FULCRUM_PAT"), 
                              appName = "Personnel-List",
                              queryField = "_status",
                              queryValues = c("Active", "Not-Active"))

p <- p[!is.na(p$orcid)& p$orcid!="0000-0000-0000-0000",]


write.csv(look, paste(l0Box, newFolder, 'originalL0download/phe_annual_missingNames', sep='/'), row.names = FALSE)


## EDIT

df_out <- look

## edit df_out records as needed...

for(i in df_out$fulcrumPrimaryKey){
  df_out$measuredBy[df_out$fulcrumPrimaryKey==i] <- fulRec$measuredby[fulRec$`_child_record_id`==i]
  df_out$recordedBy[df_out$fulcrumPrimaryKey==i] <- fulRec$recordedby[fulRec$`_child_record_id`==i]
}

for(i in 1:nrow(df_out)){
  if(grepl("@", df_out$measuredBy[i])==TRUE){
    df_out$measuredBy[i] <- p$orcid[p$neon_email_address==df_out$measuredBy[i]]
    df_out$recordedBy[i] <- p$orcid[p$neon_email_address==df_out$recordedBy[i]]
  }
}
  
                                                        
write.table(df_out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_annual_orcidsAdded.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')


######
# now replace all the other emails with orcids

# parallel
df_phe <- par.get.os.l0.data(stack='prod',
                             dpID = 'DP0.10002.001', 
                             ingestTable='phe_perindividualperyear_in',
                             startDate = "2016-01-12",
                             endDate = "2025-03-01",
                             format_for_L0_editor=TRUE)

write.csv(look, paste(l0Box, newFolder, 'originalL0download/phe_annual_missingNames', sep='/'), row.names = FALSE)



l2 <- df_phe[grepl("@", df_phe$measuredBy)==TRUE & df_phe$measuredBy%in%p$neon_email_address,]

write.csv(l2, paste(l0Box, newFolder, 'originalL0download/phe_annual_missingOrcid_2', sep='/'), row.names = FALSE)


out2 <- l2

for(i in 1:nrow(out2)){
    out2$measuredBy[i] <- ifelse(out2$measuredBy[i] %in% p$neon_email_address, 
                                   p$orcid[p$neon_email_address==out2$measuredBy[i]],
                                   out2$measuredBy[i])
    out2$recordedBy[i] <- ifelse(out2$recordedBy[i] %in% p$neon_email_address, 
                                   p$orcid[p$neon_email_address==out2$recordedBy[i]],
                                   out2$recordedBy[i])
}

write.table(out2[1:5000,], paste(l0Box, "/", newFolder, "/editedL0upload/phe_annual_oldOrcidsAdded.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

write.table(out2[5001:10000,], paste(l0Box, "/", newFolder, "/editedL0upload/phe_annual_oldOrcidsAdded_2.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

write.table(out2[10001:15000,], paste(l0Box, "/", newFolder, "/editedL0upload/phe_annual_oldOrcidsAdded_3.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

write.table(out2[15001:20000,], paste(l0Box, "/", newFolder, "/editedL0upload/phe_annual_oldOrcidsAdded_4.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

write.table(out2[20001:nrow(out2),], paste(l0Box, "/", newFolder, "/editedL0upload/phe_annual_oldOrcidsAdded_5.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')
