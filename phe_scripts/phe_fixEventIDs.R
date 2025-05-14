
library(restR)
library(restR2)

### fix eventIDs, remove _(##/##)

l0Box <- 'C:/Users/kjones/Box/L0dataEditing'

newFolder <- 'phe_dbComparison_20250305'

##### get L0 record
#parallel for large pulls
df_phe <- par.get.os.l0.data(stack='prod',
                             dpID = 'DP0.10002.001',
                             startDate = '2023-01-01',
                             endDate = '2025-03-01',
                             ingestTable='phe_perindividualperyear_in',
                             format_for_L0_editor=TRUE)

fix <- df_phe[grepl(")", df_phe$eventID),]

write.csv(fix, paste(l0Box, newFolder, 'originalL0download/phe_eventID_orig.csv', sep='/'), row.names = FALSE)

out <- fix

for (i in 1:nrow(out)){
  out$eventID[i] <- paste(unlist(strsplit(out$eventID[i], "_"))[1:3], collapse = "_")
}

table(out$eventID)

out$eventID[out$eventID=="phe_YELL_undefined"] <- "phe_YELL_2024"

write.table(out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_eventID_edit.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')
