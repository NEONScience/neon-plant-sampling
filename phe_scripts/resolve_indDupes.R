
l0Box <- 'C:/Users/kjones/Desktop'

# create the new folders you'll need for L0 update
newFolder <- 'phe_cleanUp_20250513'
dir.create(path = l0Box, newFolder)
newSubs <- c('originalL0download', 'editedL0upload', 'editingActivities', 'comparisonResults')

for(i in newSubs){
  dir.create(path=paste(l0Box, newFolder, i, sep='/'), recursive = TRUE)
}


# find dupes by tag

ind <-  par.get.os.l0.data(stack='prod',
                           dpID = 'DP0.10002.001',
                           startDate = '2013-01-01',
                           endDate = '2025-05-01',
                           ingestTable='phe_perindividual_in',
                           format_for_L0_editor=TRUE)


ind_dupe <- ind[duplicated(ind$individualID, fromLast = T)|duplicated(ind$individualID, fromLast = F),]

idChange <- data.frame()
keep <- data.frame()
delete <- data.frame()
edit <- data.frame()


i=i+1

for (i in 1:length(unique(ind_dupe$individualID))){
  a <- unique(ind_dupe$individualID)[i]
  print(paste(i, "of", length(unique(ind_dupe$individualID))))
  print(a)
  temp <- ind_dupe[ind_dupe$individualID==a,]
  print(unique(temp$taxonID))
  # look <- temp%>%
  #   select(uid, fulcrumID, individualID, createdDate, transactionDate, 
  #          taxonID, growthForm, vstTag, transectMeter, ninetyDegreeDistance)
  if(length(unique(temp$taxonID))>1){
    # within dupes find diff taxonID
    idChange <- bind_rows(idChange, temp)
    print ("idChange")
  }else{
    if(length(unique(temp$taxonID))==1){
      keep <- bind_rows(keep, temp[temp$transactionDate==max(temp$transactionDate),])
      # create delete file of older dupes with same tax
      delete <- bind_rows(delete, temp[temp$transactionDate!=max(temp$transactionDate),])
    }
  }
  # clean
  rm(a)
  rm(temp)
}

any(is.na(keep$individualID))
any(is.na(delete$individualID))
any(is.na(idChange$individualID))


# write orig delete recs
write.csv(delete, paste(l0Box, newFolder, 'originalL0download/phe_ind_dupesToDelete.csv', sep='/'), row.names = FALSE)

## write delete full dupe
uuid_only <- select(delete, uuid=uid)

write.table(uuid_only, paste(l0Box, "/", newFolder, "/editedL0upload/deleteDuplicateInd.txt", sep=''), 
            sep="\t", row.names = FALSE, col.names="uuid", na='')

#delete from fulcrum
del_list = unique(delete$fulcrumID[!is.na(delete$fulcrumID)])


sapply(X = del_list, FUN = delete_record, 
       api_token = Sys.getenv('FULCRUM_KEY'))
