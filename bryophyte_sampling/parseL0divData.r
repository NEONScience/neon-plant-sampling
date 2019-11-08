
library(dplyr)

##############################################################################################
#Load datasets

folderName <- 'bryophyte_sampling'
fileName <- 'kjones-1472138500463.csv'
tableName <- 'div_1m2Data_in'
siteName <- 'UNDE'
year <- '2015'
titlesFile <- 'div_dataingest_NEONDOC001399.csv'


if (file.exists(
  'C:/Users/kjones/Documents/GitHub/neonPlantSampling')){
  myPathtoDir <- 'C:/Users/kjones/Documents/GitHub/neonPlantSampling'
}

divData <- read.csv(paste (myPathtoDir, folderName, fileName, sep='/'),stringsAsFactors = F,
                       row.names=NULL,header=T)


if(tableName=="phe_perindividual_in"){
  names(pheData) <- names(pheData)[2:length(names(pheData))]
}

titles<-read.csv(paste (myPathtoDir, folderName, titlesFile, sep='/'),
                 stringsAsFactors = F)
titles <- titles$fieldName[titles$table==tableName]


#Parse data from CI output to ingest table format
#1) Create empty dataframe to hold parsed and reorganized data
newdf <- data.frame(matrix(NA, nrow = nrow(divData), ncol = length(titles)))
names(newdf) <- titles

#Populate the new dataframe
for (i in 1:nrow(divData)){
#    newdf$domainID[i] <- strsplit(divData$Sample.Tag[i],'[.]')[[1]][3]
    newdf$siteID[i] <- strsplit(divData$Named.Location[i],'_')[[1]][1]
    newdf$plotID[i] <- strsplit(divData$Named.Location[i],'[.]')[[1]][1]
    ################## if tableName = x don't use Start Date ###########
    if (tableName=='phe_perindividual_in'){
      newdf$addDate[i] <- paste('"',strsplit(divData$Start.Date[i],'T')[[1]][1],'"',sep='')
    }else{
      newdf$date[i] <- paste('"',strsplit(divData$Start.Date[i],'T')[[1]][1],'"',sep='')
    }
    newdf$individualID[i] <- divData$Sample.Tag[i]

  for(j in 1:ncol(divData)){
    if (!is.na(divData[i,j]) && grepl(';',divData[i,j]) == TRUE){
      if (strsplit(strsplit(divData[i,j],' ')[[1]][6],';')[[1]][1] %in% names(newdf)){
        newdf[i,which(names(newdf)==strsplit(strsplit(divData[i,j],' ')[[1]][6],';')[[1]][1])] <-
          strsplit(strsplit(divData[i,j],'Value_Data=')[[1]][2],';')[[1]][1]

      }
    }
  }
}


#Write output
write.csv(newdf, paste(myPathtoDir, folderName, paste(siteName, year,
                                         strsplit(tableName, '_') [[1]][2], '.csv',sep=''), sep='/'),
                      row.names=F)

#NOTE - Spatial data are not assigned in the L1 output of the data viewer. If we need spatial data,
# it needs to be assigned separately