'j' = d20, not applicable to locales


###Here we go ###
system.time(
  taxaInLocales <- foreach(i=1:nrow(uTaxaNativity))%dopar% {
    inLocales <- NULL
    for (j in locales){
      if(max(grepl(j, uTaxaNativity$Native.Status[i]))){
        inLocales <- c(inLocales, j)
      }
    }
    return(inLocales)
  })

names(taxaInLocales) <- uTaxaNativity$acceptedTaxonID
## this list is = to taxaInStates #
## No examples for AK #


#generates columns of Presence/Absence

#original code
system.time(taxaInDomains <- foreach(i=1:nrow(uTaxa))%dopar%{    
  states <- taxaInStates[[i]]
  inDomains <- NULL
for (j in domainIDs){
  if (length(which(states%in%statesInDomains[[j]])) > 0){
    inDomains <- c(inDomains, 'P')
  } else {inDomains <- c(inDomains, 'A')}
}
return(inDomains)


localesList<-list('l48NativeStatusCode'=c('L48'),
                  'akNativeStatusCode'=c('AK'),
                  'hiNativeStatusCode'=c('HI'),
                  'prNativeStatusCode'=c('PR')
                  )

  
system.time( 
  taxaInLocales2<-foreach(i=1:nrow(uTaxaNativity))%dopar%{     
    locales <- taxaInLocales[[i]]
     inLocales <- NULL
    for (j in taxaInLocales){
    if (length(which(locales%in%taxaInLocales[[j]])) > 0){   
      inLocales <- c(inLocales, 'P')
    } else {inLocales <- c(inLocales, 'A')}
  }
  return(inLocales)
})

names(taxaInLocales2) <- uTaxaNativity$acceptedTaxonID

#produces a list of P/A (only 'A') with columns (un-named) for each locale, no taxonId yet

# are these next 2 lines of code necessary? I(k8) commented them out for now
# z <- taxaInDomains
# taxaInDomains <- z
taxaInLocales2 <- data.frame(matrix(unlist(taxaInLocales2), ncol=4, byrow=TRUE))  #Break starts here
names(taxaInLocales) <- locales
taxaInLocales2$acceptedTaxonID <- uTaxaNativity$acceptedTaxonID

#see where we are so far
#print (head(taxaInDomains))

#moved to next chunk
#usda <- merge(usda, taxaInDomains, by = "acceptedTaxonID")

````

#next repeat for Native/Introduced/ETC
#then leave absences as A, but overwrite presences with nativeStatus

```{r replace Presence data with Native Status Codes}

## locales indicates the spatial resolutions at which usda reports nativity status
## KT I think we only need to deal with locales that are within NEON's realm

#locales <- c('L48', 'AK', 'HI', 'PR', 'VI','CAN','GL', 'SPM', 'NA')
locales <- c('L48', 'AK', 'HI', 'PR')
localesInDomains <- list(d01=c('L48'),
                         d02=c('L48'),
                         d03=c('L48'),
                         d04=c('PR'),##in 2014, all sites in D04 are in PR - 
                         ##no sites yet in the FL portion of the domain
                         d05=c('L48'),
                         d06=c('L48'),
                         d07=c('L48'),
                         d08=c('L48'),
                         d09=c('L48'),
                         d10=c('L48'),
                         d11=c('L48'),
                         d12=c('L48'),
                         d13=c('L48'),
                         d14=c('L48'),
                         d15=c('L48'),
                         d16=c('L48'),
                         d17=c('L48'),
                         d18=c('AK'),
                         d19=c('AK'),
                         d20=c('HI'))  

# pull out acceptedTaxonIDs where nativity status data are provided by the usda
uTaxaNativity <- unique(subset(usda, str_length(usda$Native.Status) > 0, 
                               select = c("acceptedTaxonID","Native.Status")))

#need to convert taxaInDomains to character from factors, so that the Ps can be 
##updated and not cause an invalid factor level error
taxaInDomains <- data.frame(lapply(taxaInDomains, as.character), 
                            stringsAsFactors=FALSE)

#then need to convert the uTaxaNatviity$acceptedTaxonID to character to all
##comparison with taxaInDomains$acceptedTaxonID
uTaxaNativity$acceptedTaxonID <- as.character(uTaxaNativity$acceptedTaxonID)

#could not get the parallel foreach function to work here, I think because 
#"foreach differs from a for loop in that its return is a list of values, 
#whereas a for loop has no value and uses side effects to convey its result."

#Here are the rules for defining Nativity Codes:
#For Domains 1-3 and 5-17 where P/A = 'P', and Native.Status = L48(x),
# replace 'P' with 'x' (That is, whatever is between the parentheses following 'L48')
# acceptable values for 'x' = N, N?, GP, GP?, I, I?, W, W?

# this function extracts the value from within a parentheses#
# gsub("[\\(\\)]", "", regmatches(usda$Native.Status, gregexpr("\\(.*?\\)", usda$Native.Status)))#

#For Domain 4 where P/A = 'P', if Native.Status = L48(x), and/or PR(y)
# replace 'P' with 'y'
# else replace 'P' with 'x'

#For Domains 18-19 where P/A = 'P', and Native.Status = AK(x)
# replace 'P' with 'x'

#For Domain 20 where P/A = 'P', and Native.Status = HI(x)
# replace 'P' with 'x'


system.time(
  for (i in 1:nrow(uTaxaNativity)){
    for (a in 1:nrow(taxaInDomains)){
      #work with records in taxaInDomains for relevant taxon:
      if (taxaInDomains$acceptedTaxonID[a] == uTaxaNativity$acceptedTaxonID[i]){
        #extract native status code for each region (locale)
        for (b in locales) {
          if(max(grepl(b, uTaxaNativity$Native.Status[i]))){
            k <- paste(b, '\\(.*?\\)', sep = ' ')
            l <- gsub("[\\(\\)]", "", 
                      regmatches(uTaxaNativity$Native.Status[i],
                                 regexpr(k, uTaxaNativity$Native.Status[i])))
            print (nativeStatus <- strsplit(l, " ")[[1]][2])
            #replace 'P's in relevant region with correct native status code
            for (j in 1:length(domainIDs)){
              if (length(which(b%in%localesInDomains[[j]])) > 0){
                if (taxaInDomains[a,j] == 'P'){
                  taxaInDomains[a,j] <- nativeStatus
                }
              }
            }
          }
        }
      }
    }
  }
)

usda <- merge(usda, taxaInDomains, by = "acceptedTaxonID")
