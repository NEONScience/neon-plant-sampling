#Original url:
#'http://plants.usda.gov/java/AdvancedSearchServlet?pfa=l48&pfa=ak&pfa=hi&pfa=pr&family=Acanthaceae&dsp_vernacular=on&dsp_statefips=on&dsp_genus=on&dsp_family=on&dsp_orders=on&dsp_class=on&dsp_division=on&dsp_kingdom=on&dsp_itis_tsn=on&dsp_dur=on&dsp_grwhabt=on&dsp_nativestatuscode=on&dsp_fed_nox_status_ind=on&dsp_state_nox_status=on&dsp_invasive_pubs=on&dsp_fed_te_status=on&dsp_state_te_status=on&dsp_nat_wet_ind=on&includeAuthors=on&Synonyms=all&dsp_synonyms=on&dsp_authorname_separate=on&viewby=sciname'


rm(list=ls())

``` {r get table}
#disable internet timeout
setInternet2(use=NA)
setInternet2(use=FALSE)
setInternet2(use=NA)

q1<-read.table('http://plants.usda.gov/java/AdvancedSearchServlet?pfa=l48&pfa=ak&pfa=hi&pfa=pr&dsp_vernacular=on&dsp_statefips=on&dsp_genus=on&dsp_family=on&dsp_orders=on&dsp_class=on&dsp_division=on&dsp_kingdom=on&dsp_itis_tsn=on&dsp_dur=on&dsp_grwhabt=on&dsp_nativestatuscode=on&dsp_fed_nox_status_ind=on&dsp_state_nox_status=on&dsp_invasive_pubs=on&dsp_fed_te_status=on&dsp_state_te_status=on&dsp_nat_wet_ind=on&includeAuthors=on&Synonyms=all&dsp_synonyms=on&dsp_authorname_separate=on&viewby=sciname&download=on',
               fill= TRUE,header=TRUE, sep=',',encoding='UTF8')
      #Don't forget to add '&download=on' to the end of the url
      #Special characters still not coming through correctly. USDA metadata specifies UTF8. Help???

write.csv (q1,'c:/Users/kjones/Documents/GitHub/organismalIPT/USDA_db.csv')

attach(q1)

#rename fields  
library(reshape)
usda <- rename(q1, c(Accepted.Symbol='taxonID',Synonym.Symbol='acceptedTaxonID', Kingdom='kingdom',
                     Division='phylum',  Order= 'order',  Family='family',  Genus='genus',
                     Species='specificEpithet',Scientific.Name='scientificName',
                     Common.Name='vernacularName', Species='specificEpithet', Class='class'))
names(usda)

#generate infraspecificEpithet
usda$infraspecificEpithet <- paste(usda$subspecies.Prefix, usda$Hybrid.Subspecies.Indicator,
                                   usda$Subspecies, usda$Variety.Prefix, usda$Hybrid.Variety.Indicator, 
                                   usda$Variety, usda$Subvariety.Prefix, usda$Subvariety, 
                                   usda$Forma.Prefix, usda$Forma,
                                   sep=' ')

#generate scientificNameAuthorship
usda$scientificNameAuthorship <- paste(usda$Genera.Binomial.Author, usda$Trinomial.Author, 
                                       usda$Quadranomial.Author, sep=' ')



#determine taxonRank by assigning value='fieldName' of finest taxonomy field.  
# Genus > Species > Subspecies > Variety > Forma


#Assign l48, ak, hi and pr NativeStatusCodes


#Assign dxxNativeStatusCodes
    #d01NativeStatusCode -> State.and.Province	if Native.Status = l48, extract CT, DE, ME, MD, MA, NH, NJ, NY, PA, RI, VT, WV
    # or something like this.(?) rules have been written out, need to get correct format
    #saved in 'taxonomyTemplate20140807'

# cut extra columns
usdaCut<-names(usda) %in% c('Hybrid.Genus.Indicator', 'Hybrid.Species.Indicator', 
             'Subspecies.Prefix', 'Hybrid.Subspecies.Indicator','Subspecies', 
             'Variety.Prefix', 'Hybrid.Variety.Indicator', 'Variety',
             'Subvariety.Prefix', 'Subvariety','Forma.Prefix','Forma',
             'Genera.Binomial.Author','Trinomial.Author','Quadranomial.Author',
             'Questionable.Taxon.Indicator','Parents', 'State.and.Province', 
             'Genus.1', 'Duration', 'Growth.Habit','Native.Status', 
             'Federal.Noxious.Status', 'State.Noxious.Status', 
             'Invasive','Federal.T.E.Status', 'State.T.E.Status', 'ITIS.TSN',
             'National.Wetland.Indicator.Status')
usda2<-usda[!usdaCut]

#add placeholders for new columns
usda2$startUseDate<-NA
usda2$endUseDate<-NA
usda2$speciesGroup<-NA
usda2$taxonRank<-NA
usda2$tribe<-NA
usda2$subgenus<-NA
usda2$speciesGroup<-NA
usda2$subfamily<-NA

           
#re-order comlumns
#mynames <- c("ID", "date", "estimate", "actual") 
#write.csv(DF[, mynames], ....) 

reorder<- c('taxonID', 'acceptedTaxonID', 'startUseDate', 'endUseDate', 'kingdom', 'phylum', 
                  'class', 'order', 'family', 'subfamily', 'tribe', 'genus', 'subgenus', 'speciesGroup', 
                  'specificEpithet', 'infraspecificEpithet', 'scientificName', 
                  'scientificNameAuthorship', 'taxonRank', 'vernacularName')

write.csv(usda2[, reorder], 'c:/Users/kjones/Documents/GitHub/organismalIPT/USDA_test.csv')

#append to master taxonomy,

