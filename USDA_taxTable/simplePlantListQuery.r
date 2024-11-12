plantlist<-read.csv('N:common/TOS/ATBDs/Lookups/pubPkg_pla/NEON_pla_taxonomy.csv') 
names(plantlist)

#query dataframe for all acceptedTaxonID values begining with QUMA
QuercusChange<-plantlist[grepl("Quercus", plantlist$genus) ,]

Quercus2<-as.data.table(QuercusChange)
Quercus3<-select(Quercus2, taxonID, acceptedTaxonID, scientificName)
write.table(Quercus3, 'C:/Users/kjones/Documents/GitHub/kjones13/QuercusExample.csv', col.names=T, 
            row.names =F, na ='', sep=',')

#subset dataframe for all Fagaceae
Fagaceae<-plantlist[grepl("Fagaceae", plantlist$family) ,]
VariableCut<-names(Fagaceae) %in% c('startUseDate',"endUseDate", "kingdom", "phylum", "class",
                                    "order", "family",  "subfamily", "tribe", "genus",
                                    "subgenus", "speciesGroup", "specificEpithet", "infraspecificEpithet",
                                    "scientificName", "scientificNameAuthorship", "taxonRank", "vernacularName",
                                    "nameAccordingTo", "nameAccordingToID",  "taxonProtocolCategory")
Fagaceae_old<-Fagaceae[!VariableCut]                      
q1<-read.table('http://plants.usda.gov/java/AdvancedSearchServlet?pfa=l48&pfa=ak&pfa=hi&pfa=pr&family=Fagaceae&dsp_familySym=on&dsp_vernacular=on&dsp_statefips=on&dsp_genus=on&dsp_family=on&dsp_orders=on&dsp_class=on&dsp_division=on&dsp_kingdom=on&dsp_itis_tsn=on&dsp_dur=on&dsp_grwhabt=on&dsp_nativestatuscode=on&dsp_fed_nox_status_ind=on&dsp_state_nox_status=on&dsp_invasive_pubs=on&dsp_fed_te_status=on&dsp_state_te_status=on&dsp_nat_wet_ind=on&includeAuthors=on&Synonyms=all&dsp_synonyms=on&dsp_authorname_separate=on&viewby=sciname&download=on',
               fill= TRUE,header=TRUE, sep=',',encoding='UTF-8')