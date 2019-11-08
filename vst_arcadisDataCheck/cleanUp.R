merged<- function(folder){
  #after you have put all the files to be compiled together in a folder,
  #The first step is to make a list of all the files
  list.files<- list.files(folder, pattern="*.csv", full.names=TRUE)
  #second, read all of the csv's into a list of dataframes
  all.data<-lapply(list.files,read.csv,stringsAsFactors=FALSE)
  #Next, use the Reduce & merge functions to compile all of the data frames
  #into one data frame
  final <- Reduce(function(...) merge(..., all=T), all.data)##This reads 
  #in the data by rows, not sure if there's a way to just add data from the next
  #table
  file.path<- sprintf("%s/%s.csv", folder,"compiled")#This names the location &
  #name of the output to the same as the folder with all the files
  write.csv(final,file.path,row.names=FALSE)#This stores the compiled data frame as a csv
  #in the same folder with all the files
}

#2013 files
setwd("K:/FSU1/2013_field_campaign_D17/data_veg_csv")
merged("Stem_mappin2")

setwd("K:/FSU1/2013_field_campaign_D17/data_veg_csv/Stem_mappin2")
data<-read.csv("compiled.csv",stringsAsFactors=FALSE)
#Correct codes as identified by Ben Chemel
data$Species.ID[data$Species.ID=="arvi2"] <- "ARVI4"
data$Species.ID[data$Species.ID=="arvi"] <- "ARVI4"
data$Species.ID[data$Species.ID=="cade"] <- "CADE27"
data$Species.ID[data$Species.ID=="cade2"] <- "CADE27"
data$Species.ID[data$Species.ID=="cein"] <- "CAIN3"
data$Species.ID[data$Species.ID=="cebe4"] <- "CEBE3"
data$Species.ID[data$Species.ID=="coco"] <- "COCO6"
data$Species.ID[data$Species.ID=="quch27"] <- "QUCH2"
data$Species.ID[data$Species.ID=="quwi"] <- "QUWI2"
data$Species.ID[data$Species.ID=="quwi3"] <- "QUWI2"
data$Species.ID[data$Species.ID=="rimo2"] <- "RIRO"

#Change original codes from lower to upper case following USDA formatting
data$Species.ID<-toupper(data$Species.ID)

#Update synonyms
data$Species.ID[data$Species.ID=="SEDO2"]<- "SEFLD"
data$Species.ID[data$Species.ID=="SAME4"]<- "SARAM4"
data$Species.ID[data$Species.ID=="CEBE3"]<- "CEMOG"

#Create scientificName column
sp.names17<-data.frame(Species.ID=c("CADE27","CAIN3","CEMOG","COCO6","QUCH2","QUWI2",
                                "RIRO","SARAM4","SEFLD","LUAL4","KEBR","QUDO","PISA2","ARVI4",
                                "RHIL","CECU","CELE2","QUKE","ABCO","PIPO","PILA","CEIN3","QUGA4",
                                "AMUT","PREM"),scientificName=c("Calocedrus decurrens",
                                                                "Ceanothus integerrimus","Cercocarpus montanus var. glaber","Corylus cornuta",
                                                                "Quercus chrysolepis","Quercus wislizeni","Ribes roezlii",
                                                                "Sambucus racemosa var. melanocarpa","Senecio flaccidus var. douglasii",
                                                                "Lupinus albifrons","Keckiella breviflora","Quercus douglasii",
                                                                "Pinus sabiniana","Arctostaphylos viscida","Rhamnus ilicifolia",
                                                                "Ceanothus cuneatus","Ceanothus leucodermis","Quercus kelloggii",
                                                                "Abies concolor","Pinus ponderosa","Pinus lambertiana",
                                                                "Ceanothus integerrimus","Quercus garryana","Amelanchier utahensis",
                                                                "Prunus emarginata"),stringsAsFactors=FALSE)
data.with.sp<-merge(sp.names17,data,by='Species.ID')

colnames(data.with.sp)[colnames(data.with.sp) == "Species.ID"]<-"taxonID"#changed from Species ID (DWC)
colnames(data.with.sp)[colnames(data.with.sp) == "Plot.ID"]<-"plotID" #changed from Plot ID (DWC)
colnames(data.with.sp)[colnames(data.with.sp) == "StemID"]<-"indvidualID"#changed from stemID (DWC)
colnames(data.with.sp)[colnames(data.with.sp) == "TruPulse.Location"]<-"pointID"#changed from Trupulse Location 
colnames(data.with.sp)[colnames(data.with.sp) == "Stem.Distance..m."]<-"individualDistance" #changed from Stem distance (m)
colnames(data.with.sp)[colnames(data.with.sp) == "Stem.Azimuth"]<-"individualAzimuth"#stem azimuth
colnames(data.with.sp)[colnames(data.with.sp) == "Max.Basal.Dia_..cm."]<-"basalDiameter_cm"#Max Basal Dia_ (cm)(VegC)
colnames(data.with.sp)[colnames(data.with.sp) == "Alt.Basal.Dia_..cm."]<-"90degBasalDiameter_cm"#Alt Basal Dia_ (cm)
colnames(data.with.sp)[colnames(data.with.sp) == "DBH..cm."]<-"diameterBreastHeight_cm"#DBH (cm) (VegC)
colnames(data.with.sp)[colnames(data.with.sp) == "DBH.height..cm."]<-"diameterBreastHeight_Height_cm"#DBH height (cm)
colnames(data.with.sp)[colnames(data.with.sp) == "Max.Crown.Dia_..m."]<-"canopyDiameterMax_m"#Max Crown Dia_ (m)
colnames(data.with.sp)[colnames(data.with.sp) == "Alt.Crown.Dia..m."]<-"90degCanopyDiameter_m"#Alt Crown Dia (m)
colnames(data.with.sp)[colnames(data.with.sp) == "Crown.Height..VD1."]<-"canopyHeight_VD1"#Crown Height (VD1)
colnames(data.with.sp)[colnames(data.with.sp) == "Crown.Height..VD2."]<-"canopyHeight_VD2"#Crown Height (VD2)
colnames(data.with.sp)[colnames(data.with.sp) == "Stem.Notes"]<-"stemRemarks"#Stem Notes
colnames(data.with.sp)[colnames(data.with.sp) == "Crown.Shape"]<-"canopyForm"#Crown Shape (VegC)
colnames(data.with.sp)[colnames(data.with.sp) == "Percent.Living"]<-"living_percent"#Percent Living (format taken from VegC)
colnames(data.with.sp)[colnames(data.with.sp) == "X..Crown.in.Plot"]<-"canopyInPlot_percent"#% Crown in Plot
colnames(data.with.sp)[colnames(data.with.sp) == "Health.Status"]<-"healthStatus"#Health Status
colnames(data.with.sp)[colnames(data.with.sp) == "Foliar.Sample.ID"]<-"materialSampleID"#Foliar Sample ID (DWC)
colnames(data.with.sp)[colnames(data.with.sp) == "Stem.Status"]<-"stemStatus"#Stem Status
colnames(data.with.sp)[colnames(data.with.sp) == "Point_ID"]<-"unknownID"#changed from point_id to unknownID -- not really sure what it is or how it's assigned

##WRITE CODE TO CHANGE POINTID VALUES FROM LONG NAMES TO SHORT (center, NW, SW, SE, NE)
data.with.sp$pointID[data.with.sp$pointID == "Center"] <- "center"


#If DBH >0, then dbh_h should also be >0
#SO...create a flag (in column qf_DBH) for rows where dbh has a >0 value but dbh_h = 0
data.with.sp["qf_DBH"] <- NA
data.with.sp$qf_DBH <- ifelse(data.with.sp$diameterBreastHeight_cm>0 & data.with.sp$diameterBreastHeight_Height_cm==0.00, 1, 0)

#if dbh=0 and dbh_h = 130 then make both dbh and dbh_h NULL
data.with.sp$diameterBreastHeight_cm[data.with.sp$diameterBreastHeight_cm==0 & data.with.sp$diameterBreastHeight_Height_cm==130]<-NA
data.with.sp$diameterBreastHeight_Height_cm[data.with.sp$diameterBreastHeight_cm==0 & data.with.sp$diameterBreastHeight_Height_cm==130]<-NA

##add a line of code that would flag trupulse location (pointID) that puts tree outside of plot!
#this will be done in a separate .R file "K:\FSU1\2013_field_campaign_D17\Veg_data_csv\stem_coord_qf.R"


-------------------------------------------------
##try out IQR stuff on stem height
#stemHeight<-data.with.sp$canopyHeight_VD1-data.with.sp$canopyHeight_VD2
#data.with.sp$stemHeight<-stemHeight
#data.with.sp<-data.with.sp[data.with.sp$stemHeight>0,]
#data.with.sp<-data.with.sp[data.with.sp$scientificName=="Calocedrus decurrens",]
#species_stemHeight<-with(data.with.sp,data.with.sp[order(stemHeight),])


#sp<-split(data.with.sp,data.with.sp$scientificName)
#summary<-sapply(sp,function(sp)summary(sp$stemHeight))
#which(stemHeight==0)
#low<-quantile(data.with.sp$stemHeight,probs=0.25)-1.5*IQR(data.with.sp$stemHeight) 
#high<-quantile(data.with.sp$stemHeight,probs=0.75)+1.5*IQR(data.with.sp$stemHeight)
#data.with.sp2<-data.with.sp[data.with.sp$stemHeight>quantile(data.with.sp$stemHeight,probs=0.25)-1.5*IQR(data.with.sp$stemHeight) & 
#                             data.with.sp$stemHeight<quantile(data.with.sp$stemHeight,probs=0.75)+1.5*IQR(data.with.sp$stemHeight)]
#plot(data.with.sp$stemHeight,data.with.sp$indvidualID)


#try out IQR stuff on stem height by tree (>= 10cm) vs shrub-sapling (<10cm)
###not sure that this is the proper way to split it into only 2 groups (tree and shrub/sapling)
#group<-split(data.with.sp,data.with.sp$diameterBreastHeight_cm>=10|data.with.sp$diameterBreastHeight_cm<10)
#group2<-lapply(group, subset,stemHeight>quantile(stemHeight,probs=0.25)-1.5*IQR(stemHeight) & 
#                   stemHeight<quantile(stemHeight,probs=0.75)+1.5*IQR(stemHeight))

#hist(species[["Abies concolor"]]$stemHeight)
#summary(species[["Abies concolor"]]$stemHeight)

#hist(species2[["Quercus kelloggii"]]$stemHeight)
#summary(species2[["Abies concolor"]]$stemHeight)

#calculate stem height - not sure how to do this
#stemHeight<-data.with.sp$canopyHeight_VD1-data.with.sp$canopyHeight_VD2
#data.with.sp$stemHeight<-stemHeight
#which(stemHeight<0)
#which(stemHeight==0)
#max(stemHeight)

##evaluate how much data is lost after outlier removal
#create vector from 'before' list of dataframes of the number of individual records
#species_len<-sapply(species,function(species) length(species$stemHeight))
#create vector from 'after' list of dataframes of the number of individual records
#species2_len<-sapply(species2,function(species2) length(species2$stemHeight))
#create dataframe with the before & after numbers & species names
#species_len_dat<-data.frame(sp=species_len,sp2=species2_len)
#species_len_dat<-data.frame(species=row.names(species_len_dat),species_len_dat)
#add column that calculates the pct difference between before & after
#species_len_dat$pct_diff<-(species_len_dat$sp-species_len_dat$sp2)/species_len_dat$sp*100
#remove NA values (created because the SCIENTIFIC NAME column was a factor column)
#species_len_dat_large<-species_len_dat[!is.na(species_len_dat$pct_diff),]
#order dataframe by pct_diff column
#species_len_dat_large<-with(species_len_dat_large,species_len_dat_large[order(pct_diff),])

------------------------------------------------------------------------

#2012 files
setwd("K:/FSU1/2013_field_campaign_D17/data_veg_csv")
merged("2012_files")

setwd("K:/FSU1/2013_field_campaign_D17/data_veg_csv/2012_files")
data.2012<-read.csv("compiled_edited.csv",stringsAsFactors=FALSE)#NOTE: I had to manually edit the compiled file to collapse the 
#common_species & other_species. There were question marks in some of the other_species columns, so I dealt with that by
#adding a column for identificationQualifier and putting in 'c.f. species' for these instances

#correct misspelling of Betula alleghaniensis
data.2012$common_species[data.2012$common_species=="Betula alleghaniensi"] <- "Betula alleghaniensis"

sp.2012<-data.frame(scientificName=unique(data.2012$common_species),stringsAsFactors=FALSE)

setwd("K:/FSU1/2013_field_campaign_D17/data_veg_csv")
usda.MA<-read.csv("USDAPlantsMA20131205.csv",stringsAsFactors=FALSE)#downloaded from USDA plants (advanced search for Massachussets)

#subset the usda list for MA by removing var's & ssp's so that it can be compared to our species list from HF
var<-grepl("var\\.",usda.MA$Scientific.Name)
var.count<-grep("var\\.",usda.MA$Scientific.Name)
usda.MA.novar<-usda.MA[!var,][,]
ssp<-grepl("ssp\\.",usda.MA.novar$Scientific.Name)
usda.MA.nossp<-usda.MA.novar[!ssp,][,]

#further subset the usda list by our species list from HF
present<-numeric(0)
for (i in usda.MA.nossp$Scientific.Name){
  present=c(present,ifelse(any(i == sp.2012$scientificName),0,1))}
usda.MA.nossp$present<-present
present_subset<-usda.MA.nossp$present==0
usda.MA.subset<-usda.MA.nossp[present_subset,][,]

#merge the subsetted usda list & our data set to add taxonID to the list
usda.sciNames<-data.frame(taxonID=usda.MA.subset$Accepted.Symbol,common_species=usda.MA.subset$Scientific.Name,stringsAsFactors=FALSE)
data.2012<-merge(data.2012,usda.sciNames,all=TRUE)

#drop shotgun_sampling column
data.2012$shotgun_sampling<-NULL

#rename columns to be consistent with D17 effort & DWC/VegC where possible
colnames(data.2012)[colnames(data.2012) == "common_species"] <- "scientificName"#changed from common_species(DWC)
colnames(data.2012)[colnames(data.2012) == "plot_id"] <- "plotID" #changed from plot_id (DWC)
colnames(data.2012)[colnames(data.2012) == "dbh"] <- "diameterBreastHeight" #changed from dbh(VegC)
colnames(data.2012)[colnames(data.2012) == "stem_notes"] <- "stemRemarks" #changed from stem_notes
colnames(data.2012)[colnames(data.2012) == "stem_height"] <- "stemHeight" #changed from stem_height
colnames(data.2012)[colnames(data.2012) == "canopy_diameter"] <- "canopyDiameter_m" #changed from canopy_diameter
colnames(data.2012)[colnames(data.2012) == "vertical_position"] <- "verticalPosition" #changed from vertical_position
colnames(data.2012)[colnames(data.2012) == "stem_status"] <- "stemStatus" #changed from stem_status
colnames(data.2012)[colnames(data.2012) == "status_notes"] <- "statusRemarks" #changed from status_notes
colnames(data.2012)[colnames(data.2012) == "point_id"] <- "individualID" #changed from point_id (DWC)
colnames(data.2012)[colnames(data.2012) == "shotgun_id"] <- "materialSampleID" #changed from shotgun_id (DWC)
colnames(data.2012)[colnames(data.2012) == "ddh"] <- "diameterDecimeterHeight" #changed from ddh
#colnames(data.2012)[colnames(data.2012) == "easting"] <- "decimalLatitude" #changed from easting (Latitude= Northing) (DWC)
#colnames(data.2012)[colnames(data.2012) == "northing"] <- "decimalLongitude" #changed from Northing (Longitude = Easting) (DWC)
colnames(data.2012)[colnames(data.2012) == "trupulse_position"] <- "pointID" #changed from trupulse_position
colnames(data.2012)[colnames(data.2012) == "stem_distance"] <- "individualDistance" #changed from stem_distance
colnames(data.2012)[colnames(data.2012) == "stem_angle"] <- "individualAzimuth" #changed from stem_angle
colnames(data.2012)[colnames(data.2012) == "first_branch_height"] <- "heightFirstBranch_m" #changed from first_branch_height (VegC)
colnames(data.2012)[colnames(data.2012) == "canopy_diam_90deg"] <- "90degCanopyDiameter" #changed from canopy_diameter_90deg
colnames(data.2012)[colnames(data.2012) == "canopy_diam_150deg"] <- "150degCanopyDiameter_m" #changed from canopy_diameter_150deg
colnames(data.2012)[colnames(data.2012) == "canopy_diam_210deg"] <- "210degCanopyDiameter_m" #changed from canopy_diameter_210deg
colnames(data.2012)[colnames(data.2012) == "nested_subplot"] <- "subplotID" #changed from nested_subplot

##WRITE CODE TO CHANGE POINTID VALUES FROM LONG NAMES TO SHORT (center, NW, SW, SE, NE)
data.2012$pointID[data.2012$pointID == "Plot center"] <- "center"
data.2012$pointID[data.2012$pointID == "NW corner"] <- "NW"
data.2012$pointID[data.2012$pointID == "SW corner"] <- "SW"
data.2012$pointID[data.2012$pointID == "NE corner"] <- "NE"
data.2012$pointID[data.2012$pointID == "SE corner"] <- "SE"
data.2012$pointID[data.2012$pointID == "Intensive SW"] <- "iSW"
data.2012$pointID[data.2012$pointID == "Center-line (10 m)"] <- "center_10"
data.2012$pointID[data.2012$pointID == "Plot origin (0 m)"] <- "center_0"
data.2012$pointID[data.2012$pointID == "Intensive SE"] <- "iSE"
data.2012$pointID[data.2012$pointID == "Center-line (30 m)"] <- "center_30"
data.2012$pointID[data.2012$pointID == "Center-line (50 m)"] <- "center_50"
data.2012$pointID[data.2012$pointID == "Center-line (40 m)"] <- "center_40"
data.2012$pointID[data.2012$pointID == "Center-line (20 m)"] <- "center_20"
data.2012$pointID[data.2012$pointID == "Intensive NW"] <- "iNW"
data.2012$pointID[data.2012$pointID == "Intensive NE"] <- "iNE"

#create a reference table with max height at maturity for each species in the 2012 data file
setwd("K:/FSU1/2013_field_campaign_D17/data_veg_csv/2012_files")
ref_height_2012<-read.csv("2012_species_max_height.csv",stringsAsFactors=FALSE)
ref_height_2012<- within(ref_height_2012, max_height_m <- max_height_ft*0.3048)

#for each row in data.2012, compare the stemHeight to the max_height value in ref_height_2012
#if stemHeight is greater than the ref value, put a flag in qf_stemHeight column in data.2012

data.2012["qf_stemHeight"]<- NA

data.2012["qf_DBH"]<- NA

data.2012.merged <- merge(data.2012, ref_height_2012, by.x = "scientificName", by.y = "Species")

data.2012.merged$qf_stemHeight<-ifelse(data.2012.merged$stemHeight > data.2012.merged$max_height_m, 1, 0)

data.2012.merged$qf_DBH <- ifelse(data.2012.merged$diameterBreastHeight == 99, 1, 0)

data.2012.qf <- subset(data.2012.merged, select = scientificName:qf_DBH)

##add a line of code that would flag trupulse location (pointID) that puts tree outside of plot!
#this will be done in a separate .R file "K:\FSU1\2013_field_campaign_D17\Veg_data_csv\stem_coord_qf.R"


-----------------------------------------------------------------------
#panel.histogram(data.2012$diameterBreastHeight, scientificName) 

#hist(data.2012$stemHeight)
#x<-split(data.2012,data.2012$scientificName)
#y<-sapply(x,function(x) summary(x$stemHeight))

#w<-lapply(x, function(x) summary(x$stemHeight))
#z<-lapply(x, function(x) length(x$stemHeight))
------------------------------------------------------------------------------

setwd("K:/FSU1/2013_field_campaign_D17/data_veg_csv/final_corrected_datasets")
write.csv(data.with.sp,"2013.csv")
write.csv(data.2012.qf,"2012.csv")
