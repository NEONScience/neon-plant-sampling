###  Calculation of stem positions relative to the SW corner (0,0) of each AOP plot used in the 
## 2012 Harvard Forest field campaign and 2013 D17 field campaign.

#	Read in functions that convert radians to degrees and vice versa
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv")
source("Degrees_radians.R")
#--------------------------------#
##Stem Coordinate Checks - 2012
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv/final_corrected_datasets")
temp2012 = read.csv("2012.csv", header=T, check.names = FALSE)
  
#	Check aplot data for missing values in these columns: pointID, individualDistance, individualAzimuth
#temp2012[!complete.cases(temp2012$pointID), ]
#temp2012[!complete.cases(temp2012$individualDistance), ]
#temp2012[!complete.cases(temp2012$individualAzimuth), ]

#	Read in relative coordinates of Aplot positions used to map stem locations
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv/2012_files")
TandAplots = read.csv("Relative_coords_AandTplots.csv", header=T)

##	Use a "for" loop to calculate the position of each stem relative to the SW corner of each plot - i.e. the (0,0) position in each plot.

for (j in 1:nrow(temp2012)){
  
  #	Retrieve relative position of TruPulse within the plot
  X.tru = TandAplots$X_coord[TandAplots$point_ID==as.character(temp2012$pointID[j])]
  Y.tru = TandAplots$Y_coord[TandAplots$point_ID==as.character(temp2012$pointID[j])]
  
  #	Calculate the measured X and Y offset for the stem from individualDistance and individualAzimuth
  X.offset = temp2012$individualDistance[j]*sin(radians(temp2012$individualAzimuth[j]))
  Y.offset = temp2012$individualDistance[j]*cos(radians(temp2012$individualAzimuth[j]))
  
  
  #	Calculate and store the relative X and Y coordinates in the temp.df dataframe
  temp2012$xcoord[j] = round(X.tru + X.offset, digits=1)
  temp2012$ycoord[j] = round(Y.tru + Y.offset, digits=1)
  
  
  #	End "for" loop bracket
}


##  Check to see if ranges for stem positions are appropriate for A plots, if outside the range, put a flag in qf_pointID
aplot_2012 <- subset(temp2012, grepl("^A", plotID))
aplot_2012["qf_stemMap"] <- NA    
aplot_2012$qf_stemMap <- ifelse(aplot_2012$xcoord<0 | aplot_2012$xcoord>20 | aplot_2012$ycoord < 0 | aplot_2012$ycoord > 20, 1, 0)

## Read in aplot GPS coordinates for each plot's origin (SW corner)
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv/2012_files")
aplot_gps = read.csv("aplot_gps_origin.csv", header=T)

##for each row, look at plot_id and import the easting and northing for the origin (SW corner)
aplot_2012$easting <- NULL
aplot_2012$northing <- NULL
aplot_gps2 <- aplot_gps
aplot_gps2$pointid <- NULL
aplot_2012_gps <- merge(aplot_2012, aplot_gps2, by.x = "plotID", by.y = "plot_id")
aplot_2012_gps$plot_id <- NULL

## then add the xcoord and easting and y coord and northing to calculate the easting and northing for each individual tree
aplot_2012_gps$individualEasting <- rowSums(aplot_2012_gps[,c("xcoord","easting")])
aplot_2012_gps$individualNorthing <- rowSums(aplot_2012_gps[,c("ycoord","northing")])

#make a final table for aplot_2012 and remove easting and northing for SW corner of plot
aplot_2012_final <- aplot_2012_gps
aplot_2012_final$easting <- NULL
aplot_2012_final$northing <- NULL
names(aplot_2012_final)[names(aplot_2012_final)=="individualEasting"] <- "easting"
names(aplot_2012_final)[names(aplot_2012_final)=="individualNorthing"] <- "northing"


##  Check to see if ranges for stem positions are appropriate for T plots
tplot_2012 <- subset(temp2012, grepl("^T", plotID))
tplot_2012["qf_stemMap"] <- NA    
tplot_2012$qf_stemMap <- ifelse(tplot_2012$xcoord < 0 | tplot_2012$xcoord > 50 | tplot_2012$ycoord < 0 | tplot_2012$ycoord > 20, 1, 0)

## Read in tplot GPS coordinates for each plot's origin (SW corner)
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv/2012_files")
tplot_gps = read.csv("tplot_gps_origin.csv", header=T)

##for each row, look at plot_id and assign the easting and northing for the origin (SW corner)
tplot_2012$easting <- NULL
tplot_2012$northing <- NULL
tplot_gps2 <- tplot_gps
tplot_gps2$pointid <- NULL
tplot_2012_gps <- merge(tplot_2012, tplot_gps2, by.x = "plotID", by.y = "plot_id")
tplot_2012_gps$plot_id <- NULL

## then add the xcoord and easting and y coord and northing to calculate the easting and northing for each individual tree
tplot_2012_gps$individualEasting <- rowSums(tplot_2012_gps[,c("xcoord","easting")])
tplot_2012_gps$individualNorthing <- rowSums(tplot_2012_gps[,c("ycoord","northing")])

#make a final table for tplot_2012 and remove easting and northing for SW corner of plot
tplot_2012_final <- tplot_2012_gps
tplot_2012_final$easting <- NULL
tplot_2012_final$northing <- NULL
names(tplot_2012_final)[names(tplot_2012_final)=="individualEasting"] <- "easting"
names(tplot_2012_final)[names(tplot_2012_final)=="individualNorthing"] <- "northing"


#Put the aplot and tplot tables made in the 2 sections above back together so we have one large table for 2012 data
final2012 <- rbind(aplot_2012_final, tplot_2012_final)
final2012$xcoord <- NULL
final2012$ycoord <- NULL

#-------------------------#

##Stem Coordinate Checks - 2013
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv/final_corrected_datasets")
temp2013 = read.csv("2013.csv", header=T, check.names = FALSE)

#subset the temp2013 data for only stems in the SOAP plots
SOAP_plot <- subset(temp2013, grepl("^SOAP", plotID))

## Read in SOAP GPS coordinates for each plot's corners and centroid
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv")
SOAP_gps = read.csv("SOAP_gps.csv", header=T)


##  Use a "for" loop to calculate the position of each stem relative to position of the TruPulse in the plot.

for (i in 1:nrow(SOAP_plot)){
  plot1 = as.character(SOAP_plot$plotID[i])
  
  #  Retrieve relative position of TruPulse within the plot
  X.tru = SOAP_gps$easting[which(SOAP_gps$plotID==plot1 & SOAP_gps$pointID==as.character(SOAP_plot$pointID[i]))]
  Y.tru = SOAP_gps$northing[which(SOAP_gps$plotID==plot1 & SOAP_gps$pointID==as.character(SOAP_plot$pointID[i]))]
  
  #  Calculate the measured X and Y offset for the stem from individualDistance and individualAzimuth
  X.offset = SOAP_plot$individualDistance[i]*sin(radians(SOAP_plot$individualAzimuth[i]))
  Y.offset = SOAP_plot$individualDistance[i]*cos(radians(SOAP_plot$individualAzimuth[i]))
  
  #  Calculate and store the relative X and Y coordinates in the temp.df dataframe
  SOAP_plot$Easting[i] = round(X.tru + X.offset, digits=1)
  SOAP_plot$Northing[i] = round(Y.tru + Y.offset, digits=1)
  
}

##  Check to see if ranges for stem positions are appropriate for SOAP plots; if outside the range, put a flag in qf_pointID
SOAP_plot$qf_stemMap <- NA   

for (j in 1:nrow(SOAP_plot)){
  
  #  Dynamically define the coordinates of the origin (SW)
  plot2 = as.character(SOAP_plot$plotID[j])
  
  originEasting <- SOAP_gps$easting[which(SOAP_gps$plotID==plot2 & SOAP_gps$pointID=='SW')]
  originNorthing <- SOAP_gps$northing[which(SOAP_gps$plotID==plot2 & SOAP_gps$pointID=='SW')]
  
  #  Does easting value fall within appropriate easting range? If yes = 0, If no = 1
  qf_Easting <- ifelse(SOAP_plot$Easting[j]< originEasting | SOAP_plot$Easting[j]>(originEasting+20), 1, 0)
  
  #  Does northing value fall within appropriate northing range? If yes = 0, If no = 1
  qf_Northing <- ifelse(SOAP_plot$Northing[j]< originNorthing | SOAP_plot$Northing[j]>(originNorthing+20), 1, 0)
  
  #  Sum the values of the two checks above (= 0, 1, or 2)
  rangeSum <- qf_Easting + qf_Northing
  
  SOAP_plot$qf_stemMap[j] <- ifelse(rangeSum!=0, 1, 0)
  
}

SOAP_final <- SOAP_plot

#----------#

#subset the temp2013 data for only stems in the SJER plots
SJER_plot <- subset(temp2013, grepl("^SJER", plotID))

## Read in SJER GPS coordinates for each plot's corners and centroid
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv")
SJER_gps = read.csv("SJER_gps.csv", header=T)


##  Use a "for" loop to calculate the position of each stem relative to position of the TruPulse in the plot.

for (z in 1:nrow(SJER_plot)){
  plot3 = as.character(SJER_plot$plotID[z])
  
  #  Retrieve relative position of TruPulse within the plot
  X.tru = SJER_gps$easting[which(SJER_gps$plotID==plot3 & SJER_gps$pointID==as.character(SJER_plot$pointID[z]))]
  Y.tru = SJER_gps$northing[which(SJER_gps$plotID==plot3 & SJER_gps$pointID==as.character(SJER_plot$pointID[z]))]
  
  #  Calculate the measured X and Y offset for the stem from individualDistance and individualAzimuth
  X.offset = SJER_plot$individualDistance[z]*sin(radians(SJER_plot$individualAzimuth[z]))
  Y.offset = SJER_plot$individualDistance[z]*cos(radians(SJER_plot$individualAzimuth[z]))
  
  #  Calculate and store the relative X and Y coordinates in the temp.df dataframe
  SJER_plot$Easting[z] = round(X.tru + X.offset, digits=1)
  SJER_plot$Northing[z] = round(Y.tru + Y.offset, digits=1)
  
}

##  Check to see if ranges for stem positions are appropriate for SJER plots; if outside the range, put a flag in qf_pointID
SJER_plot$qf_stemMap <- NA   

for (q in 1:nrow(SJER_plot)){
  
  #  Dynamically define the coordinates of the origin (SW)
  plot4 = as.character(SJER_plot$plotID[q])
  
  originEasting <- SJER_gps$easting[which(SJER_gps$plotID==plot4 & SJER_gps$pointID=='SW')]
  originNorthing <- SJER_gps$northing[which(SJER_gps$plotID==plot4 & SJER_gps$pointID=='SW')]
  
  #  SJER had a 20x20 subplot nested inside a 40x40 plot (SW corner of 20x20 subplot 
  #is origin).Does easting value fall within appropriate easting range? If yes = 0, If no = 1
  qf_Easting <- ifelse(SJER_plot$Easting[q]< (originEasting-10) | SJER_plot$Easting[q]>(originEasting+30), 1, 0)
  
  #  SJER had a 20x20 subplot nested inside a 40x40 plot (SW corner of 20x20 subplot 
  #is origin).Does northing value fall within appropriate northing range?  If yes = 0, If no = 1
  qf_Northing <- ifelse(SJER_plot$Northing[q]< (originNorthing-10) | SJER_plot$Northing[q]>(originNorthing+30), 1, 0)
  
  #  Sum the values of the two checks above (= 0, 1, or 2)
  rangeSum <- qf_Easting + qf_Northing
  
  SJER_plot$qf_stemMap[q] <- ifelse(rangeSum!=0, 1, 0)
  
}

SJER_final <- SJER_plot

#Put the SOAP and SJER tables made in the 2 sections above back together so we have one large table for 2013 data
final2013 <- rbind(SOAP_final, SJER_final)

#-------------------------#

##  Write files to save changes
setwd("N:/Science/FSU/2013_field_campaign_D17/Data_veg_csv/final_corrected_datasets")
write.csv(final2012, file="2012_final.csv")
write.csv(final2013, file="2013_final.csv")
