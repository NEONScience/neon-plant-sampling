
if (file.exists(
  'C:/Users/kjones')){
  myPathToTeams <- "C:/Users/kjones/National Ecological Observatory Network, Inc/OS Team - OS Protocol Revision/protocols_SOPs/TOS-VST_vegetationStructure/dendrometerDevelopment/kj_dendroFiles"
}

df <- read.csv("SourceData/vst_data_d_cleaned.csv", stringsAsFactors=F )

nlcd_plots <- read.csv(paste(myPathToTeams, "SourceData/updatedApplicableModules_20200605.csv", sep="/"), 
                       stringsAsFactors=F)

dbh_agb_thresh <- read.csv(paste(myPathToTeams, 'SourceData/dbh_agb_thresholds.csv', sep='/'),
                           stringsAsFactors = F)
dbh_agb_thresh <- dplyr::rename(dbh_agb_thresh, `75th`=X75th, `95th`=X95th)

vst_annualPlot <- unique(nlcd_plots$plotID[nlcd_plots$annualTowerVst=='Y'])
vst_annualPlot <- vst_annualPlot[!is.na(vst_annualPlot)]
