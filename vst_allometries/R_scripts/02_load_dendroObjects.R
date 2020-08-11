

df <- read.csv("vst_allometries/SourceData/vst_data_cleaned.csv", stringsAsFactors=F )

nlcd_plots <- read.csv("vst_allometries/SourceData/updatedApplicableModules_20200605.csv", 
                       stringsAsFactors=F)

#dbh_agb_thresh <- read.csv('vst_allometries/SourceData/dbh_agb_thresholds.csv',
#                           stringsAsFactors = F)
#dbh_agb_thresh <- dplyr::rename(dbh_agb_thresh, `75th`=X75th, `95th`=X95th)

vst_annualPlot <- unique(nlcd_plots$plotID[nlcd_plots$annualTowerVst=='Y'])
vst_annualPlot <- vst_annualPlot[!is.na(vst_annualPlot)]
