
if (file.exists(
  'C:/Users/kjones')){
  wdir <- "C:/Users/kjones/Documents/GitHub/neon-plant-sampling"
}


df <- read.csv(paste(wdir, "vst_dendrometer/SourceData/vst_data_d_cleaned.csv", sep='/'),
               stringsAsFactors=F )

nlcd_plots <- read.csv(paste(wdir, "vst_dendrometer/SourceData/updatedApplicableModules_20200605.csv", sep='/'),
                       stringsAsFactors=F)

dbh_agb_thresh <- read.csv(paste(wdir, "vst_dendrometer/SourceData/dbh_agb_thresholds.csv", sep='/'),
                           stringsAsFactors = F)

dbh_agb_thresh <- dplyr::rename(dbh_agb_thresh, `75th`=X75th, `95th`=X95th)

vst_annualPlot <- unique(nlcd_plots$plotID[nlcd_plots$annualTowerVst=='Y'])
vst_annualPlot <- vst_annualPlot[!is.na(vst_annualPlot)]
