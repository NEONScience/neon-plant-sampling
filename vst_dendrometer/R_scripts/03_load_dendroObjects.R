
if (file.exists(
  'C:/Users/kjones')){
  wdir <- "C:/Users/kjones/Documents/GitHub/neon-plant-sampling/vst_dendrometer"
}

df <-  readRDS(paste(wdir, year, "sourceData/vst_merged_data.rds", sep='/'))

plots <- readRDS(paste(wdir, year, "sourceData/plot_data.rds", sep='/'))

dbh_agb_thresh <- read.csv(paste(wdir, year, "sourceData/dbh_abg_thresholds.csv", sep='/'),
                           stringsAsFactors = F)

dbh_agb_thresh <- dplyr::rename(dbh_agb_thresh, `75th`=X75th, `95th`=X95th)

vst_annualPlot <- unique(plots$plotid[plots$annualtowervst=='Y'])
vst_annualPlot <- vst_annualPlot[!is.na(vst_annualPlot)]
