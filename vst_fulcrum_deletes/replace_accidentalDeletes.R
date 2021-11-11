## help with missing tags
## ITASK0019137


library(devtools)
devtools::install_local('C:/Users/kjones/Documents/GitHub/how-to-make-a-data-product/REST_R/restR',
                        force = TRUE)

library(restR)
library(tidyverse)

api_token = Sys.getenv('FULCRUM_KEY')

if (file.exists(
  'C:/Users/kjones')){
  myPathToFulcrumDev <- "C:/Users/kjones/Documents/GitHub/fulcrumDev/admin_tools_scripts/magpie_requests"
  myPathToFulcrumDevNew <- "C:/Users/kjones/Documents/GitHub/fulcrumDev/admin_tools_scripts/fulcrumAPI/R"
}


# Import Magpie functions
source(paste(myPathToFulcrumDev, 'batch_record_exists.fulcrum.R', sep ="/"))
source(paste(myPathToFulcrumDev, 'batch_record_delete.fulcrum_googlesheet.R', 
             sep ="/"))
source(paste(myPathToFulcrumDev, 'check_del_set_dups.R', sep ="/"))
source(paste(myPathToFulcrumDev, 'get_fulcrum_data.R', sep ="/"))


#vst - dpID= "DP0.10098.001"

##### get L0 record
df_1 <- get.os.l0.by.query(stack='prod',
                         tab='DP0.10098.001:vst_apparentindividual_in',
                         fieldDataSearchStrings = "multi-bole tree",
                         fieldName="growthForm",
                         parentNamedLoc = "WREF",
                         format_for_L0_editor=TRUE)

df_2 <- get.os.l0.by.query(stack='prod',
                           tab='DP0.10098.001:vst_apparentindividual_in',
                           fieldDataSearchStrings = "multi-bole tree",
                           fieldName="growthForm",
                           parentNamedLoc = "ABBY",
                           format_for_L0_editor=TRUE)

ai_dat <- df_1 %>%
  bind_rows(df_2) 

ai_sub <- filter(ai_dat, 
                 str_detect(individualID, "A$|B$|C$|D$|E$"))

d16_alphaTags <- unique(ai_sub$individualID)

l0_mt <- data.frame()
  for (i in d16_alphaTags){
    temp <- get.os.l0.by.query(stack='prod',
                           tab='DP0.10098.001:vst_mappingandtagging_in',
                           tag=i,
                           format_for_L0_editor=TRUE)
    l0_mt <- bind_rows(l0_mt, temp)
  }





# fulcrum tags from manual download of all D16 recs that ends in A-E
ful_tags <- read.csv('C:/Users/kjones/Documents/GitHub/neon-plant-sampling/vst_fulcrum_deletes/vst_mt_d16_alphaTags.csv', stringsAsFactors = F)

intersect(d16_alphaTags, ful_tags$individualid)
setdiff(d16_alphaTags, ful_tags$individualid)
### all ai alpha tags are present in fulcrum mt

intersect(l0_mt$fulcrumID, ful_tags$X_record_id)

# I cannot find any missing tags


