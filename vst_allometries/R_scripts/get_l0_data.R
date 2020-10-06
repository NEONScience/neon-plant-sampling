library(restR)
library(here)
library(readr)


pmd_df <- restR::get.os.l0(stack = 'prod', tab = 'DP0.10098.001:vst_perplotperyear_in') 
                             startDate = '2012-01-01', endDate = '2020-07-26')


if (!require("neonUtilities")) install.packages("neonUtilites")
library(neonUtilities)

#ltr = dpID="DP1.10033.001"
#phe = dpID= "DP1.10055.001"

##
vstDat <- loadByProduct(dpID="DP1.10098.001",
                        #site = "SJER",
                        #startdate = NA,
                        #enddate = NA,
                        package = "basic",
                        check.size = FALSE, 
                        token = Sys.getenv('NEON_KEY'))

# unlist all data frames
list2env(vstDat ,.GlobalEnv)

date <- '20200715'

dir.create(path=paste(getwd(), 'vst/data', date, sep='/'))

folder <- paste(getwd(), 'vst/data', date, sep='/')

write.csv(vst_perplotperyear, 
           paste(folder, 'l0_vst_pmd.csv', sep='/'))
write.csv(vst_mappingandtagging,
          paste(folder, 'l0_vst_mt.csv', sep='/'))
write.csv(vst_apparentindividual,
          paste(folder, 'l0_vst_ai.csv', sep='/'))
write.csv(vst_shrubgroup,
          paste(folder, 'l0_vst_sg.csv', sep='/'))





