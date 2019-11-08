setwd("Z:/")
library(dplyr)
########################################## 2) Setup File Grabbing ##########################################
#set this to the prefix for the sheets in your module; make specific to file name batch 
pathToFiles <- getwd()

prefix.perind <- 'vst_perindividual_'
mySuffix <- '.csv'

fileList <- list.files(pathToFiles, full.names=TRUE) #

#######################################################################
# separate the files by prefix
prefix.perindbout <- 'vst_perindividual_perbout_'
files.perindbout <- fileList[grep(prefix.perindbout,fileList)] # s

prefix.perind <- 'vst_perindividual_D'
files.perind <- fileList[grep(prefix.perind,fileList)] # s

prefix.perother <- 'vst_perother_per'
files.perother <- fileList[grep(prefix.perother,fileList)] # s

prefix.perplot <- 'vst_perplot'
files.perplot <- fileList[grep(prefix.perplot,fileList)] # s

prefix.pershrub <- 'vst_pershrub'
files.pershrub <- fileList[grep(prefix.pershrub,fileList)] # s

#######################################################################

# grab a file with the plot type data
plotFile = read.csv(fileList[grep('L_Plot',fileList)], sep = ",", stringsAsFactors=FALSE) # grab azimuths 

#######################################################################

multipleCombine <- function(input, ply = llply){
  ply(input, function(x){
    t <- read.csv(x, header=TRUE, sep=",",stringsAsFactors = FALSE) # read the csv
    t1 <- rbind(t) # rbind it to a temporary variable
    return(t1) # return the full variable
  }
  )
}

# combine all of the vst_ files
c.perindbout <- multipleCombine(files.perindbout, ply = ldply)
c.perind  <-  multipleCombine(files.perind, ply = ldply)
c.perother  <- multipleCombine(files.perother, ply = ldply)
c.perplot  <- multipleCombine(files.perplot, ply = ldply)
c.pershrub  <- multipleCombine(files.pershrub, ply = ldply)



# Make the tower only lists for each subset
towerList = filter(plotFile, Type == "Tower")

# towerOnly rows
towerOnly_perindbout = c.perindbout[c.perindbout$plotID %in% towerList$Plot_ID, ]
towerOnly_perind = c.perind[c.perind$plotID %in% towerList$Plot_ID, ]
towerOnly_perother = c.perother[c.perother$plotID %in% towerList$Plot_ID, ]
towerOnly_perplot = c.perplot[c.perplot$plotID %in% towerList$Plot_ID, ]
towerOnly_pershrub = c.pershrub[c.pershrub$plotID %in% towerList$Plot_ID, ]

# check if a distrubted exists
filter(towerOnly_vst, plotID == "CPER_077")

# check the first pass
c.perind$plotID
c.perind$plotID %in% towerList$Plot_ID
c.perind %>% filter(c.perind$plotID %in% towerList$Plot_ID)

c.perindbout$plotID %in% towerList$Plot_ID

head(c.perind$plotID)

# check individual DFs
c.perindbout$plotID

unique(c.perindbout$plotID)

# check the rows
nrow(c.perindbout)
nrow(towerOnly_perindbout)
nrow(c.perother)
nrow(towerOnly_perother)
nrow(c.perind)
nrow(towerOnly_perind)
nrow(c.perplot)
nrow(towerOnly_perplot)
nrow(c.pershrub)
nrow(towerOnly_pershrub)

# write the files
write.csv(towerOnly_perindbout, file = "towerOnly_vst_perindividual_perbout")
write.csv(towerOnly_perother, file = "towerOnly_vst_perother_perbout")
write.csv(towerOnly_perind, file = "towerOnly_vst_perindividual")
write.csv(towerOnly_perplot, file = "towerOnly_vst_perplot")
write.csv(towerOnly_pershrub, file = "towerOnly_vst_pershrub")


