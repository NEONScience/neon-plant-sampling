

moss <- read.csv(file.choose(),header=T)


library(stringr)
library(plyr)


# grab first word i.e. the genus
moss$undeGenera <- str_trim(word(moss$undeMoss, 1))
moss$wiscGenera <- str_trim(word(moss$wiscMoss,1))

# extract last word
str_extract(moss$undeMoss, '\\w+$')

# alternative?
# xa <- rep("CPER_001_21_M_6_39_20140101", 100)
# x_sp <- stringr::str_split(xa, "_")
# plyr::ldply(x_sp)[5:6]
# 


# compare lists (does not include Pteridophytes)
moss$undeGenera %in% moss$wiscGenera




x <- rep("CPER_001",10)

xlist <- str_split(x, "_")

xlist[[1]][[1]]
