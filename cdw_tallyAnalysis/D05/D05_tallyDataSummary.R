# Summary of CDW tally sampling effort
##  Read in D05 tally data
tally <- read.csv("D05_tally_abundance.csv", header=T, stringsAsFactors = F)

##  Total number of DSTs tallied
nrow(tally) # --> 48

##  Total unique taxonIDs encountered during tally
length(unique(tally$taxonid)) # --> 17 total taxonIDs

##  Total DSTs resulting in 80% cumulative abundance cut-off
library(dplyr)
tally %>% filter(cumulativeAbundance < 82) %>% count()  # --> 21 
