library(tidyverse)


allDat <- read.csv('characterization/neon-tos-sampling-design/results/task_1b_single_table_ranks_and_weights.csv', stringsAsFactors = F)

ferns <- read.csv('characterization/neon-tos-sampling-design/code/neonutils/data-raw/Fern species.csv')

allFerns <- filter(allDat, taxonID%in%ferns$Accepted.Symbol)

fernSummary <- allFerns%>%
  group_by(siteID)%>%
  summarize(meanPC=sum(meanPercentCover), nsMeanPC=sum(nsMeanPercentCover))

fernSummary$sampleFerns <- ifelse(fernSummary$meanPC>=10 | fernSummary$nsMeanPC>=10, 'yes', 'no')

write.csv(fernSummary, 'characterization/fernSummary.csv', row.names = F)
