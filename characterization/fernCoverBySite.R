library(tidyverse)


setwd('C:/Users/kjones/Documents/GitHub/CSP/neon-tos-sampling-design')

allDat <- read.csv('results/task_1b_single_table_ranks_and_weights.csv', stringsAsFactors = F)

ferns <- read.csv('code/neonutils/data-raw/Fern species.csv')

allFerns <- filter(allDat, taxonID%in%ferns$Accepted.Symbol)

fernSummary <- allFerns%>%
  group_by(siteID)%>%
  summarize(meanPC=sum(meanPercentCover), nsMeanPC=sum(nsMeanPercentCover))

fernSummary$sampleFerns <- ifelse(fernSummary$meanPC>=10 | fernSummary$nsMeanPC>=10, 'yes', 'no')
