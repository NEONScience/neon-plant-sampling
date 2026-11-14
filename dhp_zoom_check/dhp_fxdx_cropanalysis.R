### Analyze images collected with FX vs DX crop from same plots ####

library(tidyverse)

#   Read in data
dhpDF <- read.csv("dhp_fxdx_cropdata.csv", header = TRUE)

#   Make wide to enable calculating difference
wideDF <- dhpDF %>%
  tidyr::pivot_wider(names_from = "sensorCrop",
                     values_from = "PAI") %>%
  dplyr::mutate(diff = DX - FX,
                relDiff = round((diff/FX) * 100,
                                digits = 0))

median(wideDF$relDiff)
#--> -39.5

mean(wideDF$relDiff)
#--> -26.5

t.test(wideDF$relDiff)

# One Sample t-test
# 
# data:  wideDF$relDiff
# t = -1.5682, df = 7, p-value = 0.1608
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -66.45945  13.45945
# sample estimates:
#   mean of x 
# -26.5 