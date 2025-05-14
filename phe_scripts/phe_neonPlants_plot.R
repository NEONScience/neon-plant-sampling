#' @title Calculate Phenological Transition Dates
#'
#' @author
#' Katie Jones \email{kjones@battelleecology.org} \cr
#'
#' @description This function uses observation data from the NEON Plant Phenology Observation (DP1.10055.001) to calculate phenophase transition dates and descriptive statistics.
#'
#'
#' @param pheData data.frame from NEON Plant Phenology Observation (DP1.10055.001) phe_statusintensity table as returned from neonUtilities::loadByProduct().
#
#' @details
#' This function ...
#' Calculated values inclued:
#'  * transition date - the mid-point between two consecutive dates with different phenophase status values
#'
#' @return This function returns a data frame
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # load additional packages for these examples
#' library(neonUtilities)
#' library(dplyr)
#'
#' # get data
#' pheDat <- loadByProduct(
#'   dpID = "DP1.10055.001",
#'   site = "UKFS",
#'   startdate = "2022-01",
#'   enddate = "2022-12",
#'   package = "basic",
#'   check.size = FALSE)
#'  }
#'

# changelog and author contributions / copyrights
#   Katie Jones (2024-08-28)
#     original creation
##############################################################################################

library(tidyverse)
library(neonUtilities)

pheDat <- loadByProduct(
    dpID = "DP1.10055.001",
    site = "HARV",
    startdate = "2021-01",
    enddate = "2024-12",
    package = "basic",
    include.provisional = T,
    check.size = FALSE)

transitions <- estimatePheTransByTag(inputDataList = pheDat)

doy_xform <- unlist(lapply(seq(0,364,by=45), function(x){ format(as.Date(as.Date(x, origin = "2023-01-01")),'%b %d')}))

transitions$phenophaseName <- factor(transitions$phenophaseName, levels="Breaking leaf buds", "Increasing leaf size", "Leaves", "Young leaves", "Colored leaves", "Falling leaves", "Breaking needle buds", "Young needles",  "Initial growth",   "Open flowers", "Open pollen cones")))) %>%


p <- ggplot(transitions, aes(x=doy_transition, y=taxonID, show.legend = TRUE)) +
  geom_line(color="light blue", linewidth=1)+
  geom_pointrange(aes(y=taxonID, x=doy_transition,xmin=doy_intervalStart, xmax=doy_intervalEnd, color=year, shape = transitionType))+
  scale_x_continuous(breaks = seq(0,364,by=45),labels = doy_xform)+
  facet_wrap(~phenophaseName, scales="free_y")+
  ggtitle('')+
  #ylab("tagID")+
  xlab("transition date")


