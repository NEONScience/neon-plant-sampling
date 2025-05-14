##############################################################################################
#' @title Calculate Phenologic Transition Dates

#' @author Katie Jones \email{kjones@battelleecology.org} \cr

#' @description Function to 

#' @param df Data frame containing phenophase status observations. [data frame]

#' @return This function returns a new data frame with **transition dates**, calculated as the 
#' mid-point between two consecutive dates with different phenophase status values, **transition_type**, 
#' indicating the phenophase status values of the transition, **sampling interval**,
#' the number of days between observations, **uncertainty** = sampling interval/2 and **nth tranition**, 
#' assigns a count of onset events per individualID, phenophase name, within a given calendar year. 

#' @examples
#' \dontrun{
#'out <- calculate_transition_dates(df=phe_statusintensity)
#' }

#' @export degrees


##############################################################################################


calculate_transition_dates <- function(df) {
  if (!require("tidyverse")) {
    install.packages("tidyverse")
    library("tidyverse")
  } else {
    library("tidyverse")
  }
  df%>%
    mutate(year = substr(date, 1,4))%>%
    group_by(individualID, phenophaseName) %>%
    filter(phenophaseStatus!="uncertain") %>%
    mutate(status_lag = lag(phenophaseStatus),
           date_lag = lag(date), doy_lag = lag(dayOfYear), transitionType = paste0(status_lag, "-", phenophaseStatus)) %>%
    filter(!is.na(status_lag), phenophaseStatus != status_lag) %>%
    mutate(transition_date = as.Date(date_lag + (date - date_lag) / 2),
           doy_transition = yday(transition_date),
           uncertainty = as.numeric(difftime(date, date_lag, units = "days"))/2,
           samplingInterval = as.numeric(difftime(date, date_lag, units = "days"))) %>%
    group_by(year,individualID, phenophaseName)%>%
    mutate(nth_transition = cumsum(status_lag == "no" & phenophaseStatus == "yes"))%>%
    select(year, siteID, individualID, phenophaseName, transitionType, nth_transition, date, date_lag, 
           doy_lag, samplingInterval, transition_date, doy_transition, uncertainty)%>%
    arrange(year, phenophaseName, individualID)
}


