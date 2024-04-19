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
           date_lag = lag(date), transitionType = paste0(status_lag, "-", phenophaseStatus)) %>%
    filter(!is.na(status_lag), phenophaseStatus != status_lag) %>%
    mutate(transition_date = as.Date(date_lag + (date - date_lag) / 2),
           uncertainty = as.numeric(difftime(date, date_lag, units = "days"))/2,
           samplingInterval = as.numeric(difftime(date, date_lag, units = "days"))) %>%
    select(year, siteID, individualID, phenophaseName, transitionType, date, date_lag, 
           samplingInterval, transition_date, uncertainty)%>%
    arrange(year, phenophaseName, individualID)
}
