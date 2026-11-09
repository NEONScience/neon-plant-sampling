### Function to process flagged table from estimateWoodProd output to identify anomalous stemDiameter values relative to lag and lead neighbors then model a new predictedStemDiam value to replace the anomalous one.

predictStemDiameter <- function(flaggedData) {
  
  flaggedDF <- flaggedData
  
  ### Remove unneeded columns
  flaggedDF <- flaggedDF %>%
    dplyr::select(-"namedLocation",
                  -"subplotID",
                  -c("scientificName":"family"),
                  -"measurementStrategy",
                  -"plantStatus",
                  -"incrementFlag",
                  -"agb_kg",
                  -c("baseCrownHeight":"bandStemDiameter"),
                  -"recordedBy",
                  -"measuredBy") %>%
    dplyr::relocate("uid", .before = "domainID")
  
  
  
  ### Filter to individualIDs with all liveDeadStatus == "live" or individuals with mortality that show a "clean" transition
  flaggedDF <- flaggedDF %>%
    dplyr::group_by(individualID) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::group_modify(~ {
      
      #   Remove NAs from liveDeadStatus group
      s <- .x$liveDeadStatus[!is.na(.x$liveDeadStatus)]
      
      #   Find positions where the status changes between consecutive non-NA records (may be > 1)
      changes <- which(head(s, -1) != tail(s, -1))
      
      #   Determine when all non-NA records are "live"
      allLive <- length(s) > 0 && all(s == "live")
      
      #   Determine when there is a single "live" to "dead" transition
      cleanLiveDead <- any(.x$transitionStatus == "mortality", na.rm = TRUE) && 
        length(changes) == 1 &&
        s[changes] == "live" &&
        s[changes + 1] == "dead"
      
      #   Remove NAs from growthForm and determine when exactly one unique value remains
      gf <- .x$growthForm[!is.na(.x$growthForm)]
      singleGrowthForm <- length(unique(gf)) == 1
      
      #   Retain those groups meeting criteria and with a minimum of 3 stemDiameters (need 2 remaining once flagged stemDiam removed)
      if (isTRUE(allLive || cleanLiveDead) && singleGrowthForm) {
        .x
      } else {
        .x[0, ]
      }
    }) %>%
    dplyr::relocate("individualID", .after = "taxonID")
  
  
  
  ### For mortality events, filter out all rows after the transitionStatus == "mortality" row
  #--> Note: No need to correct any stemDiameter values after the tree has died and may be losing diameter in non-linear ways.
  flaggedDF <- flaggedDF %>%
    dplyr::group_by(individualID) %>%
    dplyr::arrange(year,
                   .by_group = TRUE) %>%
    
    #   Find the "mortality" row in each group
    dplyr::mutate(
      mortalityRowNum = max(which(transitionStatus == "mortality"), 0),
      .after = "transitionStatus"
    ) %>%
    
    #   Retain those rows less than or equal to the mortality row
    dplyr::filter(mortalityRowNum == 0 | dplyr::row_number() <= mortalityRowNum) %>%
    dplyr::select(-"mortalityRowNum") %>%
    
    #   For all groups, keep only those with 3 non-NA stemDiameters remaining (minimum needed for model prediction)
    dplyr::filter(sum(!is.na(stemDiameter)) >= 3)
  
  
  
  ### Identify anomalous stemDiameter rows by comparing rates of stemDiameter change
  #--> Note: This approach is robust when the anomalous row is either the first or last row in the group
  flaggedDF <- flaggedDF %>%
    dplyr::group_by(individualID) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    
    dplyr::group_modify(~{
      .x <- .x %>%
        
        #   For each record, calculate lag/lead rates of stemDiameter change compared to neighboring rows
        dplyr::mutate(
          rateBack = (stemDiameter - lag(stemDiameter)) / 
            lubridate::time_length(estimatedDate - lag(estimatedDate), unit = "year"),
          rateForward = (lead(stemDiameter) - stemDiameter) / 
            lubridate::time_length(lead(estimatedDate) - estimatedDate, unit = "year")
        )
      
      #   Calculate a "deviation" metric: determine how much adjacent rates differ; a high deviation suggests the current row is an outlier. For edge rows: use the available rate; for middle rows: use the deviation
      .x <- .x %>%
        dplyr::mutate(
          rateDev = dplyr::case_when(
            is.na(rateBack) & is.na(rateForward) ~ 0,
            is.na(rateBack) ~ abs(rateForward),
            is.na(rateForward) ~ abs(rateBack),
            TRUE ~ abs(rateForward - rateBack)
          )
        )
      
      anomaly_idx <- which.max(.x$rateDev)
      
      .x %>%
        dplyr::mutate(estimatedDiamFlag = row_number() == anomaly_idx,
                      .after = "stemDiameter") %>%
        dplyr::select(-"rateBack",
                      -"rateForward",
                      -"rateDev")
    }) %>%
    
    dplyr::relocate("individualID", .before = "growthForm")
  
  
  
  ### Model predicted stemDiameter after omitting flagged stemDiameter rows
  flaggedDF <- flaggedDF %>%
    dplyr::group_by(individualID) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    
    dplyr::mutate(
      modelInputStemDiameter = dplyr::if_else(estimatedDiamFlag == TRUE, NA_real_, stemDiameter),
      
      predictedDiam = tryCatch(
        broom::augment(lm(.data$modelInputStemDiameter ~ .data$estimatedDate), 
                       newdata = dplyr::pick(dplyr::everything()))$.fitted,
        error = function(e) NA_real_
      ),
      
      predictedDiam = round(predictedDiam, digits = 1),
      
      .after = "stemDiameter"
    )
  
  
  
  ### Clean up columns for output and return data for estimatedDiamFlag == TRUE
  flaggedDF <- flaggedDF %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("plotType":"totalSampledAreaTrees"),
                  -"tempStemID",
                  -"tagStatus",
                  -"modelInputStemDiameter",
                  -c("measurementHeight":"dataEntryRecordID")) %>%
    dplyr::filter(estimatedDiamFlag == TRUE)
  
  return(flaggedDF)
  
} # End function