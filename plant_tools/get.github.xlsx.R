#################################################################################
#' @title Retrieve an Excel file from GitHub
#' @author Courtney Meier \email{cmeier@@battelleEcology.org} \cr
#' @description A private function (i.e. does not load in global environment) to access Excel files on GitHub. Pulls files by using a GitHub token. Using private functions requires prefixing with a period '.' when the package is loaded or triple colons if calling without loading library.
#' @param xlsxURL Path to the file on GitHub; for example: "https://raw.githubusercontent.com/gitbarnitt/NEON-OS-spatial-data/master/test.xlsx"
#' @param xlsxSheet Name or number of the sheet within the xlsx file to retrieve; if not specified function will return the first sheet; use quotes if specifying the sheet by name like "sheetName"
#' @param gitToken User's personal GitHub access token. Recommended that people store this in their R environment.
#'
#' @return Function returns a data frame after reading the specified sheet from the specified Excel file on GitHub.
#'
#' @examples \dontrun{
#' tempURL <- "https://raw.githubusercontent.com/gitbarnitt/NEON-OS-spatial-data/master/TOS/data/UniquePlotIDsSamplingModulesPriorityLists.xlsx"
#' plotPriority <- .get.github.xlsx(xlsxURL = tempURL, xlsxSheet = "plotPriorityLists", gitToken = Sys.getenv('GITHUB_PAT'))
#' }
#################################################################################


get.github.xlsx <- function(xlsxURL, xlsxSheet = 1, gitToken){
  # Load required libraries
  require(httr)
  require(openxlsx)
  
  # Retrieve file from Git and return desired sheet
  httr::GET(xlsxURL, authenticate(gitToken, ''), write_disk(tf <- tempfile(fileext = ".xlsx")))
  output <- openxlsx::read.xlsx(xlsxFile = tf, sheet = xlsxSheet, colNames = TRUE)
  return(output)
}