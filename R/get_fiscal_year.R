# Function to calculate fiscal year
#' get_fiscal_year
#'
#' @param date 
#'
#' @returns value
#' @export

get_fiscal_year <- function(date) {
  if (is.na(date)) {
    return(NA)
  }
  
  current_year <- as.numeric(format(date, "%Y"))
  current_month <- as.numeric(format(date, "%m"))
  
  if (current_month >= 10) {
    return(current_year + 1)
  } else {
    return(current_year)
  }
}