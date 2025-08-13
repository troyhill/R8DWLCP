#' Process proficiency test data from multiple sources
#'
#' @param df data frame with PT data
#'
#' @returns dataframe
#' @export
#'
proc_pt_data <- function(df) {
  ### goal of function is to detect different lab data formats and homogenize them.
  ### 1. ERA/WS data
  ### assuming analysesdate is the relevant date.
  era_names <- c('ParticipantIdentifier', 'AnalyteName', 'MethodDescription', 'FinalAssignedValue', 'LAUALimit', 'Evaluation', 'AnalysesDate')
  if(all(era_names %in% names(df))) {
    df <- df[, era_names]
    df <- df[grep(x = tolower(df$Evaluation), pattern = 'acceptable'), ]
    df <- df[df$ParticipantIdentifier %in% locations$EPA_ID, ]
    df$laboratory_name     <- locations$laboratory_name[match(df$ParticipantIdentifier, locations$EPA_ID)]
    df$laboratory_location <- locations$laboratory_location[match(df$ParticipantIdentifier, locations$EPA_ID)]
    df$method            <- df$MethodDescription
    df$analyte           <- df$AnalyteName
    df$method_comb       <- paste0(df$MethodDescription, '; ', df$AnalyteName)
    df$PT_test_date <- df$AnalysesDate
    df$PT_result    <- sapply(X = df$Evaluation, FUN = function(x) {ifelse(grepl(x = tolower(x), pattern = '^acceptable$'), 'Pass', ifelse(grepl(x = tolower(x), pattern = '^not acceptable$'), 'Fail', ''))})
    df$value        <- df$FinalAssignedValue
    df$limits       <- df$LAUALimit
    df$EPA_ID       <- df$ParticipantIdentifier
    df <- df[, c("EPA_ID", "laboratory_name", "laboratory_location", "method", "analyte", "method_comb", "PT_result", "PT_test_date")]
  }
  return(df)
}
