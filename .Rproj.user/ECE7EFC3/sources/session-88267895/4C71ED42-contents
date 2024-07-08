#' Function to check if columns are proportions
#'
#' @param df a data frame to be checked
#' @param col_names column names to be checked
#'
#' @return list
#' @export
#'
#' @examples
#' #' attach(exampleData)
#' result <- is_prop(exampleData,c("Male","Age"))
#' result
#' @author Shubhram Pandey \email{shubhram1992@@gmail.com}
is_prop <- function(df, col_names) {
  if (!is.character(col_names)) {
    stop("Please provide column names as a character vector.")
  }
  if (!all(col_names %in% names(df))) {
    stop("One or more column names are not found in the data frame.")
  }
  prop_results <- list()
  for (col_name in col_names) {
    if (col_name %in% names(df)) {
      if (any(is.na(df[[col_name]]))) {
        stop(paste0("Column '", col_name, "' contains NA values. Please remove blank cells."))
      }
      if (all(df[[col_name]] >= 0 & df[[col_name]] <= 1)) {
        prop_results[[col_name]] <- TRUE
      } else {
        prop_results[[col_name]] <- FALSE
        warning(paste0("Column '", col_name, "' contains values not within [0, 1]. Ensure all values are proportions."))
      }
    } else {
      warning(paste0("Column '", col_name, "' not found in data frame."))
      prop_results[[col_name]] <- NA
    }
  }
  return(prop_results)
}
