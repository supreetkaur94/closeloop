#' Function to check if all values are numeric in data
#'
#' @param df A data frame contains columns that represent covariates
#' @param col_names A numeric vector of covariates that can be binary or continuous
#'
#' @return logical
#' @export
#'
#' @examples
#' attach(exampleData)
#' var = c("Age","Male")
#' x = check_data(df = exampleData, col_names = var)
#' x
#'@author Shubhram Pandey \email{shubhram1992@@gmail.com}
#'
check_data <- function(df, col_names = NULL) {
  # Check if col_names is NULL or empty
  if (is.null(col_names) || length(col_names) == 0) {
    stop("Please provide a vector of column names to check.")
  }

  # Loop through each column name
  for (col_name in col_names) {
    # Check if column exists
    if (!is.element(col_name, names(df))) {
      stop(paste0("Column '", col_name, "' does not exist in the data frame."))
    }

    # Check if column is numeric
    if (!is.numeric(df[[col_name]])) {
      stop(paste0("Column '", col_name, "' is not numeric."))
    }
  }

  # If all checks pass, return TRUE
  return(TRUE)
}
