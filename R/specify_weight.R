#' Title specify_weight
#'
#' @param var Variables for which weights can be assigned
#' @param weights weights in same sequence as variables
#'
#' @return list
#' @export
#'
#' @examples
#' var = c("Male","Age")
#' weights = specify_weight(var, weights = c(0.5,0.5))
#' weights
#' @author Shubhram Pandey \email{shubhram1992@@gmail.com}
#'
specify_weight <- function(var, weights) {

  if (!is.null(weights) && length(weights) != length(var)) {
    stop("Length of weights must be equal to var")
  }

  x <- vector("list", length(var))

  for (i in 1:length(weights)) {
    weight <- as.numeric(weights[i])

    if (is.na(weight) || weight <= 0) {
      stop("Weight must be a positive number.")
    }

    x[[i]] <- weight
  }

  return(x)
}
