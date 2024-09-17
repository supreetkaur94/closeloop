#' Title To calculate distance between two studies using covariate information
#'
#' @param df A data frame consists of columns namely "Study", "Treatment", and at least one covariate.
#' @param col_names A vector of column names specifying covariate names.
#' @param Study A column name in a data frame named as "Study" specifying study names.
#' @param Treat A column name in a data frame named as "Treatment" specifying treatment names.
#' @param weights A variable in which the results of specify_weight() function was stored.
#' @param digits A numeric value indicating the number of decimal places in the Distance calculated.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' attach(exampleData)
#' var = c("Male","Age")
#' weights = specify_weight(var, weights = c(0.5,0.5))
#' weights
#' dist = calc_dist(df = exampleData, col_names = var, weights = weights,digits = 4)
#' dist
#'@author Shubhram Pandey \email{shubhram1992@@gmail.com}
#'
calc_dist <- function(df, col_names, Study = "Study", Treat = "Treatment", weights, digits) {

  # Ensure weights is a numeric vector of the correct length
  if (length(weights) != length(col_names)) {
    stop("Length of weights must be equal to the length of col_names.")
  }

  # Check if columns contain proportions
  is_prop_result <- is_prop(df, col_names)

  # Normalize the columns to a range [0,1] if they are not proportions
  for (col_name in col_names) {
    if (col_name %in% names(df)) {
      if (!is.numeric(df[[col_name]])) {
        warning(paste0("Column '", col_name, "' is not numeric. Skipping standardization."))
        next
      }
      if (!is_prop_result[[col_name]]) {
        min_val <- min(df[[col_name]], na.rm = TRUE)
        max_val <- max(df[[col_name]], na.rm = TRUE)
        df[[col_name]] <- (df[[col_name]] - min_val) / (max_val - min_val)
        df[[col_name]] <- round(df[[col_name]], digits)
      }
    } else {
      warning(paste0("Column '", col_name, "' not found in data frame. Skipping standardization."))
    }
  }

  # Initialize lists to store study, treatment, and differences
  study_comb_str <- NULL
  treat_comb_str <- NULL
  value_diffs_list <- list()

  # Calculate all pairwise differences for each variable
  for (i in seq_along(col_names)) {
    value_col <- col_names[i]
    weight <- weights[i]

    # Dataframe for current column
    res_df <- data.frame(Study = as.character(df[[Study]]), Value = df[[value_col]], Treat = as.character(df[[Treat]]))

    # Generate all pairwise combinations
    value_combs <- combinat::combn(res_df$Value, 2)

    if (is.null(study_comb_str) && is.null(treat_comb_str)) {
      study_combs <- combinat::combn(res_df$Study, 2)
      study_comb_str <- paste0(study_combs[1,], " - ", study_combs[2,])

      treat_combs <- combinat::combn(res_df$Treat, 2)
      treat_comb_str <- paste0(treat_combs[1,], " - ", treat_combs[2,])
    }

    # Calculate differences
    value_diffs <- abs(value_combs[1,] - value_combs[2,])
    value_diffs_list[[value_col]] <- value_diffs
  }

  # Create result dataframe
  result_df <- data.frame(Study = study_comb_str, Treatment = treat_comb_str)

  # Add the calculated differences and weights to the result dataframe
  for (i in seq_along(col_names)) {
    value_col <- col_names[i]
    result_df[[paste0("Diff_", value_col)]] <- value_diffs_list[[value_col]]
    result_df[[paste0("Weight_", value_col)]] <- weights[[i]]
  }

  # Calculate weighted differences
  for (i in seq_along(col_names)) {
    value_col <- col_names[i]
    diff_col <- paste0("Diff_", value_col)
    weight_col <- paste0("Weight_", value_col)
    weighted_col <- paste0("Weighted_", value_col)
    result_df[[weighted_col]] <- result_df[[diff_col]] * result_df[[weight_col]]
  }

  # Calculate total distance by summing weighted differences
  weighted_cols <- grep("^Weighted_", names(result_df), value = TRUE)
  weight_cols <- grep("^Weight_", names(result_df), value = TRUE)
  result_df$Distance <- rowSums(result_df[weighted_cols]) / rowSums(result_df[weight_cols])
  result_df$Distance <- round(result_df$Distance, digits)

  # Extract unique study-treatment pairs for the matrix
  unique_study_treatment <- unique(paste0(df[[Study]], " (", df[[Treat]], ")"))

  # Initialize a matrix to store distances
  distance_matrix <- matrix(NA, nrow = length(unique_study_treatment), ncol = length(unique_study_treatment))
  rownames(distance_matrix) <- unique_study_treatment
  colnames(distance_matrix) <- unique_study_treatment

  # Populate the matrix with the calculated distances
  for (i in 1:nrow(result_df)) {
    study_pair <- unlist(strsplit(as.character(result_df$Study[i]), " - "))
    treat_pair <- unlist(strsplit(as.character(result_df$Treatment[i]), " - "))

    row_name <- paste0(study_pair[1], " (", treat_pair[1], ")")
    col_name <- paste0(study_pair[2], " (", treat_pair[2], ")")

    distance_matrix[row_name, col_name] <- result_df$Distance[i]
    distance_matrix[col_name, row_name] <- result_df$Distance[i]  # Symmetric matrix
  }

  # Select only the relevant columns for the dataframe
  result_df <- result_df[, c("Study", "Treatment", "Distance")]

  return(list(data_frame = result_df, matrix = distance_matrix))
}
