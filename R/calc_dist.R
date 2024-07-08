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
calc_dist<- function(df, col_names, Study = "Study", Treat = "Treatment", weights, digits) {


  # Ensure weights is a numeric vector of the correct length
  if (length(weights) != length(col_names)) {
    stop("Length of weights must be equal to the length of col_names.")
  }

  # Call specify_weight to get weights for col_names
  if (!is.character(col_names)) {
    stop("Please provide column names as a character vector.")
  }

  is_prop_result <- is_prop(df, col_names)

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

  study_comb_str <- NULL
  treat_comb_str <- NULL
  value_diffs_list <- list()

  for (i in seq_along(col_names)) {
    value_col <- col_names[i]
    weight <- weights[i]
    res_df <- data.frame(Study = as.character(df[[Study]]), Value = df[[value_col]], Treat = as.character(df[[Treat]]))
    value_combs <- combinat::combn(res_df$Value, 2)
    if (is.null(study_comb_str) && is.null(treat_comb_str)) {
      study_combs <- combinat::combn(res_df$Study, 2)
      study_comb_str <- paste0(study_combs[1,], " - ", study_combs[2,])
      treat_combs <- combinat::combn(res_df$Treat, 2)
      treat_comb_str <- paste0(treat_combs[1,], " - ", treat_combs[2,])
    }
    value_diffs <- abs(value_combs[1,] - value_combs[2,])
    value_diffs_list[[value_col]] <- value_diffs
  }

  result_df <- data.frame(Study = study_comb_str, Treatment = treat_comb_str)

  for (i in seq_along(col_names)) {
    value_col <- col_names[i]
    result_df[[paste0("Diff_", value_col)]] <- value_diffs_list[[value_col]]
    result_df[[paste0("Weight_", value_col)]] <- weights[[i]]
  }

  for (i in seq_along(col_names)) {
    value_col <- col_names[i]
    diff_col <- paste0("Diff_", value_col)
    weight_col <- paste0("Weight_", value_col)
    weighted_col <- paste0("Weighted_", value_col)
    result_df[[weighted_col]] <- result_df[[diff_col]] * result_df[[weight_col]]
  }

  weighted_cols <- grep("^Weighted_", names(result_df), value = TRUE)
  weight_cols <- grep("^Weight_", names(result_df), value = TRUE)
  result_df$Distance <- rowSums(result_df[weighted_cols]) / rowSums(result_df[weight_cols])
  result_df$Distance <- round(result_df$Distance, digits)

  # Extract unique studies and treatments
  unique_studies <- unique(unlist(strsplit(as.character(result_df$Study), " - ")))
  unique_treatments <- unique(unlist(strsplit(as.character(result_df$Treatment), " - ")))

  # Initialize an empty matrix
  study_treatment <- unique(c(paste0(unique_studies, " (", unique_treatments, ")")))
  distance_matrix <- matrix(NA, nrow = length(study_treatment), ncol = length(study_treatment))
  rownames(distance_matrix) <- study_treatment
  colnames(distance_matrix) <- study_treatment

  # Fill the matrix with the corresponding distances
  for (i in 1:nrow(result_df)) {
    study_pair <- unlist(strsplit(as.character(result_df$Study[i]), " - "))
    treatment_pair <- unlist(strsplit(as.character(result_df$Treatment[i]), " - "))

    row_name <- paste0(study_pair[1], " (", treatment_pair[1], ")")
    col_name <- paste0(study_pair[2], " (", treatment_pair[2], ")")

    distance_matrix[row_name, col_name] <- result_df$Distance[i]
    distance_matrix[col_name, row_name] <- result_df$Distance[i]
  }

  # Select only the required columns
  result_df <- result_df[, c("Study", "Treatment", "Distance")]

  return(list(data_frame = result_df, matrix = distance_matrix))
}


