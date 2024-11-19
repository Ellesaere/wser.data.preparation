library(data.table)

#' Column Information with Grouping
#'
#' Analyzes a given data table or frame, providing detailed information about each column. 
#' The function calculates statistics like mean, standard deviation, maximum, and minimum values for numeric columns. 
#' For categorical columns, it identifies the type and distribution characteristics. Optionally, 
#' it calculates correlations with a specified column. It supports grouping by one or two variables, 
#' presenting grouped analyses within the output.
#'
#' @param dt A data.table or data frame to analyze.
#' @param corr_variable An optional string specifying the name of a column with which to calculate 
#'        correlations for numeric columns. This parameter is ignored for non-numeric columns.
#' @param by An optional character vector specifying one or two column names by which to group the data before analysis. 
#'        If provided, statistics are calculated within each group, and the output includes a `Grouped_by` column 
#'        showing the grouping variables and their values for each row.
#' @return Returns a data.table containing statistics for each column in the input data. 
#'         For numeric columns, statistics include the mean, standard deviation, maximum, minimum, and number of observations. 
#'         The type of variable (e.g., continuous, discrete) and distribution characteristics are also provided. 
#'         If `corr_variable` is specified, the correlation of each numeric column with the specified column is included. 
#'         The output includes a `Grouped_by` column if the data is grouped.
#' @details This function is particularly useful for exploratory data analysis, allowing for a quick overview 
#'          of the data structure, including basic statistics and correlations. Grouping functionality enables 
#'          more granular analysis based on specified segments of the data.
#' @examples
#' \dontrun{
#'   library(data.table)
#'   dt <- data.table(id = 1:4, 
#'                    score = c(23, 42, 16, 8), 
#'                    category = factor(c("A", "B", "A", "B")), 
#'                    value = rnorm(4))
#'   # Basic usage without grouping
#'   column_info(dt, corr_variable = "score")
#'   
#'   # With grouping by a single variable
#'   column_info(dt, by = "category")
#'   
#'   # Grouping by two variables and calculating correlation
#'   column_info(dt, corr_variable = "score", by = c("category", "id"))
#' }
#' @export
#' @importFrom data.table rbindlist setcolorder
#' 

column_info <- function(df, corr_variable = NULL, by = NULL) {
  # Load required libraries
  library(data.table)
  
  # Initialize the list to store information for each group
  grouped_info_list <- list()
  
  # Determine the groups
  if (is.null(by)) {
    groups <- list(df)
    group_names <- list(NULL)
  } else {
    groups <- split(df, df[, by, with = FALSE])
    group_names <- names(groups)
  }
  
  # Iterate over each group
  for (group_idx in seq_along(groups)) {
    group <- groups[[group_idx]]
    group_name <- group_names[group_idx]
    
    # Initialize the list to store information for each column
    info_list <- list()
    
    # Iterate over each column in the group
    for (col_name in names(group)) {
      col_data <- group[[col_name]]
      col_label <- col_name
      col_mean <- if (is.numeric(col_data)) mean(col_data, na.rm = TRUE) else NA
      col_var <- if (is.numeric(col_data)) sd(col_data, na.rm = TRUE) else NA
      col_max <- if (is.numeric(col_data)) max(col_data, na.rm = TRUE) else NA
      col_min <- if (is.numeric(col_data)) min(col_data, na.rm = TRUE) else NA
      col_nobs <- sum(!is.na(col_data))
      col_non_zeros <- if (is.numeric(col_data)) sum(col_data != 0, na.rm = TRUE) else NA
      variable_type <- if (is.factor(col_data)) "factor" else if (is.character(col_data)) "character" else "other"
      dist_type <- if (is.numeric(col_data)) "numeric" else "non-numeric"
      col_classes <- class(col_data)
      col_corr <- if (!is.null(corr_variable) && is.numeric(col_data)) cor(col_data, group[[corr_variable]], use = "complete.obs") else NA
      
      # Determine unique values for factor or string columns
      unique_values <- if (variable_type %in% c("factor", "character")) {
        unique_vals <- unique(col_data)
        if (length(unique_vals) > 10) {
          "Too many unique values to list"
        } else {
          paste(unique_vals, collapse = ", ")
        }
      } else {
        NA
      }
      
      # Append the column information to the list
      info_list[[col_name]] <- list(
        Grouped_by = group_name,
        Label = as.character(col_label),
        Mean = as.numeric(col_mean),
        StandardDev = as.numeric(col_var),
        Max = as.numeric(col_max),
        Min = as.numeric(col_min),
        Observations = as.integer(col_nobs),
        NonZeroCount = as.integer(col_non_zeros),
        VariableType = as.character(variable_type),
        DistributionType = as.character(dist_type),
        Class = as.character(paste(col_classes, collapse = ", ")),
        Correlation = as.numeric(col_corr),
        UniqueValues = unique_values
      )
    }
    
    # Combine the information for all columns within the current group
    grouped_info_list[[group_idx]] <- rbindlist(info_list, idcol = "ColumnName")
  }
  
  # Combine the information for all groups and return
  info_dt <- rbindlist(grouped_info_list, use.names = TRUE)
  
  return(info_dt)
}


# column_info <- function(dt, corr_variable = NULL, by = NULL, rounding_digits=2) {

  

#   # Convert to data.table if not already one
#   if (!inherits(dt, "data.table")) {
#     dt <- as.data.table(dt)
#   }
  
#   clean_column_names <- function(names) {
#     gsub("[[:space:]]|[^[:alnum:]_]", "", names)
#   }
  
#   # Clean all column names
#   setnames(dt, old = names(dt), new = clean_column_names(names(dt)))
  
#   # Clean 'by' and 'corr_variable' parameters
#   if (!is.null(by)) by <- clean_column_names(by)
#   if (!is.null(corr_variable)) corr_variable <- clean_column_names(corr_variable)

#   # Improved conv_to_num_check function
#   conv_to_num_check <- function(z, tolerance = 0.95) {
#     if (!is.character(z)) return(FALSE)
#     conversion_success <- suppressWarnings(!is.na(as.numeric(z)))
#     proportion_numeric <- mean(conversion_success, na.rm = TRUE)
#     return(proportion_numeric > tolerance)
#   }

#   # Function to conditionally convert character columns to numeric
#   convert_columns_to_numeric <- function(dt, tolerance = 0.95) {
#     for (col_name in names(dt)) {
#       column <- dt[[col_name]]
#       if (is.character(column) && conv_to_num_check(column, tolerance)) {
#         # Warning about potential NA values due to conversion failures
#         if (anyNA(as.numeric(column))) {
#           warning(sprintf("Conversion of column '%s' to numeric will introduce NA values for non-numeric data.", col_name))
#         }
#         dt[[col_name]] <- as.numeric(column)
#       }
#     }
#     return(dt)
#   }

#   # Validate 'by' parameter
#   if (!is.null(by) && all(by %in% names(dt))) {
#     dt <- dt[, .(data = list(.SD)), by = by]
#   } else {
#     dt <- list(data = list(dt))
#     by <- NULL # Ensure 'by' is NULL if not used
#   }
  
#   grouped_info_list <- vector("list", length(dt$data))
  
#   for (group_idx in seq_along(dt$data)) {
#     current_group <- dt$data[[group_idx]]
#     if (!is.null(by)) {
#         group_values <- dt[group_idx, ..by]
#         group_name_parts <- lapply(names(group_values), function(gv) {
#             gv_value <- group_values[[gv]]
#             if (is.factor(dt[[gv]])) {
#                 # Include the column name with the factor's label
#                 paste(gv, ":", as.character(gv_value))
#             } else {
#                 # Include the column name with its value for non-factors as well
#                 paste(gv, ":", as.character(gv_value))
#             }
#         })
#         group_name <- paste(group_name_parts, collapse = ", ")
#     } else {
#         group_name <- "All Data"
#     }
    
#     info_list <- list()
#     corr_exists <- !is.null(corr_variable) && corr_variable %in% names(current_group) &&
#                    any(class(current_group[[corr_variable]]) %in% c("numeric", "integer", "double", "logical"))

#     corr_key <- if (!is.null(corr_variable)) {
#       paste("CorrelationWith", corr_variable, sep = "_")
#     } else {
#       "CorrelationWith" # Fallback if corr_variable is NULL
#     }
    
#     # Calculate statistics for each column in the current group
#     for (col_name in names(current_group)) {
#       col_data <- current_group[[col_name]]
#       col_classes <- class(col_data)
#       col_label <- if (!is.null(attributes(col_data)$label)) {
#         attributes(col_data)$label
#       } else {
#         NA # NA if no label exists
#       }
#       is_numeric_or_logical <- any(col_classes %in% c("numeric", "integer", "double", "logical"))
      
#       col_mean <- if (is_numeric_or_logical) {
#         round(mean(col_data, na.rm = TRUE),rounding_digits)
#       } else {
#         NA # NA for non-numeric and non-logical columns
#       }
      
#       col_var <- if (is_numeric_or_logical) {
#         round(sd(col_data, na.rm = TRUE),rounding_digits)
#       } else {
#         NA # NA for non-numeric and non-logical columns
#       }
      
#       col_nobs <- length(na.omit(col_data))
      
#       col_corr <- NA
#       if (corr_exists && is_numeric_or_logical && col_name != corr_variable) {
#         col_corr <- tryCatch({
#           round(cor(current_group[[col_name]], current_group[[corr_variable]], use = "complete.obs"), 2)
#         }, error = function(e) { NA }) # Set correlation to NA if error occurs
#       }
      
#       variable_type <- if (is_numeric_or_logical) {
#         if (all(col_data >= 0, na.rm = TRUE)) {
#           if (all(col_data == floor(col_data), na.rm = TRUE)) {
#             "discrete_non_neg"
#           } else {
#             "continuous_non_neg"
#           }
#         } else {
#           if (all(col_data == floor(col_data), na.rm = TRUE)) {
#             "discrete"
#           } else {
#             "continuous"
#           }
#         }
#       } else {
#         "not_numeric_or_logical"
#       }
      
#       dist_type <- NA
#       if (is_numeric_or_logical && !is.na(col_var) && !is.na(col_mean)) {
#         if (col_var > col_mean) {
#           dist_type <- "Possibly Overdispersed"
#         } else {
#           dist_type <- "Possibly Poisson or Normal"
#         }
#       }
      
#       col_max <- if (is_numeric_or_logical && any(!is.na(col_data))) {
#         round(max(col_data, na.rm = TRUE),rounding_digits)
#       } else {
#         NA # Return NA if column has only NAs or is not numeric/logical
#       }
      
#       col_min <- if (is_numeric_or_logical && any(!is.na(col_data))) {
#         round(min(col_data, na.rm = TRUE),rounding_digits)
#       } else {
#         NA # Return NA if column has only NAs or is not numeric/logical
#       }

#       col_non_zeros <- if (is_numeric_or_logical) {
#         sum(col_data != 0, na.rm = TRUE) # Count of non-zeros, ignoring NAs
#       } else {
#         NA # NA for non-numeric and non-logical columns
#       }

#       # Append the 'Grouped_by' information and rest of column info
#       info_list[[col_name]] <- list(
#         Grouped_by = group_name,
#         Label = as.character(col_label),
#         Mean = as.numeric(col_mean),
#         StandardDev = as.numeric(col_var),
#         Max = as.numeric(col_max),
#         Min = as.numeric(col_min),
#         Observations = as.integer(col_nobs),
#         NonZeroCount = as.integer(col_non_zeros),        
#         VariableType = as.character(variable_type),
#         DistributionType = as.character(dist_type),
#         Class = as.character(paste(col_classes, collapse = ", ")),
#         Correlation = as.numeric(col_corr)
#       )
#     }
    
#     # Combine the information for all columns within the current group
#     grouped_info_list[[group_idx]] <- rbindlist(info_list, idcol = "ColumnName")
#   }
  
#   # Combine the information for all groups and return
#   info_dt <- rbindlist(grouped_info_list, use.names = TRUE)

#   # Dynamic columns
#   setnames(info_dt, old="Correlation", new=corr_key)

#   return(info_dt)
# }
