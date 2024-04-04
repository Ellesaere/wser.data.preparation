library(data.table)

#' Column Information with Grouping
#'
#' This function returns a data.table with information about each column in a provided data table,
#' optionally grouped by one or two variables.
#' @param dt A data.table or data frame.
#' @param corr_variable An optional string specifying a column name to calculate correlations with.
#' @param by An optional vector of one or two column names to group the data by.
#' @return A data.table with information on each column, optionally grouped.
column_info <- function(dt, corr_variable = NULL, by = NULL) {
  # Convert to data.table if not already one
  if (!inherits(dt, "data.table")) {
    dt <- as.data.table(dt)
  }
  
  # Validate 'by' parameter
  if (!is.null(by) && all(by %in% names(dt))) {
    dt <- dt[, .(data = list(.SD)), by = by]
  } else {
    dt <- list(data = list(dt))
    by <- NULL # Ensure 'by' is NULL if not used
  }
  
  grouped_info_list <- vector("list", length(dt$data))
  
  for (group_idx in seq_along(dt$data)) {
    current_group <- dt$data[[group_idx]]
    if (!is.null(by)) {
        group_values <- dt[group_idx, ..by]
        group_name_parts <- lapply(names(group_values), function(gv) {
            gv_value <- group_values[[gv]]
            if (is.factor(dt[[gv]])) {
                # Include the column name with the factor's label
                paste(gv, ":", as.character(gv_value))
            } else {
                # Include the column name with its value for non-factors as well
                paste(gv, ":", as.character(gv_value))
            }
        })
        group_name <- paste(group_name_parts, collapse = ", ")
    } else {
        group_name <- "All Data"
    }
    
    info_list <- list()
    corr_exists <- !is.null(corr_variable) && corr_variable %in% names(current_group) &&
                   any(class(current_group[[corr_variable]]) %in% c("numeric", "integer", "double", "logical"))

    corr_key <- if (!is.null(corr_variable)) {
      paste("CorrelationWith", corr_variable, sep = "_")
    } else {
      "CorrelationWith" # Fallback if corr_variable is NULL
    }
    
    # Calculate statistics for each column in the current group
    for (col_name in names(current_group)) {
      col_data <- current_group[[col_name]]
      col_classes <- class(col_data)
      col_label <- if (!is.null(attributes(col_data)$label)) {
        attributes(col_data)$label
      } else {
        NA # NA if no label exists
      }
      is_numeric_or_logical <- any(col_classes %in% c("numeric", "integer", "double", "logical"))
      
      col_mean <- if (is_numeric_or_logical) {
        round(mean(col_data, na.rm = TRUE),2)
      } else {
        NA # NA for non-numeric and non-logical columns
      }
      
      col_var <- if (is_numeric_or_logical) {
        round(sd(col_data, na.rm = TRUE),2)
      } else {
        NA # NA for non-numeric and non-logical columns
      }
      
      col_nobs <- length(na.omit(col_data))
      
      col_corr <- NA
      if (corr_exists && is_numeric_or_logical && col_name != corr_variable) {
        col_corr <- tryCatch({
          round(cor(current_group[[col_name]], current_group[[corr_variable]], use = "complete.obs"), 2)
        }, error = function(e) { NA }) # Set correlation to NA if error occurs
      }
      
      variable_type <- if (is_numeric_or_logical) {
        if (all(col_data >= 0, na.rm = TRUE)) {
          if (all(col_data == floor(col_data), na.rm = TRUE)) {
            "discrete_non_neg"
          } else {
            "continuous_non_neg"
          }
        } else {
          if (all(col_data == floor(col_data), na.rm = TRUE)) {
            "discrete"
          } else {
            "continuous"
          }
        }
      } else {
        "not_numeric_or_logical"
      }
      
      dist_type <- NA
      if (is_numeric_or_logical && !is.na(col_var) && !is.na(col_mean)) {
        if (col_var > col_mean) {
          dist_type <- "Possibly Overdispersed"
        } else {
          dist_type <- "Possibly Poisson or Normal"
        }
      }
      
      col_max <- if (is_numeric_or_logical && any(!is.na(col_data))) {
        round(max(col_data, na.rm = TRUE),2)
      } else {
        NA # Return NA if column has only NAs or is not numeric/logical
      }
      
      col_min <- if (is_numeric_or_logical && any(!is.na(col_data))) {
        round(min(col_data, na.rm = TRUE),2)
      } else {
        NA # Return NA if column has only NAs or is not numeric/logical
      }

      # Append the 'Grouped_by' information and rest of column info
      info_list[[col_name]] <- list(
        Grouped_by = group_name,
        Label = as.character(col_label),
        Mean = as.numeric(col_mean),
        StandardDev = as.numeric(col_var),
        Max = as.numeric(col_max),
        Min = as.numeric(col_min),
        Observations = as.integer(col_nobs),
        VariableType = as.character(variable_type),
        DistributionType = as.character(dist_type),
        Class = as.character(paste(col_classes, collapse = ", ")),
        Correlation = as.numeric(col_corr)
      )
    }
    
    # Combine the information for all columns within the current group
    grouped_info_list[[group_idx]] <- rbindlist(info_list, idcol = "ColumnName")
  }
  
  # Combine the information for all groups and return
  info_dt <- rbindlist(grouped_info_list, use.names = TRUE)
  return(info_dt)
}
