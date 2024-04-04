#' Column Information
#'
#' This function returns a data.table with information about each column in a provided data table.
#' @param dt A data.table or data frame.
#' @param corr_variable An optional string specifying a column name to calculate correlations with.
#' @return A data.table with information on each column.

#' @examples
#' dt <- data.table(x = 1:10, y = rnorm(10))
#' column_info(dt)

#' @export
column_info_old <- function(dt, corr_variable = NULL) {
  # Convert to data.table if not already one
  if (!inherits(dt, "data.table")) {
    dt <- as.data.table(dt)
  }
  
  # Initialize a list to store column info
  info_list <- list()
  
  # Check if corr_variable is provided and exists in the data.table
  corr_exists <- !is.null(corr_variable) && corr_variable %in% names(dt) &&
                 any(class(dt[[corr_variable]]) %in% c("numeric", "integer", "double", "logical"))

  corr_key <- if (!is.null(corr_variable)) {
    paste("CorrelationWith", corr_variable, sep = "_")
  } else {
    "CorrelationWith" # Fallback if corr_variable is NULL
  }
  
  # Iterate over each column
  for (col_name in names(dt)) {
    col_data <- dt[[col_name]]
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
        round(cor(dt[[col_name]], dt[[corr_variable]], use = "complete.obs"), 2)
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

    # Create the list entry for this column
    column_info_list <- list(
      Label = as.character(col_label),
      Mean = as.numeric(col_mean),
      StandardDev = as.numeric(col_var),
      Max = as.numeric(col_max),
      Min = as.numeric(col_min),
      Observations = as.integer(col_nobs),
      VariableType = as.character(variable_type),
      DistributionType = as.character(dist_type),
      Class = as.character(paste(col_classes, collapse = ", "))
    )
    
    # Dynamically name the correlation key
    column_info_list[corr_key] <- as.numeric(col_corr)
    
    # Append the collected data to the info_list
    info_list[[col_name]] <- column_info_list
  }
  
  # Convert the list to a data.table for a nice format and return
  info_dt <- rbindlist(info_list, idcol = "ColumnName")
  
  return(info_dt)
}