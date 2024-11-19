library(data.table)

#' Check if Character Vector is Mostly Numeric
#'
#' Determines whether a character vector can predominantly be converted to numeric based on a tolerance threshold.
#' This is useful for preprocessing and cleaning data, ensuring that character vectors
#' that represent numeric values are correctly identified and potentially converted to numeric type.
#'
#' @param z A character vector to be evaluated for numeric convertibility.
#' @param tolerance A numeric value between 0 and 1 indicating the proportion of elements in `z`
#'        that need to be convertible to numeric for the vector to be considered numeric.
#'        Defaults to 0.95.
#' @return A logical value; `TRUE` if the proportion of numeric-convertible elements in `z`
#'         meets or exceeds the `tolerance` threshold, otherwise `FALSE`.
#' @examples
#' \dontrun{
#'   vec <- c("123", "456", "abc", "789")
#'   conv_to_num_check(vec) # Expected to return FALSE
#'   vec <- c("123", "456", "789", "1000.5")
#'   conv_to_num_check(vec) # Expected to return TRUE
#' }
#' @export

conv_to_num_check <- function(z, tolerance = 0.95) {
  if (!is.character(z)) return(FALSE)
  
  # Suppress warnings and check for numeric-convertible values, treating NA as numeric-like
  conversion_success <- suppressWarnings(!is.na(as.numeric(z)) | is.na(z))
  
  # Calculate the proportion of numeric-like values
  proportion_numeric <- mean(conversion_success, na.rm = TRUE)
  
  return(proportion_numeric >= tolerance)
}


#' Convert Character Columns to Numeric Where Applicable
#'
#' Iterates over the columns of a data.table or data frame, converting those character columns to numeric
#' which are predominantly numeric according to a specified tolerance level. Columns that are not character
#' or do not meet the numeric content threshold are left unchanged.
#'
#' @param dt A data.table or data frame whose character columns are to be evaluated for conversion to numeric.
#' @param tolerance A numeric value between 0 and 1 indicating the minimum proportion of elements in a character
#'        column that need to be convertible to numeric for the column to be converted. Defaults to 0.95.
#' @return The original data.table or data frame with applicable character columns converted to numeric.
#' @examples
#' \dontrun{
#'   library(data.table)
#'   dt <- data.table(a = c("1", "2", "3"), b = c("a", "b", "c"), c = c("4.5", "5.6", "NA"))
#'   dt <- convert_columns_to_numeric(dt)
#'   # Columns 'a' and 'c' are converted to numeric, while 'b' remains as character
#' }
#' @export
#' @importFrom data.table set

# Function to conditionally convert character columns to numeric
convert_columns_to_numeric <- function(dt, tolerance = 0.95) {
  for (col_name in names(dt)) {
    column <- dt[[col_name]]
    if (is.character(column) && conv_to_num_check(column, tolerance)) {
      # Warning about potential NA values due to conversion failures
      if (anyNA(as.numeric(column))) {
        warning(sprintf("Conversion of column '%s' to numeric will introduce NA values for non-numeric data.", col_name))
      }
      dt[[col_name]] <- as.numeric(column)
    }
  }
  return(dt)
}
