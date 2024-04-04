library(data.table)

#' Clean and Prepare Data for Analysis
#'
#' This function converts the input data frame to a data.table (if not already), checks for unsupported data types (complex or raw),
#' and converts logical or character columns to factors. It stops with a comprehensive error message if unsupported data types are found
#' or if "AsIs" objects with logical or character mode are detected. It aims to prepare data for analysis by ensuring that it is in a consistent format.
#'
#' @param dataframe A data.frame or data.table intended for analysis.
#' @return A data.table where logical and character columns have been converted to factors, and all data is in a consistent format for analysis.
#' @export
#' @examples
#' \dontrun{
#'   # Assuming `data.table` is already loaded
#'   my_data <- data.table(a = 1:10, b = rnorm(10), c = LETTERS[1:10], d = I(matrix(1:4, 2)))
#'   clean_data <- cleanAndPrepareData(my_data)
#'   print(clean_data)
#' }
clean_problematic_variables <- function(dataframe) {
  requireNamespace("data.table", quietly = TRUE)
  setDT(dataframe)
  
  # Get mode and class of all columns
  var_mode <- sapply(dataframe, mode)
  var_class <- sapply(dataframe, class)
  
  # Initialize an error message container
  errors <- c()
  
  # Check for unsupported data types
  if (any(var_mode %in% c("complex", "raw"))) {
    errors <- c(errors, "Complex or raw data types are not allowed.")
  }
  
  # Check for "AsIs" objects with unsupported modes
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    errors <- c(errors, "Matrix variables with 'AsIs' class must be 'numeric'.")
  }
  
  # If errors were found, stop and report all at once
  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"))
  }
  
  # Identify and convert logical / character columns to factors
  ind1 <- which(var_mode %in% c("logical", "character"))
  if (length(ind1)) {
    dataframe[, (ind1) := lapply(.SD, as.factor), .SDcols = ind1]
  }
  
  return(dataframe)
}
