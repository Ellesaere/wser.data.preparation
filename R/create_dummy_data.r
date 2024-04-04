library(data.table)

#' Create Dummy Data from Original Data with Configurable Noise Percentage
#'
#' This function iterates over each numeric column of a given data.table or data frame,
#' adding noise expressed as a percentage of each value to generate dummy data. The process 
#' maintains the original sign of non-zero numeric values and ensures that zeros and NAs remain 
#' unchanged. Non-numeric columns are left as-is, without any alteration. Please make sure 
#' that they do not contain sensitive information. If the dataset includes sensitive 
#' information in non-numeric columns, it is the user's responsibility to anonymize or remove 
#' these columns. The aim is to facilitate the creation of a non-sensitive, dummy dataset that 
#' retains the statistical properties of the original data for analysis or sharing purposes, 
#' without compromising the confidentiality of the original information.
#'
#' @param data A data.table or data frame containing the original data.
#' @param percent The maximum percentage of the original value by which to adjust. 
#' Default is 10 (for 10%).
#' @return A data.table with dummy data for numeric columns.
#' @examples
#' \dontrun{
#'   # Assuming `data.table` is already loaded
#'   original_data <- data.table(a = 1:10, b = rnorm(10), c = LETTERS[1:10])
#'   dummy_data <- create_dummy_data(original_data, percent = 5)
#'   print(dummy_data)
#' }
#' @export
create_dummy_data <- function(data, percent = 10) {
  if (!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }

  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    if (is.numeric(col_data)) {
      # Calculate maximum allowable noise based on percent parameter
      max_noise <- abs(col_data) * (percent / 100)
      
      # Generate noise within the range [-max_noise, max_noise]
      noise <- runif(n = .N, min = -max_noise, max = max_noise)
      
      # Apply noise, ensuring NAs and zeros are handled correctly
      adjusted_col_data <- col_data + ifelse(is.na(col_data) | col_data == 0, 0, noise)
      
      # Update the column with adjusted values
      data[, (col_name) := adjusted_col_data]
    } else {
      cat(sprintf("Column %s is non-numeric and will be left unchanged.\n", col_name))
    }
  }
  
  return(data)
}
