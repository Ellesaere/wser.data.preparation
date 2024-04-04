library(data.table)

#' Clean Column Names
#'
#' Removes spaces and special characters from column names, leaving only alphanumeric characters and underscores.
#' This function is particularly useful for preprocessing data tables or data frames before analysis,
#' ensuring that column names are syntactically valid variable names in R and consistent for further operations.
#'
#' @param names A character vector of column names to be cleaned.
#' @return A character vector with cleaned column names, where spaces and special characters have been removed.
#' @examples
#' \dontrun{
#'   library(data.table)
#'   dt <- data.table(`Column 1` = 1:5, `Another Column!` = letters[1:5])
#'   setnames(dt, old = names(dt), new = clean_column_names(names(dt)))
#'   # Now dt has column names 'Column1' and 'AnotherColumn'
#' }
#' @export
clean_column_names <- function(names) {
  gsub("[[:space:]]|[^[:alnum:]_]", "", names)
}
