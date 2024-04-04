library(data.table)

#' Move Columns in a Data Table
#'
#' This function rearranges the columns of a data.table based on specified criteria. 
#' It allows columns to be moved to the first or last position, or before or after a specific column.
#'
#' @param data A data.table whose columns are to be rearranged.
#' @param tomove A vector of column names or indices to move. This can be a mix of numeric indices and column names.
#' @param where A character string indicating where to move the specified columns. 
#'        Valid options are "first", "last", "before", and "after". The default is "first".
#' @param ba A column name (as a character string) indicating the base column relative to which the columns 
#'        specified in \code{tomove} should be moved. This parameter is required if \code{where} is "before" or "after".
#' @return A data.table with the columns rearranged according to the specified criteria.
#' @details This function modifies the column order of a data.table without altering the data within. 
#'          When using "before" or "after", ensure the \code{ba} parameter is correctly specified, or an error will be thrown.
#' @examples
#' \dontrun{
#'   library(data.table)
#'   dt <- data.table(A = 1:5, B = letters[1:5], C = rnorm(5))
#'   # Move column B to the first position
#'   moveMeDataTable(dt, tomove = "B", where = "first")
#'   # Move columns B and C to the last position
#'   moveMeDataTable(dt, tomove = c("B", "C"), where = "last")
#'   # Move column A before column C
#'   moveMeDataTable(dt, tomove = "A", where = "before", ba = "C")
#'   # Move column A after column B
#'   moveMeDataTable(dt, tomove = "A", where = "after", ba = "B")
#' }
#' @export
#' @importFrom data.table setcolorder
moveMeDataTable <-function(data, tomove, where = "first", ba = NULL) {
  suppressWarnings(nums <- as.numeric(tomove))
  nums[is.na(nums)] <- suppressWarnings(sapply(tomove[is.na(as.numeric(tomove))], 
                              function(x) which(names(data) == x)))
  nums <- unlist(nums, use.names=FALSE)
  tomove <- (names(data)[nums])  
  temp <- setdiff(names(data), tomove)
  tomove <- unique(tomove)

  x <- switch(
    where,
    first = setcolorder(data,c(tomove, temp)),
    last = setcolorder(data,c(temp, tomove)),
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)-1))
      setcolorder(data,order)
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)))
      setcolorder(data,order)
    })
  x
}
