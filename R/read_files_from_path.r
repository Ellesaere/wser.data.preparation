#' Read Multiple Data Files from a Directory into a Named List
#'
#' This function reads all files in a specified directory that match the provided file extensions
#' and loads them into R as a named list. Each list element is named after the file (without extension),
#' and the file is read using the appropriate method based on its extension.
#'
#' @param path Character. The path to the folder containing the files. Backslashes (`\\`) are supported.
#' @param extensions Character vector. A vector of file extensions (without dots) to read, such as `c("xlsx", "csv")`.
#'
#' @return A named list of data frames or tibbles, one for each file. The names correspond to the original filenames (without extensions).
#'
#' @details
#' Supported file extensions and their readers:
#' \itemize{
#'   \item \code{csv} - Read using \code{read.csv()}
#'   \item \code{xlsx}, \code{xls} - Read using \code{readxl::read_excel()}
#' }
#' Files with unsupported extensions will result in an error.
#'
#' @examples
#' \dontrun{
#' data_list <- read_files_from_path("W:/PROJECTS/MESN-WSER_2025/InitialData/", extensions = c("csv", "xlsx"))
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tools file_path_sans_ext file_ext
#' @export
#' 
library(readxl)
library(tools)
read_files_from_path <- function(path, extensions = c("xlsx", "csv")) {
  # Normalize the path
  path <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  if (!grepl("[/\\\\]$", path)) path <- paste0(path, "\\")

  # Create file pattern for matching
  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$", collapse = "")
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  # Read each file based on extension
  data_list <- lapply(files, function(file) {
    ext <- tolower(file_ext(file))
    switch(ext,
           "csv" = read.csv(file, stringsAsFactors = FALSE),
           "xlsx" = read_excel(file),
           "xls"  = read_excel(file),
           stop(paste("Unsupported file type:", ext))
    )
  })

  # Name the list using file names (without extensions)
  names(data_list) <- file_path_sans_ext(basename(files))

  return(data_list)
}
