#' Read Multiple Data Files from a Directory into a Named List
#'
#' This function reads all files in a specified directory that match the provided file extensions
#' and loads them into R as a named list. Each list element is named after the file (without extension),
#' and optionally includes sheet names for Excel files. File and sheet names can be cleaned (spaces replaced with underscores).
#'
#' @name read_files_from_path
#' @param path Character. The path to the folder containing the files.
#' @param extensions Character vector. File extensions to include (e.g., c("xlsx", "csv")).
#' @param add_all_tabs Logical. If TRUE, reads all sheets from Excel files and names list entries using file + sheet name.
#' @param clean Logical. If TRUE, replaces spaces in names with underscores.
#'
#' @return A named list of data frames or tibbles.
#'
#' @examples
#' \dontrun{
#' data_list <- read_files_from_path("data/", extensions = c("csv", "xlsx"), add_all_tabs = TRUE, clean = TRUE)
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tools file_path_sans_ext file_ext
#' @export
read_files_from_path <- function(path, extensions = c("xlsx", "csv"), add_all_tabs = FALSE, clean = FALSE) {
  # Normalize path
  path <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  if (!grepl("[/\\\\]$", path)) path <- paste0(path, "\\")

  # Create pattern and list files
  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$", collapse = "")
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  data_list <- list()

  for (file in files) {
    ext <- tolower(file_ext(file))
    file_base <- file_path_sans_ext(basename(file))

    if (ext == "csv") {
      name <- file_base
      if (clean) name <- gsub(" ", "_", name)
      data_list[[name]] <- read.csv(file, stringsAsFactors = FALSE)

    } else if (ext %in% c("xlsx", "xls")) {
      if (add_all_tabs) {
        sheets <- excel_sheets(file)
        for (sheet in sheets) {
          name <- paste(file_base, sheet, sep = "_")
          if (clean) name <- gsub(" ", "_", name)
          data_list[[name]] <- read_excel(file, sheet = sheet)
        }
      } else {
        name <- file_base
        if (clean) name <- gsub(" ", "_", name)
        data_list[[name]] <- read_excel(file)
      }

    } else {
      stop(paste("Unsupported file type:", ext))
    }
  }

  return(data_list)
}
