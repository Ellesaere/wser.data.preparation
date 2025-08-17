#' Read Multiple Data Files from a Directory into a Named List
#'
#' This function reads all files in a specified directory that match the provided file extensions
#' and loads them into R as a named list. Each list element is named after the file (without extension),
#' and optionally includes sheet names for Excel files. File and sheet names can be cleaned (symbols replaced).
#' 
#' Problematic columns (e.g., columns with missing values at the top that confuse type guessing) can be
#' explicitly coerced to a desired type.
#'
#' @name read_files_from_path
#' @param path Character. The path to the folder containing the files.
#' @param extensions Character vector. File extensions to include (e.g., c("xlsx", "csv")).
#' @param add_all_tabs Logical. If TRUE, reads all sheets from Excel files and names list entries using file + sheet name.
#' @param clean Logical. If TRUE, cleans names: replaces & with _and_ and all other symbols with _.
#' @param problematic_cols Character vector. Names of columns to force into a type.
#' @param force_type Character vector of same length as `problematic_cols`. Supported: "character", "numeric", "date".
#'
#' @return A named list of data frames or tibbles.
#'
#' @examples
#' \dontrun{
#' data_list <- read_files_from_path(
#'   "data/", 
#'   extensions = c("csv", "xlsx"), 
#'   add_all_tabs = TRUE, 
#'   clean = TRUE,
#'   problematic_cols = c("rav_code", "LBV_ONTVANGSTDATUM"),
#'   force_type = c("character", "date")
#' )
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tools file_path_sans_ext file_ext
#' @export
#' 
read_files_from_path <- function(path, 
                                 extensions = c("xlsx", "csv"), 
                                 add_all_tabs = FALSE, 
                                 clean = FALSE,
                                 problematic_cols = NULL,
                                 force_type = NULL) {
  path <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  if (!grepl("[/\\\\]$", path)) 
    path <- paste0(path, "\\")
  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")", collapse = "")
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  # internal helper for name cleaning
  clean_name <- function(name) {
    name <- gsub("&", "_and_", name)
    name <- gsub("[^[:alnum:]]+", "_", name)
    name <- gsub("_+", "_", name)
    name <- gsub("^_|_$", "", name)
    return(name)
  }

  # internal helper for forcing column types
  force_columns <- function(df, problematic_cols, force_type) {
    stopifnot(length(problematic_cols) == length(force_type))
    for (i in seq_along(problematic_cols)) {
      col <- problematic_cols[i]
      typ <- force_type[i]
      if (col %in% names(df)) {
        if (typ == "character") {
          df[[col]] <- as.character(df[[col]])
        } else if (typ == "numeric") {
          df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
        } else if (typ == "date") {
          # If numeric (Excel serials), convert with origin
          if (is.numeric(df[[col]])) {
            df[[col]] <- as.Date(df[[col]], origin = "1899-12-30")
          } else {
            # Try common string formats
            suppressWarnings({
              df[[col]] <- as.Date(df[[col]], format = "%Y-%m-%d")
              if (all(is.na(df[[col]]))) {
                df[[col]] <- as.Date(df[[col]], format = "%d-%m-%Y")
              }
            })
          }
        } else {
          warning(sprintf("Unknown force_type '%s' for column '%s' — skipped.", typ, col))
        }
      }
    }
    return(df)
  }

  data_list <- list()

  for (file in files) {
    ext <- tolower(tools::file_ext(file))
    file_base <- tools::file_path_sans_ext(basename(file))

    if (ext == "csv") {
      name <- if (clean) clean_name(file_base) else file_base
      df <- read.csv(file, stringsAsFactors = FALSE)
      if (!is.null(problematic_cols)) {
        df <- force_columns(df, problematic_cols, force_type)
      }
      data_list[[name]] <- df

    } else if (ext %in% c("xlsx", "xls")) {
      if (add_all_tabs) {
        sheets <- readxl::excel_sheets(file)
        for (sheet in sheets) {
          name <- if (clean) clean_name(paste(file_base, sheet, sep = "_")) else paste(file_base, sheet, sep = "_")
          df <- readxl::read_excel(file, sheet = sheet, guess_max = Inf)
          if (!is.null(problematic_cols)) {
            df <- force_columns(df, problematic_cols, force_type)
          }
          data_list[[name]] <- df
        }
      } else {
        name <- if (clean) clean_name(file_base) else file_base
        df <- readxl::read_excel(file, guess_max = Inf)
        if (!is.null(problematic_cols)) {
          df <- force_columns(df, problematic_cols, force_type)
        }
        data_list[[name]] <- df
      }

    } else {
      stop(paste("Unsupported file type:", ext))
    }
  }
  return(data_list)
}
