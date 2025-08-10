#' Create Dummy/Scrambled Data
#'
#' This function takes an input data frame or data.table and produces a scrambled
#' (dummy) version for anonymization or testing purposes.
#'
#' - Numeric/integer values are randomly varied by a percentage range.
#' - Character/factor values are randomly re-assigned from existing unique values.
#' - Date/time values are randomly shifted by a specified range of days.
#' - ID columns can be replaced with unique synthetic IDs.
#' - Certain columns can be explicitly excluded from modification.
#' - If no scramble list is provided, all non-ID, non-excluded columns are scrambled.
#'
#' @param dt Data frame or data.table to scramble.
#' @param columns_to_scramble Character vector of columns to scramble.
#'   If empty, all non-ID and non-excluded columns will be scrambled.
#' @param id_columns Character vector of columns to treat as IDs and replace with fake IDs.
#' @param exclude_columns Character vector of columns to protect from modification.
#' @param min_var Minimum percentage variation for numeric/integer scrambling (e.g., 0.10 = 10\%).
#' @param max_var Maximum percentage variation for numeric/integer scrambling (e.g., 0.25 = 25\%).
#' @param days_min Minimum number of days to shift for date/datetime scrambling.
#' @param days_max Maximum number of days to shift for date/datetime scrambling.
#'
#' @return A new data.table with scrambled/dummy data.
#'
#' @examples
#' dt <- data.table(
#'   id = 1:5,
#'   name = letters[1:5],
#'   amount = c(100, 200, 300, 400, 500),
#'   date = as.Date("2023-01-01") + 0:4
#' )
#' create_dummy_data(dt, id_columns = "id", exclude_columns = "name")
#'
create_dummy_data <- function(dt, 
                              columns_to_scramble = character(0), 
                              id_columns = character(0),
                              exclude_columns = character(0),
                              min_var = 0.10,
                              max_var = 0.25,
                              days_min = 1,
                              days_max = 7) {
    # Make a copy to avoid modifying the original data
    dt_dummy <- copy(as.data.table(dt))
    
    # Remove excluded columns from both ID and scramble lists
    if (length(exclude_columns) > 0) {
        id_columns <- setdiff(id_columns, exclude_columns)
        columns_to_scramble <- setdiff(columns_to_scramble, exclude_columns)
    }
    
    # If no scramble list provided → scramble all non-ID, non-excluded columns
    if (length(columns_to_scramble) == 0) {
        columns_to_scramble <- setdiff(names(dt_dummy), c(id_columns, exclude_columns))
    }
    
    # Helper: generate a unique random ID string
    generate_random_id <- function(existing_ids) {
        repeat {
            # Format: 2 letters + 2 digits, e.g., "AB42"
            letters2 <- paste(sample(LETTERS, 2, replace = TRUE), collapse = "")
            digits2  <- sprintf("%02d", sample(10:99, 1))
            new_id <- paste0(letters2, digits2)
            if (!(new_id %in% existing_ids)) return(new_id)
        }
    }
    
    # Gather existing IDs for uniqueness tracking
    existing_ids <- character(0)
    if (length(id_columns) > 0) {
        existing_ids <- unique(unlist(lapply(id_columns, function(col) {
            if (col %in% names(dt_dummy)) as.character(dt_dummy[[col]]) else NULL
        })))
    }
    
    n <- nrow(dt_dummy)
    
    # Generate new fake IDs for ID columns
    if (length(id_columns) > 0) {
        for (col in id_columns) {
            if (col %in% names(dt_dummy)) {
                new_vals <- character(n)
                for (i in seq_len(n)) {
                    id <- generate_random_id(existing_ids)
                    new_vals[i] <- id
                    existing_ids <- c(existing_ids, id) # track all used IDs
                }
                dt_dummy[[col]] <- new_vals
            }
        }
    }
    
    # Scramble the requested columns
    if (length(columns_to_scramble) > 0) {
        for (col in columns_to_scramble) {
            # Skip if the column is missing or is an ID column
            if (!(col %in% names(dt_dummy)) || (col %in% id_columns)) next
            
            x <- dt_dummy[[col]]
            
            # --- Character or factor columns ---
            if (is.character(x) || is.factor(x)) {
                uniq <- unique(x[!is.na(x)])
                if (length(uniq) > 0) {
                    dt_dummy[[col]] <- sample(uniq, n, replace = TRUE)
                }
            
            # --- Date or datetime columns ---
            } else if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
                abs_shift <- sample(days_min:days_max, n, replace = TRUE)
                signs <- sample(c(-1L, 1L), n, replace = TRUE)
                delta_days <- abs_shift * signs
                if (inherits(x, "Date")) {
                    dt_dummy[[col]] <- x + delta_days
                } else {
                    dt_dummy[[col]] <- x + delta_days * 86400 # shift in seconds
                }
            
            # --- Integer columns ---
            } else if (is.integer(x)) {
                r <- runif(n, min_var, max_var)       # random variation
                s <- sample(c(-1, 1), n, replace = TRUE) # random sign
                factor <- 1 + s * r
                new_values <- as.integer(round(as.numeric(x) * factor))
                
                # Mask for zero replacement, excluding NA values
                zero_mask <- (new_values == 0) & !is.na(new_values) & !is.na(x)
                
                if (any(zero_mask)) {
                    adj <- sign(x[zero_mask])
                    adj[is.na(adj) | adj == 0] <- 1L
                    new_values[zero_mask] <- x[zero_mask] + adj
                }
                dt_dummy[[col]] <- new_values
            
            # --- Numeric (floating point) columns ---
            } else if (is.numeric(x)) {
                r <- runif(n, min_var, max_var)
                s <- sample(c(-1, 1), n, replace = TRUE)
                factor <- 1 + s * r
                new_values <- x * factor
                
                zero_mask <- (new_values == 0) & !is.na(new_values) & !is.na(x)
                
                if (any(zero_mask)) {
                    eps <- .Machine$double.eps
                    new_values[zero_mask] <- x[zero_mask] + eps
                }
                dt_dummy[[col]] <- new_values
            }
        }
    }
    
    return(dt_dummy)
}
