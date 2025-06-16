# Utility functions for Sleeper API package

#' Check status of Sleeper API response
#'
#' @param response httr response object
#' @return NULL if successful, stops execution if error
#' @keywords internal
status_sleeper_api <- function(response) {
  if (httr::http_error(response)) {
    status_code <- httr::status_code(response)
    error_message <- httr::http_status(response)$message
    log_error("Sleeper API request failed with status {status_code}. Error: {error_message}")
  }

  status_code <- httr::status_code(response)
  if (status_code != 200) {
    log_warn("Sleeper API returned status {status_code}")
  } else {
    log_debug("Sleeper API request successful (status 200)")
  }

  invisible(NULL)
}

# Timestamp Utility Functions for Sleeper API Data

#' Convert Sleeper API Timestamp to Readable Format
#'
#' Converts Sleeper API millisecond timestamps to formatted datetime strings
#' in the America/New_York timezone.
#'
#' @param ms Numeric. Timestamp in milliseconds since Unix epoch, as returned
#'   by Sleeper API endpoints.
#'
#' @return Character string. Formatted datetime in "YYYY-MM-DD HH:MM:SS TZ" format
#'   using America/New_York timezone. Returns NA if input is NULL or invalid.
#'
#' @details
#' Sleeper API returns timestamps as milliseconds since Unix epoch (1970-01-01).
#' This function converts them to human-readable format in Eastern Time, which
#' is commonly used for fantasy football due to NFL scheduling.
#'
#' The function handles NULL and invalid inputs gracefully by returning NA,
#' making it safe to use in data processing pipelines.
#'
#' @examples
#' \dontrun{
#' # Convert a Sleeper timestamp
#' sleeper_time <- 1609459200000 # 2021-01-01 00:00:00 UTC
#' convert_sleeper_timestamp(sleeper_time)
#' # Returns: "2020-12-31 19:00:00 EST"
#'
#' # Handle NULL input
#' convert_sleeper_timestamp(NULL)
#' # Returns: NA
#'
#' # Use in data processing
#' league_data <- league_data |>
#'   mutate(
#'     formatted_time = convert_sleeper_timestamp(last_message_time)
#'   )
#' }
#'
#' @export
convert_sleeper_timestamp <- function(ms) {
  # Handle NULL or invalid input
  if (is.null(ms) || !is.numeric(ms) || length(ms) != 1) {
    return(NA_character_)
  }

  # Handle zero or negative timestamps
  if (ms <= 0) {
    return(NA_character_)
  }

  # Convert milliseconds to seconds and create POSIXct object
  # Use America/New_York timezone (Eastern Time) for fantasy football context
  datetime_obj <- as.POSIXct(
    ms / 1000,
    origin = "1970-01-01",
    tz = "America/New_York"
  )

  # Format as readable string with timezone abbreviation
  formatted_time <- format(
    datetime_obj,
    "%Y-%m-%d %H:%M:%S %Z"
  )

  return(formatted_time)
}

#' Convert Sleeper API Timestamp to POSIXct Object
#'
#' Converts Sleeper API millisecond timestamps to POSIXct objects for
#' further date/time manipulation and analysis.
#'
#' @param ms Numeric. Timestamp in milliseconds since Unix epoch.
#' @param tz Character string. Timezone for the conversion.
#'   Defaults to "America/New_York".
#'
#' @return POSIXct object. Datetime object in specified timezone.
#'   Returns NA if input is NULL or invalid.
#'
#' @details
#' This function is useful when you need to perform date/time arithmetic
#' or comparison operations on Sleeper timestamps. Unlike
#' `convert_sleeper_timestamp()`, this returns a POSIXct object rather
#' than a formatted string.
#'
#' @examples
#' \dontrun{
#' # Convert to POSIXct for analysis
#' sleeper_time <- 1609459200000
#' datetime_obj <- sleeper_timestamp_to_posixct(sleeper_time)
#'
#' # Perform date arithmetic
#' days_ago <- as.numeric(Sys.time() - datetime_obj, units = "days")
#'
#' # Convert to different timezone
#' utc_time <- sleeper_timestamp_to_posixct(sleeper_time, tz = "UTC")
#' }
#'
#' @export
sleeper_timestamp_to_posixct <- function(ms, tz = "America/New_York") {
  # Handle NULL or invalid input
  if (is.null(ms) || !is.numeric(ms) || length(ms) != 1) {
    return(as.POSIXct(NA))
  }

  # Handle zero or negative timestamps
  if (ms <= 0) {
    return(as.POSIXct(NA))
  }

  # Validate timezone
  if (!tz %in% OlsonNames()) {
    warning(glue::glue("Invalid timezone '{tz}', using 'America/New_York'"))
    tz <- "America/New_York"
  }

  # Convert milliseconds to seconds and create POSIXct object
  datetime_obj <- as.POSIXct(
    ms / 1000,
    origin = "1970-01-01",
    tz = tz
  )

  return(datetime_obj)
}

#' Vectorized Sleeper Timestamp Conversion
#'
#' Converts a vector of Sleeper API millisecond timestamps to formatted
#' datetime strings. Optimized for processing multiple timestamps efficiently.
#'
#' @param ms_vector Numeric vector. Timestamps in milliseconds since Unix epoch.
#' @param tz Character string. Timezone for conversion.
#'   Defaults to "America/New_York".
#' @param format Character string. Format string for datetime output.
#'   Defaults to "%Y-%m-%d %H:%M:%S %Z".
#'
#' @return Character vector. Formatted datetime strings. Invalid inputs
#'   return NA in corresponding positions.
#'
#' @details
#' This function is optimized for processing large datasets with many
#' timestamp values. It handles vectorized operations efficiently and
#' preserves the order and length of the input vector.
#'
#' @examples
#' \dontrun{
#' # Convert multiple timestamps
#' timestamps <- c(1609459200000, 1609545600000, 1609632000000)
#' convert_sleeper_timestamps_vectorized(timestamps)
#'
#' # Custom format
#' convert_sleeper_timestamps_vectorized(
#'   timestamps,
#'   format = "%m/%d/%Y %I:%M %p"
#' )
#'
#' # Use in data processing
#' data <- data |>
#'   mutate(
#'     formatted_times = convert_sleeper_timestamps_vectorized(timestamp_column)
#'   )
#' }
#'
#' @export
convert_sleeper_timestamps_vectorized <- function(ms_vector,
                                                  tz = "America/New_York",
                                                  format = "%Y-%m-%d %H:%M:%S %Z") {
  # Input validation
  if (!is.numeric(ms_vector)) {
    stop("'ms_vector' must be a numeric vector")
  }

  # Validate timezone
  if (!tz %in% OlsonNames()) {
    warning(glue::glue("Invalid timezone '{tz}', using 'America/New_York'"))
    tz <- "America/New_York"
  }

  # Create logical mask for valid timestamps
  valid_mask <- !is.na(ms_vector) & ms_vector > 0

  # Initialize result vector with NAs
  result <- rep(NA_character_, length(ms_vector))

  # Process only valid timestamps
  if (any(valid_mask)) {
    valid_timestamps <- ms_vector[valid_mask]

    # Convert to POSIXct objects
    datetime_objects <- as.POSIXct(
      valid_timestamps / 1000,
      origin = "1970-01-01",
      tz = tz
    )

    # Format to strings
    result[valid_mask] <- format(datetime_objects, format)
  }

  return(result)
}

#' Validate transaction type parameter
#'
#' Checks if the provided transaction type is valid for Sleeper API
#'
#' @param tx_type Character string representing transaction type or NULL.
#'   Valid values are "waiver", "free_agent", "trade".
#' @return The original tx_type if valid, or NULL if invalid
#' @export
#' @examples
#' \dontrun{
#' validate_tx_type("trade") # Returns "trade"
#' validate_tx_type("invalid") # Shows message, returns NULL
#' }
validate_tx_type <- function(tx_type) {
  log_debug("Starting validate_tx_type with input: {tx_type %||% 'NULL'}")

  # Input validation
  if (!rlang::is_null(tx_type)) {
    if (!is.character(tx_type) || length(tx_type) != 1) {
      log_error("tx_type must be a single character string or NULL")
    }
  }

  # Valid transaction types
  valid_tx_types <- c("waiver", "free_agent", "trade")

  # Create a validator function
  is_valid_tx_type <- function(x) {
    rlang::is_null(x) || (x %in% valid_tx_types)
  }

  # Check validation and return appropriate value
  if (is_valid_tx_type(tx_type)) {
    log_debug("Transaction type validation successful: {tx_type %||% 'NULL'}")
    return(tx_type)
  } else {
    valid_types_str <- paste(valid_tx_types, collapse = ", ")
    log_warn("Invalid transaction type: '{tx_type}'. Must be one of: {valid_types_str}")
    log_info("Returning NULL (for unfiltered transactions)")
    return(NULL)
  }
}
