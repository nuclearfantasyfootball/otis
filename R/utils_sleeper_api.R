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
