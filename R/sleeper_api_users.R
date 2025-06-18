# Sleeper API Endpoints for Users

#' Retrieve User Information from Sleeper API
#'
#' @description
#' Fetches user data from the Sleeper API using either a username or user ID.
#' This function returns the full user object containing profile information.
#'
#' @param username Character string. The Sleeper username to look up. Default is NULL.
#' @param user_id Character string. The Sleeper user ID to look up. Default is NULL.
#'
#' @return A list containing user information returned by the Sleeper API.
#'
#' @details
#' Exactly one of username or user_id must be provided. The function will make
#' an HTTP GET request to the appropriate Sleeper API endpoint and return the
#' parsed JSON response.
#'
#' @examples
#' \dontrun{
#' # Get user by username
#' user_info <- get_user(username = "sleeper_username")
#'
#' # Get user by user_id
#' user_info <- get_user(user_id = "12345678")
#' }
#'
#' @export
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
get_user <- function(username = NULL, user_id = NULL) {
  # Validate that exactly one of username or user_id is provided
  if (is.null(username) && is.null(user_id)) {
    stop("You must provide either a 'username' or a 'user_id'.")
  }
  if (!is.null(username) && !is.null(user_id)) {
    stop("Please provide only one of 'username' or 'user_id', not both.")
  }

  # Construct the appropriate URL
  if (!is.null(username)) {
    url <- glue::glue("https://api.sleeper.app/v1/user/{username}")
  } else {
    url <- glue::glue("https://api.sleeper.app/v1/user/{user_id}")
  }

  # Log the URL being fetched
  log_info(glue::glue("Fetching user info from endpoint: {url}"))

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_info("Extracting content from user request.")
  content <- httr::content(response, "text")

  # Convert and return JSON
  log_info("Converting user info response to R object.")
  json <- jsonlite::fromJSON(content)

  # Convert and return tibble
  user_tbl <- tibble::as_tibble(as.data.frame(t(json), stringsAsFactors = FALSE))
  return(user_tbl)
}
