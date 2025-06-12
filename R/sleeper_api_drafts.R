# Sleeper API Endpoints for Drafts

#' @title Get all drafts for a specific user and season
#' @description Retrieves all NFL drafts that a user has participated in during a specified season
#' @param user_id Character. The unique identifier for the user
#' @param season Character or numeric. The NFL season year (e.g., "2023" or 2023)
#' @return A list containing draft data for the specified user and season
#' @examples
#' \dontrun{
#' user_drafts <- get_all_user_drafts("123456789", "2023")
#' }
#' @export
get_all_user_drafts <- function(user_id, season) {
  # Fetch all user drafts from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/user/{user_id}/drafts/nfl/{season}")

  # Log the URL being fetched
  log_info(glue::glue("Fetching user drafts data from {season} season."))
  log_info(glue::glue("User draft data endpoint: {url}"))

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_info("Extracting content from user draft data request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_info("Converting user draft data R objects to JSON.")
  json <- jsonlite::fromJSON(content)

  log_info("Returning traded draft picks as tibble.")
  return(tibble::as_tibble(json))
}

#' @title Get all drafts for a specific league
#' @description Retrieves all drafts associated with a league.
#'   Note that a league can have multiple drafts (e.g., dynasty leagues).
#'   Drafts are sorted from most recent to earliest.
#' @param league_id Character. The unique identifier for the league
#' @return A list containing all draft data for the specified league
#' @examples
#' \dontrun{
#' league_drafts <- get_all_league_drafts("987654321")
#' }
#' @export
get_all_league_drafts <- function(league_id) {
  # Fetch all league drafts from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/drafts")

  # Log the URL being fetched
  log_info(glue::glue("Fetching all league draft data from endpoint: {url}"))

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_info("Extracting content from all league draft data request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_info("Converting all league draft data R objects to JSON.")
  json <- jsonlite::fromJSON(content)

  log_info("Returning traded draft picks as tibble.")
  return(tibble::as_tibble(json))
}

#' @title Get data for a specific draft
#' @description Retrieves metadata and information about a specific draft
#' @param draft_id Character. The unique identifier for the draft
#' @return A list containing data for the specified draft
#' @examples
#' \dontrun{
#' draft_data <- get_specific_draft("123456789")
#' }
#' @export
get_specific_draft <- function(draft_id) {
  # Fetch specific draft from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/draft/{draft_id}")

  # Log the URL being fetched
  log_info(glue::glue("Fetching draft data from endpoint: {url}"))

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_info("Extracting content from draft data request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_info("Converting draft data R objects to JSON.")
  return(jsonlite::fromJSON(content))
}

#' @title Get all draft picks for a specific draft
#' @description Retrieves all picks made in a specific draft
#' @param draft_id Character. The unique identifier for the draft
#' @return A list containing all picks made in the specified draft
#' @examples
#' \dontrun{
#' draft_picks <- get_all_draft_picks("123456789")
#' }
#' @export
get_all_draft_picks <- function(draft_id) {
  # Fetch specific draft from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/draft/{draft_id}/picks")

  # Log the URL being fetched
  log_info(glue::glue("Fetching all draft picks from endpoint: {url}"))

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_info("Extracting content from all draft picks request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_info("Converting all draft picks R objects to JSON.")
  json <- jsonlite::fromJSON(content)

  log_info("Returning traded draft picks as tibble.")
  return(tibble::as_tibble(json))
}

#' @title Get traded draft picks for a specific draft
#' @description Retrieves all draft picks that were traded during a specific draft
#' @param draft_id Character. The unique identifier for the draft
#' @return A list containing all traded picks from the specified draft
#' @examples
#' \dontrun{
#' traded_picks <- get_traded_draft_picks("123456789")
#' }
#' @export
get_traded_draft_picks <- function(draft_id) {
  # Fetch specific draft from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/draft/{draft_id}/traded_picks")

  # Log the URL being fetched
  log_info(glue::glue("Fetching traded draft picks from endpoint: {url}"))

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_info("Extracting content from traded draft picks request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_info("Converting traded draft picks R objects to JSON.")
  json <- jsonlite::fromJSON(content)

  log_info("Returning traded draft picks as tibble.")
  log_info("Returning traded draft picks as tibble.")
  return(tibble::as_tibble(json))
}
