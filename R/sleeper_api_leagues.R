# Sleeper API Endpoints for Leagues
# Updated to use utils_logging_with_glue.R system

#' @importFrom httr GET content http_error status_code http_status
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr filter
#' @importFrom rlang is_null
#' @importFrom glue glue
NULL

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
#' validate_tx_type("trade")    # Returns "trade"
#' validate_tx_type("invalid")  # Shows message, returns NULL
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
    valid_types_str <- paste(valid_tx_types, collapse = ', ')
    log_warn("Invalid transaction type: '{tx_type}'. Must be one of: {valid_types_str}")
    log_info("Returning NULL (for unfiltered transactions)")
    return(NULL)
  }
}

#' Get specific league information
#'
#' Fetches information about a specific league from the Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @return A tibble containing league information
#' @export
#' @examples
#' \dontrun{
#' league_data <- get_specific_league("123456789")
#' }
get_specific_league <- function(league_id) {
  log_debug("Starting get_specific_league with league_id: {league_id}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    log_error("league_id must be a non-empty character string")
  }

  # Fetch specific league from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}")
  log_debug("Fetching specific league data from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  log_debug("Converting R objects to tibble")
  result <- content |>
    jsonlite::fromJSON() |>
    tibble::tibble()

  # Handle case where there might be nested data
  if (ncol(result) == 1 && is.list(result[[1]])) {
    result <- result |>
      tidyr::unnest_wider(1, names_sep = "_")
  }

  log_info("Successfully retrieved league data for league_id: {league_id}")
  return(result)
}

#' Get league users information
#'
#' Fetches information about all users in a specific league from the Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @return A tibble containing users information
#' @export
#' @examples
#' \dontrun{
#' users_data <- get_league_users("123456789")
#' }
get_league_users <- function(league_id) {
  log_debug("Starting get_league_users with league_id: {league_id}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    log_error("league_id must be a non-empty character string")
  }

  # Fetch specific league users from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/users")
  log_debug("Fetching all league users from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble and unnest
  log_debug("Converting R objects to tibble")
  users <- content |>
    jsonlite::fromJSON() |>
    tibble::tibble()

  # Handle case where there might be nested data
  if (ncol(users) == 1 && is.list(users[[1]])) {
    users <- users |>
      tidyr::unnest_wider(1, names_sep = "_")
  }

  user_count <- nrow(users)
  log_info("Successfully retrieved {user_count} users for league_id: {league_id}")
  return(users)
}

#' Get league rosters information
#'
#' Fetches information about all rosters in a specific league from the Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @return A tibble containing rosters information
#' @export
#' @examples
#' \dontrun{
#' rosters_data <- get_league_rosters("123456789")
#' }
get_league_rosters <- function(league_id) {
  log_debug("Starting get_league_rosters with league_id: {league_id}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    log_error("league_id must be a non-empty character string")
  }

  # Fetch specific league rosters from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/rosters")
  log_debug("Fetching all league rosters from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  log_debug("Converting R objects to tibble")
  rosters <- content |>
    jsonlite::fromJSON() |>
    tibble::tibble()

  # Handle case where there might be nested data
  if (ncol(rosters) == 1 && is.list(rosters[[1]])) {
    rosters <- rosters |>
      tidyr::unnest_wider(1, names_sep = "_")
  }

  roster_count <- nrow(rosters)
  log_info("Successfully retrieved {roster_count} rosters for league_id: {league_id}")
  return(rosters)
}

#' Get league matchups for a specific week
#'
#' Fetches matchup information for a specific week in a league from the Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @param week Integer representing the week number (typically 1-18)
#' @return A tibble containing matchups information
#' @export
#' @examples
#' \dontrun{
#' matchups_data <- get_league_matchups("123456789", 5)
#' }
get_league_matchups <- function(league_id, week) {
  log_debug("Starting get_league_matchups with league_id: {league_id}, week: {week}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    log_error("league_id must be a non-empty character string")
  }

  if (!is.numeric(week) || length(week) != 1 || week < 1 || week > 18) {
    log_error("week must be a single integer between 1 and 18")
  }

  # Fetch specific league matchups from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/matchups/{week}")
  log_debug("Fetching all league matchups for week {week} from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  log_debug("Converting R objects to tibble")
  matchups <- content |>
    jsonlite::fromJSON() |>
    tibble::tibble()

  # Handle case where there might be nested data
  if (ncol(matchups) == 1 && is.list(matchups[[1]])) {
    matchups <- matchups |>
      tidyr::unnest_wider(1, names_sep = "_")
  }

  # Add week info
  matchups$week <- week

  matchup_count <- nrow(matchups)
  log_info("Successfully retrieved {matchup_count} matchups for week {week}, league_id: {league_id}")
  return(matchups)
}

#' Get league transactions for a specific week
#'
#' Fetches transaction information for a specific week in a league from the Sleeper API
#' with optional filtering by transaction type
#'
#' @param league_id Character string representing the league ID
#' @param week Integer representing the week number (typically 1-18)
#' @param tx_type Optional character string representing the transaction type
#'   to filter by. Must be one of: "waiver", "free_agent", "trade". If NULL,
#'   returns all transactions.
#' @return A tibble containing transactions information, or empty tibble if no transactions found
#' @export
#' @examples
#' \dontrun{
#' # Get all transactions for week 3
#' all_transactions <- get_league_transactions_week("123456789", 3)
#'
#' # Get only trade transactions for week 3
#' trade_transactions <- get_league_transactions_week("123456789", 3, "trade")
#' }
get_league_transactions_week <- function(league_id, week, tx_type = NULL) {
  tx_type_str <- tx_type %||% "NULL"
  log_debug("Starting get_league_transactions_week with league_id: {league_id}, week: {week}, tx_type: {tx_type_str}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    log_error("league_id must be a non-empty character string")
  }

  if (!is.numeric(week) || length(week) != 1 || week < 1 || week > 18) {
    log_error("week must be a single integer between 1 and 18")
  }

  # Fetch specific league transactions from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/transactions/{week}")
  log_debug("Fetching all league transactions for week {week} from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  log_debug("Converting R objects to tibble")
  transactions <- content |>
    jsonlite::fromJSON() |>
    tibble::tibble()

  # Handle empty response
  if (nrow(transactions) == 0) {
    log_info("No transactions found for week {week} in league_id: {league_id}")
    return(tibble::tibble())
  }

  # Handle case where there might be nested data
  if (ncol(transactions) == 1 && is.list(transactions[[1]])) {
    transactions <- transactions |>
      tidyr::unnest_wider(1, names_sep = "_")
  }

  # Validate and filter by transaction type if specified
  validated_tx_type <- validate_tx_type(tx_type)

  if (!rlang::is_null(validated_tx_type)) {
    original_count <- nrow(transactions)
    transactions <- transactions |>
      dplyr::filter(type == validated_tx_type)
    filtered_count <- nrow(transactions)
    log_debug("Filtered transactions from {original_count} to {filtered_count} for type: {validated_tx_type}")
  }

  # Add week info
  transactions$week <- week

  transaction_count <- nrow(transactions)
  log_info("Successfully retrieved {transaction_count} transactions for week {week}, league_id: {league_id}")
  return(transactions)
}
