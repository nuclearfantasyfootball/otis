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
  otis::log_debug("Starting get_specific_league with league_id: {league_id}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  # Fetch specific league from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}")
  otis::log_debug("Fetching specific league data from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  otis::log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  otis::log_debug("Converting R objects to tibble")
  result <- content |>
    jsonlite::fromJSON()
  # tibble::tibble()

  # Handle case where there might be nested data
  # if (ncol(result) == 1 && is.list(result[[1]])) {
  #   result <- result |>
  #     tidyr::unnest_wider(1, names_sep = "_")
  # }

  otis::log_info("Successfully retrieved league data for league_id: {league_id}")
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
  otis::log_debug("Starting get_league_users with league_id: {league_id}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1L || nchar(league_id) == 0L) {
    otis::log_error("league_id must be a non-empty character string")
  }

  # Construct URL and make request
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/users")
  otis::log_info("Fetching all league users from endpoint: {url}")

  response <- tryCatch(
    {
      httr::GET(url)
    },
    error = function(e) {
      otis::log_error("Failed to make HTTP request: {e$message}")
    }
  )

  status_sleeper_api(response)

  # Extract and parse content
  otis::log_debug("Extracting content from users request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  otis::log_debug("Converting users response to tibble")
  users_data <- tryCatch(
    {
      parsed_data <- jsonlite::fromJSON(content)

      if (length(parsed_data) == 0L) {
        otis::log_info("No users found for league_id: {league_id}")
        return(tibble::tibble())
      }

      # Convert to tibble
      result <- tibble::as_tibble(parsed_data)

      # Add league_id for reference
      result |>
        dplyr::mutate(league_id = league_id, .before = 1L)
    },
    error = function(e) {
      otis::log_error("Failed to parse users response: {e$message}")
    }
  )

  user_count <- nrow(users_data)
  otis::log_info("Successfully retrieved {user_count} users for league_id: {league_id}")
  return(users_data)
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
  otis::log_debug("Starting get_league_rosters with league_id: {league_id}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  # Fetch specific league rosters from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/rosters")
  otis::log_debug("Fetching all league rosters from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  otis::log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  otis::log_debug("Converting R objects to tibble")
  rosters <- content |>
    jsonlite::fromJSON() |>
    tibble::tibble()

  # Handle case where there might be nested data
  if (ncol(rosters) == 1 && is.list(rosters[[1]])) {
    rosters <- rosters |>
      tidyr::unnest_wider(1, names_sep = "_")
  }

  roster_count <- nrow(rosters)
  otis::log_info("Successfully retrieved {roster_count} rosters for league_id: {league_id}")
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
  otis::log_debug("Starting get_league_matchups with league_id: {league_id}, week: {week}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  if (!is.numeric(week) || length(week) != 1 || week < 1 || week > 18) {
    otis::log_error("week must be a single integer between 1 and 18")
  }

  # Fetch specific league matchups from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/matchups/{week}")
  otis::log_debug("Fetching all league matchups for week {week} from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  otis::log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  otis::log_debug("Converting R objects to tibble")
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
  otis::log_info("Successfully retrieved {matchup_count} matchups for week {week}, league_id: {league_id}")
  return(matchups)
}

#' Get League Transactions for a Specific Week
#'
#' Fetches transaction data for a specific week in a league from Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @param week Integer between 1 and 18 representing the week number
#' @param tx_type Character string representing transaction type or NULL.
#'   Valid values are "waiver", "free_agent", "trade". NULL returns all types.
#' @param is_complete Logical indicating whether to filter for completed
#'   transactions only. Default is FALSE (returns all transactions)
#' @return A tibble containing transaction data for the specified week
#' @export
#' @examples
#' \dontrun{
#' # Get all transactions for week 1
#' get_league_transactions_week("123456", 1)
#'
#' # Get only completed transactions
#' get_league_transactions_week("123456", 1, is_complete = TRUE)
#'
#' # Get only completed trades
#' get_league_transactions_week("123456", 1, tx_type = "trade", is_complete = TRUE)
#' }
get_league_transactions_week <- function(league_id, week, tx_type = NULL, is_complete = FALSE) {
  tx_type_str <- tx_type %||% "NULL"
  otis::log_debug("Starting get_league_transactions_week with league_id: {league_id}, week: {week}, tx_type: {tx_type_str}, is_complete: {is_complete}")

  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  if (!is.numeric(week) || length(week) != 1 || week < 1 || week > 18) {
    otis::log_error("week must be a single integer between 1 and 18")
  }

  if (!is.logical(is_complete) || length(is_complete) != 1) {
    otis::log_error("is_complete must be a single logical value (TRUE or FALSE)")
  }

  # Fetch specific league transactions from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/league/{league_id}/transactions/{week}")
  otis::log_debug("Fetching all league transactions for week {week} from endpoint: {url}")

  response <- httr::GET(url)
  status_sleeper_api(response)

  # Extract/parse content
  otis::log_debug("Extracting content from request")
  content <- httr::content(response, "text", encoding = "UTF-8")

  # Convert to tibble
  otis::log_debug("Converting R objects to tibble")
  transactions <- content |>
    jsonlite::fromJSON() |>
    tibble::tibble()

  # Handle empty response
  if (nrow(transactions) == 0) {
    otis::log_info("No transactions found for week {week} in league_id: {league_id}")
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
    otis::log_debug("Filtered transactions from {original_count} to {filtered_count} for type: {validated_tx_type}")
  }

  # Filter for completed transactions if requested
  if (is_complete && "status" %in% names(transactions)) {
    original_count <- nrow(transactions)
    transactions <- transactions |>
      dplyr::filter(status == "complete")
    filtered_count <- nrow(transactions)
    otis::log_debug("Filtered transactions from {original_count} to {filtered_count} for completed transactions only")
  } else if (is_complete && !"status" %in% names(transactions)) {
    otis::log_warn("is_complete = TRUE requested but 'status' column not found in transaction data")
    otis::log_info("Available columns: {paste(names(transactions), collapse = ', ')}")
  }

  # Add week info
  transactions$week <- week

  transaction_count <- nrow(transactions)
  completion_filter_msg <- if (is_complete) " (completed only)" else ""
  otis::log_info("Successfully retrieved {transaction_count} transactions{completion_filter_msg} for week {week}, league_id: {league_id}")

  return(transactions)
}
