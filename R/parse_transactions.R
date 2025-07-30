# Parse League Transactions

#' Get League Transactions for Entire Season
#'
#' This function retrieves all transactions for a league across all weeks
#' in a season by looping through each week and combining the results.
#'
#' @param league_id A character string representing the league ID for the
#'   specific season
#' @param .progress Logical indicating whether to show progress messages.
#'   Default is TRUE
#' @param max_weeks Integer specifying maximum number of weeks to check.
#'   Default is 18 to cover regular season plus playoffs
#'
#' @return A tibble containing all transactions for the season with an
#'   additional 'week' column indicating the week number for each transaction
#'
#' @details The function loops through weeks 1 to max_weeks, calling
#'   otis::get_league_transactions_week() for each week with tx_type = NULL
#'   to get all transaction types. It stops early if no transactions are
#'   found for consecutive weeks to avoid unnecessary API calls.
#'
#' @examples
#' \dontrun{
#' # Get all transactions for a season
#' season_transactions <- get_league_transactions_season("123456789")
#' }
#'
#' @export
get_league_transactions_season <- function(league_id, tx_type = NULL, .progress = TRUE, max_weeks = 18) {
  # Input validation
  if (missing(league_id) || is.null(league_id) || !is.character(league_id)) {
    stop("league_id must be a non-null character string")
  }

  if (length(league_id) != 1) {
    stop("league_id must be a single character string")
  }

  # Initialize storage for all transaction data
  all_transactions <- list()
  consecutive_empty_weeks <- 0
  max_consecutive_empty <- 3 # Stop if 3 consecutive weeks have no transactions

  # Progress message
  if (.progress) {
    otis::log_info("Starting transaction collection for league {league_id}")
  }

  # Loop through all weeks
  for (week in 1:max_weeks) {
    # Progress indicator
    if (.progress) {
      otis::log_info("Pulling transactions for week {week}")
    }

    # Attempt to pull week transactions with error handling
    week_transactions <- tryCatch(
      {
        otis::get_league_transactions_week(
          league_id = league_id,
          week = week,
          tx_type = tx_type
        )
      },
      error = function(e) {
        if (.progress) {
          otis::log_info("Error pulling transactions for week {week}: {e$message}")
        }
        return(NULL)
      }
    )

    # Check if we got any transactions
    if (is.null(week_transactions) || nrow(week_transactions) == 0) {
      consecutive_empty_weeks <- consecutive_empty_weeks + 1

      if (.progress) {
        otis::log_info("No transactions found for week {week}")
      }

      # Stop if we've hit too many consecutive empty weeks
      if (consecutive_empty_weeks >= max_consecutive_empty) {
        if (.progress) {
          otis::log_info("Stopping after {consecutive_empty_weeks} consecutive weeks with no transactions")
        }
        break
      }

      next # Skip to next week
    }

    # Reset consecutive empty counter since we found transactions
    consecutive_empty_weeks <- 0

    # Add week column to the transactions
    week_transactions <- week_transactions %>%
      mutate(
        week = week,
        .before = 1 # Add week as first column
      )

    # Store the week's transactions
    all_transactions[[length(all_transactions) + 1]] <- week_transactions

    if (.progress) {
      otis::log_info("Found {nrow(week_transactions)} transactions for week {week}")
    }
  }

  # Check if we collected any data
  if (length(all_transactions) == 0) {
    if (.progress) {
      otis::log_info("No transactions found for any week in league {league_id}")
    }

    # Return empty tibble with expected structure
    return(tibble(
      week = integer(0),
      # Add other expected columns here if known
      .rows = 0
    ))
  }

  # Combine all transaction data into a single tibble
  combined_transactions <- all_transactions %>%
    bind_rows()

  # Final progress message
  if (.progress) {
    otis::log_info("Successfully retrieved {nrow(combined_transactions)} total transactions across {length(all_transactions)} weeks")
  }

  return(combined_transactions)
}

#' Get League Transactions for All Seasons
#'
#' This function retrieves all transactions for a league and all of its
#' historical predecessor leagues by combining pull_league_info_historical()
#' and get_league_transactions_season().
#'
#' @param league_id A character string representing the starting league ID
#'   (typically the most recent league)
#' @param .progress Logical indicating whether to show progress messages.
#'   Default is TRUE
#' @param max_weeks Integer specifying maximum number of weeks to check per season.
#'   Default is 18
#'
#' @return A tibble containing all transactions from all historical seasons
#'   with columns for week, season year (if available), historical_iteration,
#'   and league_id
#'
#' @details The function first calls pull_league_info_historical() to get
#'   all historical league information, then loops through each league_id
#'   to get all transactions for that season using get_league_transactions_season().
#'   Results are combined with season and league identification information.
#'
#' @examples
#' \dontrun{
#' # Get all transactions for all historical seasons
#' all_transactions <- get_league_transactions_all_seasons("123456789")
#' }
#'
#' @export
get_league_transactions_historical <- function(league_id, tx_type = NULL, .progress = TRUE, max_weeks = 18) {
  # Input validation
  if (missing(league_id) || is.null(league_id) || !is.character(league_id)) {
    stop("league_id must be a non-null character string")
  }

  # Get historical league information
  if (.progress) {
    otis::log_info("Getting historical league information...")
  }

  historical_leagues <- tryCatch(
    {
      otis::pull_league_info_historical(league_id = league_id, .progress = .progress)
    },
    error = function(e) {
      stop("Failed to retrieve historical league information: ", e$message)
    }
  )

  # Initialize storage for all seasons' transactions
  all_seasons_transactions <- list()

  if (.progress) {
    otis::log_info("Found {nrow(historical_leagues)} historical leagues to process")
  }

  # Loop through each historical league
  for (i in 1:nrow(historical_leagues)) {
    current_league <- historical_leagues[i, ]
    current_league_id <- current_league$league_id

    if (.progress) {
      league_name <- if ("name" %in% names(current_league)) current_league$name else "Unknown"
      season_year <- if ("season" %in% names(current_league)) current_league$season else current_league$historical_iteration
      otis::log_info("Processing season {season_year} (League: {league_name})")
    }

    # Get all transactions for this season
    season_transactions <- tryCatch(
      {
        get_league_transactions_season(
          league_id = current_league_id,
          tx_type = tx_type,
          .progress = .progress,
          max_weeks = max_weeks
        )
      },
      error = function(e) {
        if (.progress) {
          otis::log_info("Error getting transactions for league {current_league_id}: {e$message}")
        }
        return(NULL)
      }
    )

    # Skip if no transactions found
    if (is.null(season_transactions) || nrow(season_transactions) == 0) {
      if (.progress) {
        otis::log_info("No transactions found for league {current_league_id}")
      }
      next
    }

    # Add league and season identification columns
    season_transactions <- season_transactions %>%
      mutate(
        league_id = current_league_id,
        historical_iteration = current_league$historical_iteration,
        .before = 1
      )

    # Add season year if available
    if ("season" %in% names(current_league)) {
      season_transactions <- season_transactions %>%
        mutate(
          season = current_league$season,
          .after = historical_iteration
        )
    }

    # Add league name if available
    if ("name" %in% names(current_league)) {
      season_transactions <- season_transactions %>%
        mutate(
          league_name = current_league$name,
          .after = historical_iteration
        )
    }

    # Store this season's transactions
    all_seasons_transactions[[i]] <- season_transactions

    if (.progress) {
      otis::log_info("Added {nrow(season_transactions)} transactions from this season")
    }
  }

  # Check if we collected any data
  if (length(all_seasons_transactions) == 0) {
    if (.progress) {
      otis::log_info("No transactions found for any historical season")
    }

    # Return empty tibble with expected structure
    return(tibble(
      league_id = character(0),
      historical_iteration = integer(0),
      week = integer(0),
      .rows = 0
    ))
  }

  # Combine all seasons' transaction data
  combined_all_transactions <- all_seasons_transactions %>%
    bind_rows()

  # Final progress message
  if (.progress) {
    total_transactions <- nrow(combined_all_transactions)
    seasons_processed <- length(all_seasons_transactions)
    otis::log_info("Successfully retrieved {total_transactions} total transactions across {seasons_processed} historical seasons")
  }

  return(combined_all_transactions)
}
