# Load required packages
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(dplyr, tibble, glue, httr, jsonlite, purrr, tidyr, rlang, otis)

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
get_league_transactions_season <- function(league_id, .progress = TRUE, max_weeks = 18) {
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
          tx_type = NULL
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


# Test the function with the provided league_id
league_id <- "1190192546172342272"

# Run the function and store results
tryCatch(
  {
    historical_leagues <- otis::pull_league_info_historical(league_id)
    tx_season <- get_league_transactions_season(league_id)
  },
  error = function(e) {
    cat("Error testing function:", e$message, "\n")
  }
)

# league_id <- "1190192546172342272"
# 2025 league
# league_current <- otis::pull_league_info(league_id)
# league_hist <- pull_league_historical_info(league_id)
#
# username <- "nolmacdonald"
# user_id <- "332632476830679040"
# avatar_id <- "4ce10fd3c0c13eee3371d49d1d35aa62"
# draft_id <- "1190192546172342273"
# otis::pull_league_roster_positions(league_id)

# league_settings <- otis::pull_league_settings(league_id)

# otis::pull_league_scoring_settings(league_id)

# otis::pull_league_info_detailed(league_id)
#
