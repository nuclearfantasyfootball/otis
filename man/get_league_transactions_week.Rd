% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sleeper_api_leagues.R
\name{get_league_transactions_week}
\alias{get_league_transactions_week}
\title{Get League Transactions for a Specific Week}
\usage{
get_league_transactions_week(
  league_id,
  week,
  tx_type = NULL,
  is_complete = FALSE
)
}
\arguments{
\item{league_id}{Character string representing the league ID}

\item{week}{Integer between 1 and 18 representing the week number}

\item{tx_type}{Character string representing transaction type or NULL.
Valid values are "waiver", "free_agent", "trade". NULL returns all types.}

\item{is_complete}{Logical indicating whether to filter for completed
transactions only. Default is FALSE (returns all transactions)}
}
\value{
A tibble containing transaction data for the specified week
}
\description{
Fetches transaction data for a specific week in a league from Sleeper API
}
\examples{
\dontrun{
# Get all transactions for week 1
get_league_transactions_week("123456", 1)

# Get only completed transactions
get_league_transactions_week("123456", 1, is_complete = TRUE)

# Get only completed trades
get_league_transactions_week("123456", 1, tx_type = "trade", is_complete = TRUE)
}
}
