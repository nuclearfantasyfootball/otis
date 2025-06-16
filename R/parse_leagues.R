# Parse Sleeper League Data Functions

#' Parse Sleeper League Data into Structured Format
#'
#' Retrieves and parses Sleeper league data into a comprehensive tibble with
#' expanded metadata fields and properly formatted timestamps.
#'
#' @param league_id Character string. The unique ID of the Sleeper league
#'   (e.g., `"1190192546172342272"`).
#'
#' @return A tibble with one row containing comprehensive league information including:
#' \itemize{
#'   \item Basic league info: `name`, `status`, `avatar`, `season`, `season_type`
#'   \item Metadata fields: `metadata_auto_continue`, `metadata_copy_from_league_id`, etc.
#'   \item Message data: `last_message_id`, `last_author_display_name`, etc.
#'   \item League structure: `total_rosters`, `draft_id`, bracket information
#' }
#'
#' @details
#' This function fetches raw league data using `get_specific_league()` and transforms
#' it into a clean, flat tibble structure. Nested metadata fields are expanded with
#' descriptive column names, and timestamp fields are converted to readable format
#' using the America/New_York timezone.
#'
#' @seealso [get_specific_league()] for raw league data retrieval.
#' @seealso [parse_league_settings()], [parse_league_scoring_settings()],
#'   [parse_league_roster_positions()] for parsing specific league components.
#'
#' @examples
#' \dontrun{
#' league_id <- "1190192546172342272"
#' league_data <- parse_league_data(league_id)
#'
#' # View basic league information
#' league_data |>
#'   select(name, season, status, total_rosters)
#' }
#'
#' @export
parse_league_data <- function(league_id) {
  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  # Fetch league data from Sleeper API
  otis::log_debug("Fetching league data for league_id: {league_id}")
  league <- otis::get_specific_league(league_id)

  # Parse main league data with expanded metadata
  otis::log_debug("Parsing league data into structured tibble")
  league_data <- tibble::tibble(
    # Basic league information
    name = league$name,
    status = league$status,
    avatar = league$avatar,
    season = league$season,
    season_type = league$season_type,
    shard = league$shard,
    sport = league$sport,

    # Expanded metadata fields
    metadata_auto_continue = league$metadata$auto_continue,
    metadata_copy_from_league_id = league$metadata$copy_from_league_id,
    metadata_keeper_deadline = league$metadata$keeper_deadline,
    metadata_latest_league_winner_roster_id = league$metadata$latest_league_winner_roster_id,

    # Company and message information
    company_id = league$company_id,
    last_message_id = league$last_message_id,
    draft_id = league$draft_id,

    # Last message author details
    last_author_avatar = league$last_author_avatar,
    last_author_display_name = league$last_author_display_name,
    last_author_id = league$last_author_id,
    last_author_is_bot = league$last_author_is_bot,

    # Message content and timing
    last_message_attachment = league$last_message_attachment,
    last_message_text_map = league$last_message_text_map,
    last_message_time = otis::convert_sleeper_timestamp(league$last_message_time), # Fixed bug

    # League structure and brackets
    league_id = league$league_id,
    previous_league_id = league$previous_league_id,
    bracket_id = league$bracket_id,
    bracket_overrides_id = league$bracket_overrides_id,
    group_id = league$group_id,
    loser_bracket_id = league$loser_bracket_id,
    loser_bracket_overrides_id = league$loser_bracket_overrides_id,
    total_rosters = league$total_rosters
  )

  otis::log_info("Successfully parsed league data for league: {league$name}")
  return(league_data)
}

#' Parse League Settings into Long Format
#'
#' Extracts and transforms league settings from raw league data into a
#' long-format tibble for easy analysis and filtering.
#'
#' @param league_id Character string. The unique ID of the Sleeper league.
#'
#' @return A tibble in long format with columns:
#' \itemize{
#'   \item `setting` - Name of the league setting
#'   \item `value` - Value of the setting
#' }
#'
#' @details
#' This function is useful for analyzing league configurations, comparing
#' settings across leagues, or filtering for specific setting values.
#' All settings are converted to a consistent long format.
#'
#' @examples
#' \dontrun{
#' league_id <- "1190192546172342272"
#' settings <- parse_league_settings(league_id)
#'
#' # View specific settings
#' settings |>
#'   filter(setting %in% c("playoff_teams", "trade_deadline"))
#' }
#'
#' @export
parse_league_settings <- function(league_id) {
  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  # Fetch league data
  otis::log_debug("Fetching league settings for league_id: {league_id}")
  league <- otis::get_specific_league(league_id)

  # Transform settings to long format
  otis::log_debug("Converting league settings to long format")
  league_settings <- league$settings |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "setting",
      values_to = "value"
    )

  otis::log_info("Successfully parsed {nrow(league_settings)} league settings")
  return(league_settings)
}

#' Parse League Scoring Settings into Long Format
#'
#' Extracts and transforms league scoring settings from raw league data into a
#' long-format tibble for analysis of scoring configurations.
#'
#' @param league_id Character string. The unique ID of the Sleeper league.
#'
#' @return A tibble in long format with columns:
#' \itemize{
#'   \item `setting` - Name of the scoring setting (e.g., "pass_td", "rush_yd")
#'   \item `value` - Point value for the setting
#' }
#'
#' @details
#' Scoring settings define how fantasy points are awarded for different
#' statistical achievements. This function makes it easy to compare scoring
#' systems across leagues or analyze specific scoring categories.
#'
#' @examples
#' \dontrun{
#' league_id <- "1190192546172342272"
#' scoring <- parse_league_scoring_settings(league_id)
#'
#' # View touchdown scoring
#' scoring |>
#'   filter(str_detect(setting, "_td$"))
#' }
#'
#' @export
parse_league_scoring_settings <- function(league_id) {
  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  # Fetch league data
  otis::log_debug("Fetching league scoring settings for league_id: {league_id}")
  league <- otis::get_specific_league(league_id)

  # Transform scoring settings to long format
  otis::log_debug("Converting league scoring settings to long format")
  league_scoring_settings <- league$scoring_settings |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "setting",
      values_to = "value"
    )

  otis::log_info("Successfully parsed {nrow(league_scoring_settings)} scoring settings")
  return(league_scoring_settings)
}

#' Parse League Roster Positions
#'
#' Extracts and summarizes the roster position requirements for a league,
#' showing the count of each position type.
#'
#' @param league_id Character string. The unique ID of the Sleeper league.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item `position` - Position type (e.g., "QB", "RB", "WR", "FLEX")
#'   \item `n` - Number of roster slots for this position
#' }
#' Positions are sorted by count in descending order.
#'
#' @details
#' This function analyzes the `roster_positions` field to understand league
#' roster construction requirements. Useful for comparing league formats
#' or understanding positional scarcity.
#'
#' @examples
#' \dontrun{
#' league_id <- "1190192546172342272"
#' positions <- parse_league_roster_positions(league_id)
#'
#' # View roster structure
#' positions |>
#'   arrange(desc(n))
#' }
#'
#' @export
parse_league_roster_positions <- function(league_id) {
  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  # Fetch league data
  otis::log_debug("Fetching league roster positions for league_id: {league_id}")
  league <- otis::get_specific_league(league_id)

  # Summarize roster positions
  otis::log_debug("Counting roster positions by type")
  league_roster_positions <- tibble::tibble(position = league[["roster_positions"]]) |>
    dplyr::count(position, sort = TRUE)

  otis::log_info("Successfully parsed {nrow(league_roster_positions)} unique roster positions")
  return(league_roster_positions)
}

#' Parse Complete League Information
#'
#' Convenience function that parses all league components (data, settings,
#' scoring, and roster positions) and returns them as a named list.
#'
#' @param league_id Character string. The unique ID of the Sleeper league.
#'
#' @return A named list containing:
#' \itemize{
#'   \item `league_data` - Main league information tibble
#'   \item `settings` - League settings in long format
#'   \item `scoring_settings` - Scoring settings in long format
#'   \item `roster_positions` - Roster position counts
#' }
#'
#' @details
#' This function combines all the individual parsing functions for comprehensive
#' league analysis. It's efficient as it only makes one API call and then
#' processes the data multiple ways.
#'
#' @examples
#' \dontrun{
#' league_id <- "1190192546172342272"
#' complete_league <- parse_complete_league_info(league_id)
#'
#' # Access different components
#' complete_league$league_data
#' complete_league$settings
#' complete_league$scoring_settings
#' complete_league$roster_positions
#' }
#'
#' @export
parse_complete_league_info <- function(league_id) {
  # Input validation
  if (!is.character(league_id) || length(league_id) != 1 || nchar(league_id) == 0) {
    otis::log_error("league_id must be a non-empty character string")
  }

  otis::log_info("Starting complete league info parsing for league_id: {league_id}")

  # Parse all components
  league_data <- otis::parse_league_data(league_id)
  settings <- otis::parse_league_settings(league_id)
  scoring_settings <- otis::parse_league_scoring_settings(league_id)
  roster_positions <- otis::parse_league_roster_positions(league_id)

  # Combine into named list
  complete_info <- list(
    league_data = league_data,
    settings = settings,
    scoring_settings = scoring_settings,
    roster_positions = roster_positions
  )

  otis::log_info("Successfully parsed complete league information")
  return(complete_info)
}
