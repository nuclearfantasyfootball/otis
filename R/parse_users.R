# Parse Sleeper League User Information

#' Sleeper League User Details
#'
#' Retrieves user data for a given Sleeper league and expands nested metadata fields
#' into a clean and detailed tibble. This function is useful for mapping league users
#' to their display names, team names, and notification preferences.
#'
#' @param league_id A character string. The unique ID of the Sleeper league (e.g., `"111000111000111000"`).
#'
#' @return A tibble with one row per user in the league and expanded columns including:
#' \itemize{
#'   \item `league_id`, `user_id`, `display_name`
#'   \item `is_bot`, `is_owner`
#'   \item `user_avatar_id`, `metadata_avatar`, `metadata_team_name`
#'   \item User notification preferences like `metadata_allow_pn`, `metadata_mention_pn`, etc.
#'   \item Metadata booleans like `metadata_archived`
#' }
#'
#' @details
#' This function uses the Sleeper API endpoint to get user data for a league and expands the `metadata`
#' column using `tidyr::unnest_wider()` for ease of access and downstream analysis. The `user_avatar_id`
#' and `metadata_avatar` fields can be used to generate user image URLs from Sleeper's CDN.
#'
#' @seealso [get_league_users()] for the raw user data before expansion.
#'
#' @examples
#' \dontrun{
#' league_id <- "111000111000111000" # Example Sleeper league ID
#' users <- league_users_detailed(league_id)
#' }
#'
#' @export
league_users_details <- function(league_id) {
  # Get league users data from Sleeper API endpoint
  users <- otis::get_league_users(league_id) |>
    # Unpack metadata column with nested list
    tidyr::unnest_wider(metadata, names_sep = "_")

  # Keep columns in more relevant order
  users_detailed <- users |>
    dplyr::select(
      league_id, user_id, display_name,
      is_bot, is_owner,
      user_avatar_id = avatar, # Used to get user avatar, not team
      metadata_avatar, # https://sleepercdn.com/uploads/<avatar_id>.jpg
      metadata_team_name,
      # User push notifications
      metadata_allow_pn, metadata_mention_pn, metadata_user_message_pn,
      metadata_league_report_pn, metadata_trade_block_pn,
      # User notifications for transactions
      metadata_transaction_commissioner, metadata_transaction_free_agent,
      metadata_transaction_trade, metadata_transaction_waiver,
      # Additional user notifications
      metadata_player_like_pn,
      metadata_player_nickname_update,
      metadata_team_name_update,
      metadata_mascot_message,
      metadata_allow_sms,
      # User allows archiving metadata boolean
      metadata_archived
      # settings, # Remove - all vals are NA
    )

  return(users_detailed)
}
