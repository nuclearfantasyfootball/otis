# Sleeper API Player Data
# Warning! All player data should be fetched sparingly. The file is ~15 mb.

# Load required packages
pacman::p_load(httr, jsonlite, dplyr, glue, progress, tibble, purrr, RSQLite)

# Helper functions - Remove for package after development
source("./R/utils_logging.R")
source("./R/utils_sleeper_api.R")

#' Fetch all NFL players from Sleeper API
#'
#' @description Fetches complete NFL player data from the Sleeper API and saves it as a JSON file
#' @param save_path Character string specifying the directory path to save the JSON file
#' @return No return value, called for side effect of saving player data to disk
#' @export
#' @examples
#' \dontrun{
#' fetch_all_players("./data/raw/sleeper_players")
#' }
fetch_all_players <- function(save_path = "./data/raw/sleeper_players") {
  url <- "https://api.sleeper.app/v1/players/nfl"
  log_info(paste0("Fetching all players from endpoint: ", url))

  # Make GET request
  response <- httr::GET(url)

  # Check for successful response
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch data from Sleeper API.")
  }

  # Parse content as JSON
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed_json <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  # Get current date in desired format
  formatted_date <- format(Sys.Date(), "%Y_%b_%d") |> toupper()

  # Construct the file name with the date
  file_name <- sprintf("sleeper_player_data_%s.json", formatted_date)
  log_info(paste0("Saving player data to JSON file: ", file_name))

  # Define the save path
  file_path <- file.path(save_path, file_name)

  # Write to file
  jsonlite::write_json(parsed_json, path = file_path, pretty = TRUE, auto_unbox = TRUE)
  log_info(paste0("Successfully saved JSON file of all players to: ", file_path))
}

#' Load Sleeper NFL player data from JSON file
#'
#' @description Loads previously saved Sleeper NFL player data from a JSON file
#' @param file_path Character string specifying the full path to the JSON file
#' @return A named list containing complete player data
#' @export
#' @examples
#' \dontrun{
#' players <- load_all_sleeper_players("./data/raw/sleeper_players/sleeper_player_data_2025_MAY_14.json")
#' }
load_all_sleeper_players <- function(file_path) {
  # Check if file exists before loading
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Load and parse JSON as a named list
  player_list <- jsonlite::read_json(file_path, simplifyVector = FALSE)
  log_info(paste0("Loaded all players JSON file from: ", file_path))

  return(player_list)
}

#' Get Sleeper player variable names
#'
#' @description Returns a character vector of variable names available in the Sleeper player data
#' @return A character vector of variable names
#' @export
#' @examples
#' vars <- get_sleeper_player_vars()
get_sleeper_player_vars <- function() {
  # Define list of variable names for all players
  var_list <- c("full_name", "first_name", "last_name",
                "position", # "fantasy_positions",
                "metadata$channel_id", "metadata$rookie_year",
                "high_school", "college", "team", "team_abbr", "team_changed_at",
                "age", "years_exp", "height", "weight", "number",
                "birth_date", "birth_state", "birth_city", "birth_country",
                "search_full_name", "search_first_name", "search_last_name",
                "search_rank", "depth_chart_order", "depth_chart_position",
                "injury_status", "injury_body_part", "injury_notes", "injury_start_date",
                "news_updated", "practice_participation", "practice_description",
                "competitions", "status", "active", "hashtag", "sport",
                "sportradar_id", "player_id", "espn_id", "rotowire_id", "swish_id",
                "gsis_id", "yahoo_id", "rotoworld_id", "stats_id", "oddsjam_id",
                "fantasy_data_id", "pandascore_id", "opta_id"
  )

  log_info("Pulling list of all players variables names.")

  return(var_list)
}

#' Safely extract nested fields from a player list
#'
#' @description Helper function to safely extract potentially missing nested fields from a player object
#' @param field Character string specifying the field to extract, with $ as delimiter for nested fields
#' @param players Named list of player data from which to extract fields
#' @return A function that takes a player ID and returns the extracted field value
#' @keywords internal
safe_extract <- function(field, players) {
  function(id) {
    # Split the field string (e.g., "metadata$channel_id") into parts
    field_parts <- strsplit(field, "\\$")[[1]]

    # Start with the player object identified by the id
    val <- players[[id]]

    # Traverse the nested structure step by step
    for (part in field_parts) {
      val <- val[[part]]
      if (is.null(val)) return(NA_character_)  # Return NA if field is missing
    }

    # If the value is empty, return NA
    if (length(val) == 0) return(NA_character_)

    # Convert to character for consistent return type
    as.character(val)
  }
}

#' Extract player information into a structured tibble
#'
#' @description Extracts player information from a nested list structure into a structured tibble
#' @param players Named list of player data as returned by load_all_sleeper_players()
#' @param db_path Character string specifying the path to save the SQLite database, NULL to skip saving
#' @return A tibble containing extracted player information
#' @export
#' @examples
#' \dontrun{
#' players <- load_all_sleeper_players("./data/raw/sleeper_players/sleeper_player_data_2025_MAY_14.json")
#' players_extracted <- extract_player_info(players, "./data/parsed/sleeper_all_players")
#' }
extract_player_info <- function(players, db_path = NULL) {
  log_info("Starting extraction of player information.")

  # Retrieve the variable names to extract
  player_vars <- get_sleeper_player_vars()
  log_info("Retrieved player variable names: ", paste(player_vars, collapse = ", "))

  player_vars <- lapply(player_vars, rlang::as_symbol)
  result <- tibble::tibble(sleeper_id = names(players))

  # Dynamically build format string
  n_players <- length(players)
  log_info(glue::glue("Extracting data for {n_players} players: "))
  format_str <- glue::glue("\U23F3 :elapsed | [:bar] :current/:total (:percent) | ETA: :eta | Var: :var")

  # Initialize progress bar
  pb <- progress::progress_bar$new(
    format = format_str,
    total = length(player_vars),
    clear = FALSE,
    width = 80,
    complete = "\U0001F7E9",  # Green square unicode
    incomplete = "\U0001F7E5"  # Red square unicode
  )

  # Iterate through variables and extract with progress updates
  for (var in player_vars) {
    # Progress bar
    pb$tick(tokens = list(var = rlang::as_string(var)))

    # Safe extract
    extractor <- safe_extract(rlang::as_string(var), players)
    result <- result |>
      dplyr::mutate(!!var := purrr::map_chr(sleeper_id, extractor))
  }

  n_vars <- length(result)

  success_str = glue::glue("Successfully extracted all player data.\nExtracted {n_vars} variables for {n_players} players.")
  log_info(success_str)

  if (is.null(db_path)) {
    log_info("No database path provided. Skipping database save.")
  } else {
    # Save to path if not null
    # Get current date in desired format
    formatted_date <- format(Sys.Date(), "%Y_%b_%d") |> toupper()
    # Construct the file name with the date
    table_name <- sprintf("sleeper_all_players_%s", formatted_date)
    # db_path <- "./data/parsed/sleeper_all_players"
    write_tbl_db(result, db_path, table_name, rm_current_tbl = TRUE)

    log_info(glue::glue("Saved extracted data to SQLite database: {db_path}"))
  }
  return(result)
}

#' Load existing player data or fetch new data if not available
#'
#' @description Checks for existing player data for the current date and either loads it or fetches new data
#' @param save_path Character string specifying the directory path to save or look for player data
#' @return A named list containing complete player data
#' @export
#' @examples
#' \dontrun{
#' players <- load_or_fetch_sleeper_players("./data/raw/sleeper_players")
#' }
load_or_fetch_sleeper_players <- function(save_path = "./data/raw/sleeper_players") {
  # Get current date in desired format
  formatted_date <- format(Sys.Date(), "%Y_%b_%d") |> toupper()

  # Construct the file name with the date
  file_name <- sprintf("sleeper_player_data_%s.json", formatted_date)
  file_path <- file.path(save_path, file_name)

  if (file.exists(file_path)) {
    log_info("Raw JSON file for all player data exists. Loading player data.")
    return(load_all_sleeper_players(file_path))
  } else {
    log_info("Raw JSON file for all player data not found! Fetching player data from Sleeper API.")
    fetch_all_players(save_path = save_path)
    return(load_all_sleeper_players(file_path))
  }
}

#' Get trending NFL players from Sleeper API
#'
#' @description Retrieves trending players (adds or drops) from the Sleeper API
#' @param type Character string specifying the trend type, either "add" or "drop"
#' @param lookback_hours Numeric specifying how many hours to look back for trends
#' @param limit Numeric specifying the maximum number of players to return
#' @return A list containing trending player data
#' @export
#' @examples
#' \dontrun{
#' trending_adds <- get_trending_players("add", lookback_hours = 24, limit = 25)
#' trending_drops <- get_trending_players("drop", lookback_hours = 24, limit = 25)
#' }
get_trending_players <- function(type, lookback_hours = 24, limit = 25) {
  # Fetch specific draft from Sleeper API
  url <- glue::glue("https://api.sleeper.app/v1/players/nfl/trending/{type}?lookback_hours={lookback_hours}&limit={limit}")

  # Log the URL being fetched
  log_info(glue::glue("Fetching trending players from endpoint: {url}"))

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_info("Extracting content from trending players request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_info("Converting trending players R objects to JSON.")
  return(jsonlite::fromJSON(content))
}

