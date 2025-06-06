# Sleeper API Endpoints for Avatars
# Full-size images or thumbnails

#
# Load required packages
pacman::p_load(httr, jsonlite, glue)

# Helper functions - Remove for package after development
source("./R/utils_logging.R")
source("./R/utils_sleeper_api.R")

#' Retrieve Sleeper API Avatar Images
get_avatars <- function(avatar_id, image_type = NULL, save_path = "outputs/") {
  # Fetch avatar from Sleeper API
  if (is.null(image_type) || image_type %in% c("full", "full-size", "full size")) {
    # Full Size Avatar
    url <- glue::glue("https://sleepercdn.com/avatars/{avatar_id}")
    # Log the URL being fetched
    log_info(glue::glue("Fetching full-size avatar from endpoint: {url}"))
  } else if (image_type %in% c("thumb", "thumbnail")) {
    # Thumbnail Avatar
    url <- glue::glue("https://sleepercdn.com/avatars/thumbs/{avatar_id}")
    # Log the URL being fetched
    log_info(glue::glue("Fetching thumbnail avatar from endpoint: {url}"))
  } else {
    # Invalid image type
    log_info(glue::glue("Avatar image_type == '{image_type}' not found!"))
    log_info("Try full, full-size, full size or NULL for a full-size avatar.")
    log_info("Try thumb or thumbnail for a thumbnail avatar.")
    stop("Stopping. Avatar image type not found.")
  }

  # Make the GET request
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract raw content (avatars are binary image data, not JSON)
  log_info("Extracting binary content from avatar request.")
  binary_content <- httr::content(response, "raw")

  if (!is.null(save_path)) {
    # Save the binary content to a file
    writeBin(binary_content, glue::glue("{save_path}/avatar_{image_type}_{avatar_id}.png"))
    log_info(glue::glue("Avatar saved to {save_path}/avatar_{image_type}_{avatar_id}.png"))
  }
  else {
    log_info("No save path provided. Avatar not saved to file.")
  }

  # Return the raw binary content
  return(binary_content)
}
# Run code if not package build/install with avatar_id
if (interactive()) {
  avatar <- get_avatars("<avatar_id>",
                        image_type = "full",
                        save_path = "outputs/")
}
