# Parse Sleeper Transactions with Sleeper API Functions

#' Retrieve Sleeper API Avatar Images
#'
#' Downloads avatar images from the Sleeper fantasy football platform API.
#' Avatars can be retrieved in full-size or thumbnail format and optionally
#' saved to disk as PNG files.
#'
#' @param avatar_id Character string. The unique avatar identifier from Sleeper.
#'   This ID corresponds to a specific avatar image in the Sleeper system.
#' @param image_type Character string or NULL. The type of avatar image to retrieve.
#'   Valid options are:
#'   \itemize{
#'     \item \code{"full"}, \code{"full-size"}, \code{"full size"}, or \code{NULL} - Full-size avatar
#'     \item \code{"thumb"} or \code{"thumbnail"} - Thumbnail version of avatar
#'   }
#'   Defaults to \code{NULL} (full-size).
#' @param save_path Character string or NULL. Directory path where the avatar
#'   image should be saved. If \code{NULL}, the image will not be saved to disk.
#'   Defaults to \code{"outputs/"}.
#'
#' @return Raw binary content of the avatar image. This can be used to display
#'   the image or perform further processing.
#'
#' @details
#' This function interfaces with the Sleeper CDN to download avatar images.
#' The Sleeper API provides avatars at two endpoints:
#' \itemize{
#'   \item Full-size: \code{https://sleepercdn.com/avatars/{avatar_id}}
#'   \item Thumbnail: \code{https://sleepercdn.com/avatars/thumbs/{avatar_id}}
#' }
#'
#' When \code{save_path} is provided, the image is saved as
#' \code{avatar_{image_type}_{avatar_id}.png} in the specified directory.
#' The function will create logging messages about the download process and
#' file saving operations.
#'
#' @examples
#' \dontrun{
#' # Download a full-size avatar and save to default location
#' avatar_data <- get_avatars("123456789")
#'
#' # Download a thumbnail avatar and save to custom path
#' thumb_data <- get_avatars("123456789",
#'                          image_type = "thumbnail",
#'                          save_path = "my_avatars/")
#'
#' # Download avatar without saving to disk
#' raw_avatar <- get_avatars("123456789", save_path = NULL)
#'
#' # Different ways to specify full-size
#' full_avatar1 <- get_avatars("123456789", image_type = "full")
#' full_avatar2 <- get_avatars("123456789", image_type = "full-size")
#' full_avatar3 <- get_avatars("123456789", image_type = "full size")
#' }
#'
#' @export
get_avatars <- function(avatar_id, image_type = NULL, save_path = "outputs/") {
  # Fetch avatar from Sleeper API
  if (is.null(image_type) || image_type %in% c("full", "full-size", "full size")) {
    # Full Size Avatar
    url <- glue::glue("https://sleepercdn.com/avatars/{avatar_id}")
    # Log the URL being fetched
    otis::log_info(glue::glue("Fetching full-size avatar from endpoint: {url}"))
  } else if (image_type %in% c("thumb", "thumbnail")) {
    # Thumbnail Avatar
    url <- glue::glue("https://sleepercdn.com/avatars/thumbs/{avatar_id}")
    # Log the URL being fetched
    otis::log_info(glue::glue("Fetching thumbnail avatar from endpoint: {url}"))
  } else {
    # Invalid image type
    otis::log_info(glue::glue("Avatar image_type == '{image_type}' not found!"))
    otis::log_info("Try full, full-size, full size or NULL for a full-size avatar.")
    otis::log_info("Try thumb or thumbnail for a thumbnail avatar.")
    stop("Stopping. Avatar image type not found.")
  }
  # Make the GET request
  response <- httr::GET(url)
  # If not successful, stop and return error message
  otis::status_sleeper_api(response)
  # Extract raw content (avatars are binary image data, not JSON)
  otis::log_info("Extracting binary content from avatar request.")
  binary_content <- httr::content(response, "raw")
  if (!is.null(save_path)) {
    # Save the binary content to a file
    writeBin(binary_content, glue::glue("{save_path}/avatar_{image_type}_{avatar_id}.png"))
    otis::log_info(glue::glue("Avatar saved to {save_path}/avatar_{image_type}_{avatar_id}.png"))
  }
  else {
    otis::log_info("No save path provided. Avatar not saved to file.")
  }
  # Return the raw binary content
  return(binary_content)
}
