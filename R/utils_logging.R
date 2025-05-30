#' Logging Utilities
#'
#' Logging configuration and functions utilized in package development

#' @importFrom glue glue

# Global log level setting (can be controlled via options)
.get_log_level <- function() {
  level <- getOption("mypackage.log_level", "INFO")
  return(toupper(level))
}

# Log level hierarchy
.log_levels <- list(
  "DEBUG" = 1,
  "INFO" = 2,
  "WARN" = 3,
  "ERROR" = 4
)

#' Internal logging function
#'
#' @param level Character string indicating log level
#' @param message Character string with the log message (can contain glue syntax)
#' @param .envir Environment for glue evaluation
.log_message <- function(level, message, .envir = parent.frame()) {
  current_level <- .get_log_level()

  # Only log if the message level is >= current log level
  if (.log_levels[[level]] >= .log_levels[[current_level]]) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    # Use glue for message formatting
    formatted_message <- glue::glue(message, .envir = .envir)

    # Color coding for different levels (if supported)
    level_colors <- list(
      "DEBUG" = "\033[36m",   # Cyan
      "INFO" = "\033[32m",    # Green
      "WARN" = "\033[33m",    # Yellow
      "ERROR" = "\033[31m"    # Red
    )
    reset_color <- "\033[0m"

    # Check if colors are supported
    use_colors <- getOption("mypackage.use_colors",
                            interactive() && .Platform$OS.type != "windows")

    if (use_colors && level %in% names(level_colors)) {
      color_code <- level_colors[[level]]
      log_line <- glue::glue("{color_code}[{timestamp}] {level}: {formatted_message}{reset_color}")
    } else {
      log_line <- glue::glue("[{timestamp}] {level}: {formatted_message}")
    }

    # Use appropriate output function based on level
    if (level == "ERROR") {
      stop(log_line, call. = FALSE)
    } else if (level == "WARN") {
      warning(log_line, call. = FALSE, immediate. = TRUE)
    } else {
      message(log_line)
    }
  }
}

#' Log debug message
#'
#' @param message Character string with the log message (can use glue syntax)
#' @param .envir Environment for glue evaluation
#' @export
log_debug <- function(message, .envir = parent.frame()) {
  .log_message("DEBUG", message, .envir = .envir)
}

#' Log info message
#'
#' @param message Character string with the log message (can use glue syntax)
#' @param .envir Environment for glue evaluation
#' @export
log_info <- function(message, .envir = parent.frame()) {
  .log_message("INFO", message, .envir = .envir)
}

#' Log warning message
#'
#' @param message Character string with the log message (can use glue syntax)
#' @param .envir Environment for glue evaluation
#' @export
log_warn <- function(message, .envir = parent.frame()) {
  .log_message("WARN", message, .envir = .envir)
}

#' Log error message (will stop execution)
#'
#' @param message Character string with the log message (can use glue syntax)
#' @param .envir Environment for glue evaluation
#' @export
log_error <- function(message, .envir = parent.frame()) {
  .log_message("ERROR", message, .envir = .envir)
}

#' Set logging level
#'
#' @param level Character string: "DEBUG", "INFO", "WARN", or "ERROR"
#' @export
set_log_level <- function(level) {
  level <- toupper(level)
  if (!level %in% names(.log_levels)) {
    stop(glue::glue("Invalid log level. Must be one of: {paste(names(.log_levels), collapse = ', ')}"))
  }
  options("mypackage.log_level" = level)
  log_info("Log level set to {level}")
}

#' Enable or disable colored logging output
#'
#' @param use_colors Logical indicating whether to use colors
#' @export
set_log_colors <- function(use_colors = TRUE) {
  options("mypackage.use_colors" = use_colors)
  status <- if(use_colors) "enabled" else "disabled"
  log_info("Color logging {status}")
}
