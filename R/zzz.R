# Load necessary libraries
library(showtext)
library(ggtext)

options(encoding = "UTF-8")

#' Custom Font Registration for JFITheme
#'
#' Registers custom fonts for the JFITheme package.
#'
#' @export
#' @examples
#' custom_font()
custom_font <- function() {
  sysfonts::font_add("melior",
                     regular = system.file("fonts", "MeliorLTPro.otf", package = "JFITheme"),
                     bold = system.file("fonts", "MeliorLTPro-Bold.otf", package = "JFITheme"),
                     italic = system.file("fonts", "MeliorLTPro-Italic.otf", package = "JFITheme"),
                     bolditalic = system.file("fonts", "MeliorLTPro-BoldItalic.otf", package = "JFITheme"))

  sysfonts::font_add("roboto-mono",
                     regular = system.file("fonts", "RobotoMono-Medium.ttf", package = "JFITheme"),
                     bold = system.file("fonts", "RobotoMono-Bold.ttf", package = "JFITheme"))

  showtext::showtext_auto()

}

#' Caption with data source and/or note
#'
#' Generate a caption with optional source and note.
#'
#' @param source Optional source text.
#' @param note Optional note text.
#' @return A character string containing the formatted caption.
#'
#' @export
#' @examples
#' source_note(source = "Data Source", note = "Additional Note")
source_note <- function(source = NULL, note = NULL) {
  details <- c()
  if (!is.null(note)) {
    details <- c(details, paste0("Note: ", note))
  }
  if (!is.null(source)) {
    details <- c(details, paste0("Source: ", source))
  }

  # Return a single string with details separated by a newline
  paste(details, collapse = "\n")
}



# Automatically run custom_font() when the package is loaded
.onLoad <- function(libname, pkgname) {
  # Try-catch to ensure error handling if showtext is unavailable
  tryCatch({
    requireNamespace("showtext", quietly = TRUE)
    custom_font()
  }, error = function(e) {
    warning("JFITheme font registration failed: ", e$message, call. = FALSE)
  })

  # Set JFI defaults (theme, geoms, scales)
  jfi_set_default()
}

