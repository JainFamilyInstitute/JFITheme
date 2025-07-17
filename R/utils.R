# Helper function to calculate color luminance. This should be an internal
get_luminance <- function(hex_color) {
  # Convert hex to RGB
  rgb_vals <- grDevices::col2rgb(hex_color)
  # Calculate luminance using a standard formula
  luminance <- (0.299 * rgb_vals["red",] + 0.587 * rgb_vals["green",] + 0.114 * rgb_vals["blue",]) / 255
  return(luminance)
}

# Helper function to format labels based on user input.
format_labels <- function(value, type = "number", scale = 1, suffix = "", accuracy = 1) {
  formatter <- switch(
    type,
    "number" = scales::label_comma(scale = scale, suffix = suffix, accuracy = accuracy),
    "percent" = scales::label_percent(scale = 100, suffix = "%", accuracy = accuracy),
    "dollar" = scales::label_dollar(scale = scale, suffix = suffix, accuracy = accuracy)
  )
  formatter(value)
}

#' @importFrom dplyr %>%
NULL

#' @importFrom lubridate year
#' @importFrom tidyr pivot_longer
#' @importFrom ggtext element_markdown
NULL
