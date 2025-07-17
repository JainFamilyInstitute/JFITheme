#' A ggplot2 theme for maps in the JFI style
#'
#' `jfi_theme_map()` builds on top of `jfi_theme()` to create a theme that is
#' optimized for spatial data visualizations. It removes all non-essential
#' plot elements like axis lines, text, ticks, and panel grids.
#'
#' @param ... Arguments passed on to `jfi_theme()`.
#'
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#'
#' ggplot(data = nc) +
#'   geom_sf(aes(fill = AREA)) +
#'   theme_jfi_map()
#' }
jfi_theme_map <- function(...) {
  jfi_theme(...) +
    ggplot2::theme(
      # Remove all axis elements
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),

      # Remove all panel grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Create a ggplot for maps with JFI Theme
#'
#' `jfi_map()` wraps around `ggplot()` to automatically apply the `jfi_theme_map()`
#' and insert a default tag and caption if none are provided. It is the ideal
#' starting point for creating maps.
#'
#' @param ... Arguments passed to `ggplot()`, such as `data` and `aes()`.
#'
#' @return A `ggplot` object with the JFI map theme.
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#'
#' jfi_map(data = nc) +
#'   geom_sf(aes(fill = AREA)) +
#'   labs(
#'     title = "Map of North Carolina Counties",
#'     subtitle = "Area of each county",
#'     caption = source_note(source = "U.S. Census Bureau")
#'   )
#' }
jfi_map <- function(...) {
  p <- ggplot2::ggplot(...)

  # Automatically add the JFI tag and a default caption if not already set
  # This allows the user to still override them with + labs()
  if (is.null(p$labels$tag)) {
    p$labels$tag <- "JFI"
  }
  if (is.null(p$labels$caption)) {
    p$labels$caption <- source_note()
  }

  p + jfi_theme_map()
}
