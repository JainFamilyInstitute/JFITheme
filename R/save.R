#' Save a JFI-themed ggplot with flexible sizing
#'
#' A wrapper around `ggplot2::ggsave()` with presets for common plot sizes
#' and the ability to specify custom dimensions. It automatically handles
#' DPI synchronization to ensure fonts are scaled correctly.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to the last plot displayed.
#' @param size A character string specifying the size preset. Can be one of
#'   `"small"`, `"medium"`, `"large"`, or `"custom"`. Defaults to `"medium"`.
#' @param width The width of the plot. Only used if `size = "custom"`.
#' @param height The height of the plot. Only used if `size = "custom"`.
#' @param dpi The plot resolution in dots per inch (DPI). Defaults to 300.
#' @param ... Other arguments passed on to `ggplot2::ggsave()`.
#'
#' @export
#' @examples
#' \dontrun{
#' my_plot <- jfi_plot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#'
#' # Save a medium-sized plot (default)
#' jfi_save("my_plot_medium.png", plot = my_plot)
#'
#' # Save a small plot
#' jfi_save("my_plot_small.png", plot = my_plot, size = "small")
#'
#' # Save a plot with custom dimensions
#' jfi_save("my_plot_custom.png", plot = my_plot, size = "custom", width = 8, height = 4)
#' }
jfi_save <- function(filename, plot = ggplot2::last_plot(), size = "medium",
                     width = NA, height = NA, dpi = 300, ...) {

  # Define the dimensions based on the size preset
  sizes <- list(
    small = list(width = 3.5, height = 3),
    medium = list(width = 7, height = 5),
    large = list(width = 10, height = 8),
    donut = list(width = 5.98, height = 6.5)
  )

  size <- match.arg(size, c("small", "medium", "large", "donut", "custom"))

  if (size == "custom") {
    if (is.na(width) || is.na(height)) {
      stop("For size = 'custom', you must provide numeric values for width and height.", call. = FALSE)
    }
    plot_dims <- list(width = width, height = height)
  } else {
    plot_dims <- sizes[[size]]
  }

  # --- DPI Synchronization Logic ---
  # Get the original DPI from showtext to restore it later
  original_dpi <- showtext::showtext_opts()$dpi
  # Set the DPI for showtext to match the ggsave DPI
  showtext::showtext_opts(dpi = dpi)
  # Ensure the original DPI is restored when the function exits
  on.exit(showtext::showtext_opts(dpi = original_dpi), add = TRUE)

  message(paste0("Saving '", filename, "' (", plot_dims$width, "\" x ", plot_dims$height, "\") with ", dpi, " DPI."))

  # Call the original ggsave function
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = plot_dims$width,
    height = plot_dims$height,
    dpi = dpi,
    device = ragg::agg_png,
    units = "in",
    ...
  )

  invisible(filename)
}
