# 1. Central list of all color palettes. This is not exported.
jfi_palettes <- list(
  jfi_discrete = c("#5E31FA", "#42C2DB", "#005d5d", "#B03A5B", "#7B0920"),
  jfi_sequential = c("#D6DCEE", "#5E31FA"),
  jfi_divergent = c("#F9E600", "#B38780", "#652eff"),

  pw_blue_discrete = c("#3F6BEE", "#E61F00", "#42C2DB", "#30BF5F", "#FAB531"),
  pw_blue_sequential = c("#D6DCEE", "#3F6BEE"),
  pw_blue_divergent = c("#3F6BEE", "#9F4584", "#E61F00"),

  pw_red_sequential = c("#E6D2CF", "#E61F00")
)

# 2. A single, non-exported helper function to generate the scales.
# This function acts as a "factory" for the final scale functions.
jfi_scale_builder <- function(aesthetic, palette = "jfi", type = "discrete", ...) {

  # Construct the palette name and retrieve the colors
  pal_name <- paste(palette, type, sep = "_")
  pal <- jfi_palettes[[pal_name]]

  if (is.null(pal)) {
    stop("Invalid palette name or type specified.", call. = FALSE)
  }

  # Return the appropriate scale function
  if (type == "discrete") {
    ggplot2::discrete_scale(aesthetic, pal_name, palette = function(n) rep(pal, length.out = n), ...)
  } else { # For "sequential" or "divergent"
    ggplot2::scale_color_gradientn(colours = pal, aesthetics = aesthetic, ...)
  }
}

#' JFI discrete color scale
#'
#' @param palette The color palette to use: 'jfi' or 'pw_blue'.
#' @param ... Other arguments passed to `ggplot2::discrete_scale()`.
#' @export
jfi_scale_color_d <- function(palette = "jfi", ...) {
  jfi_scale_builder("color", palette, type = "discrete", ...)
}

#' JFI discrete fill scale
#'
#' @param palette The color palette to use: 'jfi' or 'pw_blue'.
#' @param ... Other arguments passed to `ggplot2::discrete_scale()`.
#' @export
jfi_scale_fill_d <- function(palette = "jfi", ...) {
  jfi_scale_builder("fill", palette, type = "discrete", ...)
}

#' JFI continuous color scale (sequential or divergent)
#'
#' @param palette The color palette to use: 'jfi', 'pw_blue', or 'pw_red'.
#' @param divergent If `TRUE`, a divergent palette is used.
#' @param ... Other arguments passed to `ggplot2::scale_color_gradientn()`.
#' @export
jfi_scale_color_c <- function(palette = "jfi", divergent = FALSE, ...) {
  type <- if (divergent) "divergent" else "sequential"
  jfi_scale_builder("color", palette, type = type, ...)
}

#' JFI continuous fill scale (sequential or divergent)
#'
#' @param palette The color palette to use: 'jfi', 'pw_blue', or 'pw_red'.
#' @param divergent If `TRUE`, a divergent palette is used.
#' @param ... Other arguments passed to `ggplot2::scale_fill_gradientn()`.
#' @export
jfi_scale_fill_c <- function(palette = "jfi", divergent = FALSE, ...) {
  type <- if (divergent) "divergent" else "sequential"
  jfi_scale_builder("fill", palette, type = type, ...)
}
