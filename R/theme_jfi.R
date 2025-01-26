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



# Automatically run custom_font() when the package is loaded
.onLoad <- function(libname, pkgname) {
  # Try-catch to ensure error handling if showtext is unavailable
  tryCatch({
    requireNamespace("showtext", quietly = TRUE)
    custom_font()
  }, error = function(e) {
    message("Error loading custom fonts: ", e$message)
  })

  # Set JFI defaults (theme, geoms, scales)
  set_jfi_defaults()
}


# Define the color palettes
jfi_discrete_colors <- c("#5E31FA", "#42C2DB", "#005d5d", "#B03A5B", "#7B0920")
pw_discrete_colors <- c("#3F6BEE", "#E61F00", "#42C2DB", "#30BF5F", "#FAB531")

#' JFI Color Scale
#'
#' Custom color scale based on the JFI style guide.
#'
#' @param type The type of scale: 'discrete', 'divergent', 'sequential', 'sequential_discrete'.
#' @param palette The color palette to use: 'jfi', 'pw_blue', 'pw_red'.
#' @param midpoint The midpoint for the divergent scale (if applicable).
#' @param discrete_values The discrete values for the sequential discrete scale (if applicable).
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_jfi(type = "discrete", palette = "jfi")
scale_color_jfi <- function(type = c("discrete", "divergent", "sequential", "sequential_discrete"),
                            palette = c("jfi", "pw_blue", "pw_red"),
                            midpoint = NULL,
                            discrete_values = NULL) {
  type <- match.arg(type)
  palette <- match.arg(palette)

  if (palette == "jfi") {
    discrete_colors <- jfi_discrete_colors
    divergent_colors <- c("#F9E600", "#B38780", "#652eff")
    sequential_colors <- c("#D6DCEE", "#5E31FA")
  } else if (palette == "pw_blue") {
    discrete_colors <- pw_discrete_colors
    divergent_colors <- c("#3F6BEE", "#9F4584", "#E61F00")
    sequential_colors <- c("#D6DCEE", "#3F6BEE")
  } else if (palette == "pw_red") {
    discrete_colors <- pw_discrete_colors
    divergent_colors <- c("#3F6BEE", "#9F4584", "#E61F00")
    sequential_colors <- c("#E6D2CF", "#E61F00")
  }

  if (type == "discrete") {
    scale_color_manual(values = discrete_colors)
  } else if (type == "divergent") {
    if (is.null(midpoint)) {
      stop("Please provide a midpoint for the divergent scale.")
    }
    scale_color_gradient2(low = divergent_colors[1], mid = divergent_colors[2], high = divergent_colors[3], midpoint = midpoint)
  } else if (type == "sequential") {
    scale_color_gradient(low = sequential_colors[1], high = sequential_colors[2])
  } else if (type == "sequential_discrete") {
    if (is.null(discrete_values)) {
      stop("Please provide discrete values for the sequential discrete scale.")
    }
    n <- length(unique(discrete_values))
    scale_color_manual(values = colorRampPalette(sequential_colors)(n))
  }
}

#' JFI Fill Scale
#'
#' Custom fill scale based on the JFI style guide.
#'
#' @param type The type of scale: 'discrete', 'divergent', 'sequential', 'sequential_discrete'.
#' @param palette The color palette to use: 'jfi', 'pw_blue', 'pw_red'.
#' @param midpoint The midpoint for the divergent scale (if applicable).
#' @param discrete_values The discrete values for the sequential discrete scale (if applicable).
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_jfi(type = "discrete", palette = "jfi")
scale_fill_jfi <- function(type = c("discrete", "divergent", "sequential", "sequential_discrete"),
                           palette = c("jfi", "pw_blue", "pw_red"),
                           midpoint = NULL,
                           discrete_values = NULL) {
  type <- match.arg(type)
  palette <- match.arg(palette)

  if (palette == "jfi") {
    discrete_colors <- jfi_discrete_colors
    divergent_colors <- c("#F9E600", "#B38780", "#652eff")
    sequential_colors <- c("#D6DCEE", "#5E31FA")
  } else if (palette == "pw_blue") {
    discrete_colors <- pw_discrete_colors
    divergent_colors <- c("#3F6BEE", "#9F4584", "#E61F00")
    sequential_colors <- c("#D6DCEE", "#3F6BEE")
  } else if (palette == "pw_red") {
    discrete_colors <- pw_discrete_colors
    divergent_colors <- c("#3F6BEE", "#9F4584", "#E61F00")
    sequential_colors <- c("#E6D2CF", "#E61F00")
  }

  if (type == "discrete") {
    scale_fill_manual(values = discrete_colors)
  } else if (type == "divergent") {
    if (is.null(midpoint)) {
      stop("Please provide a midpoint for the divergent scale.")
    }
    scale_fill_gradient2(low = divergent_colors[1], mid = divergent_colors[2], high = divergent_colors[3], midpoint = midpoint)
  } else if (type == "sequential") {
    scale_fill_gradient(low = sequential_colors[1], high = sequential_colors[2])
  } else if (type == "sequential_discrete") {
    if (is.null(discrete_values)) {
      stop("Please provide discrete values for the sequential discrete scale.")
    }
    n <- length(unique(discrete_values))
    scale_fill_manual(values = colorRampPalette(sequential_colors)(n))
  }
}


#’ Caption with Logo for JFI
#’
#’ Generate a caption with the JFI logo and optional source and note.
#’
#’ @param source Optional source text.
#’ @param note Optional note text.
#’ @return A character string containing the formatted caption.
#’ @export
#’ @examples
#’ caption_w_logo(source = “Data Source”, note = “Additional Note”)
caption_w_logo <- function(source = NULL, note = NULL) {
  # Second element: concatenate source and note
  second_element <- NULL

  if (!is.null(note)) {
    second_element <- paste0("Note: ", note)
  }

  if (!is.null(source)) {
    if (!is.null(note)) {
      second_element <- paste0(second_element, "\n")  # Add newline if note exists
    }
    second_element <- paste0(second_element, "Source: ", source)
  }

  if (!is.null(second_element)) {
    caption <- c("JFI\n")
  }

  # Ensure second element is not empty
  if (is.null(second_element)) {
    second_element <- ""
    caption <- c("JFI")
  }
  # Return a vector of two elements: "JFI" and the concatenated source/note
  caption <- c(caption, second_element)

  return(caption)
}


#' Custom JFI Theme
#'
#' A custom ggplot2 theme based on the JFI style guide.
#'
#' @param legend_right Logical, should the legend be on the right?
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param base_line_size Base line size.
#' @param base_rect_size Base rectangle size.
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_jfi()
#'
theme_jfi <- function(
    legend_right = FALSE,
    base_size = 8,
    base_family = "melior",
    base_line_size = base_size / 170,
    base_rect_size = base_size / 170
) {
  half_line <- base_size/2
  grid_line_color <- "grey70"
  grid_line_size <- 0.2

  # Legend positioning
  spec_legend_position <- if (legend_right) "right" else "top"
  spec_legend_direction <- if (legend_right) "vertical" else "horizontal"
  legend_box_spacing_spec <- if (legend_right) ggplot2::unit(2 * half_line, "pt") else ggplot2::unit(0, "char")

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family, base_line_size = base_line_size) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "black", size = base_size, family = base_family),
      plot.background = ggplot2::element_rect(
        fill = "#F9F9F9",
        color = NA
      ), # background color, no border color
      panel.background = ggplot2::element_rect(
        fill = "#F9F9F9",
        color = NA
      ), # background color, no border color
      plot.title = ggplot2::element_text(
        size = 14,
        hjust = 0,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        margin = ggplot2::margin(b = 15)
      ),
      plot.caption = ggplot2::element_text(size = c(20, 7), family = c("roboto-mono", base_family),
                                           color = c("#B0B0B0", "black"), hjust = c(0,0), vjust = c(0,0), margin = ggplot2::margin(t = 4)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6, b = 0), hjust = .5, color = "black", size = base_size, family = base_family),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6), hjust = .5, angle = 90, color = "black", size = base_size, family = base_family),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = .75), color = "black", size = base_size, family = base_family),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = .75), color = "black", size = base_size, family = base_family),
      axis.line = ggplot2::element_line(
        color = grid_line_color,
        size = grid_line_size + .25
      ),
      axis.ticks = ggplot2::element_line(
        color = grid_line_color,
        size = grid_line_size
      ),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(0.3,"char"),
      panel.grid.major.y = ggplot2::element_line(
        color = grid_line_color,
        size = grid_line_size
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(1.25, "lines"),
      panel.spacing.y = ggplot2::unit(.5, "lines"),
      legend.position = spec_legend_position,
      legend.direction = spec_legend_direction,
      legend.title = ggplot2::element_text(hjust = .98),
      legend.title.position = "top",
      legend.spacing.x = ggplot2::unit(1, "char"),
      legend.text = ggplot2::element_text(hjust = 0),
      legend.margin = ggplot2::margin(b = 6),
      legend.box.spacing = legend_box_spacing_spec,
      legend.background = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.justification.top = "right",
      plot.title.position = "plot",
      plot.caption.position = "plot",
      strip.text = ggplot2::element_text(margin = ggplot2::margin(0,0,6,0), size = 10, hjust = 0.02),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(15, 15, 15, 15),
      complete = TRUE
    )
}

#' Custom JFI Map Theme
#'
#' A simplified ggplot2 theme for maps, removing gridlines, otherwise keeping everything the same as `theme_jfi()`.
#'
#' @param legend_right Logical, should the legend be on the right?
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param base_line_size Base line size.
#' @param base_rect_size Base rectangle size.
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' library(ggplot2)
#' # Load map data for the US
#' us_map <- ggplot2::map_data("state")
#'
#' # Plot the US map
#' ggplot(us_map, aes(long, lat, group = group)) +
#'   geom_polygon(fill = "#D6DCEE", color = "white") +
#'   coord_fixed(1.3) +  # Fix aspect ratio
#'   theme_jfi_map()
#'
theme_jfi_map <- function(
    legend_right = FALSE,
    base_size = 8,
    base_family = "melior",
    base_line_size = base_size / 170,
    base_rect_size = base_size / 170
) {
  half_line <- base_size / 2
  grid_line_color <- "grey70"
  grid_line_size <- 0.2

  # Legend positioning
  spec_legend_position <- if (legend_right) "right" else "top"
  spec_legend_direction <- if (legend_right) "vertical" else "horizontal"
  legend_box_spacing_spec <- if (legend_right) ggplot2::unit(2 * half_line, "pt") else ggplot2::unit(0, "char")

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family, base_line_size = base_line_size) +
    ggplot2::theme(
      # Modified elements specific to maps
      axis.line = ggplot2::element_blank(),         # Remove axis lines
      axis.text = ggplot2::element_blank(),         # Remove axis text
      axis.ticks = ggplot2::element_blank(),        # Remove axis ticks
      panel.grid = ggplot2::element_blank(),        # Remove gridlines
      panel.border = ggplot2::element_blank(),      # Remove panel border
      plot.background = ggplot2::element_rect(fill = "#fffdfc", color = NA),  # Ensuring white background
      panel.background = ggplot2::element_rect(fill = "#fffdfc", color = NA), # Ensuring white background

      # Original elements retained from theme_jfi
      text = ggplot2::element_text(color = "black", size = base_size, family = base_family),
      plot.title = ggplot2::element_text(size = 14, hjust = 0, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(b = 15)),
      plot.caption = ggplot2::element_text(size = c(20, 7), family = c("roboto-mono", base_family),
                                           color = c("#B0B0B0", "black"), hjust = c(0, 0), vjust = c(0, 0), margin = ggplot2::margin(t = 4)),
      #axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6, b = 0), hjust = .5, color = "black", size = base_size, family = base_family),
      #axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6), hjust = .5, angle = 90, color = "black", size = base_size, family = base_family),
      #axis.ticks.length = ggplot2::unit(0.3, "char"),
      legend.position = spec_legend_position,
      legend.direction = spec_legend_direction,
      legend.title = ggplot2::element_text(hjust = .98),
      legend.title.position = "top",
      legend.spacing.x = ggplot2::unit(1, "char"),
      legend.text = ggplot2::element_text(hjust = 0),
      legend.margin = ggplot2::margin(b = 6),
      legend.box.spacing = legend_box_spacing_spec,
      legend.background = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      plot.margin = ggplot2::margin(15, 15, 15, 15),
      complete = TRUE
    )
}

#' Set JFI ggplot2 Defaults
#'
#' \code{set_jfi_defaults} sets the default ggplot2 theme, font, and geom aesthetics for the R session.
#'
#' @param base_size The base font size for the theme. All fonts are relative to this value.
#' @param base_family The base font family for the theme.
#' @param base_line_size The base line size for the theme. All line sizes are relative to this value.
#' @param base_rect_size The base rect size for the theme. All rect sizes are relative to this value.
#'
#' @import ggrepel
#'
#' @export
set_jfi_defaults <- function(base_size = 8,
                             base_family = "melior",
                             base_line_size = base_size / 170,
                             base_rect_size = base_size / 170,
                             scale = "continuous") {


  # Set default font for text and label
  ggplot2::update_geom_defaults("text", list(family = base_family, size = 1 / 0.352777778))
  ggplot2::update_geom_defaults("label", list(family = base_family, size = 1 / 0.352777778))

  # Conditionally set defaults for ggrepel if available
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggplot2::update_geom_defaults("text_repel", list(family = base_family, size = 1 / 0.352777778))
    ggplot2::update_geom_defaults("label_repel", list(family = base_family, size = 1 / 0.352777778))
  } else {
    message("ggrepel is not installed. Skipping defaults for text_repel and label_repel.")
  }

  options(
    ggplot2.continuous.colour = "gradient",
    ggplot2.continuous.fill = "gradient"
  )

  # Set default aesthetics for common geoms
  ggplot2::update_geom_defaults("bar", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("col", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("point", list(colour = "#5E31FA", size = 3))
  ggplot2::update_geom_defaults("line", list(colour = "#5E31FA", size = 1))
  ggplot2::update_geom_defaults("step", list(colour = "#5E31FA", size = 1))
  ggplot2::update_geom_defaults("path", list(colour = "#5E31FA", size = 1))
  ggplot2::update_geom_defaults("boxplot", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("density", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("area", list(fill = "#5E31FA", alpha = 0.8))
  ggplot2::update_geom_defaults("ribbon", list(fill = "#5E31FA", alpha = 0.8))
  ggplot2::update_geom_defaults("polygon", list(fill = "#5E31FA", colour = "#5E31FA", size = 1))
  ggplot2::update_geom_defaults("violin", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("sf", list(fill = "#5E31FA", color = "white", size = 0.1))

  # Set defaults for stats
  ggplot2::update_stat_defaults("count", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("boxplot", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("density", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("summary", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("ydensity", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("smooth", list(colour = "#5E31FA", size = 1))

}

#' Create a ggplot with JFI Theme and Default Caption
#'
#' `jfi_plot` wraps around `ggplot()` to automatically apply the JFI custom theme and insert a default caption if none is provided.
#'
#' @param ... Arguments passed to `ggplot()`, such as `data` and `aes()`.
#'
#' @return A `ggplot` object with the JFI theme and default or custom caption.
#'
#' @examples
#' jfi_plot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#'
#' jfi_plot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(caption = caption_w_logo(source = "Data Source"))
#' @export
jfi_plot <- function(...) {
  p <- ggplot(...)

  # Automatically add caption if not provided
  if (is.null(p$labels$caption)) {
    p <- p + labs(caption = caption_w_logo())
  }
  p + theme_jfi()
}
