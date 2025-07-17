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
#' \dontrun{
#' ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
#'   ggplot2::geom_point() +
#'   jfi_theme()
#' }
jfi_theme <- function(
    legend_right = FALSE,
    base_size = 11,
    base_family = "melior",
    base_line_size = base_size / 170,
    base_rect_size = base_size / 170
) {
  half_line <- base_size/2
  grid_line_color <- "grey70"
  grid_line_size <- 0.2

  # Legend positioning
  if (legend_right) {
    spec_legend_position <- "right"
    spec_legend_justification <- "center"
    spec_legend_direction <- "vertical"
  } else {
    spec_legend_position <- "top"
    spec_legend_justification <- "right"
    spec_legend_direction <- "horizontal"
  }

  legend_box_spacing_spec <- if (legend_right) ggplot2::unit(2 * half_line, "pt") else ggplot2::unit(2.5, "pt")

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
        size = ggplot2::rel(1.6),
        hjust = 0,
        margin = ggplot2::margin(b = 25)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        margin = ggplot2::margin(t = -19, b = 25)
      ),
      plot.tag.position = "bottom", plot.tag.location = "plot",
      plot.tag = ggplot2::element_text(
        family = "roboto-mono",
        size = ggplot2::rel(2.2),
        color = "#B0B0B0",
        hjust = 1,
        vjust = .72, margin = ggplot2::margin(t = 15, b = 5)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.65),
        hjust = 0, vjust = 0,
        color = "black",
        lineheight = 1.1,
        margin = ggplot2::margin(t = 17, b = -10) # Space above caption
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12, b = 2), hjust = .5,
                                           color = "black", size = base_size, family = base_family),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8), hjust = .5, angle = 90,
                                           color = "black", size = base_size, family = base_family),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = .5), size = base_size*.95,
                                          color = "black", family = base_family),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = .5), size = base_size*.95,
                                          color = "black", family = base_family),
      axis.line = ggplot2::element_line(
        color = grid_line_color,
        size = grid_line_size + .35
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
      legend.justification = spec_legend_justification,
      legend.box.spacing = legend_box_spacing_spec,
      legend.spacing.x = ggplot2::unit(2, "cm"),
      legend.title = ggplot2::element_text(hjust = .5, size = ggplot2::rel(1.1),
                                           margin = ggplot2::margin(-4, 0, 2, 0)),
      legend.title.position = "top",
      legend.text = ggplot2::element_text(hjust = 0),
      legend.margin = ggplot2::margin(0, 0, 10, 0),
      legend.background = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      strip.text = ggplot2::element_text(margin = ggplot2::margin(0,0,6,0), size = 10, hjust = 0.02),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(15, 15, 15, 15),
      complete = TRUE
    )
}



#' Set JFI ggplot2 Defaults
#'
#' \code{jfi_set_default} sets the default ggplot2 theme, font, and geom aesthetics for the R session.
#'
#' @param base_size The base font size for the theme. All fonts are relative to this value.
#' @param base_family The base font family for the theme.
#' @param base_line_size The base line size for the theme. All line sizes are relative to this value.
#' @param base_rect_size The base rect size for the theme. All rect sizes are relative to this value.
#'
#'
#' @export
jfi_set_default <- function(base_size = 11,
                            base_family = "melior",
                            base_line_size = base_size / 170,
                            base_rect_size = base_size / 170) {

  options(
    ggplot2.continuous.color = "gradient",
    ggplot2.continuous.fill = "gradient"
  )

  ggplot2::update_geom_defaults("text", list(family = base_family))
  ggplot2::update_geom_defaults("label", list(family = base_family))

  # Set default aesthetics for common geoms
  ggplot2::update_geom_defaults("bar", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("col", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("point", list(color = "#5E31FA", size = 3))
  ggplot2::update_geom_defaults("line", list(color = "#5E31FA", size = 1))
  ggplot2::update_geom_defaults("step", list(color = "#5E31FA", size = 1))
  ggplot2::update_geom_defaults("path", list(color = "#5E31FA", size = 1))
  ggplot2::update_geom_defaults("boxplot", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("density", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("area", list(fill = "#5E31FA", alpha = 0.8))
  ggplot2::update_geom_defaults("ribbon", list(fill = "#5E31FA", alpha = 0.8))
  ggplot2::update_geom_defaults("polygon", list(fill = "#5E31FA", color = "#5E31FA", size = 1))
  ggplot2::update_geom_defaults("violin", list(fill = "#5E31FA"))
  ggplot2::update_geom_defaults("sf", list(fill = "#5E31FA", color = "white", size = 0.1))

  # Set defaults for stats
  ggplot2::update_stat_defaults("count", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("boxplot", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("density", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("summary", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("ydensity", list(fill = "#5E31FA"))
  ggplot2::update_stat_defaults("smooth", list(color = "#5E31FA", size = 1))

}
