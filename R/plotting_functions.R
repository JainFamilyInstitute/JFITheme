#' Create a ggplot with JFI Theme and Default Caption
#'
#' `jfi_plot` wraps around `ggplot()` to automatically apply the JFI custom theme and insert a default caption if none is provided.
#' @param ... Arguments passed to `ggplot()`, such as `data` and `aes()`.
#' @return A `ggplot` object with the JFI theme and default or custom caption.
#' @export
#' @examples
#' \dontrun{
#' jfi_plot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
#'   ggplot2::geom_point()
#'
#' jfi_plot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(caption = source_note(source = "Data Source"))
#' }
jfi_plot <- function(...) {
  p <- ggplot2::ggplot(...)

  # Automatically add the JFI tag and a default caption if not already set
  # This allows the user to still override them with + labs()
  if (is.null(p$labels$tag)) {
    p$labels$tag <- "JFI"
  }
  if (is.null(p$labels$caption)) {
    p$labels$caption <- source_note()
  }

  p + jfi_theme()
}



#' Create a JFI-styled scatter plot
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping.
#' @param ... Other arguments passed to geom_point().
#' @export
jfi_scatter <- function(data, mapping, ...) {
  jfi_plot(data, mapping) +
    ggplot2::geom_point(size = 1.25, alpha = .95,...) +
    ggplot2::scale_y_continuous(n.breaks = 8, expand = ggplot2::expansion(mult = c(0.02, 0.02)), ...)
}


#' Create a JFI-styled line plot
#'
#' A wrapper that combines `jfi_plot()` and `ggplot2::geom_line()` to quickly
#' create styled line charts.
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping.
#' @param ... Other arguments passed on to `ggplot2::geom_line()`.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
#'   value = c(12, 18, 15)
#' )
#' jfi_line(df, aes(x = date, y = value))
#' }
jfi_line <- function(data, mapping, ...) {
  jfi_plot(data, mapping) +
    ggplot2::geom_line(linewidth = .8, alpha = .95,...) +
    ggplot2::scale_y_continuous(n.breaks = 8, expand = ggplot2::expansion(mult = c(0.02, 0.02)), ...)
}

#' Create a JFI-styled point-line plot
#'
#' A wrapper that combines `jfi_plot()`, `ggplot2::geom_line()`, and
#' `ggplot2::geom_point()` to quickly create styled point-line charts.
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping.
#' @param ... Other arguments passed on to `ggplot2::geom_line()` and `ggplot2::geom_point()`.
#'
#' @export
#' @examples
#' \dontrun{
#' jfi_point_line(economics, aes(x = date, y = psavert))
#' }
jfi_point_line <- function(data, mapping, ...) {
  jfi_plot(data, mapping) +
    ggplot2::geom_line(linewidth = .6, alpha = .9,...) +
    ggplot2::geom_point(size = 1.5, alpha = .95,...) +
    ggplot2::scale_y_continuous(n.breaks = 8, expand = ggplot2::expansion(mult = c(0.02, 0.02)), ...)
}

#' Create a JFI-styled histogram
#'
#' A wrapper that combines `jfi_plot()` and `ggplot2::geom_histogram()` to
#' quickly create styled histograms.
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping.
#' @param ... Other arguments passed on to `ggplot2::geom_histogram()`.
#'
#' @export
#' @examples
#' \dontrun{
#' jfi_histogram(mpg, aes(x = hwy))
#' }
jfi_histogram <- function(data, mapping, ...) {
  jfi_plot(data, mapping) +
    ggplot2::geom_histogram(...) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::theme(
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.title = ggplot2::element_text(hjust = .5),
      legend.text = ggplot2::element_text(hjust = .25)
    )
}



#' Create a JFI-styled bar/column plot
#'
#' A wrapper that combines `jfi_plot()` and `ggplot2::geom_col()` to quickly
#' create styled column charts.
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping.
#' @param show_labels Logical. If `TRUE` (the default), value labels are added.
#' @param label_type The format for the labels: "number", "percent", or "dollar".
#' @param label_scale A scaling factor to apply to the labels.
#' @param label_suffix A suffix to add to the labels.
#' @param ... Other arguments passed on to `ggplot2::geom_col()`.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   category = c("A", "B", "C"),
#'   value = c(10, 20, 15)
#' )
#' jfi_bar(df, aes(x = category, y = value))
#' }
jfi_bar <- function(data, mapping, show_labels = TRUE, label_type = "number",
                    label_scale = 1, label_suffix = "", ...) {

  p <- jfi_plot(data, mapping) +
    ggplot2::geom_col(...) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), ...)

  if (show_labels) {
    y_var <- rlang::as_name(rlang::quo_get_expr(mapping$y))

    label_data <- data %>%
      dplyr::mutate(
        .label_text = format_labels(.data[[y_var]], type = label_type, scale = label_scale, suffix = label_suffix)
      )

    p <- p +
      ggplot2::geom_text(
        data = label_data,
        ggplot2::aes(y = .data[[y_var]], label = .label_text),
        size = 3.5, fontface = "bold", vjust = -0.5, color = "grey10"
      )
  }

  return(p)
}

#' Create a JFI-styled stacked bar chart
#'
#' A wrapper that combines `jfi_plot()` and `ggplot2::geom_bar()`. This function
#' automatically calculates and adds percentage labels to the stacked segments,
#' with the label color automatically adjusting for contrast.
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping. Must include `x`, `y`, and `fill`.
#' @param position The position adjustment. Defaults to "stack".
#' @param show_percents Logical. If `TRUE` (the default), percentage labels are added.
#' @param palette The color palette to use for the labels. This should match the
#'   palette used in the `jfi_scale_fill_*` function.
#' @param ... Other arguments passed on to `ggplot2::geom_bar()`.
#'
#' @export
jfi_bar_stacked <- function(data, mapping, position = "stack", show_percents = TRUE, palette = "jfi", ...) {

  x_var <- rlang::as_name(rlang::quo_get_expr(mapping$x))
  y_var <- rlang::as_name(rlang::quo_get_expr(mapping$y))
  fill_var <- rlang::as_name(rlang::quo_get_expr(mapping$fill))

  fill_levels <- levels(as.factor(data[[fill_var]]))
  num_colors <- length(fill_levels)
  pal_name <- paste(palette, "discrete", sep = "_")
  bar_colors <- jfi_palettes[[pal_name]][1:num_colors]
  color_map <- stats::setNames(bar_colors, fill_levels)

  plot_data <- data %>%
    # Arrange by the fill variable to ensure correct label positioning
    dplyr::arrange(dplyr::desc(.data[[fill_var]])) %>%
    dplyr::group_by(.data[[x_var]]) %>%
    dplyr::mutate(
      .percent = .data[[y_var]] / sum(.data[[y_var]]),
      .label_y = cumsum(.data[[y_var]]) - 0.5 * .data[[y_var]],
      .label_text = format_labels(.percent, type = "percent"),
      .bar_color = color_map[as.character(.data[[fill_var]])],
      .luminance = get_luminance(.bar_color),
      .label_color = dplyr::if_else(.luminance > 0.5, "grey10", "white")
    ) %>%
    dplyr::ungroup()

  # --- 3. Build the plot ---
  p <- jfi_plot(data = plot_data, mapping = mapping) +
    ggplot2::geom_bar(position = position, stat = "identity", ...)  +
    jfi_scale_fill_d(palette = palette) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.02)), ...)

  # Add the labels if requested
  if (show_percents) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(y = .data$.label_y, label = .data$.label_text, color = .data$.label_color),
        size = 3.5, fontface = "bold"
      ) +
      ggplot2::scale_color_identity()
  }

  return(p)
}


#' Create a JFI-styled box plot
#'
#' A wrapper that combines `jfi_plot()` and `ggplot2::geom_boxplot()` to
#' quickly create styled box plots.
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping.
#' @param ... Other arguments passed on to `ggplot2::geom_boxplot()`.
#'
#' @export
#' @examples
#' \dontrun{
#' jfi_boxplot(mpg, aes(x = class, y = hwy))
#' }
jfi_boxplot <- function(data, mapping, ...) {
  jfi_plot(data, mapping) +
    ggplot2::geom_boxplot(...) +
    ggplot2::scale_y_continuous(n.breaks = 8, expand = ggplot2::expansion(mult = c(0.01, 0.02)), ...)
}

#' Create a JFI-styled donut chart with automatic labeling
#'
#' This function creates a donut chart with a larger hole and automatically
#' adds labels showing the value and percentage for each slice.
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping. Must include `y` and `fill`.
#' @param palette The color palette to use. Default == "jfi".
#' @param label_type The format for the labels: "number", "percent", or "dollar".
#' @param label_scale A scaling factor to apply to the labels.
#' @param label_suffix A suffix to add to the labels.
#' @param ... Other arguments passed on to `ggplot2::geom_col()`.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   category = c("A", "B", "C", "D"),
#'   value = c(30, 20, 15, 35)
#' )
#' # The function automatically handles the rest
#' jfi_donut(df, aes(y = value, fill = category))
#' }
jfi_donut <- function(data, mapping, palette = "jfi", label_type = "number", label_scale = 1, label_suffix = "", ...) {

  value_var <- rlang::as_name(rlang::quo_get_expr(mapping$y))
  category_var <- rlang::as_name(rlang::quo_get_expr(mapping$fill))

  fill_levels <- levels(as.factor(data[[category_var]]))
  num_colors <- length(fill_levels)

  pal_name <- paste(palette, "discrete", sep = "_")
  bar_colors <- jfi_palettes[[pal_name]][1:num_colors]
  color_map <- stats::setNames(bar_colors, fill_levels)

  donut_data <- data %>%
    dplyr::arrange(dplyr::desc(.data[[category_var]])) %>%
    dplyr::mutate(
      .percent = .data[[value_var]] / sum(.data[[value_var]]),
      .label_text = paste0(format_labels(.data[[value_var]], type = label_type, scale = label_scale, suffix = label_suffix),
                           "\n(", scales::percent(.data$.percent, accuracy = 1), ")"),
      .label_pos = cumsum(.data[[value_var]]) - 0.5 * .data[[value_var]],
      .bar_color = color_map[as.character(.data[[category_var]])],
      .luminance = get_luminance(.bar_color),
      .label_color = dplyr::if_else(.luminance > 0.5, "grey10", "white")
    )

  p <- jfi_plot(data = donut_data, mapping = mapping) +
    ggplot2::geom_col(ggplot2::aes(x = 2), width = 1, ...) +
    ggplot2::scale_y_continuous(expand = c(0,0), ...) +
    ggplot2::geom_text(
      ggplot2::aes(x = 2, y = .data$.label_pos, label = .data$.label_text, color = .data$.label_color),
      size = 3.5, fontface = "bold"
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::xlim(c(0.05, 2.5)) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(t = -20, b = 2)),
      plot.background =  ggplot2::element_rect(fill = "#F9F9F9", color = NA),
      panel.background = ggplot2::element_rect(fill = "#F9F9F9", color = NA),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5, 0, 15, 0),
      aspect.ratio = 1,
      legend.position = c(0.535, 0.5),
      legend.direction = "vertical",
      legend.justification = "center",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank()
    ) +
    jfi_scale_fill_d(palette = palette)

  return(p)
}

#' Custom print method for jfi_donut objects
#'
#' This function overrides the default print behavior for donut charts to
#' solve the `coord_polar` background issue. It manually creates a new
#' graphics page and then prints the ggplot object without allowing it
#' to create another new page, preserving the transparent background.
#'
#' @param x A plot object of class `jfi_donut`.
#' @param newpage Whether to draw on a new page.
#' @param vp A viewport to draw in.
#' @param ... Other arguments passed to `print()`.
#' @export
print_donut <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) {
    grid::grid.newpage()
    grid::grid.draw(grid::rectGrob(gp = grid::gpar(fill = "#F9F9F9", col = NA)))
  }

  # Remove the custom class to avoid infinite recursion
  class(x) <- class(x)[-1]

  # Print the ggplot object, telling it not to create a new page
  print(x, newpage = FALSE, vp = vp, ...)
}

#' Create a JFI-styled scatter plot with boxplots
#'
#'
#' @param data The data frame to use.
#' @param mapping The aesthetic mapping.
#' @param ... Other arguments passed on to `geom_boxplot()`. A common argument is
#'   `outlier.shape = NA` to hide the boxplot's outliers, as the individual
#'   points are already plotted.
#'
#' @export
#' @examples
#' \dontrun{
#' jfi_scatter_box(
#'   mpg,
#'   aes(x = class, y = hwy, color = class),
#'   outlier.shape = NA
#' ) +
#' jfi_scale_color_d() +
#' theme(legend.position = "none")
#' }
jfi_scatter_box <- function(data, mapping, ...) {
  jfi_plot(data, mapping) +
    ggplot2::geom_boxplot(alpha = 0.04, linewidth = .35,  ...) +
    ggplot2::geom_jitter(alpha = 0.2, size = 1.3, width = 0.25) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.025, 0.025)))
}

