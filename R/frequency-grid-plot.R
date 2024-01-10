#' Frequency grid ggplot
#'
#' @description NOTE: This function is currently experimental and shouldn't be
#'   relied upon.
#'
#'   Call `frequency_grid_plot()` to visualize the absolute frequencies of
#'   values in a vector. Each observation is plotted distinctly, resulting in a
#'   hybrid of a histogram and a scatterplot.
#'
#'   - Boxes are known values.
#'   - Circles with `NA` labels are missing values.
#'   - Empty circles are no values at all: They signify that certain unique
#'   values would have to be more frequent in order for all unique values to be
#'   equally frequent.
#'
#' @param x A vector with frequencies to visualize.
#' @param show_line_grid Logical. Should gridlines be present, crossing at each
#'   observation? Default is `FALSE`.
#' @param show_line_mode Logical. Should a dashed line demarcate the mode(s)
#'   among known values from the missing values that might add to these modes,
#'   if there are any? Default is `FALSE`.
#' @param label_hypothetical String. Label used for missing values. Default is
#'   `"NA"`.
#' @param color_label_hypothetical,color_hypothetical,color_non_hypothetical String. Colors of
#'   the data points. Defaults are `"red2"` for missing data points as well as
#'   their labels, and `"blue2"` for non-missing data points.
#' @param alpha_hypothetical,alpha_non_hypothetical Numeric. Opacity of the data points.
#'   Defaults are `1` and `0.75`, respectively.
#' @param size_label_hypothetical,size_hypothetical,size_non_hypothetical Numeric. Sizes of the
#'   data points. Defaults are `3` for the label and `10` for both symbols.
#' @param shape_hypothetical,shape_non_hypothetical Numeric or string. Signifiers for the
#'   shapes of the data points. Defaults are `1` (circle) and `15` (square
#'   filled), respectively.
#' @param expand Numeric. Padding whitespace between the axes and the data
#'   points. The distance is the same on all four sides due to the grid
#'   structure. Default is `0.1`.
#'
#' @section Limitations: Certain assumptions about missing values are currently
#'   hard-coded in the function. In the future, they should become optional.
#'   These assumptions are:
#'   - All missings represent a known value. For example, in `c(1, 2, NA)`, the
#'   `NA` is either `1` or `2`.
#'   - The missings are as evenly distributed across known values as possible.
#'   Therefore, in `c(1, 2, NA, NA)`, one `NA` is a `1` and the other one is a
#'   `2`. This is clearly not reasonable as a general assumption. It is derived
#'   from moder's way of determining possible extreme cases.
#'
#' @return A ggplot object. To save it, call `ggplot2::ggsave()`.
#'
#' @seealso [frequency_grid_df()], which forms the basis of the current
#'   function.
#'
#' @export
#'
#' @examples
#' x <- c("a", "a", "a", "b", "b", "c", NA, NA, NA, NA, NA)
#'
#' # Basic usage:
#' frequency_grid_plot(x)
#'
#' # With "N/A" as a marker of missing values
#' # instead of "NA":
#' frequency_grid_plot(x, label_hypothetical = "N/A")
#'
#' # Black and white mode:
#' frequency_grid_plot(
#'   x, color_label_hypothetical = "black",
#'   color_hypothetical = "black", color_non_hypothetical = "black"
#' )

frequency_grid_plot <- function(x,
                                show_line_grid = FALSE,
                                show_line_mode = FALSE,
                                label_hypothetical = "NA",
                                color_label_hypothetical = "red2",
                                color_hypothetical = "red2",
                                color_non_hypothetical = "blue2",
                                alpha_hypothetical = 1,
                                alpha_non_hypothetical = 0.75,
                                size_label_hypothetical = 3,
                                size_hypothetical = 10,
                                size_non_hypothetical = 10,
                                shape_hypothetical = 1,
                                shape_non_hypothetical = 15,
                                expand = 0.1) {

  # Arrange the data. The `x` column is converted to a factor so that it always
  # has a discrete scale level. This is necessary because a single scale
  # function like `scale_x_discrete()` must be able to reliably operate on it:
  freq_table <- frequency_grid_df(x)
  freq_table$x <- as.factor(freq_table$x)

  # Optional line geom:
  geom_line_mode <- if (show_line_mode) {
    # A modal line only makes sense if there are supermodal `NA`s. If so,
    # `freq_max_known` is the modal frequency among known values:
    if (any(freq_table$is_supermodal)) {
      freq_max_known <- which(freq_table$is_supermodal)[1L] - 1L
      ggplot2::geom_hline(yintercept = freq_max_known + 0.5, linetype = 2)
    } else {
      warning(paste(
        "`show_line_mode` is `TRUE` but no missings",
        "can add to the mode among known values"
      ))
      NULL
    }
  } else {
    NULL
  }

  # Build and return the plot:
  ggplot2::ggplot(freq_table, ggplot2::aes(x = x, y = freq)) +
    ggplot2::geom_point(
      shape = ifelse(freq_table$is_hypothetical, shape_hypothetical, shape_non_hypothetical),
      size  = ifelse(freq_table$is_hypothetical, size_hypothetical , size_non_hypothetical),
      color = ifelse(freq_table$is_hypothetical, color_hypothetical, color_non_hypothetical),
      alpha = ifelse(freq_table$is_hypothetical, alpha_hypothetical, alpha_non_hypothetical)
    ) +
    ggplot2::geom_text(
      label = ifelse(freq_table$can_be_filled, label_hypothetical, ""),
      size  = size_label_hypothetical,
      color = color_label_hypothetical
    ) +
    geom_line_mode +
    ggplot2::scale_x_discrete(expand = c(expand, expand)) +
    ggplot2::scale_y_continuous(
      breaks = seq_len(max(freq_table$freq)),
      expand = c(expand, expand)
    ) +
    ggplot2::labs(x = "Value", y = "Frequency") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = if (show_line_grid) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )
}
