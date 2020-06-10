#' plot_stackedbardate
#'
#' @export
#'
plot_stackedbardate <- function(data, title) {

  plot.this <- data %>%
    tibble::rownames_to_column("rowid") %>%
    reshape2::melt(id.vars = "rowid") %>%
    dplyr::mutate(rowid = factor(rowid,
                                 levels = c("Hospital", "Care Home",
                                            "Home / Non-institution",
                                            "Other institution")),
                  variable = as.Date(variable))

  total <- plot.this %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(total = sum(value), .groups = "drop")

  # ggplot2::ggplot(plot.this) +
  #   ggplot2::geom_bar(ggplot2::aes(x = variable, y = value,
  #                                  group = rowid, fill = rowid),
  #                     stat = "identity", colour = "white") +
  #   ggplot2::geom_text(ggplot2::aes(x = variable, y = total,
  #                                   label = total, vjust = -0.1), total) +
  #   ggplot2::scale_x_date(date_breaks = "1 week",
  #                         date_labels = "%d %b") +
  #   ggplot2::labs(x = "Week commencing", y = "Number of deaths",
  #                 fill = ggplot2::element_blank()) +
  #   ggplot2::theme(rect = ggplot2::element_rect(fill = "transparent"),
  #                  panel.grid.minor = ggplot2::element_blank(),
  #                  panel.grid.major.x = ggplot2::element_blank(),
  #                  legend.position = "bottom")

  buffer <- max(total$total) / 20

  plotly::plot_ly(plot.this, x = ~variable, y = ~value) %>%
    plotly::add_bars(color = ~rowid) %>%
    plotly::add_annotations(xref = "total", yref = "rowid",
                            x = ~variable, y = ~(total + buffer),
                            text = ~total,
                            data = total,
                            font = list(family = 'Arial', size = 12,
                                        color = 'rgb(50, 171, 96)'),
                            showarrow = FALSE) %>%
    plotly::layout(barmode = "stack",
                   title = title,
                   xaxis = list(title = "Number of deaths"),
                   yaxis = list(title = "Data zone"),
                   legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5))
}
