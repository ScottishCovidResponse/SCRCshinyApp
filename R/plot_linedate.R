#' plot_linedate
#'
#' @export
#'
plot_linedate <- function(data, groupby, n) {
  plot.this <- data %>%
    tibble::rownames_to_column("variable") %>%
    reshape2::melt(id.vars = "variable", variable.name = "date") %>%
    dplyr::mutate(date = as.Date(date))

  if(!missing(n)) {
    ind <- plot.this %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(total = sum(value), .groups = "drop") %>%
      dplyr::arrange(total) %$%
      variable

    len <- length(ind)

    if(n == "Top 5") {
      topn <- min(5, len)
    } else if(n == "Top 10") {
      topn <- min(10, len)
    } else if(n == "All") {
      topn <- len
    }

    plot.this %<>% dplyr::filter(variable %in% head(ind, topn)) %>%
      dplyr::mutate(variable = factor(variable, levels = ind))
  }

  # ggplot2::ggplot(plot.this) + ggplot2::theme_minimal() +
  #   ggplot2::geom_line(ggplot2::aes(x = date, y = value, group = variable,
  #                                   colour = variable)) +
  #   ggplot2::theme(rect = ggplot2::element_rect(fill = "transparent"),
  #                  panel.grid.minor = ggplot2::element_blank(),
  #                  legend.position = "bottom") +
  #   ggplot2::labs(x = "Week commencing", y = "Number of deaths") +
  #   ggplot2::scale_x_date(date_breaks = "1 week",
  #                         date_labels = "%d %b")

  plotly::plot_ly(plot.this, x = ~date, y = ~value) %>%
    plotly::add_trace(type = "scatter", mode = "markers+lines",
                      color = ~variable) %>%
    # plotly::add_markers(color = ~variable) %>%
    plotly::layout(xaxis = list(title = "Week commencing",
                                type = "date",
                                tickformat = "%d. %b"),
                   yaxis = list(title = "Number of deaths"),
                   legend = list(title = list(text = paste0("<b>", groupby,
                                                            "</b>"))),
                   autosize = TRUE)

}
