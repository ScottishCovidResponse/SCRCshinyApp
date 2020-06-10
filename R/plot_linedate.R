#' plot_linedate
#'
#' @export
#'
plot_linedate <- function(data, n) {
  plot.this <- data %>%
    tibble::rownames_to_column("variable") %>%
    reshape2::melt(id.vars = "variable", variable.name = "date") %>%
    dplyr::mutate(date = as.Date(date))

  if(!missing(n)) {
    ind <- plot.this %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(total = sum(value)) %>%
      dplyr::arrange(total) %$%
      variable

    # If n is not missing
    if(n == "Top 5") {
      ind %<>% head(5)
    } else if(n == "Top 10") {
      ind %<>% head(10)
    }

    plot.this %<>%
      dplyr::filter(variable %in% ind)
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
    plotly::add_lines(color = ~variable) %>%
    plotly::layout(title = title,
                   xaxis = list(title = "Number of deaths"),
                   yaxis = list(title = "Data zone"),
                   autosize = TRUE,
                   legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5))
}
