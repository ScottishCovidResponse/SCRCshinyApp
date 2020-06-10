#' plot_stackedbar
#'
#' @export
#'
plot_stackedbar <- function(data, title, sortby) {

  plot.this <- data %>%
    tibble::rownames_to_column("rowid") %>%
    reshape2::melt(id.vars = "rowid")

  if(sortby == "carehome") {
    vec <- data %>%
      dplyr::arrange(`Care Home`) %>%
      rownames()
  } else if(sortby == "home") {
    vec <- data %>%
      dplyr::arrange(`Home / Non-institution`) %>%
      rownames()
  } else if(sortby == "hospital") {
    vec <- data %>%
      dplyr::arrange(Hospital) %>%
      rownames()
  } else if(sortby == "other") {
    vec <- data %>%
      dplyr::arrange(`Other institution`) %>%
      rownames()
  } else if(sortby == "total") {
    vec <- plot.this %>%
      dplyr::group_by(rowid) %>%
      dplyr::summarise(total = sum(value)) %>%
      dplyr::arrange(total) %$%
      rowid
  }

  plot.this <- plot.this %>%
    dplyr::mutate(rowid = factor(rowid, levels = vec))

  total <- plot.this %>%
    dplyr::group_by(rowid) %>%
    dplyr::summarise(total = sum(value), .groups = "drop")

  # ggplot2::ggplot(plot.this)  + ggplot2::theme_minimal() +
  #                    ggplot2::coord_flip() +
  #                    ggplot2::geom_bar(ggplot2::aes(x = rowid, y = value,
  #                                                   group = variable, fill = variable),
  #                                      stat = "identity") +
  #                    ggplot2::labs(x = "Data zone", y = "Number of deaths",
  #                                  fill = ggplot2::element_blank()) +
  #                    ggplot2::theme(rect = ggplot2::element_rect(fill = "transparent"),
  #                                   panel.grid.minor = ggplot2::element_blank(),
  #                                   panel.grid.major.y = ggplot2::element_blank(),
  #                                   legend.position = "bottom")

  buffer <- max(total$total) / 20

  plotly::plot_ly(plot.this, x = ~value, y = ~rowid) %>%
    plotly::add_bars(color = ~variable, orientation = "h") %>%
    plotly::add_annotations(xref = "total", yref = "rowid",
                            x = ~(total + buffer), y = ~rowid,
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
