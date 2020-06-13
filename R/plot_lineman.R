#' plot_lineman
#'
#' @export
#'
plot_lineman <- function(data, legend) {

  plot.this <- data %>%
    tibble::rownames_to_column("variable") %>%
    reshape2::melt(id.var = "variable", variable.name = "date") %>%
    dplyr::mutate(value = dplyr::case_when(
      grepl("\\*", value) ~ "0",
      grepl("NA", value) ~ "0",
      T ~ value
    )) %>%
    dplyr::mutate(value = as.numeric(value))

  pal <- RColorBrewer::brewer.pal(unique(plot.this$variable), name = 'Dark2')

  plotly::plot_ly(plot.this, x = ~date, y = ~value) %>%
    plotly::add_trace(type = "scatter", mode = "markers+lines",
                      color = ~variable, colors = pal) %>%
    plotly::layout(xaxis = list(title = "Date",
                                type = "date",
                                tickformat = "%d. %b"),
                   yaxis = list(title = "Value"),
                   legend = list(title = list(text = paste0("<b>", legend,
                                                            "</b>"))),
                   autosize = TRUE)
}
