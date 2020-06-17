#' plot_lineman
#'
#' @export
#'
plot_lineman <- function(data) {

  plot.this <- data %>%
    tibble::rownames_to_column("variable") %>%
    reshape2::melt(id.var = "variable", variable.name = "date") %>%
    dplyr::mutate(value = as.numeric(value))

  n <- length(unique(plot.this$variable))
  if(n == 1) {
    pal <- "#1B9E77"
  } else if (n == 2) {
    pal <- c("#1B9E77", "#D95F02")
  } else
    pal <- RColorBrewer::brewer.pal(n = n,
                                    name = 'Dark2')

  plotly::plot_ly(plot.this, x = ~date, y = ~value) %>%
    plotly::add_trace(type = "scatter", mode = "markers+lines",
                      color = ~variable, colors = pal) %>%
    plotly::layout(xaxis = list(title = "Date",
                                type = "date",
                                tickformat = "%d. %b"),
                   yaxis = list(title = "Value"),
                   autosize = TRUE,
                   legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.2))
}
