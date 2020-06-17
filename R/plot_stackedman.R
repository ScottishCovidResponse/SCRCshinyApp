#' plot_stackedman
#'
#' @export
#'
plot_stackedman <- function(data, stacked = FALSE) {

  plot.this <- data %>%
    tibble::rownames_to_column("variable") %>%
    reshape2::melt(id.var = "variable", variable.name = "date") %>%
    dplyr::mutate(value = dplyr::case_when(
      grepl("\\*", value) ~ "0",
      grepl("NA", value) ~ "0",
      T ~ value
    )) %>%
    dplyr::mutate(value = as.numeric(value))

  n <- length(unique(plot.this$variable))
  if(n == 1) {
    pal <- "#1B9E77"
  } else if (n == 2) {
    pal <- c("#1B9E77", "#D95F02")
  } else
    pal <- RColorBrewer::brewer.pal(n = n,
                                    name = 'Dark2')

  total <- plot.this %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(total = sum(value), .groups = "drop")

  buffer <- max(total$total) / 20

  if(stacked) barmode = "stack" else  barmode <- "group"

  plotly::plot_ly(plot.this, x = ~date, y = ~value, type = "bar",
                  color = ~variable, colors = pal) %>%
    plotly::layout(barmode = barmode,
                   xaxis = list(title = "Date",
                                type = "date",
                                tickformat = "%d. %b"),
                   yaxis = list(title = "Value"),
                   legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.2))
}
