#' plot_summary
#'
#' @export
#'
plot_summary <- function(bars, line) {

  plot.bars <- bars %>%
    tibble::rownames_to_column("rowid") %>%
    reshape2::melt(id.vars = "rowid") %>%
    dplyr::mutate(labels = value,
                  rowid = dplyr::case_when(
                    grepl("all", rowid) ~ "All deaths",
                    grepl("covid", rowid) ~ "Covid-related deaths")) %>%
    dplyr::mutate(labels = dplyr::if_else(is.na(labels), "",
                                          as.character(labels)))

  plot.line <- line %>%
    tibble::rownames_to_column("rowid") %>%
    reshape2::melt(id.vars = "rowid") %>%
    dplyr::mutate(rowid =  dplyr::case_when(
      grepl("5years", rowid) ~ "All deaths (averaged over last 5 years)"))

  pal <- rev(viridis::viridis(2))

  plotly::plot_ly(plot.bars, x = ~variable, y = ~value, type = "bar",
                  color = ~rowid, colors = pal) %>%
    plotly::add_trace(type = "scatter", mode = "markers+lines",
                      data = plot.line,
                      color = ~rowid, colors = pal) %>%
    # plotly::add_annotations(xref = "total", yref = "rowid",
    #                         text = ~labels,
    #                         data = plot.bars,
    #                         textposition = "auto",
    #                         font = list(family = 'Arial', size = 12),
    #                         showarrow = FALSE) %>%
    plotly::layout(xaxis = list(title = "Week commencing",
                                type = "date",
                                tickformat = "%d. %b"),
                   yaxis = list(title = "Number of deaths"),
                   legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.2))

}
