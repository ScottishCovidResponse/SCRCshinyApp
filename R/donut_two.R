#' donut_two
#'
#' @export
#'
donut_two <- function(data) {

  plot.this <- data %>%
    tibble::rownames_to_column("var") %>%
    reshape2::melt(id.vars = "var", variable.name = "loc") %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(total = sum(value), .groups = "drop") %>%
    dplyr::arrange(total)

  pal <- rev(viridis::viridis(nrow(plot.this)))

  plotly::plot_ly(plot.this, labels = ~var, values = ~total,
                  colors = pal,
                  textinfo = 'percent',
                  hoverinfo = 'text',
                  marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
    plotly::add_pie(insidetextorientation = 'radial') %>%
    plotly::layout(legend = list(orientation = "h",
                                 yanchor = "top")
  )
}
