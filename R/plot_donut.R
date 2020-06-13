#' plot_donut
#'
#' @export
#'
plot_donut <- function(data) {
  plot.this <- data %>%
    tibble::rownames_to_column("var") %>%
    reshape2::melt(id.vars = "var", variable.name = "loc") %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(total = sum(value), .groups = "drop")

  if("Hospital" %in% plot.this$var) {
    plot.this %<>% dplyr::mutate(ind = dplyr::case_when(var == "Hospital" ~ 1,
                                         var == "Care Home" ~ 2,
                                         var == "Home / Non-institution" ~ 3,
                                         var == "Other institution" ~ 4)) %>%
      dplyr::arrange(ind)

    pal <- rev(viridis::viridis(4))

  } else if("Female" %in% plot.this$var) {
    plot.this %<>% dplyr::mutate(ind = dplyr::case_when(var == "Female" ~ 1,
                                         var == "Male" ~ 2)) %>%
      dplyr::arrange(ind)

    pal <- c("#df65b0", "#2b8cbe")
  }

  plotly::plot_ly(plot.this, labels = ~var, values = ~total,
                  marker = list(colors = pal), sort = FALSE) %>%
    plotly::add_pie(hole = 0.4, insidetextorientation='radial')
}
