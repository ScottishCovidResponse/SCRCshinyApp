#' plot_stackedbardate
#'
#' @export
#'
plot_stackedbardate <- function(data) {

  plot.this <- data %>%
    tibble::rownames_to_column("rowid") %>%
    reshape2::melt(id.vars = "rowid")

  if("Hospital" %in% plot.this$rowid) {
    plot.this %<>%
      dplyr::mutate(rowid = factor(rowid,
                                   levels = rev(c("Hospital", "Care Home",
                                                  "Home / Non-institution",
                                                  "Other institution"))),
                    variable = as.Date(variable))
  } else if("female" %in% plot.this$rowid) {
    plot.this %<>%
      dplyr::mutate(rowid = factor(rowid,
                                   levels = rev(c("Female", "Male"))),
                    variable = as.Date(variable))
  }

  total <- plot.this %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(total = sum(value), .groups = "drop")

  buffer <- max(total$total) / 20

  pal <- viridis::viridis(4)

  plotly::plot_ly(plot.this, x = ~variable, y = ~value, type = "bar",
                  color = ~rowid, colors = pal) %>%
    plotly::add_annotations(xref = "total", yref = "rowid",
                            x = ~variable, y = ~(total + buffer),
                            text = ~total,
                            textposition = "auto",
                            data = total,
                            font = list(family = 'Arial', size = 12),
                            showarrow = FALSE) %>%
    plotly::layout(barmode = "stack",
                   xaxis = list(title = "Week commencing",
                                type = "date",
                                tickformat = "%d. %b"),
                   yaxis = list(title = "Number of deaths"),
                   legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.2))
}
