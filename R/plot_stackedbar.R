#' plot_stackedbar
#'
#' @export
#'
plot_stackedbar <- function(data, sortby) {

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
    dplyr::mutate(variable = as.character(variable)) %>%
    dplyr::mutate(rowid = factor(rowid, levels = vec),
                  variable = dplyr::case_when(
                    variable == "Other institution" ~ "Other",
                    variable == "Home / Non-institution" ~ "Home / Non-inst.",
                    T ~ variable
                  )) %>%
    dplyr::mutate(variable = factor(variable,
                                    levels = rev(c("Hospital", "Care Home",
                                                   "Home / Non-inst.",
                                                   "Other"))))

  total <- plot.this %>%
    dplyr::group_by(rowid) %>%
    dplyr::summarise(total = sum(value), .groups = "drop")

  buffer <- max(total$total) / 20

  pal <- viridis::viridis(4)

  plotly::plot_ly(plot.this, x = ~value, y = ~rowid) %>%
    plotly::add_bars(color = ~variable, orientation = "h", colors = pal) %>%
    plotly::add_annotations(xref = "total", yref = "rowid",
                            x = ~(total + buffer), y = ~rowid,
                            text = ~total,
                            data = total,
                            font = list(family = 'Arial', size = 12),
                            showarrow = FALSE) %>%
    plotly::layout(barmode = "stack",
                   xaxis = list(title = "Number of deaths"),
                   yaxis = list(title = "Data zone"),
                   legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.2))
}
