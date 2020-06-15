#' table_three
#'
#' @export
#'
table_three <- function(data) {

  tmp <- data %>%
    tibble::rownames_to_column("area") %>%
    reshape2::melt(id.var = "area", variable.name = "date") %>%
    dplyr::mutate(value = dplyr::case_when(
      grepl("\\*", value) ~ "0",
      grepl("NA", value) ~ "0",
      T ~ value)) %>%
    dplyr::mutate(value = as.numeric(value))

  tab.this <- tmp %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(people_tested = list(value),
                     total = sum(value, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(confirmed_sparkline = NA)


  reactable::reactable(tab.this, columns = list(
    area = reactable::colDef(name = "NHS health board"),
    people_tested = reactable::colDef(
      name = "Confirmed",
      show = FALSE,
      cell = function(x)
        sparkline::sparkline(x, type = "bar", chartRangeMin = 0,
                             chartRangeMax = max(tmp$value))
    ),
    total = reactable::colDef(
      name = "COVID-19 patients in hospital - Confirmed",
      align = "left", cell = function(x)
        bar_chart(x, x, max(tab.this$total))),
    confirmed_sparkline = reactable::colDef(
      name = "Plot",
      cell = function(people_tested, index)
        sparkline::sparkline(tab.this$people_tested[[index]])
    )
  ))
}
