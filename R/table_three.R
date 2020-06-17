#' table_three
#'
#' @export
#'
table_three <- function(data, totals) {

  tmp <- data %>%
    tibble::rownames_to_column("area") %>%
    reshape2::melt(id.var = "area", variable.name = "date") %>%
    dplyr::mutate(value = as.numeric(value))

  tab.this <- tmp %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(people_tested = list(value),
                     total = sum(value, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(confirmed_sparkline = NA) %>%
    merge(totals, by = "area", all.x = TRUE) %>%
    dplyr::mutate(per100 = round((total / population) * 100000)) %>%
    dplyr::select(area, population, people_tested, confirmed_sparkline, total,
                  per100)

  reactable::reactable(tab.this, columns = list(
    area = reactable::colDef(name = "NHS health board"),

    population = reactable::colDef(
      name = "Population size (2018)",
      align = "left", cell = function(x) {
        width = paste0(((x / max(tab.this$population)) * 100), "%")
        buffer = max(nchar(tab.this$population)) - nchar(x)
        marginLeft = paste0(((buffer * 6) + 8), "px")
        bar_chart(x, width, marginLeft)
      }),

    people_tested = reactable::colDef(
      name = "COVID-19 patients in hospital - Confirmed",
      show = FALSE,
      cell = function(x)
        sparkline::sparkline(x, type = "bar", chartRangeMin = 0,
                             chartRangeMax = max(tmp$value))
    ),

    confirmed_sparkline = reactable::colDef(
      name = "COVID-19 patients in hospital - Confirmed",
      cell = function(people_tested, index)
        sparkline::sparkline(tab.this$people_tested[[index]])
    ),

    total = reactable::colDef(
      name = "Cumulative total",
      align = "left", cell = function(x) {
        width = paste0(((x / max(tab.this$total)) * 100), "%")
        buffer = max(nchar(tab.this$total)) - nchar(x)
        marginLeft = paste0(((buffer * 6) + 8), "px")
        bar_chart(x, width, marginLeft)
      }),

    per100 = reactable::colDef(
      name = "Per 100k",
      align = "left", cell = function(x) {
        width = paste0(((x / max(tab.this$per100)) * 100), "%")
        buffer = max(nchar(tab.this$per100)) - nchar(x)
        marginLeft = paste0(((buffer * 6) + 8), "px")
        bar_chart(x, width, marginLeft)
      })
  ))
}
