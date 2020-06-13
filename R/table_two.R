#' table_two
#'
#' @export
#'
table_two <- function(confirmed, suspected, total) {

  areas <- rownames(total)
  dates <- colnames(total)

  tmp_confirmed <- confirmed %>%
    tibble::rownames_to_column("area") %>%
    reshape2::melt(id.var = "area", variable.name = "date") %>%
    dplyr::mutate(value = dplyr::case_when(
      grepl("\\*", value) ~ "0",
      grepl("NA", value) ~ "0",
      T ~ value
    )) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    tidyr::complete(area = areas, date = dates, fill = list(value = 0))

  tmp_suspected <- suspected %>%
    tibble::rownames_to_column("area") %>%
    reshape2::melt(id.var = "area", variable.name = "date") %>%
    dplyr::mutate(value = dplyr::case_when(
      grepl("\\*", value) ~ "0",
      grepl("NA", value) ~ "0",
      T ~ value
    )) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    tidyr::complete(area = areas, date = dates, fill = list(value = 0))

  tmp_total <- total %>%
    tibble::rownames_to_column("area") %>%
    reshape2::melt(id.var = "area", variable.name = "date") %>%
    dplyr::mutate(value = dplyr::case_when(
      grepl("\\*", value) ~ "0",
      grepl("NA", value) ~ "0",
      T ~ value
    )) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    tidyr::complete(area = areas, date = dates, fill = list(value = 0))

  dat_confirmed <- tmp_confirmed %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(confirmed_deaths = list(value),
                     confirmed_total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(confirmed_sparkline = NA)

  dat_suspected <- tmp_suspected %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(suspected_deaths = list(value),
                     suspected_total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(suspected_sparkline = NA)

  dat_total <- tmp_total %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(total_deaths = list(value),
                     total_total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(total_sparkline = NA)

  tab.this <- merge(dat_confirmed, dat_suspected, by = "area", all = TRUE) %>%
    merge(dat_total, by = "area", all = TRUE)

  reactable::reactable(tab.this, columns = list(
    area = reactable::colDef(name = "Area"),
    confirmed_deaths = reactable::colDef(
      name = "Confirmed",
      show = FALSE,
      cell = function(x)
        sparkline::sparkline(x, type = "bar", chartRangeMin = 0,
                             chartRangeMax = max(tmp_confirmed$value))
    ),
    confirmed_total = reactable::colDef(
      name = "COVID-19 patients in hospital - Confirmed",
      align = "left", cell = function(x)
        bar_chart(x, x, max(tab.this$confirmed_total))),
    confirmed_sparkline = reactable::colDef(
      name = "",
      cell = function(confirmed_deaths, index)
        sparkline::sparkline(tab.this$confirmed_deaths[[index]])
    ),
    suspected_deaths = reactable::colDef(
      name = "Suspected",
      show = FALSE,
      cell = function(x)
        sparkline::sparkline(x, type = "bar", chartRangeMin = 0,
                             chartRangeMax = max(tmp_suspected$value))
    ),
    suspected_total = reactable::colDef(
      name = "COVID-19 patients in hospital - Suspected",
      align = "left", cell = function(x)
        bar_chart(x, x, max(tab.this$suspected_total))),
    suspected_sparkline = reactable::colDef(
      name = "",
      cell = function(suspected_deaths, index)
        sparkline::sparkline(tab.this$suspected_deaths[[index]])
    ),
    total_deaths = reactable::colDef(
      name = "Total",
      show = FALSE,
      cell = function(x)
        sparkline::sparkline(x, type = "bar", chartRangeMin = 0,
                             chartRangeMax = max(tmp_total$value))
    ),
    total_total = reactable::colDef(
      name = "COVID-19 patients in ICU - Total",
      align = "left", cell = function(x)
        bar_chart(x, x, max(tab.this$total_total))),
    total_sparkline = reactable::colDef(
      name = "",
      cell = function(total_deaths, index)
        sparkline::sparkline(tab.this$total_deaths[[index]])
    )
  ))
}
