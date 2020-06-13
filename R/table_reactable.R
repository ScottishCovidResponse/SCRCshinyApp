#' table_reactable
#'
#' @export
#'
table_reactable <- function(covid_dat, all_dat) {

  tmp_covid <- covid_dat %>%
    tibble::rownames_to_column("area") %>%
    reshape2::melt(id.var = "area", variable.name = "date")
  dat_covid <- tmp_covid %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(covid_deaths = list(value),
                     covid_total = sum(value), .groups = "drop") %>%
    dplyr::mutate(covid_sparkline = NA)

  tmp_all <- all_dat %>%
    tibble::rownames_to_column("area") %>%
    reshape2::melt(id.var = "area", variable.name = "date")
  dat_all <- tmp_all %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(all_deaths = list(value),
                     all_total = sum(value), .groups = "drop") %>%
    dplyr::mutate(all_sparkline = NA)

  tab.this <- merge(dat_covid, dat_all, by = "area")

  reactable::reactable(tab.this, columns = list(
    area = reactable::colDef(name = "Area"),
    covid_deaths = reactable::colDef(name = "covid_bar",
      # show = FALSE,
      cell = function(x)
      sparkline::sparkline(x, type = "bar", chartRangeMin = 0,
                           chartRangeMax = max(tmp_covid$value))
      ),
    covid_total = reactable::colDef(align = "left", cell = function(x)
      bar_chart(x, x, max(tab.this$covid_total))),
    covid_sparkline = reactable::colDef(name = "covid_perweek",
      cell = function(covid_deaths, index)
      sparkline::sparkline(tab.this$covid_deaths[[index]])
      ),
    all_deaths = reactable::colDef(name = "all_bar",
      # show = FALSE,
      cell = function(x)
      sparkline::sparkline(x, type = "bar", chartRangeMin = 0,
                           chartRangeMax = max(tmp_all$value))
      ),
    all_total = reactable::colDef(align = "left", cell = function(x)
      bar_chart(x, x, max(tab.this$all_total))),
    all_sparkline = reactable::colDef(name = "all_perweek",
      cell = function(all_deaths, index)
        sparkline::sparkline(tab.this$all_deaths[[index]])
    )
  ))

}
