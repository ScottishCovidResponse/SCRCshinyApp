#' bar_chart
#'
#' Adapted from the reactable cookbook
#' https://glin.github.io/reactable/articles/cookbook/cookbook.html
#'
#' @export
#'
bar_chart <- function(label, value, max_value, height = "16px",
                      fill = "#fc5185") {
  pos_chart <- div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value / max_value) * 100, "%")
  bar <- div(style = list(marginRight = "8px", background = fill,
                          width = width, height = height))
  chart <- div(style = list(display = "flex", alignItems = "center"),
               bar, label)
  pos_chart <- shiny::tagAppendChild(pos_chart, chart)
  div(style = list(display = "flex"), pos_chart)
}
