#' bar_chart
#'
#' https://glin.github.io/reactable/articles/cookbook/cookbook.html
#'
#' @export
#'
bar_chart <- function(label, width = "100%", marginLeft = "8px",
                      height = "16px", fill = "#fc5185", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = marginLeft,
                            background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}
