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
    dplyr::mutate(rowid = factor(rowid, levels = vec))

  ggplot2::ggplot(plot.this) + ggplot2::theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::geom_bar(ggplot2::aes(x = rowid, y = value,
                                   group = variable, fill = variable),
                      stat = "identity") +
    ggplot2::labs(x = "Data zone", y = "Number of deaths",
                  fill = ggplot2::element_blank()) +
    ggplot2::theme(rect = ggplot2::element_rect(fill = "transparent"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   legend.position = "bottom")
}
