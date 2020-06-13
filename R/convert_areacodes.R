#' convert_areacodes
#'
#' @export
#'
convert_areacodes <- function(data, conversion.table) {
  data %>%
    tibble::rownames_to_column("featurecode") %>%
    merge(conversion.table, by = "featurecode", all.x = TRUE) %>%
    dplyr::select(-featurecode) %>%
    tibble::column_to_rownames("featurename")
}
