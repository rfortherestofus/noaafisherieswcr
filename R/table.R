#' NOAA Table
#'
#' @param df Data frame used to make the table
#'
#' @return A flextable
#' @export
#'
#' @examples
noaa_table <-
  function(df) {
    df |>
      flextable::flextable() |>
      flextable::font(fontname = "Times New Roman",
           part = "all") |>
      flextable::fontsize(size = 12) |>
      flextable::theme_zebra() |>
      flextable::autofit()
  }
