#' Function to create a new Quarto memo to record template file.
#'
#' @return A new Quarto memo to record template file. Run this function to create a new Quarto file.
#' @export
#'
#' @examples
create_new_memo_to_record <- function() {
  setwd(here::here())

  temp_dir_with_colon <- as.character(lubridate::now())

  temp_dir <- stringr::str_replace_all(temp_dir_with_colon, "\\D", "")

  fs::dir_create(temp_dir)

  setwd(temp_dir)

  quarto::quarto_use_template("rfortherestofus/noaa-fisheries-memo-to-record",
                              no_prompt = TRUE)

  setwd(here::here())

  temp_quarto_file <- stringr::str_glue("{temp_dir}/{temp_dir}.qmd")
  new_quarto_file <- stringr::str_glue("memo-to-record-{temp_dir}.qmd")

  fs::file_move(temp_quarto_file, new_quarto_file)

  fs::dir_delete(temp_dir)

  quarto::quarto_add_extension(
    "rfortherestofus/noaa-fisheries-memo-to-record",
    no_prompt = TRUE
  )

  usethis::ui_done(stringr::str_glue("New Memo to Record template file created! It will automatically open with the file name {new_quarto_file}. You'll probably want to rename it before you start editing. :)"))

  rstudioapi::navigateToFile(new_quarto_file)
}
