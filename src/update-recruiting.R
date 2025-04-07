source("R/utils.R")

update_recruiting <- function() {
  recruiting <- query_cbbd("recruiting/players", list()) |>
    tidyr::unnest_wider(hometown, names_sep = "_") |>
    tidyr::unnest_wider(committed_to, names_sep = "_")
  cbbd_save(recruiting, "recruiting", "recruiting")
}

update_recruiting()
