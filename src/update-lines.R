source("R/utils.R")

build_lines <- function(query_season = cbbreadr::most_recent_season()) {
  lines <- query_games("lines", query_season, game_identifier = "game_id")
  lines <- lines |>
    tidyr::unnest_longer(lines) |>
    tidyr::unnest(lines) |>
    janitor::clean_names()
  cbbd_save(lines, paste0("lines_", query_season), "lines")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2013:cbbreadr::most_recent_season(), build_lines, .progress = T)
} else {
  build_lines()
}
