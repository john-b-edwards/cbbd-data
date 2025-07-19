source("R/utils.R")

build_lines <- function(query_season = cbbreadr::most_recent_season()) {
  lines <- query_games("lines", query_season, game_identifier = "game_id")
  lines <- lines |>
    tidyr::unnest_longer(lines) |>
    tidyr::unnest(lines) |>
    janitor::clean_names()
  data.table::setDT(lines)
  data.table::setnames(
    lines,
    old = c("home_team", "home_conference", "away_team", "away_conference"),
    new = c(
      "home_team_name",
      "home_conference_short_name",
      "away_team_name",
      "away_conference_short_name"
    )
  )
  data.table::setDF(lines)
  cbbd_save(lines, paste0("lines_", query_season), "lines")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2013:cbbreadr::most_recent_season(), build_lines, .progress = T)
} else {
  build_lines()
}
