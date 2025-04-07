source("R/utils.R")

build_games <- function(query_season = most_recent_season()) {
  games <- query_games("games", query_season, game_identifier = "id")
  games <- games |>
    tidyr::unnest_wider(home_period_points, names_sep = "_") |>
    tidyr::unnest_wider(away_period_points, names_sep = "_")
  for (colname in paste0("_period_points_", 1:7)) {
    if (!(paste0("home", colname) %in% colnames(games))) {
      games[[paste0("home", colname)]] <- NA_integer_
      games[[paste0("away", colname)]] <- NA_integer_
    }
  }
  cbbd_save(games, paste0("games_", query_season), "games")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:most_recent_season(), build_games, .progress = T)
} else {
  build_games()
}
