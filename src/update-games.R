source("R/utils.R")

build_games <- function(query_season = cbbreadr::most_recent_season()) {
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
  data.table::setDT(games)
  data.table::setnames(
    games,
    old = c(
      "id",
      "source_id",
      "home_team",
      "home_conference",
      "away_team",
      "away_conference",
      "city",
      "state"
    ),
    new = c(
      "game_id",
      "game_source_id",
      "home_team_name",
      "home_conference_short_name",
      "away_team_name",
      "away_conference_short_name",
      "venue_city",
      "venue_state"
    )
  )
  clean_game_type(games)
  data.table::setDF(games)
  cbbd_save(games, paste0("games_", query_season), "games")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:cbbreadr::most_recent_season(), build_games, .progress = T)
} else {
  build_games()
}
