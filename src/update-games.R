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
  data.table::setcolorder(
    games,
    c(
      "game_id",
      "game_source_id",
      "season_label",
      "season",
      "season_type",
      "tournament",
      "start_date",
      "start_time_tbd",
      "neutral_site",
      "conference_game",
      "game_type",
      "status",
      "game_notes",
      "attendance",
      "home_team_id",
      "home_team_name",
      "home_conference_id",
      "home_conference_short_name",
      "home_seed",
      "home_points",
      "home_period_points_1",
      "home_period_points_2",
      "home_period_points_3",
      "home_period_points_4",
      "home_period_points_5",
      "home_period_points_6",
      "home_period_points_7",
      "home_winner",
      "away_team_id",
      "away_team_name",
      "away_conference_id",
      "away_conference_short_name",
      "away_seed",
      "away_points",
      "away_period_points_1",
      "away_period_points_2",
      "away_period_points_3",
      "away_period_points_4",
      "away_period_points_5",
      "away_period_points_6",
      "away_period_points_7",
      "away_winner",
      "excitement",
      "venue_id",
      "venue",
      "venue_city",
      "venue_state"
    )
  )
  games <- as.data.frame(games)
  cbbd_save(games, paste0("games_", query_season), "games")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:cbbreadr::most_recent_season(), build_games, .progress = T)
} else {
  build_games()
}
