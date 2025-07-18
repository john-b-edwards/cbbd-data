source("R/utils.R")

build_player_box <- function(query_season = cbbreadr::most_recent_season()) {
  player_box <- query_games(
    "games/players",
    query_season,
    max_results_returned = 1000
  )
  player_box <- player_box |>
    tidyr::unnest(players)
  for (x in colnames(player_box)) {
    if (typeof(player_box[[x]]) == "list") {
      player_box <- tidyr::unnest_wider(
        player_box,
        dplyr::all_of(x),
        names_sep = "_"
      )
    }
  }
  player_box <- janitor::clean_names(player_box)
  data.table::setDT(player_box)
  data.table::setnames(
    player_box,
    old = c(
      "notes",
      "team",
      "team_conference",
      "opponent",
      "opponent_conference",
      "name",
      "position"
    ),
    new = c(
      "game_notes",
      "team_name",
      "team_conference_short_name",
      "opponent_name",
      "opponent_conference_short_name",
      "athlete_name",
      "athlete_position"
    )
  )
  player_box <- data.frame(player_box)
  cbbd_save(
    player_box,
    paste0("player_box_scores_", query_season),
    "player_box_scores"
  )
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(
    2003:cbbreadr::most_recent_season(),
    build_player_box,
    .progress = T
  )
} else {
  build_player_box()
}
