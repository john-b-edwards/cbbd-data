source("R/utils.R")

build_player_stats <- function(query_season = most_recent_season()) {
  player_stats <- query_cbbd(
    "stats/player/season",
    list(season = query_season)
  ) |>
    tidyr::unnest_wider(field_goals, names_sep = "_") |>
    tidyr::unnest_wider(two_point_field_goals, names_sep = "_") |>
    tidyr::unnest_wider(three_point_field_goals, names_sep = "_") |>
    tidyr::unnest_wider(free_throws, names_sep = "_") |>
    tidyr::unnest_wider(rebounds, names_sep = "_") |>
    tidyr::unnest_wider(win_shares, names_sep = "_") |>
    janitor::clean_names()
  data.table::setDT(player_stats)
  data.table::setnames(
    player_stats,
    old = c("team", "conference", "name", "position"),
    new = c(
      "team_name",
      "conference_short_name",
      "athlete_name",
      "athlete_position"
    )
  )
  cbbd_save(player_stats, paste0("player_stats_", query_season), "player_stats")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:most_recent_season(), build_player_stats, .progress = T)
} else {
  build_player_stats()
}
