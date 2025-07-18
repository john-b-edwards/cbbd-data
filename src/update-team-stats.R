source("R/utils.R")

build_team_stats <- function(query_season = cbbreadr::most_recent_season()) {
  team_stats <- query_cbbd("stats/team/season", list(season = query_season)) |>
    tidyr::unnest_wider(team_stats, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_fieldGoals, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_twoPointFieldGoals, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_threePointFieldGoals, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_freeThrows, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_rebounds, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_turnovers, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_fouls, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_points, names_sep = "_") |>
    tidyr::unnest_wider(team_stats_fourFactors, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_fieldGoals, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_twoPointFieldGoals, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_threePointFieldGoals, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_freeThrows, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_rebounds, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_turnovers, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_fouls, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_points, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_fourFactors, names_sep = "_") |>
    janitor::clean_names()
  cbbd_save(team_stats, paste0("team_stats_", query_season), "team_stats")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(
    2003:cbbreadr::most_recent_season(),
    build_team_stats,
    .progress = T
  )
} else {
  build_team_stats()
}
