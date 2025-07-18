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
  data.table::setDT(team_stats)
  data.table::setnames(
    team_stats,
    old = c(
      "team",
      "conference",
      "total_minutes",
      "team_stats_possessions",
      "team_stats_assists",
      "team_stats_steals",
      "team_stats_blocks",
      "team_stats_true_shooting",
      "team_stats_rating",
      "team_stats_points_total",
      "team_stats_points_fast_break",
      "team_stats_points_in_paint",
      "team_stats_points_off_turnovers",
      "team_stats_two_point_field_goals_made",
      "team_stats_two_point_field_goals_attempted",
      "team_stats_two_point_field_goals_pct",
      "team_stats_three_point_field_goals_made",
      "team_stats_three_point_field_goals_attempted",
      "team_stats_three_point_field_goals_pct",
      "team_stats_free_throws_made",
      "team_stats_free_throws_attempted",
      "team_stats_free_throws_pct",
      "team_stats_field_goals_made",
      "team_stats_field_goals_attempted",
      "team_stats_field_goals_pct",
      "team_stats_turnovers_total",
      "team_stats_turnovers_team_total",
      "team_stats_rebounds_offensive",
      "team_stats_rebounds_defensive",
      "team_stats_rebounds_total",
      "team_stats_fouls_total",
      "team_stats_fouls_technical",
      "team_stats_fouls_flagrant",
      "team_stats_four_factors_effective_field_goal_pct",
      "team_stats_four_factors_free_throw_rate",
      "team_stats_four_factors_turnover_ratio",
      "team_stats_four_factors_offensive_rebound_pct",
      "opponent_stats_possessions",
      "opponent_stats_assists",
      "opponent_stats_steals",
      "opponent_stats_blocks",
      "opponent_stats_true_shooting",
      "opponent_stats_rating",
      "opponent_stats_points_total",
      "opponent_stats_points_fast_break",
      "opponent_stats_points_in_paint",
      "opponent_stats_points_off_turnovers",
      "opponent_stats_two_point_field_goals_made",
      "opponent_stats_two_point_field_goals_attempted",
      "opponent_stats_two_point_field_goals_pct",
      "opponent_stats_three_point_field_goals_made",
      "opponent_stats_three_point_field_goals_attempted",
      "opponent_stats_three_point_field_goals_pct",
      "opponent_stats_free_throws_made",
      "opponent_stats_free_throws_attempted",
      "opponent_stats_free_throws_pct",
      "opponent_stats_field_goals_made",
      "opponent_stats_field_goals_attempted",
      "opponent_stats_field_goals_pct",
      "opponent_stats_turnovers_total",
      "opponent_stats_turnovers_team_total",
      "opponent_stats_rebounds_offensive",
      "opponent_stats_rebounds_defensive",
      "opponent_stats_rebounds_total",
      "opponent_stats_fouls_total",
      "opponent_stats_fouls_technical",
      "opponent_stats_fouls_flagrant",
      "opponent_stats_four_factors_effective_field_goal_pct",
      "opponent_stats_four_factors_free_throw_rate",
      "opponent_stats_four_factors_turnover_ratio",
      "opponent_stats_four_factors_offensive_rebound_pct"
    ),
    new = c(
      "team_name",
      "conference_short_name",
      "minutes",
      "possessions",
      "assists",
      "steals",
      "blocks",
      "true_shooting",
      "offensive_rating",
      "points",
      "points_fast_break",
      "points_in_paint",
      "points_off_turnovers",
      "two_point_field_goals_made",
      "two_point_field_goals_attempted",
      "two_point_field_goals_pct",
      "three_point_field_goals_made",
      "three_point_field_goals_attempted",
      "three_point_field_goals_pct",
      "free_throws_made",
      "free_throws_attempted",
      "free_throws_pct",
      "field_goals_made",
      "field_goals_attempted",
      "field_goals_pct",
      "turnovers",
      "team_turnovers",
      "rebounds_offensive",
      "rebounds_defensive",
      "rebounds_total",
      "fouls",
      "technical_fouls",
      "flagrant_fouls",
      "effective_field_goal_pct",
      "free_throw_rate",
      "assists_turnover_ratio",
      "offensive_rebound_pct",
      "opponent_possessions",
      "opponent_assists",
      "opponent_steals",
      "opponent_blocks",
      "opponent_true_shooting",
      "opponent_offensive_rating",
      "opponent_points",
      "opponent_points_fast_break",
      "opponent_points_in_paint",
      "opponent_points_off_turnovers",
      "opponent_two_point_field_goals_made",
      "opponent_two_point_field_goals_attempted",
      "opponent_two_point_field_goals_pct",
      "opponent_three_point_field_goals_made",
      "opponent_three_point_field_goals_attempted",
      "opponent_three_point_field_goals_pct",
      "opponent_free_throws_made",
      "opponent_free_throws_attempted",
      "opponent_free_throws_pct",
      "opponent_field_goals_made",
      "opponent_field_goals_attempted",
      "opponent_field_goals_pct",
      "opponent_turnovers",
      "opponent_team_turnovers",
      "opponent_rebounds_offensive",
      "opponent_rebounds_defensive",
      "opponent_rebounds_total",
      "opponent_fouls",
      "opponent_technical_fouls",
      "opponent_flagrant_fouls",
      "opponent_effective_field_goal_pct",
      "opponent_free_throw_rate",
      "opponent_assists_turnover_ratio",
      "opponent_offensive_rebound_pct"
    )
  )
  team_stats <- as.data.frame(team_stats)
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
