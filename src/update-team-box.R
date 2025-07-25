source("R/utils.R")

build_team_box <- function(query_season = cbbreadr::most_recent_season()) {
  team_box <- query_games("games/teams", query_season)
  team_box <- team_box |>
    tidyr::unnest_wider(team_stats, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats, names_sep = "_")
  for (x in colnames(team_box)) {
    if (typeof(team_box[[x]]) == "list") {
      team_box <- tidyr::unnest_wider(
        team_box,
        dplyr::all_of(x),
        names_sep = "_"
      )
    }
  }
  team_box <- team_box |>
    tidyr::unnest_wider(team_stats_points_byPeriod, names_sep = "_") |>
    tidyr::unnest_wider(opponent_stats_points_byPeriod, names_sep = "_")
  for (colname in paste0("_stats_points_byPeriod_", 1:7)) {
    if (!(paste0("team", colname) %in% colnames(team_box))) {
      team_box[[paste0("team", colname)]] <- NA_integer_
      team_box[[paste0("opponent", colname)]] <- NA_integer_
    }
  }
  team_box <- janitor::clean_names(team_box)
  data.table::setDT(team_box)
  data.table::setnames(
    team_box,
    c(
      "team",
      "conference",
      "opponent_id",
      "opponent",
      "opponent_conference",
      "notes",
      "team_stats_possessions",
      "team_stats_assists",
      "team_stats_steals",
      "team_stats_blocks",
      "team_stats_true_shooting",
      "team_stats_rating",
      "team_stats_game_score",
      "team_stats_points_total",
      "team_stats_points_by_period_1",
      "team_stats_points_by_period_2",
      "team_stats_points_by_period_3",
      "team_stats_points_by_period_4",
      "team_stats_points_by_period_5",
      "team_stats_points_by_period_6",
      "team_stats_points_largest_lead",
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
      "opponent_stats_game_score",
      "opponent_stats_points_total",
      "opponent_stats_points_by_period_1",
      "opponent_stats_points_by_period_2",
      "opponent_stats_points_by_period_3",
      "opponent_stats_points_by_period_4",
      "opponent_stats_points_by_period_5",
      "opponent_stats_points_by_period_6",
      "opponent_stats_points_largest_lead",
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
      "opponent_stats_four_factors_offensive_rebound_pct",
      "pace"
    ),
    c(
      "team_name",
      "team_conference_short_name",
      "opponent_team_id",
      "opponent_team_name",
      "opponent_conference_short_name",
      "game_notes",
      "possessions",
      "assists",
      "steals",
      "blocks",
      "true_shooting",
      "offensive_rating",
      "game_score",
      "points",
      "points_1h",
      "points_2h",
      "points_1ot",
      "points_2ot",
      "points_3ot",
      "points_4ot",
      "largest_lead",
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
      "opponent_game_score",
      "opponent_points",
      "opponent_points_1h",
      "opponent_points_2h",
      "opponent_points_1ot",
      "opponent_points_2ot",
      "opponent_points_3ot",
      "opponent_points_4ot",
      "opponent_largest_lead",
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
      "opponent_offensive_rebound_pct",
      "game_pace"
    )
  )
  clean_game_type(team_box)
  pct_to_decimal(team_box)
  data.table::setDF(team_box)
  cbbd_save(
    team_box,
    paste0("team_box_scores_", query_season),
    "team_box_scores"
  )
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(
    2003:cbbreadr::most_recent_season(),
    build_team_box,
    .progress = T
  )
} else {
  build_team_box()
}
