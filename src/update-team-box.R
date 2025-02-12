source("R/utils.R")

build_team_box <- function(query_season = most_recent_season()) {
  weeks <- query_weeks(query_season)
  team_box <- purrr::map2(weeks$start_date,
                          weeks$end_date,
                          \(x, y) query_cbbd("games/teams", list(
                            startDateRange = x, endDateRange = y
                          ))) |>
    purrr::list_rbind()
  team_box <- team_box |>
    tidyr::unnest_wider(offense, names_sep = "_") |>
    tidyr::unnest_wider(defense, names_sep = "_")
  for (x in colnames(team_box)) {
    if (typeof(team_box[[x]]) == "list") {
      team_box <- tidyr::unnest_wider(team_box, dplyr::all_of(x), names_sep = "_")
    }
  }
  team_box <- team_box |>
    tidyr::unnest_wider(offense_points_byPeriod, names_sep = "_") |>
    tidyr::unnest_wider(defense_points_byPeriod, names_sep = "_")
  for (colname in paste0("_points_byPeriod_", 1:7)) {
    if (!(paste0("offense", colname) %in% colnames(team_box))) {
      team_box[[paste0("offense", colname)]] <- NA_integer_
      team_box[[paste0("defense", colname)]] <- NA_integer_
    }
  }
  team_box <- janitor::clean_names(team_box)
  cbbd_save(team_box, paste0("team_box_scores_", query_season), "team_box_scores")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:most_recent_season(), build_team_box, .progress = T)
} else {
  build_team_box()
}
