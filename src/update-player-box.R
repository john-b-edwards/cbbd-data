source("R/utils.R")

build_player_box <- function(query_season = most_recent_season()) {
  days <- query_days(query_season)
  player_box <- purrr::map2(days$start_date,
                          days$end_date,
                          \(x, y) query_cbbd("games/players", list(
                            startDateRange = x, endDateRange = y
                          ))) |>
    purrr::list_rbind()
  player_box <- player_box |>
    tidyr::unnest(players)
  for (x in colnames(player_box)) {
    if (typeof(player_box[[x]]) == "list") {
      player_box <- tidyr::unnest_wider(player_box, dplyr::all_of(x), names_sep = "_")
    }
  }
  player_box <- janitor::clean_names(player_box)
  cbbd_save(player_box, paste0("player_box_scores_", query_season), "player_box_scores")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:most_recent_season(), build_player_box, .progress = T)
} else {
  build_player_box()
}
