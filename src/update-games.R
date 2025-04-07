source("R/utils.R")

build_games <- function(query_season = most_recent_season()) {
  start_date <- as.Date(paste0(query_season - 1, "-10-01"))
  end_date <- as.Date(paste0(query_season, "-05-01"))
  games <- list()
  counter <- 0
  while (start_date < end_date) {
    counter <- counter + 1
    games_df <- query_cbbd(
      "games",
      list(
        startDateRange = start_date,
        endDateRange = end_date
      )
    )
    games[[counter]] <- games_df
    if (nrow(games_df) == 3000) {
      # maximum number of games returned by API
      start_date <- max(as.Date(games_df$start_date))
    } else {
      start_date <- end_date
    }
  }
  games <- games |> purrr::list_rbind() |> dplyr::distinct(id, .keep_all = T)
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
