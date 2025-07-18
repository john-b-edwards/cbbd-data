source("R/utils.R")

build_media <- function(query_season = cbbreadr::most_recent_season()) {
  media <- query_games("games/media", query_season)
  media <- media |>
    tidyr::unnest_longer(broadcasts) |>
    tidyr::unnest_wider(broadcasts, names_sep = "_")
  media <- janitor::clean_names(media)
  data.table::setDT(media)
  data.table::setnames(
    media,
    old = c(
      "home_conference",
      "away_conference",
      "notes",
      "broadcasts_broadcast_type",
      "broadcasts_broadcast_name"
    ),
    new = c(
      "home_conference_short_name",
      "away_conference_short_name",
      "game_notes",
      "broadcast_type",
      "broadcast_name"
    )
  )
  clean_game_type(media)
  media <- as.data.frame(media)
  cbbd_save(
    media,
    paste0("media_", query_season),
    "media"
  )
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:cbbreadr::most_recent_season(), build_media, .progress = T)
} else {
  build_media()
}
