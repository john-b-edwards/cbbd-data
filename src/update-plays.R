source("R/utils.R")
options(nflreadr.verbose = FALSE)

build_plays <- function(
  query_season = most_recent_season(),
  build_from_scratch = FALSE
) {
  games <- nflreadr::load_from_url(glue::glue(
    "https://github.com/john-b-edwards/cbbd-data/releases/download/games/games_{query_season}.rds"
  ))
  if (nrow(games) == 0) {
    stop("Queried season has no games! Have you updated games for this season?")
  }
  game_days <- unique(as.Date(games$start_date[games$status == "final"]))
  if (length(game_days) == 0) {
    return(NULL)
  }
  if (!build_from_scratch) {
    existing_plays <- suppressWarnings({
      nflreadr::load_from_url(glue::glue(
        "https://github.com/john-b-edwards/cbbd-data/releases/download/plays/plays_{query_season}.rds"
      ))
    })
    if (nrow(existing_plays)) {
      play_days <- unique(as.Date(existing_plays$game_start_date))
      play_days <- play_days[1:(length(play_days) - 1)]
      to_scrape <- as.Date(setdiff(game_days, play_days))
    } else {
      to_scrape <- game_days
    }
  } else {
    to_scrape <- game_days
  }
  plays <- to_scrape |>
    as.character() |>
    purrr::map(
      \(x) query_cbbd("plays/date", list(date = x)),
      .progress = TRUE
    ) |>
    purrr::list_rbind()
  plays <- plays |>
    dplyr::mutate(play_id = dplyr::row_number())
  plays <- plays |>
    dplyr::select(play_id, participants) |>
    tidyr::unnest(c(participants), names_sep = "_") |>
    dplyr::group_by(play_id) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      names_from = "id",
      values_from = c(participants_id, participants_name),
      id_cols = play_id
    ) |>
    dplyr::right_join(
      dplyr::select(plays, -participants),
      by = dplyr::join_by("play_id")
    )
  plays <- plays |>
    dplyr::select(play_id, shot_info) |>
    tidyr::unnest_wider(shot_info, names_sep = '_') |>
    tidyr::drop_na() |>
    tidyr::unnest_wider(shot_info_shooter, names_sep = "_") |>
    tidyr::unnest_wider(shot_info_assistedBy, names_sep = "_") |>
    tidyr::unnest_wider(shot_info_location, names_sep = "_") |>
    dplyr::right_join(
      dplyr::select(plays, -shot_info),
      by = dplyr::join_by("play_id")
    )
  on_floor <- plays |>
    dplyr::select(play_id, on_floor) |>
    tidyr::drop_na()
  if (nrow(on_floor)) {
    plays <- on_floor |>
      tidyr::unnest_wider(on_floor, names_sep = "_") |>
      tidyr::unnest_wider(on_floor_id, names_sep = "_") |>
      tidyr::unnest_wider(on_floor_name, names_sep = "_") |>
      tidyr::unnest_wider(on_floor_team, names_sep = "_") |>
      dplyr::right_join(
        dplyr::select(plays, -on_floor),
        by = dplyr::join_by("play_id")
      )
  } else {
    plays <- plays |>
      dplyr::mutate(on_floor = NULL)
  }
  plays <- janitor::clean_names(plays)
  for (colname in paste0("on_floor_id_", 1:12)) {
    if (!(colname %in% colnames(plays))) {
      plays[[colname]] <- NA_integer_
    }
  }
  for (colname in paste0("on_floor_name_", 1:12)) {
    if (!(colname %in% colnames(plays))) {
      plays[[colname]] <- NA_character_
    }
  }
  for (colname in paste0("on_floor_team_", 1:12)) {
    if (!(colname %in% colnames(plays))) {
      plays[[colname]] <- NA_character_
    }
  }
  if (!build_from_scratch & nrow(existing_plays) > 0) {
    plays <- existing_plays |>
      dplyr::filter(!(as.Date(game_start_date) %in% to_scrape)) |>
      dplyr::bind_rows(
        plays
      )
  }
  cbbd_save(plays, paste0("plays_", query_season), "plays")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2006:most_recent_season(), \(x) {
    print(paste0("Building plays for ", x, "..."))
    build_plays(x)
    print("Done!")
  })
} else {
  build_plays()
}
