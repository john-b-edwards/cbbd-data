source("R/utils.R")
options(nflreadr.verbose = FALSE)

build_plays <- function(
  query_season = cbbreadr::most_recent_season(),
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
  plays <- dplyr::distinct(plays, id, .keep_all = TRUE)
  plays <- plays |>
    dplyr::select(id, participants) |>
    tidyr::unnest(c(participants), names_sep = "_") |>
    dplyr::group_by(id) |>
    dplyr::mutate(id_2 = dplyr::row_number()) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      names_from = "id_2",
      values_from = c(participants_id, participants_name),
      id_cols = id
    ) |>
    dplyr::right_join(
      dplyr::select(plays, -participants),
      by = dplyr::join_by("id")
    )
  plays <- plays |>
    dplyr::select(id, shot_info) |>
    tidyr::unnest_wider(shot_info, names_sep = '_') |>
    tidyr::drop_na() |>
    tidyr::unnest_wider(shot_info_shooter, names_sep = "_") |>
    tidyr::unnest_wider(shot_info_assistedBy, names_sep = "_") |>
    tidyr::unnest_wider(shot_info_location, names_sep = "_") |>
    dplyr::right_join(
      dplyr::select(plays, -shot_info),
      by = dplyr::join_by("id")
    )
  on_floor <- plays |>
    dplyr::select(id, on_floor) |>
    tidyr::drop_na()
  if (nrow(on_floor)) {
    plays <- on_floor |>
      tidyr::unnest_wider(on_floor, names_sep = "_") |>
      tidyr::unnest_wider(on_floor_id, names_sep = "_") |>
      tidyr::unnest_wider(on_floor_name, names_sep = "_") |>
      tidyr::unnest_wider(on_floor_team, names_sep = "_") |>
      dplyr::right_join(
        dplyr::select(plays, -on_floor),
        by = dplyr::join_by("id")
      )
  } else {
    plays <- plays |>
      dplyr::mutate(on_floor = NULL)
  }
  plays <- janitor::clean_names(plays)
  for (colname in paste0("on_floor_id_", 1:14)) {
    if (!(colname %in% colnames(plays))) {
      plays[[colname]] <- NA_integer_
    }
  }
  for (colname in paste0("on_floor_name_", 1:14)) {
    if (!(colname %in% colnames(plays))) {
      plays[[colname]] <- NA_character_
    }
  }
  for (colname in paste0("on_floor_team_", 1:14)) {
    if (!(colname %in% colnames(plays))) {
      plays[[colname]] <- NA_character_
    }
  }
  if (!build_from_scratch && nrow(existing_plays) > 0) {
    plays <- existing_plays |>
      dplyr::filter(!(as.Date(game_start_date) %in% to_scrape)) |>
      dplyr::bind_rows(
        plays
      )
  }
  data.table::setDT(plays)
  data.table::setnames(
    plays,
    old = c(
      "on_floor_id_1",
      "on_floor_id_2",
      "on_floor_id_3",
      "on_floor_id_4",
      "on_floor_id_5",
      "on_floor_id_6",
      "on_floor_id_7",
      "on_floor_id_8",
      "on_floor_id_9",
      "on_floor_id_10",
      "on_floor_id_11",
      "on_floor_id_12",
      "on_floor_id_13",
      "on_floor_id_14",
      "on_floor_name_1",
      "on_floor_name_2",
      "on_floor_name_3",
      "on_floor_name_4",
      "on_floor_name_5",
      "on_floor_name_6",
      "on_floor_name_7",
      "on_floor_name_8",
      "on_floor_name_9",
      "on_floor_name_10",
      "on_floor_name_11",
      "on_floor_name_12",
      "on_floor_name_13",
      "on_floor_name_14",
      "on_floor_team_1",
      "on_floor_team_2",
      "on_floor_team_3",
      "on_floor_team_4",
      "on_floor_team_5",
      "on_floor_team_6",
      "on_floor_team_7",
      "on_floor_team_8",
      "on_floor_team_9",
      "on_floor_team_10",
      "on_floor_team_11",
      "on_floor_team_12",
      "on_floor_team_13",
      "on_floor_team_14",
      "shot_info_shooter_id",
      "shot_info_shooter_name",
      "shot_info_made",
      "shot_info_range",
      "shot_info_assisted",
      "shot_info_assisted_by_id",
      "shot_info_assisted_by_name",
      "shot_info_location_x",
      "shot_info_location_y",
      "participants_id_1",
      "participants_id_2",
      "participants_name_1",
      "participants_name_2",
      "id",
      "source_id",
      "team",
      "conference",
      "opponent_id",
      "opponent",
      "opponent_conference",
      "wallclock"
    ),
    new = c(
      "on_floor_athlete_id_1",
      "on_floor_athlete_id_2",
      "on_floor_athlete_id_3",
      "on_floor_athlete_id_4",
      "on_floor_athlete_id_5",
      "on_floor_athlete_id_6",
      "on_floor_athlete_id_7",
      "on_floor_athlete_id_8",
      "on_floor_athlete_id_9",
      "on_floor_athlete_id_10",
      "on_floor_athlete_id_11",
      "on_floor_athlete_id_12",
      "on_floor_athlete_id_13",
      "on_floor_athlete_id_14",
      "on_floor_athlete_name_1",
      "on_floor_athlete_name_2",
      "on_floor_athlete_name_3",
      "on_floor_athlete_name_4",
      "on_floor_athlete_name_5",
      "on_floor_athlete_name_6",
      "on_floor_athlete_name_7",
      "on_floor_athlete_name_8",
      "on_floor_athlete_name_9",
      "on_floor_athlete_name_10",
      "on_floor_athlete_name_11",
      "on_floor_athlete_name_12",
      "on_floor_athlete_name_13",
      "on_floor_athlete_name_14",
      "on_floor_team_name_1",
      "on_floor_team_name_2",
      "on_floor_team_name_3",
      "on_floor_team_name_4",
      "on_floor_team_name_5",
      "on_floor_team_name_6",
      "on_floor_team_name_7",
      "on_floor_team_name_8",
      "on_floor_team_name_9",
      "on_floor_team_name_10",
      "on_floor_team_name_11",
      "on_floor_team_name_12",
      "on_floor_team_name_13",
      "on_floor_team_name_14",
      "shooter_athlete_id",
      "shooter_athlete_name",
      "shot_made",
      "shot_range",
      "shot_assisted",
      "assist_athlete_id",
      "assist_athlete_name",
      "shot_x_coordinate",
      "shot_y_coordinate",
      "primary_participant_athlete_id",
      "secondary_participant_athlete_id",
      "primary_participant_athlete_name",
      "secondary_participant_athlete_name",
      "play_id",
      "play_source_id",
      "team_name",
      "conference_short_name",
      "opponent_team_id",
      "opponent_name",
      "opponent_conference_short_name",
      "wall_clock"
    )
  )
  clean_game_type(plays)
  pct_to_decimal(plays)
  data.table::setorderv(plays, cols = c("play_id", "game_id"))
  plays[,clean_sub_data := (all(!is.na(on_floor_athlete_id_10)) & all(is.na(on_floor_athlete_id_11))), by=.(game_id)]
  data.table::setDF(plays)
  cbbd_save(plays, paste0("plays_", query_season), "plays")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2006:cbbreadr::most_recent_season(), \(x) {
    print(paste0("Building plays for ", x, "..."))
    build_plays(x, build_from_scratch = TRUE)
    print("Done!")
  })
} else {
  build_plays()
}
