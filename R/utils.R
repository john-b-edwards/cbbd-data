query_weeks <- function(year) {
  start <- seq.Date(
    from = as.Date(paste0(year - 1, "-11-01")),
    to = as.Date(paste0(year, "-04-30")),
    by = "week"
  )
  end <- start + 7
  return(data.frame(start_date = start, end_date = end))
}

query_days <- function(year) {
  start <- seq.Date(
    from = as.Date(paste0(year - 1, "-11-01")),
    to = as.Date(paste0(year, "-04-30")),
    by = "day"
  )
  end <- start + 1
  return(data.frame(start_date = start, end_date = end))
}

query_games <- function(
  my_path,
  query_season = cbbreadr::most_recent_season(),
  game_identifier = "game_id",
  max_results_returned = 3000
) {
  start_date <- as.Date(paste0(query_season - 1, "-10-01"))
  end_date <- as.Date(paste0(query_season, "-05-01"))
  dfs <- list()
  counter <- 0
  while (start_date < end_date) {
    counter <- counter + 1
    tmp_df <- query_cbbd(
      my_path,
      list(
        startDateRange = start_date,
        endDateRange = end_date
      )
    )
    dfs[[counter]] <- tmp_df
    if (nrow(tmp_df) == max_results_returned) {
      start_date <- max(as.Date(tmp_df$start_date))
    } else {
      start_date <- end_date
    }
  }
  dfs |>
    purrr::list_rbind() |>
    dplyr::distinct(!!dplyr::sym(game_identifier), .keep_all = T)
}

query_cbbd <- function(my_path, my_query) {
  df <- httr::RETRY(
    "GET",
    "https://api.collegebasketballdata.com",
    path = my_path,
    query = my_query,
    httr::add_headers(
      accept = "application/json",
      Authorization = paste("Bearer", Sys.getenv("CFBD_API_KEY"))
    ),
    config = httr::config(connecttimeout = 60)
  ) |>
    httr::content("raw") |>
    RcppSimdJson::fparse()
  if (!is.null(df)) {
    df <- janitor::clean_names(df)
  } else {
    df <- data.frame()
  }
  return(df)
}

cbbd_save <- function(df, file_name, file_tag) {
  temp_dir <- tempdir(check = TRUE)
  saveRDS(df, file.path(temp_dir, paste0(file_name, ".rds")))
  data.table::fwrite(df, file.path(temp_dir, paste0(file_name, ".csv")))
  data.table::fwrite(df, file.path(temp_dir, paste0(file_name, ".csv.gz")))
  arrow::write_parquet(df, file.path(temp_dir, paste0(file_name, ".parquet")))
  files <- list.files(temp_dir)
  files <- files[grepl(file_name, files)]
  for (file in file.path(temp_dir, files)) {
    piggyback::pb_upload(
      file,
      repo = "john-b-edwards/cbbd-data",
      tag = file_tag,
      .token = Sys.getenv("GITHUB_PAT")
    )
  }
}
