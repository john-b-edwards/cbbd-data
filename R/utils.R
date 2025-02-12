most_recent_season <- function() {
  if(as.integer(format(Sys.Date(),"%m")) < 10) {
    return(as.integer(format(Sys.Date(),"%Y")))
  } else {
    return(as.integer(format(Sys.Date(),"%Y")) + 1)
  }
}

query_weeks <- function(year) {
  start <- seq.Date(from = as.Date(paste0(year - 1, "-11-01")),
                    to = as.Date(paste0(year, "-04-30")),
                    by = "week")
  end <- start + 7
  return(data.frame(start_date = start, end_date = end))
}

query_days <- function(year) {
  start <- seq.Date(from = as.Date(paste0(year - 1, "-11-01")),
                    to = as.Date(paste0(year, "-04-30")),
                    by = "day")
  end <- start + 1
  return(data.frame(start_date = start, end_date = end))
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
    )
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
  files <-  list.files(temp_dir)
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
