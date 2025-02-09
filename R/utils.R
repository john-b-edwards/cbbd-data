most_recent_season <- function() {
  if(as.integer(format(Sys.Date(),"%m")) < 10) {
    return(as.integer(format(Sys.Date(),"%Y")))
  } else {
    return(as.integer(format(Sys.Date(),"%Y")) + 1)
  }
}

query_cbbd <- function(my_path, my_query) {
  httr::RETRY(
    "GET",
    "https://api.collegebasketballdata.com",
    path = "games",
    query = my_query,
    httr::add_headers(
      accept = "application/json",
      Authorization = paste("Bearer", Sys.getenv("CFBD_API_KEY"))
    )
  ) |>
    httr::content("raw") |>
    RcppSimdJson::fparse() |>
    janitor::clean_names()
}

cbbd_save <- function(df, file_name, file_tag) {
  temp_dir <- tempdir(check = TRUE)
  saveRDS(df, file.path(temp_dir, paste0(file_name, ".rds")))
  data.table::fwrite(df, file.path(temp_dir, paste0(file_name, ".csv")))
  data.table::fwrite(df, file.path(temp_dir, paste0(file_name, ".csv.gz")))
  arrow::write_parquet(df, file.path(temp_dir, paste0(file_name, ".parquet")))
  qs::qsave(
    df,
    file.path(temp_dir, paste0(file_name, ".qs")),
    preset = "custom",
    algorithm = "zstd_stream",
    compress_level = 22,
    shuffle_control = 15
  )
  for (file in file.path(temp_dir)) {
    piggyback::pb_upload(
      file,
      repo = "john-b-edwards/cbbd-data",
      tag = file_tag,
      .token = gh::gh_token()
    )
  }
}
