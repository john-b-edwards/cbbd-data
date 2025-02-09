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
