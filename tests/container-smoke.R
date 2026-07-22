.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

ui <- Gov2k1inSilico:::app_ui(NULL)
stopifnot(inherits(ui, c("shiny.tag.list", "shiny.tag")))
stopifnot(is.function(Gov2k1inSilico:::app_server))

port <- 3000L
app_process <- callr::r_bg(
  function(library_path, app_port) {
    .libPaths(c(library_path, .libPaths()))
    options(shiny.host = "127.0.0.1", shiny.port = app_port)
    Gov2k1inSilico::runGov2k1()
  },
  args = list(Sys.getenv("R_LIBS_USER"), port),
  supervise = TRUE
)
on.exit(app_process$kill(), add = TRUE)

url <- sprintf("http://127.0.0.1:%d", port)
response <- NULL
for (attempt in seq_len(30L)) {
  if (!app_process$is_alive()) {
    stop(paste(app_process$read_all_error(), collapse = "\n"))
  }

  response <- try(curl::curl_fetch_memory(url), silent = TRUE)
  if (!inherits(response, "try-error") && response$status_code == 200L) {
    break
  }
  Sys.sleep(1)
}

stopifnot(!inherits(response, "try-error"))
stopifnot(response$status_code == 200L)
stopifnot(grepl("<html", rawToChar(response$content), fixed = TRUE))
