# Recupero del calendario NBA ufficiale dalla pagina Games, una data alla volta.
# Questa via evita il feed annuale CDN, che puo' rispondere 403.

argomenti <- commandArgs(trailingOnly = FALSE)
file_script <- sub("^--file=", "", argomenti[grepl("^--file=", argomenti)])
radice_progetto <- if (length(file_script) == 1L) {
  normalizePath(file.path(dirname(file_script), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
setwd(radice_progetto)
argomenti_utente <- commandArgs(trailingOnly = TRUE)

if (!requireNamespace("httr2", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Sono richiesti i pacchetti httr2 e jsonlite.", call. = FALSE)
}

estrai_next_data <- function(html) {
  match <- regmatches(html, regexpr(
    '<script id="__NEXT_DATA__" type="application/json">.*?</script>',
    html,
    perl = TRUE
  ))
  if (length(match) == 0L || identical(match, "")) return(NULL)
  json <- sub('^<script id="__NEXT_DATA__" type="application/json">', "", match)
  json <- sub("</script>$", "", json)
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

scarica_gare_data <- function(data) {
  html <- tryCatch(
    httr2::request("https://www.nba.com/games") |>
      httr2::req_url_query(date = format(data, "%Y-%m-%d")) |>
      httr2::req_headers(
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
        Accept = "text/html,application/xhtml+xml"
      ) |>
      httr2::req_retry(max_tries = 3L) |>
      httr2::req_perform() |>
      httr2::resp_body_string(),
    error = function(e) stop(sprintf("%s: %s", format(data), conditionMessage(e)), call. = FALSE)
  )
  pagina <- estrai_next_data(html)
  moduli <- pagina$props$pageProps$gameCardFeed$modules %||% list()
  if (length(moduli) == 0L) {
    return(data.frame(
      yearSeason = integer(), data_partita = as.Date(character()), id_partita = character(),
      ora_utc = character(), tipo_partita = character(), id_squadra_trasferta = character(),
      squadra_trasferta = character(), sigla_trasferta = character(), id_squadra_casa = character(),
      squadra_casa = character(), sigla_casa = character(), stringsAsFactors = FALSE
    ))
  }
  carte <- moduli[[1]]$cards %||% list()
  righe <- lapply(carte, function(carta) {
    gara <- carta$cardData
    if (!identical(gara$seasonYear, "2026-27") || !identical(gara$seasonType, "Regular Season")) return(NULL)
    data.frame(
      yearSeason = 2027L,
      data_partita = as.Date(data),
      id_partita = as.character(gara$gameId),
      ora_utc = as.character(gara$gameTimeUtc %||% NA_character_),
      tipo_partita = as.character(gara$seasonType),
      id_squadra_trasferta = as.character(gara$awayTeam$teamId),
      squadra_trasferta = as.character(gara$awayTeam$teamName),
      sigla_trasferta = as.character(gara$awayTeam$teamTricode),
      id_squadra_casa = as.character(gara$homeTeam$teamId),
      squadra_casa = as.character(gara$homeTeam$teamName),
      sigla_casa = as.character(gara$homeTeam$teamTricode),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(Filter(Negate(is.null), righe))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

arg_data <- sub("^--data=", "", argomenti_utente[grepl("^--data=", argomenti_utente)])
if (length(arg_data) > 1L || (length(arg_data) == 1L && is.na(as.Date(arg_data)))) {
  stop("Usare al massimo un argomento nel formato --data=YYYY-MM-DD.", call. = FALSE)
}
date_stagione <- if (length(arg_data) == 1L) {
  as.Date(arg_data)
} else {
  seq(as.Date("2026-10-01"), as.Date("2027-04-30"), by = "day")
}
cat(sprintf("Consultazione NBA.com per %d date della regular season...\n", length(date_stagione)))
cluster <- parallel::makeCluster(min(4L, parallel::detectCores()))
on.exit(parallel::stopCluster(cluster), add = TRUE)
parallel::clusterExport(
  cluster,
  varlist = c("scarica_gare_data", "estrai_next_data", "%||%"),
  envir = environment()
)
gare_per_data <- parallel::parLapply(cluster, date_stagione, scarica_gare_data)
calendario <- dplyr::bind_rows(gare_per_data) |>
  dplyr::distinct(.data$id_partita, .keep_all = TRUE) |>
  dplyr::arrange(.data$data_partita, .data$ora_utc, .data$id_partita)

if (length(arg_data) == 0L && nrow(calendario) != 1230L) {
  stop(sprintf("Calendario incompleto: ottenute %d gare, attese 1230.", nrow(calendario)), call. = FALSE)
}

if (length(arg_data) == 1L) {
  print(calendario)
  quit(save = "no", status = 0L)
}

cartella_cache <- file.path("data", "cache")
dir.create(cartella_cache, recursive = TRUE, showWarnings = FALSE)
file_destinazione <- file.path(cartella_cache, "calendario_regular_2026_27.csv")
file_temporaneo <- tempfile(pattern = "calendario_regular_2026_27_", tmpdir = cartella_cache, fileext = ".csv")
utils::write.csv(calendario, file_temporaneo, row.names = FALSE, na = "")
if (!file.rename(file_temporaneo, file_destinazione)) {
  unlink(file_temporaneo)
  stop("Impossibile pubblicare atomicamente il calendario.", call. = FALSE)
}

cat(sprintf("Calendario regular 2026/27 salvato: %s (%d partite)\n", file_destinazione, nrow(calendario)))
