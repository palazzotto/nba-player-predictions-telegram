# Trasforma le pagine NBA giornaliere cacheate in un calendario regular 2026/27.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("E' richiesto il pacchetto jsonlite.", call. = FALSE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

estrai_next_data <- function(html) {
  blocco <- regmatches(html, regexpr(
    '<script id="__NEXT_DATA__" type="application/json">.*?</script>', html, perl = TRUE
  ))
  if (length(blocco) == 0L || identical(blocco, "")) return(NULL)
  json <- sub('^<script id="__NEXT_DATA__" type="application/json">', "", blocco)
  jsonlite::fromJSON(sub("</script>$", "", json), simplifyVector = FALSE)
}

estrai_gare_file <- function(file) {
  data_partita <- as.Date(sub("\\.html$", "", basename(file)))
  pagina <- estrai_next_data(paste(readLines(file, warn = FALSE), collapse = "\n"))
  moduli <- pagina$props$pageProps$gameCardFeed$modules %||% list()
  if (length(moduli) == 0L) return(data.frame())
  righe <- lapply(moduli[[1]]$cards %||% list(), function(carta) {
    gara <- carta$cardData
    if (!identical(gara$seasonYear, "2026-27") || !identical(gara$seasonType, "Regular Season")) return(NULL)
    if (is.null(gara$gameId) || is.null(gara$awayTeam$teamId) || is.null(gara$homeTeam$teamId)) return(NULL)
    data.frame(
      yearSeason = 2027L, data_partita = data_partita,
      id_partita = as.character(gara$gameId), ora_utc = as.character(gara$gameTimeUtc %||% NA_character_),
      tipo_partita = as.character(gara$seasonType),
      id_squadra_trasferta = as.character(gara$awayTeam$teamId), squadra_trasferta = as.character(gara$awayTeam$teamName),
      sigla_trasferta = as.character(gara$awayTeam$teamTricode),
      id_squadra_casa = as.character(gara$homeTeam$teamId), squadra_casa = as.character(gara$homeTeam$teamName),
      sigla_casa = as.character(gara$homeTeam$teamTricode), stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(Filter(Negate(is.null), righe))
}

radice <- normalizePath(".", mustWork = TRUE)
cartella_cache <- file.path(radice, "data", "cache", "pagine_nba_2026_27")
file_html <- sort(list.files(cartella_cache, pattern = "^202[67]-[0-9]{2}-[0-9]{2}\\.html$", full.names = TRUE))
if (length(file_html) != 212L) stop(sprintf("Cache incompleta: trovate %d pagine, attese 212.", length(file_html)), call. = FALSE)

calendario <- dplyr::bind_rows(lapply(file_html, estrai_gare_file)) |>
  dplyr::distinct(.data$id_partita, .keep_all = TRUE) |>
  dplyr::arrange(.data$data_partita, .data$ora_utc, .data$id_partita)
if (nrow(calendario) < 1200L || nrow(calendario) > 1230L) {
  stop(sprintf("Calendario non valido: ottenute %d gare; attese fra 1200 e 1230.", nrow(calendario)), call. = FALSE)
}

file_destinazione <- file.path(radice, "data", "cache", "calendario_regular_2026_27.csv")
file_temporaneo <- tempfile(pattern = "calendario_regular_2026_27_", tmpdir = dirname(file_destinazione), fileext = ".csv")
utils::write.csv(calendario, file_temporaneo, row.names = FALSE, na = "")
if (!file.rename(file_temporaneo, file_destinazione)) stop("Impossibile pubblicare atomicamente il calendario.", call. = FALSE)

stato <- list(
  stagione = "2026-27",
  partite_definite = nrow(calendario),
  partite_regular_stagione = 1230L,
  partite_ancora_da_definire = 1230L - nrow(calendario),
  completo = nrow(calendario) == 1230L,
  generato_il = format(Sys.time(), tz = "Europe/Rome", usetz = TRUE),
  nota = "Le partite non ancora presenti nel calendario ufficiale NBA non vengono inventate."
)
file_stato <- file.path(dirname(file_destinazione), "stato_calendario_regular_2026_27.json")
file_stato_temporaneo <- tempfile(pattern = "stato_calendario_regular_2026_27_", tmpdir = dirname(file_stato), fileext = ".json")
jsonlite::write_json(stato, file_stato_temporaneo, auto_unbox = TRUE, pretty = TRUE)
if (!file.rename(file_stato_temporaneo, file_stato)) stop("Impossibile pubblicare atomicamente lo stato del calendario.", call. = FALSE)

cat(sprintf("Calendario regular 2026/27 salvato: %s (%d partite; %d ancora da definire)\n", file_destinazione, nrow(calendario), stato$partite_ancora_da_definire))
