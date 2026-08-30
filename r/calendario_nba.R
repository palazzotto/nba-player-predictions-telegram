# Calendario NBA: sorgente isolata dalla pipeline dei modelli.
# Il codice NBA della stagione 2026/27 e' 2027 (anno in cui la stagione termina).

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

url_calendario_nba <- function() {
  "https://cdn.nba.com/static/json/staticData/scheduleLeagueV2_1.json"
}

leggi_calendario_nba <- function(url = url_calendario_nba()) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Per leggere il calendario e' richiesto il pacchetto httr2.", call. = FALSE)
  }

  tryCatch(
    httr2::request(url) |>
      httr2::req_headers(
        Accept = "application/json",
        Referer = "https://www.nba.com/",
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = FALSE),
    error = function(e) {
      stop(
        sprintf("Impossibile leggere il calendario NBA da %s: %s", url, conditionMessage(e)),
        call. = FALSE
      )
    }
  )
}

estrai_calendario_nba <- function(payload, anno_stagione = 2027L) {
  stopifnot(length(anno_stagione) == 1L, !is.na(anno_stagione))
  date_inizio <- as.Date(sprintf("%d-07-01", as.integer(anno_stagione) - 1L))
  date_fine <- as.Date(sprintf("%d-06-30", as.integer(anno_stagione)))
  date_gare <- payload$leagueSchedule$gameDates

  if (is.null(date_gare) || length(date_gare) == 0L) {
    stop("Il feed NBA non contiene leagueSchedule.gameDates.", call. = FALSE)
  }

  righe <- unlist(lapply(date_gare, function(giorno) {
    data_partita <- as.Date(giorno$gameDate %||% NA_character_)
    lapply(giorno$games %||% list(), function(gara) {
      ospite <- gara$awayTeam %||% list()
      casa <- gara$homeTeam %||% list()
      data.frame(
        yearSeason = as.integer(anno_stagione),
        data_partita = data_partita,
        id_partita = as.character(gara$gameId %||% NA_character_),
        ora_utc = as.character(gara$gameTimeUTC %||% NA_character_),
        tipo_partita = as.character(gara$gameType %||% NA_character_),
        id_squadra_trasferta = as.character(ospite$teamId %||% NA_character_),
        squadra_trasferta = as.character(ospite$teamName %||% NA_character_),
        sigla_trasferta = as.character(ospite$teamTricode %||% NA_character_),
        id_squadra_casa = as.character(casa$teamId %||% NA_character_),
        squadra_casa = as.character(casa$teamName %||% NA_character_),
        sigla_casa = as.character(casa$teamTricode %||% NA_character_),
        stringsAsFactors = FALSE
      )
    })
  }), recursive = FALSE)

  if (length(righe) == 0L) {
    stop("Il feed NBA non contiene gare.", call. = FALSE)
  }

  calendario <- dplyr::bind_rows(righe) |>
    dplyr::filter(
      .data$data_partita >= date_inizio,
      .data$data_partita <= date_fine,
      !is.na(.data$id_partita),
      !is.na(.data$id_squadra_casa),
      !is.na(.data$id_squadra_trasferta)
    ) |>
    dplyr::distinct(.data$id_partita, .keep_all = TRUE) |>
    dplyr::arrange(.data$data_partita, .data$ora_utc, .data$id_partita)

  if (nrow(calendario) == 0L) {
    stop(sprintf("Nessuna gara trovata per la stagione %d/%02d.", anno_stagione - 1L, anno_stagione %% 100L), call. = FALSE)
  }
  calendario
}

ottieni_calendario_nba <- function(anno_stagione = 2027L, lettore = leggi_calendario_nba) {
  estrai_calendario_nba(lettore(), anno_stagione = anno_stagione)
}
