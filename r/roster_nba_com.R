# Roster ufficiale NBA.com. La pagina League Roster incorpora i dati nel nodo
# __NEXT_DATA__, inclusi PERSON_ID e TEAM_ABBREVIATION; non serve dedurre gli
# ID dal nome e i rookie senza game log restano rappresentati esplicitamente.

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x[[1L]])) y else x[[1L]]

valida_roster_nba_com <- function(roster) {
  richieste <- c("idPlayer", "nome_giocatore", "sigla", "TEAM", "esito", "fonte")
  mancanti <- setdiff(richieste, names(roster))
  if (length(mancanti)) stop(sprintf("Roster NBA.com privo di: %s.", paste(mancanti, collapse = ", ")), call. = FALSE)
  if (!nrow(roster)) stop("Roster NBA.com vuoto.", call. = FALSE)
  if (any(is.na(roster$idPlayer) | !nzchar(as.character(roster$idPlayer)) | is.na(roster$nome_giocatore) | !nzchar(roster$nome_giocatore))) stop("Roster NBA.com con ID o nome vuoti.", call. = FALSE)
  confermati <- roster$esito == "confermato_nba"
  if (any(is.na(roster$sigla[confermati]) | !nzchar(roster$sigla[confermati]))) stop("Roster NBA.com confermato con sigla vuota.", call. = FALSE)
  if (any(is.na(roster$TEAM[confermati]) | roster$TEAM[confermati] != roster$sigla[confermati])) stop("TEAM deve coincidere con la sigla NBA corrente.", call. = FALSE)
  if (anyDuplicated(roster$idPlayer)) stop("Roster NBA.com con idPlayer duplicati.", call. = FALSE)
  if (any(!roster$esito %in% c("confermato_nba", "revisione_squadra"))) stop("Esito roster NBA.com non valido.", call. = FALSE)
  invisible(TRUE)
}

estrai_roster_nba_com_da_html <- function(html) {
  if (!requireNamespace("rvest", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) stop("Servono i pacchetti rvest e jsonlite.", call. = FALSE)
  documento <- if (inherits(html, "xml_document")) html else rvest::read_html(html)
  nodo <- rvest::html_element(documento, "script#__NEXT_DATA__")
  if (inherits(nodo, "xml_missing")) stop("NBA.com non contiene __NEXT_DATA__.", call. = FALSE)
  dati <- jsonlite::fromJSON(rvest::html_text2(nodo), simplifyDataFrame = TRUE)
  giocatori <- dati$props$pageProps$players
  if (!is.data.frame(giocatori)) stop("NBA.com non espone props.pageProps.players come tabella.", call. = FALSE)
  richieste <- c("PERSON_ID", "PLAYER_FIRST_NAME", "PLAYER_LAST_NAME", "TEAM_ABBREVIATION")
  mancanti <- setdiff(richieste, names(giocatori))
  if (length(mancanti)) stop(sprintf("Tabella NBA.com priva di: %s.", paste(mancanti, collapse = ", ")), call. = FALSE)
  roster <- data.frame(
    idPlayer = as.character(giocatori$PERSON_ID),
    nome_giocatore = trimws(paste(giocatori$PLAYER_FIRST_NAME, giocatori$PLAYER_LAST_NAME)),
    sigla = toupper(trimws(giocatori$TEAM_ABBREVIATION)),
    stringsAsFactors = FALSE
  )
  roster$esito <- ifelse(is.na(roster$sigla) | !nzchar(roster$sigla), "revisione_squadra", "confermato_nba")
  roster$TEAM <- roster$sigla
  roster$fonte <- "NBA.com League Roster"
  roster$scaricato_il <- format(Sys.time(), tz = "Europe/Rome", usetz = TRUE)
  valida_roster_nba_com(roster)
  roster[order(roster$sigla, roster$nome_giocatore), c("idPlayer", "nome_giocatore", "TEAM", "sigla", "esito", "fonte", "scaricato_il"), drop = FALSE]
}

scarica_roster_nba_com <- function(url = "https://www.nba.com/players") {
  estrai_roster_nba_com_da_html(url)
}

scrivi_roster_nba_com_atomico <- function(roster, destinazione) {
  valida_roster_nba_com(roster)
  dir.create(dirname(destinazione), recursive = TRUE, showWarnings = FALSE)
  temporaneo <- tempfile(".tmp_", tmpdir = dirname(destinazione), fileext = ".csv")
  utils::write.csv(roster, temporaneo, row.names = FALSE, na = "")
  if (!file.rename(temporaneo, destinazione)) { unlink(temporaneo); stop(sprintf("Impossibile pubblicare %s.", destinazione), call. = FALSE) }
  invisible(destinazione)
}
