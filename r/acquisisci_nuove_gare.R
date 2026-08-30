# Acquisizione isolata delle gare concluse: produce input separati, mai RDS storici.
source(file.path(getwd(), "r", "archivi_storici_nba.R"))

filtra_gare_concluse <- function(dati, data) dati[as.Date(dati$dateGame) == as.Date(data), , drop = FALSE]
filtra_gare_intervallo <- function(dati, dal, al) {
  date <- as.Date(dati$dateGame)
  dati[date >= as.Date(dal) & date <= as.Date(al), , drop = FALSE]
}
tag_intervallo_date <- function(dal, al) paste(as.character(as.Date(dal)), as.character(as.Date(al)), sep = "_")

tabella_box_giocatori <- function(box_data) {
  if (is.data.frame(box_data) && all(c("idGame", "idPlayer") %in% names(box_data))) return(box_data)
  if (is.data.frame(box_data) && "dataBoxScore" %in% names(box_data) && length(box_data$dataBoxScore)) return(box_data$dataBoxScore[[1L]])
  if (is.list(box_data) && "dataBoxScore" %in% names(box_data) && length(box_data$dataBoxScore)) return(box_data$dataBoxScore[[1L]])
  if (is.list(box_data) && length(box_data) >= 2L && is.list(box_data[[2L]]) && length(box_data[[2L]])) return(box_data[[2L]][[1L]])
  stop("Impossibile estrarre dataBoxScore[[1]] dal risultato box score.", call. = FALSE)
}

valida_tabella_box_giocatori <- function(box) {
  if (!is.data.frame(box)) stop("Il box score giocatori deve essere un data frame.", call. = FALSE)
  valida_archivio_nba(box, "giocatori")
  invisible(box)
}

unisci_box_giocatori <- function(storico, nuove) {
  valida_tabella_box_giocatori(storico); valida_tabella_box_giocatori(nuove)
  storico$.priorita_aggiornamento <- 1L; nuove$.priorita_aggiornamento <- 2L
  colonne <- union(names(storico), names(nuove))
  storico[setdiff(colonne, names(storico))] <- NA; nuove[setdiff(colonne, names(nuove))] <- NA
  unito <- rbind(storico[colonne], nuove[colonne])
  unito <- unito[order(unito$idGame, unito$idPlayer, unito$.priorita_aggiornamento), , drop = FALSE]
  unito <- unito[!duplicated(unito[c("idGame", "idPlayer")], fromLast = TRUE), , drop = FALSE]
  unito$.priorita_aggiornamento <- NULL; rownames(unito) <- NULL
  valida_tabella_box_giocatori(unito); unito
}

aggiorna_box_data_nba <- function(file_storico, file_nuove, file_destinazione) {
  if (!file.exists(file_storico)) stop(sprintf("Box score storico assente: %s.", file_storico), call. = FALSE)
  if (!file.exists(file_nuove)) stop(sprintf("Nuovi box score assenti: %s.", file_nuove), call. = FALSE)
  contenitore <- readRDS(file_storico)
  if (!is.data.frame(contenitore) || !"dataBoxScore" %in% names(contenitore) || !length(contenitore$dataBoxScore)) stop("box_data.rds non conserva dataBoxScore[[1]].", call. = FALSE)
  contenitore$dataBoxScore[[1L]] <- unisci_box_giocatori(tabella_box_giocatori(contenitore), tabella_box_giocatori(readRDS(file_nuove)))
  scrivi_rds_atomico(contenitore, file_destinazione); invisible(contenitore)
}

scarica_nuove_gare_nba <- function(data, stagione, cartella = "data/raw", lettore_box = NULL) {
  data <- as.Date(data)
  if (is.na(data)) stop("La data deve essere nel formato YYYY-MM-DD.", call. = FALSE)
  if (data >= Sys.Date()) stop(sprintf("Non posso acquisire osservazioni del %s: la data non e' ancora conclusa. Per stimare una data futura genera soltanto feature e CSV, senza usare game_logs().", data), call. = FALSE)
  if (!requireNamespace("nbastatR", quietly = TRUE)) stop("Serve nbastatR.", call. = FALSE)
  leggi_log <- function(tipo) tryCatch(nbastatR::game_logs(seasons = stagione, result_types = tipo, assign_to_environment = FALSE), error = function(e) stop(sprintf("NBA non ha restituito game log leggibili per la stagione %s. Verifica che la stagione sia iniziata e riprova piu' tardi. Dettaglio: %s", stagione, conditionMessage(e)), call. = FALSE))
  p <- filtra_gare_concluse(leggi_log("player"), data)
  t <- filtra_gare_concluse(leggi_log("team"), data)
  valida_archivio_nba(p, "giocatori"); valida_archivio_nba(t, "squadre")
  id_game <- unique(as.character(p$idGame))
  if (!length(id_game)) stop("Nessuna gara giocatori conclusa per la data richiesta.", call. = FALSE)
  scarica_box <- if (is.null(lettore_box)) function(ids) nbastatR::box_scores(game_ids = ids, box_score_types = c("traditional", "hustle"), result_types = "player", join_data = TRUE, assign_to_environment = FALSE, return_message = FALSE) else lettore_box
  box <- tabella_box_giocatori(scarica_box(id_game)); valida_tabella_box_giocatori(box)
  if (length(setdiff(unique(as.character(box$idGame)), id_game))) stop("Il box score contiene idGame non richiesti.", call. = FALSE)
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  scrivi_rds_atomico(p, file.path(cartella, paste0("nuove_giocatori_", data, ".rds")))
  scrivi_rds_atomico(t, file.path(cartella, paste0("nuove_squadre_", data, ".rds")))
  scrivi_rds_atomico(box, file.path(cartella, paste0("nuovi_box_", data, ".rds")))
  invisible(list(giocatori = p, squadre = t, box = box, idGame = id_game))
}

scarica_intervallo_gare_nba <- function(dal, al, stagione, cartella = "data/raw",
                                        lettore_game_logs = NULL, lettore_box = NULL) {
  dal <- as.Date(dal); al <- as.Date(al)
  if (is.na(dal) || is.na(al) || dal > al) stop("Intervallo non valido: usare --dal=YYYY-MM-DD --al=YYYY-MM-DD.", call. = FALSE)
  if (al >= Sys.Date()) stop(sprintf("Non posso acquisire fino al %s: la data finale non e' ancora conclusa.", al), call. = FALSE)
  if (!requireNamespace("nbastatR", quietly = TRUE) && is.null(lettore_game_logs)) stop("Serve nbastatR.", call. = FALSE)
  leggi_log <- if (is.null(lettore_game_logs)) function(tipo) nbastatR::game_logs(seasons = stagione, result_types = tipo, assign_to_environment = FALSE) else lettore_game_logs
  leggi_log_sicuro <- function(tipo) tryCatch(leggi_log(tipo), error = function(e) stop(sprintf("NBA non ha restituito game log leggibili per la stagione %s. Verifica che la stagione sia iniziata e riprova piu' tardi. Dettaglio: %s", stagione, conditionMessage(e)), call. = FALSE))
  p <- filtra_gare_intervallo(leggi_log_sicuro("player"), dal, al)
  t <- filtra_gare_intervallo(leggi_log_sicuro("team"), dal, al)
  if (!nrow(p)) return(invisible(list(ha_gare = FALSE, dal = dal, al = al, idGame = character())))
  valida_archivio_nba(p, "giocatori"); valida_archivio_nba(t, "squadre")
  id_game <- unique(as.character(p$idGame))
  scarica_box <- if (is.null(lettore_box)) function(ids) nbastatR::box_scores(game_ids = ids, box_score_types = c("traditional", "hustle"), result_types = "player", join_data = TRUE, assign_to_environment = FALSE, return_message = FALSE) else lettore_box
  box <- tabella_box_giocatori(scarica_box(id_game)); valida_tabella_box_giocatori(box)
  if (length(setdiff(unique(as.character(box$idGame)), id_game))) stop("Il box score contiene idGame non richiesti.", call. = FALSE)
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  tag <- tag_intervallo_date(dal, al)
  scrivi_rds_atomico(p, file.path(cartella, paste0("nuove_giocatori_", tag, ".rds")))
  scrivi_rds_atomico(t, file.path(cartella, paste0("nuove_squadre_", tag, ".rds")))
  scrivi_rds_atomico(box, file.path(cartella, paste0("nuovi_box_", tag, ".rds")))
  invisible(list(ha_gare = TRUE, giocatori = p, squadre = t, box = box, idGame = id_game, dal = dal, al = al))
}
