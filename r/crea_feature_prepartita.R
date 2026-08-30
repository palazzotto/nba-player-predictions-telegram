#!/usr/bin/env Rscript
# Esempio: Rscript r/crea_feature_prepartita.R --data=2026-10-20
args <- commandArgs(trailingOnly = TRUE)
root <- getwd()
source(file.path(root, "r", "feature_prepartita.R"))
argomento <- function(nome, predefinito = NULL) {
  valore <- args[startsWith(args, paste0("--", nome, "="))]
  if (!length(valore)) return(predefinito)
  sub(paste0("--", nome, "="), "", valore[[1L]], fixed = TRUE)
}
data_richiesta <- argomento("data")
if (is.null(data_richiesta) || is.na(as.Date(data_richiesta))) stop("Specificare --data=YYYY-MM-DD.", call. = FALSE)
file_roster <- argomento("roster", "data/cache/roster_nba_com_2026_27.csv")
file_calendario <- argomento("calendario", "data/cache/calendario_regular_2026_27.csv")
file_output <- argomento("output", file.path("data/cache", paste0("feature_prepartita_", data_richiesta, ".csv")))
calendario <- utils::read.csv(file.path(root, file_calendario), stringsAsFactors = FALSE, check.names = FALSE)
gare <- calendario[as.character(calendario$data_partita) == data_richiesta, , drop = FALSE]
if (!nrow(gare)) stop(sprintf("Nessuna gara cacheata per %s.", data_richiesta), call. = FALSE)
giocatori <- readRDS(file.path(root, "dati_nba.rds"))
box <- readRDS(file.path(root, "box_data.rds"))$dataBoxScore[[1L]]
avanzate <- intersect(c("fgContested", "fg2Contested", "screenAssist", "ptsScreenAssist", "deflections", "boxOuts", "boxOutsPlayerTREB", "boxOutsPlayerTeamRebound", "boxOutsOffense"), names(box))
if (length(avanzate)) {
  box <- box[, c("idGame", "idPlayer", avanzate), drop = FALSE]
  giocatori$idPlayer <- as.character(giocatori$idPlayer)
  box$idPlayer <- as.character(box$idPlayer)
  giocatori <- merge(giocatori, box, by = c("idGame", "idPlayer"), all.x = TRUE, sort = FALSE)
}
feature <- costruisci_feature_prepartita(giocatori, readRDS(file.path(root, "dati_nba_t.rds")), gare, utils::read.csv(file.path(root, file_roster), stringsAsFactors = FALSE, check.names = FALSE))
scrivi_feature_prepartita_atomico(feature, file.path(root, file_output))
cat(sprintf("Feature pre-partita pubblicate: %d righe; %d senza storico; %d idonee allo storico.\n", nrow(feature), sum(!feature$storico_disponibile), sum(feature$eligible_modello_storico)))
