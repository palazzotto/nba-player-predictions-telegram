#!/usr/bin/env Rscript
# Acquisisce e pubblica in una sola volta tutte le osservazioni di un intervallo.
args <- commandArgs(trailingOnly = TRUE)
arg <- function(nome) {
  x <- args[startsWith(args, paste0("--", nome, "="))]
  if (!length(x)) return(NA_character_)
  sub(paste0("--", nome, "="), "", x[[1L]], fixed = TRUE)
}
dal <- arg("dal"); al <- arg("al"); stagione <- arg("stagione")
if (is.na(dal) || is.na(al) || is.na(as.Date(dal)) || is.na(as.Date(al))) stop("Uso: Rscript r/aggiorna_intervallo_con_box.R --dal=YYYY-MM-DD --al=YYYY-MM-DD [--stagione=2027]", call. = FALSE)
if (is.na(stagione)) stagione <- as.character(as.integer(format(as.Date(al), "%Y")) + 1L)
root <- getwd(); source(file.path(root, "r", "archivi_storici_nba.R")); source(file.path(root, "r", "acquisisci_nuove_gare.R"))
nuove <- scarica_intervallo_gare_nba(dal, al, as.integer(stagione), file.path("data", "raw"))
if (!nuove$ha_gare) {
  cat(sprintf("Nessuna gara conclusa nell'intervallo %s -- %s: archivi invariati.\n", dal, al))
  quit(status = 0L)
}
tag <- tag_intervallo_date(dal, al); cartella <- file.path("data", "raw")
dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
backup <- c(dati_nba.rds = file.path(cartella, paste0("dati_nba_prima_", tag, ".rds")), dati_nba_t.rds = file.path(cartella, paste0("dati_nba_t_prima_", tag, ".rds")), box_data.rds = file.path(cartella, paste0("box_data_prima_", tag, ".rds")))
if (!all(file.copy(names(backup), backup, overwrite = TRUE))) stop("Backup degli archivi fallito: aggiornamento annullato.", call. = FALSE)
aggiorna_archivio_nba("dati_nba.rds", file.path(cartella, paste0("nuove_giocatori_", tag, ".rds")), "dati_nba.rds", "giocatori")
aggiorna_archivio_nba("dati_nba_t.rds", file.path(cartella, paste0("nuove_squadre_", tag, ".rds")), "dati_nba_t.rds", "squadre")
aggiorna_box_data_nba("box_data.rds", file.path(cartella, paste0("nuovi_box_", tag, ".rds")), "box_data.rds")
cat(sprintf("Archivi aggiornati per l'intervallo %s -- %s: %d gare. Backup: %s\n", dal, al, length(nuove$idGame), paste(unname(backup), collapse = "; ")))
