#!/usr/bin/env Rscript
# Pubblica gli archivi giocatori, squadre e box gia' acquisiti e validati.
args <- commandArgs(trailingOnly = TRUE)
data <- sub("^--data=", "", args[startsWith(args, "--data=")][1L])
if (is.na(data) || !nzchar(data) || is.na(as.Date(data))) stop("Uso: Rscript r/aggiorna_archivi_con_box.R --data=YYYY-MM-DD", call. = FALSE)
source(file.path(getwd(), "r", "archivi_storici_nba.R"))
source(file.path(getwd(), "r", "acquisisci_nuove_gare.R"))
cartella <- file.path("data", "raw")
aggiorna_archivio_nba("dati_nba.rds", file.path(cartella, paste0("nuove_giocatori_", data, ".rds")), "dati_nba.rds", "giocatori")
aggiorna_archivio_nba("dati_nba_t.rds", file.path(cartella, paste0("nuove_squadre_", data, ".rds")), "dati_nba_t.rds", "squadre")
aggiorna_box_data_nba("box_data.rds", file.path(cartella, paste0("nuovi_box_", data, ".rds")), "box_data.rds")
cat(sprintf("Archivi giocatori, squadre e box aggiornati per %s.\n", data))
