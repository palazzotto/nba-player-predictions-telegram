#!/usr/bin/env Rscript
root <- getwd()
source(file.path(root, "r", "roster_nba_com.R"))
args <- commandArgs(trailingOnly = TRUE)
file_output <- sub("^--output=", "", args[startsWith(args, "--output=")][1] %||% "data/cache/roster_nba_com_corrente.csv")
roster <- scarica_roster_nba_com()
scrivi_roster_nba_com_atomico(roster, file.path(root, file_output))
cat(sprintf("Roster NBA.com pubblicato: %d giocatori, %d confermati, %d squadre assegnate.\n", nrow(roster), sum(roster$esito == "confermato_nba"), length(unique(roster$sigla[roster$esito == "confermato_nba"]))))
