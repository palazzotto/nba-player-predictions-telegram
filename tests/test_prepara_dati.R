source(file.path("r", "prepara_dati.R"))
richiesti <- c("dati_nba.rds", "box_data.rds", "dati_nba_t.rds")
if (!all(file.exists(richiesti))) {
  cat("test_prepara_dati: SKIP (RDS locali non disponibili)\n")
  quit(save = "no", status = 0L)
}
dati <- readRDS("dati_nba.rds"); box <- readRDS("box_data.rds"); team <- readRDS("dati_nba_t.rds")
x <- prepara_dataset_storico_modelli(dati, box, team)
stopifnot(nrow(x) == nrow(dati), !anyDuplicated(x[c("idGame", "idPlayer")]), all(c("pts_L1", "fgContested_L5", "ptsOpp_L5", "eligible_modello_storico") %in% names(x)))
cat("test_prepara_dati: OK\n")
