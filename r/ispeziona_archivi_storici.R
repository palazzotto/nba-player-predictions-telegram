# Inventario in sola lettura degli RDS esistenti; pubblica solo un riepilogo JSON.

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("E' richiesto jsonlite.", call. = FALSE)
source(file.path("r", "archivi_storici_nba.R"))

radice <- normalizePath(".", mustWork = TRUE)
file_giocatori <- file.path(radice, "dati_nba.rds")
file_squadre <- file.path(radice, "dati_nba_t.rds")
file_box <- file.path(radice, "box_data.rds")
for (file in c(file_giocatori, file_squadre, file_box)) {
  if (!file.exists(file)) stop(sprintf("File sorgente assente: %s.", file), call. = FALSE)
}

giocatori <- readRDS(file_giocatori)
squadre <- readRDS(file_squadre)
box <- readRDS(file_box)
box_righe <- if (is.data.frame(box) && "dataBoxScore" %in% names(box) && length(box$dataBoxScore) >= 1L) box$dataBoxScore[[1L]] else NULL
if (!is.data.frame(box_righe)) stop("box_data.rds non contiene dataBoxScore[[1]] come data frame.", call. = FALSE)
valida_archivio_nba(box_righe, "giocatori")

stato <- list(
  generato_il = format(Sys.time(), tz = "Europe/Rome", usetz = TRUE),
  regola = "Lo storico non viene cancellato; le nuove osservazioni sostituiscono solo la stessa chiave.",
  giocatori = riepilogo_archivio(giocatori, "giocatori"),
  squadre = riepilogo_archivio(squadre, "squadre"),
  box_score = riepilogo_archivio(box_righe, "giocatori")
)
destinazione <- file.path(radice, "data", "cache", "stato_archivi_storici.json")
dir.create(dirname(destinazione), recursive = TRUE, showWarnings = FALSE)
temporaneo <- tempfile("stato_archivi_", tmpdir = dirname(destinazione), fileext = ".json")
jsonlite::write_json(stato, temporaneo, auto_unbox = TRUE, pretty = TRUE)
if (!file.rename(temporaneo, destinazione)) stop("Impossibile pubblicare lo stato degli archivi.", call. = FALSE)
cat(sprintf("Stato degli archivi salvato: %s\n", destinazione))
