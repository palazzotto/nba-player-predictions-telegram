# Scarica e salva il calendario completo NBA 2026/27.
# Da eseguire dalla root del progetto:
# Rscript r/scarica_calendario_2026_27.R

argomenti <- commandArgs(trailingOnly = FALSE)
file_script <- sub("^--file=", "", argomenti[grepl("^--file=", argomenti)])
radice_progetto <- if (length(file_script) == 1L) {
  normalizePath(file.path(dirname(file_script), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
setwd(radice_progetto)
source(file.path("r", "calendario_nba.R"))

cartella_cache <- file.path("data", "cache")
dir.create(cartella_cache, recursive = TRUE, showWarnings = FALSE)
file_calendario <- file.path(cartella_cache, "calendario_2026_27.csv")
file_temporaneo <- tempfile(pattern = "calendario_2026_27_", tmpdir = cartella_cache, fileext = ".csv")

calendario <- ottieni_calendario_nba(anno_stagione = 2027L)

if (nrow(calendario) < 1L || anyDuplicated(calendario$id_partita) > 0L) {
  stop("Calendario non valido: nessuna gara oppure id_partita duplicati.", call. = FALSE)
}

utils::write.csv(calendario, file_temporaneo, row.names = FALSE, na = "")
if (!file.rename(file_temporaneo, file_calendario)) {
  unlink(file_temporaneo)
  stop("Impossibile pubblicare atomicamente il calendario.", call. = FALSE)
}

cat(sprintf("Calendario 2026/27 salvato: %s (%d partite)\n", file_calendario, nrow(calendario)))
