#!/usr/bin/env Rscript
# Genera il CSV Q30 per Telegram da feature pre-partita e modelli gia' salvati.
args <- commandArgs(trailingOnly = TRUE)
root <- getwd()

argomento <- function(nome, predefinito = NULL) {
  valore <- args[startsWith(args, paste0("--", nome, "="))]
  if (!length(valore)) return(predefinito)
  sub(paste0("--", nome, "="), "", valore[[1L]], fixed = TRUE)
}

data_richiesta <- argomento("data")
if (is.null(data_richiesta) || is.na(as.Date(data_richiesta))) {
  stop("Uso: Rscript r/genera_previsioni_bot.R --data=YYYY-MM-DD", call. = FALSE)
}

source(file.path(root, "r", "esporta_previsioni.R"))
settimana <- argomento("settimana", format(as.Date(data_richiesta), "%G-W%V"))
feature_file <- argomento("feature", file.path("data/cache", paste0("feature_prepartita_", data_richiesta, ".csv")))
calendario_file <- argomento("calendario", "data/cache/calendario_regular_2026_27.csv")
output_file <- argomento("output", file.path("output", paste0("previsioni_", data_richiesta, ".csv")))
registro_file <- argomento("registro", file.path("output", "registro_stime_emesse.csv"))
cluster_file <- argomento("cluster")

modelli_file <- c(
  minuti = "modello_minuti", punti = "modello_punti", rimbalzi = "modello_rimbalzi",
  assist = "modello_assist", punti_assist = "modello_punti_assist",
  punti_rimbalzi = "modello_punti_rimbalzi", rimbalzi_assist = "modello_rimbalzi_assist"
)
percorsi <- file.path(root, "models", paste0(modelli_file, "_", settimana, ".rds"))
if (length(mancanti <- percorsi[!file.exists(percorsi)])) {
  stop(sprintf("Modelli mancanti: %s", paste(basename(mancanti), collapse = ", ")), call. = FALSE)
}
if (!file.exists(file.path(root, feature_file))) stop("Feature pre-partita mancanti.", call. = FALSE)
if (!file.exists(file.path(root, calendario_file))) stop("Calendario cacheato mancante.", call. = FALSE)

modelli <- lapply(percorsi, readRDS)
names(modelli) <- names(modelli_file)
feature <- utils::read.csv(file.path(root, feature_file), stringsAsFactors = FALSE, check.names = FALSE)
calendario <- utils::read.csv(file.path(root, calendario_file), stringsAsFactors = FALSE, check.names = FALSE)
calendario <- calendario[as.character(calendario$data_partita) == data_richiesta, , drop = FALSE]
if (!nrow(calendario)) stop("Nessuna partita nel calendario per la data richiesta.", call. = FALSE)

predizioni <- prevedi_sequenza_futura(modelli, feature)
righe <- crea_righe_previsioni_bot(predizioni, calendario)
file_cluster <- if (is.null(cluster_file)) NULL else file.path(root, cluster_file)
righe <- aggiungi_stile_cluster(righe, file_cluster)
scrivi_csv_previsioni_atomico(righe, file.path(root, output_file))
registra_stime_emesse(righe, file.path(root, registro_file))
cat(sprintf("Previsioni Q30 pubblicate: %d righe, %d idonee (%s). Registro cumulativo: %s\n", nrow(righe), sum(righe$eligible_bot), output_file, registro_file))
