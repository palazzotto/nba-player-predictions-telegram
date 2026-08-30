#!/usr/bin/env Rscript

# Esporta dal workbook cluster il CSV giornaliero letto dal bot. Python non
# interpreta il workbook e non calcola medie o rank.
`%||%` <- function(x, y) if (is.null(x)) y else x
args <- commandArgs(trailingOnly = TRUE)
argomento <- function(nome) {
  indice <- match(nome, args)
  if (is.na(indice) || indice == length(args)) return(NULL)
  args[[indice + 1L]]
}

data_richiesta <- argomento("--data")
file_workbook <- argomento("--workbook")
directory_output <- argomento("--output-dir") %||% "output"
if (is.null(data_richiesta) || is.null(file_workbook)) {
  stop("Uso: Rscript r/esporta_confronti_cluster.R --data YYYY-MM-DD --workbook percorso/medie_cluster_con_rank.xlsx [--output-dir output]")
}
if (is.na(as.Date(data_richiesta))) stop("Data non valida: ", data_richiesta)
if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Manca il pacchetto openxlsx.")

file_previsioni <- file.path(directory_output, paste0("previsioni_", data_richiesta, ".csv"))
file_output <- file.path(directory_output, paste0("confronti_cluster_", data_richiesta, ".csv"))
if (!file.exists(file_previsioni)) stop("Previsioni giornaliere mancanti: ", file_previsioni)
if (!file.exists(file_workbook)) stop("Workbook cluster mancante: ", file_workbook)

metriche <- data.frame(
  chiave = c("fgm", "fga", "fg3m", "fg3a", "fg2m", "fg2a", "ftm", "fta", "oreb", "ast", "tov", "pts"),
  etichetta = c("Tiri realizzati", "Tiri tentati", "Triple realizzate", "Triple tentate", "Doppie realizzate", "Doppie tentate", "Liberi realizzati", "Liberi tentati", "Rimbalzi offensivi", "Assist", "Palle perse", "Punti"),
  difensiva = c("subiti_fgm", "subiti_fga", "subiti_fg3m", "subiti_fg3a", "subiti_fg2m", "subiti_fg2a", "subiti_ftm", "subiti_fta", "rimbalzi_offensivi_concessi", "assist_concessi", "palle_perse_forzate", "punti_subiti"),
  stringsAsFactors = FALSE
)
attesi <- c("Offensivo C1", "Difensivo C1", "Offensivo C2", "Difensivo C2", "Offensivo C3", "Difensivo C3")
if (!identical(openxlsx::getSheetNames(file_workbook), attesi)) stop("Workbook cluster con fogli non validi.")
leggi_foglio <- function(nome) {
  tabella <- openxlsx::read.xlsx(file_workbook, sheet = nome)
  if (!all(c("slugTeam", "nameTeam", "Cluster", "Stile_gioco") %in% names(tabella))) stop("Schema foglio non valido: ", nome)
  tabella
}
offensivi <- lapply(attesi[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)], leggi_foglio)
difensivi <- lapply(attesi[c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)], leggi_foglio)
names(offensivi) <- names(difensivi) <- as.character(1:3)

previsioni <- read.csv(file_previsioni, stringsAsFactors = FALSE, check.names = FALSE)
campi_partita <- c("data_partita", "id_partita", "squadra_casa", "squadra_trasferta", "squadra")
if (!all(campi_partita %in% names(previsioni))) stop("Schema previsioni non valido per l'export cluster.")
righe_partita <- unique(previsioni[previsioni$data_partita == data_richiesta, campi_partita])
partite <- unique(righe_partita[c("id_partita", "squadra_casa", "squadra_trasferta")])
if (nrow(partite) == 0L) stop("Nessuna partita nel file delle previsioni per la data richiesta.")

trova_sigla <- function(id_partita, nome_squadra) {
  sigle <- unique(righe_partita$squadra[righe_partita$id_partita == id_partita])
  # Il calendario usa spesso il solo soprannome (es. Pistons), mentre il
  # workbook conserva città+soprannome (Detroit Pistons).
  risultati <- sigle[vapply(sigle, function(sigla) {
    nomi <- unlist(lapply(offensivi, function(tabella) tabella$nameTeam[tabella$slugTeam == sigla]))
    any(endsWith(tolower(nomi), tolower(nome_squadra)))
  }, logical(1))]
  if (length(risultati) != 1L) stop("Associazione non univoca della squadra ", nome_squadra, " al workbook cluster.")
  risultati[[1L]]
}

output <- list()
for (indice_partita in seq_len(nrow(partite))) {
  partita <- partite[indice_partita, ]
  casa <- trova_sigla(partita$id_partita, partita$squadra_casa)
  trasferta <- trova_sigla(partita$id_partita, partita$squadra_trasferta)
  for (cluster in names(offensivi)) {
    off <- offensivi[[cluster]]; dif <- difensivi[[cluster]]
    casa_off <- off[off$slugTeam == casa, ]; trasferta_off <- off[off$slugTeam == trasferta, ]
    casa_dif <- dif[dif$slugTeam == casa, ]; trasferta_dif <- dif[dif$slugTeam == trasferta, ]
    if (any(vapply(list(casa_off, trasferta_off, casa_dif, trasferta_dif), nrow, integer(1)) != 1L)) stop("Una squadra non compare una sola volta nelle tabelle cluster.")
    for (ordine in seq_len(nrow(metriche))) {
      metrica <- metriche[ordine, ]
      off_col <- paste0("media_tot_", metrica$chiave); dif_col <- paste0("media_", metrica$difensiva)
      off_rank <- sub("^media_", "rank_", off_col); dif_rank <- sub("^media_", "rank_", dif_col)
      if (!all(c(off_col, off_rank) %in% names(off)) || !all(c(dif_col, dif_rank) %in% names(dif))) stop("Colonne statistiche/rank mancanti nel workbook cluster.")
      output[[length(output) + 1L]] <- data.frame(
        data_partita = data_richiesta, id_partita = partita$id_partita, cluster = as.integer(cluster), stile = casa_off$Stile_gioco,
        ordine = ordine, statistica = metrica$chiave, etichetta = metrica$etichetta,
        casa_attacco_media = casa_off[[off_col]], casa_attacco_rank = casa_off[[off_rank]],
        trasferta_difesa_media = trasferta_dif[[dif_col]], trasferta_difesa_rank = trasferta_dif[[dif_rank]],
        trasferta_attacco_media = trasferta_off[[off_col]], trasferta_attacco_rank = trasferta_off[[off_rank]],
        casa_difesa_media = casa_dif[[dif_col]], casa_difesa_rank = casa_dif[[dif_rank]],
        generato_il = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), check.names = FALSE
      )
    }
  }
}
risultato <- do.call(rbind, output)
dir.create(directory_output, recursive = TRUE, showWarnings = FALSE)
temporaneo <- tempfile(pattern = paste0(basename(file_output), "_"), tmpdir = directory_output)
write.csv(risultato, temporaneo, row.names = FALSE)
controllo <- read.csv(temporaneo, stringsAsFactors = FALSE)
numeri <- c("casa_attacco_media", "trasferta_difesa_media", "trasferta_attacco_media", "casa_difesa_media")
if (nrow(controllo) != nrow(risultato) || any(!is.finite(as.matrix(controllo[numeri])))) stop("Validazione del CSV cluster fallita.")
if (!file.rename(temporaneo, file_output)) stop("Impossibile pubblicare il CSV cluster.")
cat("Pubblicato: ", file_output, "\n", sep = "")
