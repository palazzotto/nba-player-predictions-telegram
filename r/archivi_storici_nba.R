# Funzioni per mantenere separati lo storico consolidato e le nuove gare.
# Non modifica gli RDS sorgente finche' aggiorna_archivio_* non viene chiamata.

chiavi_archivio <- function(tipo = c("giocatori", "squadre")) {
  tipo <- match.arg(tipo)
  if (identical(tipo, "giocatori")) c("idGame", "idPlayer") else c("idGame", "slugTeam")
}

valida_archivio_nba <- function(dati, tipo = c("giocatori", "squadre")) {
  tipo <- match.arg(tipo)
  chiavi <- chiavi_archivio(tipo)
  if (!is.data.frame(dati)) stop("L'archivio deve essere un data frame.", call. = FALSE)
  mancanti <- setdiff(chiavi, names(dati))
  if (length(mancanti) > 0L) {
    stop(sprintf("Colonne chiave mancanti: %s.", paste(mancanti, collapse = ", ")), call. = FALSE)
  }
  if (anyNA(dati[chiavi]) || any(!nzchar(as.character(unlist(dati[chiavi], use.names = FALSE))))) {
    stop("Le chiavi dell'archivio non possono essere mancanti o vuote.", call. = FALSE)
  }
  duplicati <- duplicated(dati[chiavi])
  if (any(duplicati)) {
    stop(sprintf("Trovate %d chiavi duplicate (%s).", sum(duplicati), paste(chiavi, collapse = " + ")), call. = FALSE)
  }
  invisible(dati)
}

unisci_storico_e_nuove <- function(storico, nuove, tipo = c("giocatori", "squadre")) {
  tipo <- match.arg(tipo)
  chiavi <- chiavi_archivio(tipo)
  valida_archivio_nba(storico, tipo)
  valida_archivio_nba(nuove, tipo)

  # Le nuove osservazioni prevalgono soltanto sulla medesima chiave: lo storico
  # delle altre stagioni resta invariato.
  storico$.priorita_aggiornamento <- 1L
  nuove$.priorita_aggiornamento <- 2L
  colonne <- union(names(storico), names(nuove))
  storico[setdiff(colonne, names(storico))] <- NA
  nuove[setdiff(colonne, names(nuove))] <- NA
  unito <- rbind(storico[colonne], nuove[colonne])
  ordine <- do.call(order, c(unito[chiavi], list(unito$.priorita_aggiornamento)))
  unito <- unito[ordine, , drop = FALSE]
  unito <- unito[!duplicated(unito[chiavi], fromLast = TRUE), , drop = FALSE]
  unito$.priorita_aggiornamento <- NULL
  rownames(unito) <- NULL
  valida_archivio_nba(unito, tipo)
  unito
}

scrivi_rds_atomico <- function(oggetto, destinazione) {
  dir.create(dirname(destinazione), recursive = TRUE, showWarnings = FALSE)
  temporaneo <- tempfile(".tmp_", tmpdir = dirname(destinazione), fileext = ".rds")
  saveRDS(oggetto, temporaneo)
  if (!file.rename(temporaneo, destinazione)) {
    unlink(temporaneo)
    stop(sprintf("Impossibile pubblicare atomicamente %s.", destinazione), call. = FALSE)
  }
  invisible(destinazione)
}

aggiorna_archivio_nba <- function(file_storico, file_nuove, file_destinazione,
                                  tipo = c("giocatori", "squadre")) {
  tipo <- match.arg(tipo)
  if (!file.exists(file_storico)) stop(sprintf("Storico assente: %s.", file_storico), call. = FALSE)
  if (!file.exists(file_nuove)) stop(sprintf("Nuove osservazioni assenti: %s.", file_nuove), call. = FALSE)
  risultato <- unisci_storico_e_nuove(readRDS(file_storico), readRDS(file_nuove), tipo)
  scrivi_rds_atomico(risultato, file_destinazione)
  risultato
}

riepilogo_archivio <- function(dati, tipo = c("giocatori", "squadre")) {
  tipo <- match.arg(tipo)
  valida_archivio_nba(dati, tipo)
  list(
    tipo = tipo,
    righe = nrow(dati),
    chiave = paste(chiavi_archivio(tipo), collapse = " + "),
    partite_distinte = length(unique(dati$idGame)),
    stagioni = if ("yearSeason" %in% names(dati)) sort(unique(dati$yearSeason)) else NULL,
    data_minima = if ("dateGame" %in% names(dati)) as.character(min(as.Date(dati$dateGame), na.rm = TRUE)) else NULL,
    data_massima = if ("dateGame" %in% names(dati)) as.character(max(as.Date(dati$dateGame), na.rm = TRUE)) else NULL
  )
}
