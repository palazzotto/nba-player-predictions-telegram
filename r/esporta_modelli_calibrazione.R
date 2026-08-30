# Esporta gli oggetti addestrati dallo script di calibrazione nel formato letto
# dal generatore CSV del bot. La calibrazione e' conservata per fascia.
settimana_iso_calibrazione <- function(data) format(as.Date(data), "%G-W%V")

normalizza_calibrazione_fasce <- function(tabella, colonna_fascia, colonna_rmse,
                                          colonna_bias = NULL) {
  richieste <- c(colonna_fascia, colonna_rmse, "correzione_q30")
  mancanti <- setdiff(richieste, names(tabella))
  if (length(mancanti)) stop(sprintf("Calibrazione incompleta: %s.", paste(mancanti, collapse = ", ")), call. = FALSE)
  out <- data.frame(
    fascia = as.character(tabella[[colonna_fascia]]),
    correzione_q30 = as.numeric(tabella$correzione_q30),
    rmse = as.numeric(tabella[[colonna_rmse]]),
    bias = if (is.null(colonna_bias)) 0 else as.numeric(tabella[[colonna_bias]]),
    stringsAsFactors = FALSE
  )
  if (anyNA(out$fascia) || any(!is.finite(as.matrix(out[c("correzione_q30", "rmse", "bias")]))) ) stop("Valori non validi nella calibrazione per fascia.", call. = FALSE)
  out[!duplicated(out$fascia), , drop = FALSE]
}

crea_artefatto_modello_calibrato <- function(modello, target, feature, breaks, labels,
                                              tabella_calibrazione, colonna_fascia,
                                              colonna_rmse, colonna_bias = NULL) {
  if (!length(feature) || anyDuplicated(feature) || length(breaks) != length(labels) + 1L) stop("Configurazione modello/calibrazione non valida.", call. = FALSE)
  list(
    modello = modello,
    target = target,
    feature = as.character(feature),
    calibrazione_script = list(
      breaks = as.numeric(breaks), labels = as.character(labels),
      fasce = normalizza_calibrazione_fasce(tabella_calibrazione, colonna_fascia, colonna_rmse, colonna_bias)
    )
  )
}

pubblica_modelli_calibrazione <- function(configurazioni, data, cartella = "models") {
  richiesti <- c("minuti", "punti", "rimbalzi", "assist", "punti_assist", "punti_rimbalzi", "rimbalzi_assist")
  if (!all(richiesti %in% names(configurazioni))) stop("Mancano modelli richiesti per il bot.", call. = FALSE)
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  settimana <- settimana_iso_calibrazione(data)
  percorsi <- character(length(richiesti)); names(percorsi) <- richiesti
  for (nome in richiesti) {
    percorso <- file.path(cartella, paste0("modello_", nome, "_", settimana, ".rds"))
    tmp <- tempfile(".tmp_", dirname(percorso), ".rds")
    saveRDS(configurazioni[[nome]], tmp)
    if (!file.rename(tmp, percorso)) stop(sprintf("Pubblicazione modello fallita: %s.", nome), call. = FALSE)
    percorsi[[nome]] <- percorso
  }
  feature_settimanali <- lapply(configurazioni[richiesti], function(x) as.character(x$feature))
  if (any(lengths(feature_settimanali) != 40L) || any(vapply(feature_settimanali, anyDuplicated, integer(1)))) {
    stop("Le liste delle feature devono contenere 40 nomi unici per target.", call. = FALSE)
  }
  percorso_feature_rds <- file.path(cartella, paste0("features_", settimana, ".rds"))
  tmp_feature_rds <- tempfile(".tmp_", dirname(percorso_feature_rds), ".rds")
  saveRDS(feature_settimanali, tmp_feature_rds)
  if (!file.rename(tmp_feature_rds, percorso_feature_rds)) stop("Pubblicazione feature settimanali fallita.", call. = FALSE)
  audit_feature <- data.frame(
    target = rep(names(feature_settimanali), lengths(feature_settimanali)),
    posizione = unlist(lapply(feature_settimanali, seq_along), use.names = FALSE),
    feature = unlist(feature_settimanali, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  percorso_feature_csv <- file.path(cartella, paste0("features_", settimana, ".csv"))
  tmp_feature_csv <- tempfile(".tmp_", dirname(percorso_feature_csv), ".csv")
  utils::write.csv(audit_feature, tmp_feature_csv, row.names = FALSE)
  if (!file.rename(tmp_feature_csv, percorso_feature_csv)) stop("Pubblicazione audit feature fallita.", call. = FALSE)
  percorsi
}
