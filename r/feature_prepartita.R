# Feature pre-partita per le previsioni NBA.
# Questo modulo legge soltanto gli storici: ogni riepilogo usa date strettamente
# precedenti alla gara richiesta, così una partita della stessa data non può
# introdurre leakage.

`%||%` <- function(x, y) if (is.null(x)) y else x

richiedi_colonne <- function(dati, colonne, nome) {
  mancanti <- setdiff(colonne, names(dati))
  if (length(mancanti)) stop(sprintf("%s privo di: %s.", nome, paste(mancanti, collapse = ", ")), call. = FALSE)
}

medie_prepartita <- function(dati, data_gara, metriche, suffisso = "") {
  dati <- dati[as.Date(dati$dateGame) < as.Date(data_gara), , drop = FALSE]
  risultato <- list()
  for (metrica in metriche) {
    valori <- suppressWarnings(as.numeric(dati[[metrica]]))
    valori <- valori[!is.na(valori)]
    medie <- c(L1 = tail(valori, 1L), L3 = mean(tail(valori, 3L)),
               L5 = mean(tail(valori, 5L)), L10 = mean(tail(valori, 10L)),
               season_avg = mean(valori))
    medie[is.na(medie)] <- 0
    for (nome in names(medie)) risultato[[paste0(metrica, suffisso, "_", nome)]] <- unname(medie[[nome]])
  }
  as.data.frame(risultato, check.names = FALSE)
}

prepara_difesa_storica <- function(storico_squadre) {
  richiedi_colonne(storico_squadre, c("idGame", "dateGame", "slugTeam", "slugOpponent", "yearSeason", "ptsTeam"), "Storico squadre")
  storico_squadre$dateGame <- as.Date(storico_squadre$dateGame)
  nomi_team <- setdiff(grep("Team$", names(storico_squadre), value = TRUE), "slugTeam")
  indice <- storico_squadre[, c("idGame", "slugTeam", nomi_team), drop = FALSE]
  nomi_opp <- sub("Team$", "Opp", nomi_team)
  names(indice)[match(nomi_team, names(indice))] <- nomi_opp
  difesa <- merge(storico_squadre, indice, by.x = c("idGame", "slugOpponent"), by.y = c("idGame", "slugTeam"), all.x = TRUE, sort = FALSE)
  difesa$possessionsOpp <- with(difesa, as.numeric(fgaOpp) + 0.44 * as.numeric(ftaOpp) + as.numeric(tovOpp) - as.numeric(orebOpp))
  difesa$defRatingOpp <- ifelse(difesa$possessionsOpp > 0, 100 * as.numeric(difesa$ptsOpp) / difesa$possessionsOpp, NA_real_)
  difesa
}

normalizza_calendario_feature <- function(calendario) {
  if (all(c("data_partita", "id_partita", "sigla_trasferta", "sigla_casa") %in% names(calendario))) {
    via <- data.frame(id_partita = as.character(calendario$id_partita), dateGame = as.Date(calendario$data_partita), sigla = toupper(calendario$sigla_trasferta), sigla_avversaria = toupper(calendario$sigla_casa), stringsAsFactors = FALSE)
    casa <- data.frame(id_partita = as.character(calendario$id_partita), dateGame = as.Date(calendario$data_partita), sigla = toupper(calendario$sigla_casa), sigla_avversaria = toupper(calendario$sigla_trasferta), stringsAsFactors = FALSE)
    return(rbind(via, casa))
  }
  richiedi_colonne(calendario, c("id_partita", "dateGame", "sigla", "sigla_avversaria"), "Calendario")
  transform(calendario[, c("id_partita", "dateGame", "sigla", "sigla_avversaria")], id_partita = as.character(id_partita), dateGame = as.Date(dateGame), sigla = toupper(sigla), sigla_avversaria = toupper(sigla_avversaria))
}

costruisci_feature_prepartita <- function(storico_giocatori, storico_squadre, calendario, mappa_roster, min_partite_stagione = 6L) {
  richiedi_colonne(storico_giocatori, c("idPlayer", "dateGame", "idGame", "yearSeason", "slugTeam", "slugOpponent", "namePlayer", "minutes", "pts", "ast", "treb"), "Storico giocatori")
  richiedi_colonne(mappa_roster, c("sigla", "idPlayer", "esito"), "Mappa roster")
  calendario <- normalizza_calendario_feature(calendario)
  esiti_utilizzabili <- c("confermato", "confermato_nba")
  confermati <- mappa_roster[mappa_roster$esito %in% esiti_utilizzabili & !is.na(mappa_roster$idPlayer) & nzchar(as.character(mappa_roster$idPlayer)), , drop = FALSE]
  confermati$sigla <- toupper(confermati$sigla)
  storico_giocatori$dateGame <- as.Date(storico_giocatori$dateGame)
  # Mantiene lo stesso insieme di trasformazioni del dataset storico: le
  # statistiche avanzate possono arrivare gia' unite dal chiamante.
  metriche_base <- intersect(c("minutes", "pts", "ast", "treb", "fga", "fta", "tov", "oreb", "dreb", "fgm", "fg3m", "fg3a", "fg2m", "fg2a", "ftm", "fgContested", "fg2Contested", "screenAssist", "ptsScreenAssist", "deflections", "boxOuts", "boxOutsPlayerTREB", "boxOutsPlayerTeamRebound", "boxOutsOffense"), names(storico_giocatori))
  for (nome in metriche_base) storico_giocatori[[nome]] <- suppressWarnings(as.numeric(storico_giocatori[[nome]]))
  for (nome in setdiff(c("fga", "fta", "tov", "fgm", "fg3m", "fg3a", "fg2m", "fg2a", "ftm", "ast", "treb", "pts", "minutes"), names(storico_giocatori))) storico_giocatori[[nome]] <- 0
  storico_giocatori$AST_TOV_RATIO <- storico_giocatori$ast / (storico_giocatori$tov + 1)
  storico_giocatori$EST_USG_PCT <- ifelse(storico_giocatori$minutes == 0, 0, (storico_giocatori$fga + .44 * storico_giocatori$fta + storico_giocatori$tov) / storico_giocatori$minutes)
  storico_giocatori$pts_ast <- storico_giocatori$pts + storico_giocatori$ast
  storico_giocatori$pts_treb <- storico_giocatori$pts + storico_giocatori$treb
  storico_giocatori$treb_ast <- storico_giocatori$treb + storico_giocatori$ast
  storico_giocatori$pts_reb_ast <- storico_giocatori$pts + storico_giocatori$treb + storico_giocatori$ast
  storico_giocatori <- storico_giocatori[order(storico_giocatori$idPlayer, storico_giocatori$dateGame, storico_giocatori$idGame), , drop = FALSE]
  difesa <- prepara_difesa_storica(storico_squadre)
  difesa <- difesa[order(difesa$slugTeam, difesa$dateGame, difesa$idGame), , drop = FALSE]
  metriche_giocatore <- intersect(c(metriche_base, "AST_TOV_RATIO", "EST_USG_PCT", "pts_ast", "pts_treb", "treb_ast", "pts_reb_ast"), names(storico_giocatori))
  metriche_difesa <- intersect(c("ptsOpp", "astOpp", "trebOpp", "orebOpp", "drebOpp", "fgaOpp", "ftaOpp", "tovOpp", "defRatingOpp"), names(difesa))
  righe <- lapply(seq_len(nrow(calendario)), function(i) {
    gara <- calendario[i, , drop = FALSE]
    candidati <- confermati[confermati$sigla == gara$sigla, , drop = FALSE]
    lapply(seq_len(nrow(candidati)), function(j) {
      candidato <- candidati[j, , drop = FALSE]
      storico <- storico_giocatori[as.character(storico_giocatori$idPlayer) == as.character(candidato$idPlayer) & storico_giocatori$dateGame < gara$dateGame, , drop = FALSE]
      ultima_stagione <- if (nrow(storico)) max(storico$yearSeason, na.rm = TRUE) else NA
      n_stagione <- if (is.na(ultima_stagione)) 0L else sum(storico$yearSeason == ultima_stagione)
      nome <- if ("nome_giocatore" %in% names(candidato)) candidato$nome_giocatore else if (nrow(storico)) storico$namePlayer[[nrow(storico)]] else NA_character_
      offensiva <- medie_prepartita(storico, gara$dateGame, metriche_giocatore)
      h2h <- storico[storico$slugOpponent == gara$sigla_avversaria, , drop = FALSE]
      h2h_feature <- list()
      for (metrica in intersect(c("pts", "ast", "treb", "pts_ast", "treb_ast"), names(h2h))) {
        valore <- mean(tail(as.numeric(h2h[[metrica]]), 3L), na.rm = TRUE)
        h2h_feature[[paste0(metrica, "_H2H_L3")]] <- ifelse(is.nan(valore), 0, valore)
      }
      difesa_avversaria <- difesa[difesa$slugTeam == gara$sigla_avversaria & difesa$dateGame < gara$dateGame, , drop = FALSE]
      difensiva <- medie_prepartita(difesa_avversaria, gara$dateGame, metriche_difesa)
      offensiva$minutes_sum_L3 <- offensiva$minutes_L1 + 2 * offensiva$minutes_L3
      offensiva$pts_per_min_L5 <- offensiva$pts_L5 / max(offensiva$minutes_L5, 1)
      offensiva$fga_per_min_L5 <- offensiva$fga_L5 / max(offensiva$minutes_L5, 1)
      base <- data.frame(id_partita = gara$id_partita, dateGame = gara$dateGame, sigla = gara$sigla, sigla_avversaria = gara$sigla_avversaria, idPlayer = as.character(candidato$idPlayer), namePlayer = as.character(nome), partite_stagione_storico = n_stagione, storico_disponibile = nrow(storico) > 0L, eligible_modello_storico = n_stagione >= min_partite_stagione, stringsAsFactors = FALSE)
      cbind(base, offensiva, as.data.frame(h2h_feature, check.names = FALSE), difensiva)
    })
  })
  righe <- unlist(righe, recursive = FALSE)
  righe <- righe[!vapply(righe, is.null, logical(1))]
  if (!length(righe)) return(data.frame())
  # Un rookie senza righe storiche può non avere alcune metriche opzionali:
  # pubblichiamo comunque lo stesso schema dei veterani, con valori neutri.
  tutte_colonne <- unique(unlist(lapply(righe, names), use.names = FALSE))
  righe <- lapply(righe, function(riga) {
    mancanti <- setdiff(tutte_colonne, names(riga))
    for (nome in mancanti) riga[[nome]] <- 0
    riga[, tutte_colonne, drop = FALSE]
  })
  risultato <- do.call(rbind, righe)
  stopifnot(all(risultato$dateGame > as.Date("1900-01-01")))
  rownames(risultato) <- NULL
  risultato
}

scrivi_feature_prepartita_atomico <- function(feature, destinazione) {
  if (!is.data.frame(feature) || !all(c("id_partita", "dateGame", "idPlayer", "storico_disponibile", "eligible_modello_storico") %in% names(feature))) stop("Schema feature pre-partita non valido.", call. = FALSE)
  dir.create(dirname(destinazione), recursive = TRUE, showWarnings = FALSE)
  temporaneo <- tempfile(".tmp_", tmpdir = dirname(destinazione), fileext = ".csv")
  utils::write.csv(feature, temporaneo, row.names = FALSE, na = "")
  if (!file.rename(temporaneo, destinazione)) { unlink(temporaneo); stop(sprintf("Impossibile pubblicare %s.", destinazione), call. = FALSE) }
  invisible(destinazione)
}
