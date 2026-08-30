# ============================================================================
# Previsioni NBA: feature engineering, modelli Ranger e calibrazione Q30
# Il file costruisce previsioni temporali senza usare informazioni future;
# per ogni statistica, il Q30 fornisce una soglia prudente con copertura
# attesa vicina al 70% (valore reale >= soglia).

# Eseguire dalla root del progetto; nessun percorso assoluto o cambio directory.

# Pacchetti strettamente necessari alla pipeline.
Sys.setenv(VROOM_CONNECTION_SIZE=10000000)
library(nbastatR)
library(dplyr)
library(slider)
library(ranger)
library(tidyr)
library(stringr)
library(tidyverse)
library(ggplot2)

# Report degli errori: sintetizza accuratezza complessiva e sovrastime.
val_err <- function(df, col_reale, col_pred, nome_modello){
  #Dati
  reale <- as.numeric(df[[col_reale]])
  pred <- as.numeric(df[[col_pred]])
  #Metriche globali
  err_glob <- pred - reale
  mae_glob <- mean(abs(err_glob), na.rm = T)
  rmse_glob <- sqrt(mean(err_glob^2, na.rm = T))
  #R2
  ss_res <- sum(err_glob^2, na.rm = T)
  ss_tot <- sum((reale - mean(reale, na.rm = T))^2, na.rm = T)
  r2_glob <- 1 - (ss_res / ss_tot)
  #Sovrastime
  idx_sovr <- which(pred > reale & reale > 0)
  err_sovr <- pred[idx_sovr] - reale[idx_sovr]
  pct_sovr <- (err_sovr / reale[idx_sovr]) * 100
  freq_sovr <- (length(idx_sovr) / sum(!is.na(reale))) * 100
  mae_sovr <- mean(err_sovr, na.rm = T)
  rmse_sovr <- sqrt(mean(err_sovr^2, na.rm = T))
  mape_sovr <- mean(pct_sovr, na.rm = T)
  #Report
  cat(
    "\n==================================================",
    sprintf("  [REPORT ERRORI]: %s", nome_modello),
    "==================================================",
    sprintf("MAE Globale (Tutte le gare):    %-6.2f punti", mae_glob),
    sprintf("RMSE Globale:                   %-6.2f punti", rmse_glob),
    sprintf("R² (Varianza Spiegata):          %-6.2f %%", r2_glob * 100),
    "--------------------------------------------------",
    sprintf("Frequenza di Sovrastima:        %-6.1f %%", freq_sovr),
    sprintf("MAE sulle Sovrastime:           %-6.2f punti", mae_sovr),
    sprintf("RMSE sulle Sovrastime:          %-6.2f punti", rmse_sovr),
    sprintf("MAPE sulle Sovrastime:          %-1.1f %%", mape_sovr),
    "==================================================\n",
    sep = "\n"
  )
}

# Metriche comuni per validation e test dei modelli di previsione.
metriche_previsione <- function(df, col_reale, col_pred) {
  df %>% summarise(
    N = n(),
    Bias_medio = mean(.data[[col_pred]] - .data[[col_reale]], na.rm = TRUE),
    Errore_medio_assoluto = mean(abs(.data[[col_pred]] - .data[[col_reale]]), na.rm = TRUE),
    RMSE = sqrt(mean((.data[[col_pred]] - .data[[col_reale]])^2, na.rm = TRUE))
  )
}

riepilogo_eligibilita <- function(df, nome_modello, colonna_eligible = NULL) {
  eligible <- if (is.null(colonna_eligible)) rep(TRUE, nrow(df)) else df[[colonna_eligible]]
  tibble(split_temporale = df$split_temporale, eligible = eligible) %>%
    group_by(split_temporale) %>%
    summarise(
      modello = nome_modello,
      righe_totali = n(),
      righe_meta_feature_disponibili = sum(eligible, na.rm = TRUE),
      righe_effettivamente_usate = sum(eligible, na.rm = TRUE),
      righe_escluse = sum(!eligible | is.na(eligible)),
      .groups = "drop"
    ) %>%
    select(modello, everything())
}

numero_thread_ranger <- function() {
  n_core <- parallel::detectCores()
  if (is.na(n_core) || n_core < 2L) return(1L)
  max(1L, n_core - 1L)
}

# OOF expanding-window per date uniche, con calibrazione Q30 rolling. Le
# variabili dateGame e row_id_master restano fuori da train_data e da ranger.
genera_oof_temporali <- function(
  train_data, date_game, formula, best_params, target_col, breaks, labels,
  initial_train_frac = 0.20, calibration_frac = 0.05, n_folds = 6, seed = 123
) {
  stopifnot(nrow(train_data) == length(date_game))
  date_game <- as.Date(date_game)
  unique_dates <- sort(unique(date_game))
  n_dates <- length(unique_dates)
  raw <- q30 <- correzione_q30 <- rep(NA_real_, nrow(train_data))
  fascia_q30 <- rep(NA_character_, nrow(train_data))
  log_folds <- tibble()
  if (n_dates < 3L) return(list(raw = raw, q30 = q30, correzione_q30 = correzione_q30,
                                 fascia_q30 = fascia_q30, log = log_folds))

  n_initial <- max(1L, floor(n_dates * initial_train_frac))
  n_calibration <- max(1L, floor(n_dates * calibration_frac))
  n_calibration <- min(n_calibration, n_dates - n_initial)
  if (n_calibration < 1L) return(list(raw = raw, q30 = q30, correzione_q30 = correzione_q30,
                                      fascia_q30 = fascia_q30, log = log_folds))

  calibration_dates <- unique_dates[(n_initial + 1L):(n_initial + n_calibration)]
  historical_residuals <- tibble(fascia = character(), residuo = numeric(), dateGame = as.Date(character()))

  esegui_fold <- function(train_dates, pred_dates, fold_id, is_calibration = FALSE) {
    idx_train_fold <- which(date_game %in% train_dates)
    idx_pred_fold <- which(date_game %in% pred_dates)
    date_train_fold <- date_game[idx_train_fold]
    date_oof_fold <- date_game[idx_pred_fold]
    stopifnot(max(date_train_fold) < min(date_oof_fold))
    modello_fold <- ranger(
      formula = formula, data = train_data[idx_train_fold, , drop = FALSE],
      num.trees = best_params$num.trees, mtry = best_params$mtry,
      min.node.size = best_params$min.node.size,
      sample.fraction = best_params$sample.fraction,
      respect.unordered.factors = "order", seed = seed, verbose = FALSE,
      num.threads = numero_thread_ranger()
    )
    raw[idx_pred_fold] <<- predict(modello_fold, data = train_data[idx_pred_fold, , drop = FALSE])$predictions
    fascia_corrente <- as.character(cut(raw[idx_pred_fold], breaks = breaks, labels = labels, include.lowest = TRUE))
    fascia_q30[idx_pred_fold] <<- fascia_corrente
    n_residui <- nrow(historical_residuals)
    if (!is_calibration && n_residui > 0L) {
      q30_residuo <- vapply(fascia_corrente, function(fascia) {
        residui_fascia <- historical_residuals$residuo[historical_residuals$fascia == fascia]
        if (length(residui_fascia) == 0L) return(NA_real_)
        as.numeric(quantile(residui_fascia, probs = 0.30, na.rm = TRUE))
      }, numeric(1))
      correzione_q30[idx_pred_fold] <<- q30_residuo
      q30[idx_pred_fold] <<- pmax(0, raw[idx_pred_fold] + correzione_q30[idx_pred_fold])
    }
    historical_residuals <<- bind_rows(
      historical_residuals,
      tibble(fascia = fascia_corrente,
             residuo = train_data[[target_col]][idx_pred_fold] - raw[idx_pred_fold],
             dateGame = date_game[idx_pred_fold])
    )
    log_folds <<- bind_rows(log_folds, tibble(
      fold = fold_id, tipo = ifelse(is_calibration, "calibration_iniziale", "oof"),
      data_massima_train = max(date_train_fold), data_minima_oof = min(date_oof_fold),
      n_train = length(idx_train_fold), n_oof = length(idx_pred_fold),
      n_residui_storici_q30 = n_residui
    ))
  }

  esegui_fold(unique_dates[seq_len(n_initial)], calibration_dates, 0L, TRUE)
  remaining_dates <- unique_dates[(n_initial + n_calibration + 1L):n_dates]
  if (length(remaining_dates) > 0L) {
    n_oof_folds <- min(n_folds, length(remaining_dates))
    fold_sizes <- rep(floor(length(remaining_dates) / n_oof_folds), n_oof_folds)
    fold_sizes[seq_len(length(remaining_dates) %% n_oof_folds)] <-
      fold_sizes[seq_len(length(remaining_dates) %% n_oof_folds)] + 1L
    fold_id <- rep(seq_len(n_oof_folds), times = fold_sizes)
    for (f in unique(fold_id)) {
      pred_dates <- remaining_dates[fold_id == f]
      train_dates <- unique_dates[unique_dates < min(pred_dates)]
      esegui_fold(train_dates, pred_dates, f, FALSE)
    }
  }
  list(raw = raw, q30 = q30, correzione_q30 = correzione_q30,
       fascia_q30 = fascia_q30, log = log_folds)
}

# Report Q30: sottostima = osservazione >= Q30; obiettivo circa 70%.
# Restituisce un riepilogo globale e uno per ciascuna fascia della statistica.
analizza_sottostime_hit_q30 <- function(
  df_val, col_reale, col_q30, col_fascia, nome_modello
) {
  colonne_richieste <- c(col_reale, col_q30, col_fascia)
  if (!all(colonne_richieste %in% names(df_val))) {
    stop("Colonne mancanti nel report Q30: ",
         paste(setdiff(colonne_richieste, names(df_val)), collapse = ", "))
  }

  quantile_sicuro <- function(x, prob) {
    if (length(x) == 0L) return(NA_real_)
    as.numeric(quantile(x, probs = prob, na.rm = TRUE))
  }

  calcola_riga <- function(dati, fascia) {
    sottostime <- dati$delta_q30[dati$delta_q30 >= 0]
    sovrastime <- -dati$delta_q30[dati$delta_q30 < 0]
    tibble(
      Modello = nome_modello,
      Fascia = fascia,
      N_Osservazioni = nrow(dati),
      Percentuale_Sottostima_Q30 = mean(dati$delta_q30 >= 0) * 100,
      Sottostima_50pct = quantile_sicuro(sottostime, 0.50),
      Sottostima_70pct = quantile_sicuro(sottostime, 0.70),
      Sovrastima_50pct = quantile_sicuro(sovrastime, 0.50),
      Sovrastima_70pct = quantile_sicuro(sovrastime, 0.70)
    )
  }

  dati <- df_val %>%
    transmute(
      fascia = as.character(.data[[col_fascia]]),
      reale = as.numeric(.data[[col_reale]]),
      q30 = as.numeric(.data[[col_q30]]),
      delta_q30 = reale - q30
    ) %>%
    filter(!is.na(fascia), !is.na(delta_q30))

  report_globale <- calcola_riga(dati, "Globale")
  report_fasce <- bind_rows(lapply(sort(unique(dati$fascia)), function(fascia) {
    calcola_riga(filter(dati, fascia == .env$fascia), fascia)
  }))

  bind_rows(report_globale, report_fasce)
}

# Curva di copertura del Q30. Ogni soglia Q30 implica una probabilita'
# nominale costante del 70% che il valore reale la superi; la curva verifica
# se tale copertura resta vicina al 70% lungo tutto il range delle stime.
curva_calibrazione_q30 <- function(
  df, col_reale, col_q30, nome_modello, n_bin = 10L, probabilita_target = 0.70
) {
  dati <- df %>%
    transmute(
      reale = as.numeric(.data[[col_reale]]),
      q30 = as.numeric(.data[[col_q30]]),
      hit_q30 = reale >= q30
    ) %>%
    filter(!is.na(reale), !is.na(q30))

  if (nrow(dati) == 0L) stop("Nessuna osservazione valida per la curva Q30.")
  n_bin_effettivi <- min(as.integer(n_bin), nrow(dati))
  dati <- dati %>% mutate(bin_q30 = ntile(q30, n_bin_effettivi))

  curva <- dati %>%
    group_by(bin_q30) %>%
    summarise(
      q30_min = min(q30),
      q30_max = max(q30),
      q30_medio = mean(q30),
      N = n(),
      Copertura_Q30 = mean(hit_q30),
      Errore_standard = sqrt(Copertura_Q30 * (1 - Copertura_Q30) / N),
      IC95_inferiore = pmax(0, Copertura_Q30 - 1.96 * Errore_standard),
      IC95_superiore = pmin(1, Copertura_Q30 + 1.96 * Errore_standard),
      Scarto_da_70pct = Copertura_Q30 - probabilita_target,
      .groups = "drop"
    )

  riepilogo <- dati %>% summarise(
    Modello = nome_modello,
    N = n(),
    Copertura_Q30_globale = mean(hit_q30),
    Target = probabilita_target,
    Scarto_da_target = Copertura_Q30_globale - Target
  )

  grafico <- ggplot(curva, aes(x = q30_medio, y = Copertura_Q30)) +
    geom_hline(yintercept = probabilita_target, colour = "firebrick", linetype = "dashed") +
    geom_errorbar(aes(ymin = IC95_inferiore, ymax = IC95_superiore), width = 0) +
    geom_line(colour = "steelblue") +
    geom_point(colour = "steelblue", size = 2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = paste("Curva di calibrazione Q30 -", nome_modello),
      subtitle = "Linea tratteggiata: copertura nominale 70%",
      x = "Stima Q30 media nel decile",
      y = "Copertura osservata (reale >= Q30)"
    ) +
    theme_minimal()

  print(riepilogo)
  print(grafico)
  invisible(list(riepilogo = riepilogo, curva = curva, grafico = grafico))
}

# Calibra la soglia Q30 separatamente per ogni fascia. La correzione e' il
# 30° percentile del residuo validation: quindi il 70% dei reali resta sopra
# la soglia calibrata, salvo l'effetto discreto dei dati.
calibra_q30_per_fascia <- function(
  df, col_reale, col_pred, col_fascia, probabilita_target = 0.70
) {
  df %>%
    transmute(
      fascia = as.character(.data[[col_fascia]]),
      reale = as.numeric(.data[[col_reale]]),
      pred = as.numeric(.data[[col_pred]]),
      residuo = reale - pred
    ) %>%
    filter(!is.na(fascia), !is.na(residuo)) %>%
    group_by(fascia) %>%
    summarise(
      N = n(),
      correzione_q30 = as.numeric(quantile(residuo, probs = 1 - probabilita_target)),
      Copertura_validation = mean(reale >= pred + correzione_q30),
      Target = probabilita_target,
      Scarto_da_target = Copertura_validation - Target,
      .groups = "drop"
    )
}

# Curva di copertura Q30 distinta per fascia del modello.
curva_calibrazione_q30_per_fascia <- function(
  df, col_reale, col_q30, col_fascia, nome_modello, n_bin = 10L,
  probabilita_target = 0.70
) {
  dati <- df %>%
    transmute(
      fascia = as.character(.data[[col_fascia]]),
      reale = as.numeric(.data[[col_reale]]),
      q30 = as.numeric(.data[[col_q30]]),
      hit_q30 = reale >= q30
    ) %>%
    filter(!is.na(fascia), !is.na(reale), !is.na(q30)) %>%
    group_by(fascia) %>%
    mutate(bin_q30 = ntile(q30, min(as.integer(n_bin), n()))) %>%
    ungroup()

  curva <- dati %>%
    group_by(fascia, bin_q30) %>%
    summarise(
      q30_medio = mean(q30), N = n(), Copertura_Q30 = mean(hit_q30),
      Errore_standard = sqrt(Copertura_Q30 * (1 - Copertura_Q30) / N),
      IC95_inferiore = pmax(0, Copertura_Q30 - 1.96 * Errore_standard),
      IC95_superiore = pmin(1, Copertura_Q30 + 1.96 * Errore_standard),
      .groups = "drop"
    )
  riepilogo <- dati %>%
    group_by(fascia) %>%
    summarise(
      Modello = nome_modello, N = n(), Copertura_Q30 = mean(hit_q30),
      Target = probabilita_target, Scarto_da_target = Copertura_Q30 - Target,
      .groups = "drop"
    )
  grafico <- ggplot(curva, aes(x = q30_medio, y = Copertura_Q30)) +
    geom_hline(yintercept = probabilita_target, colour = "firebrick", linetype = "dashed") +
    geom_errorbar(aes(ymin = IC95_inferiore, ymax = IC95_superiore), width = 0) +
    geom_line(colour = "steelblue") + geom_point(colour = "steelblue", size = 2) +
    facet_wrap(~ fascia) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = paste("Curva Q30 per fascia -", nome_modello),
      subtitle = "Linea tratteggiata: copertura nominale 70%",
      x = "Stima Q30 media nel decile", y = "Copertura osservata"
    ) + theme_minimal()
  print(riepilogo)
  print(grafico)
  invisible(list(riepilogo = riepilogo, curva = curva, grafico = grafico))
}

# 1. Acquisizione dati e costruzione delle feature disponibili prima della gara.
if (!file.exists("dati_nba.rds")) {
  dati <- game_logs(seasons = c(2024, 2025, 2026), result_types = "player", assign_to_environment = FALSE)
  saveRDS(dati, "dati_nba.rds")
} else {
  dati <- readRDS("dati_nba.rds")
}

# Manteniamo soltanto i giocatori presenti nelle stagioni più recenti.
gioc_att <- dati %>% filter(yearSeason %in% c(2026, 2027)) %>% pull(idPlayer) %>% unique()
dati_att <- dati %>% filter(idPlayer %in% gioc_att)

# Dati box score di supporto per le feature avanzate.
game_ids <- unique(dati_att$idGame)
if (!file.exists("box_data.rds")) {
  box_data <- box_scores(
    game_ids = game_ids,
    box_score_types = c("traditional", "hustle"),
    result_types = "player",
    assign_to_environment = FALSE,
    return_message = FALSE
  )
  saveRDS(box_data, "box_data.rds")
} else {
  box_data <- readRDS("box_data.rds")
}
box_data1 <- box_data[[2]][[1]]

dati_att <- dati_att %>% left_join(box_data1, by=c("idGame", "idPlayer"), suffix=c("", "_box"))

dati_pul <- dati_att %>%
  select("yearSeason", "dateGame", "idGame", "numberGameTeamSeason", "isB2BSecond", 
         "locationGame", "slugTeam", "countDaysRestTeam", "slugOpponent", "namePlayer", 
         "numberGamePlayerSeason", "countDaysRestPlayer", "idPlayer", "isWin", "fgm", "fga", 
         "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", "fg2m", "fg2a", "pctFG2", "minutes", "ftm", 
         "fta", "oreb", "dreb", "treb", "ast", "stl", "blk", "tov", "pf", "pts", "plusminus", 
         "fgContested", "fg2Contested", "fg3Contested", "boxOutsPlayerTeamRebound", "screenAssist", "ptsScreenAssist", 
         "deflections", "chargesDrawn", "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
         "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts") %>%
  mutate(dateGame=as.Date(dateGame),
         minutes=as.numeric(minutes))%>%
  arrange(idPlayer, dateGame, idGame)

# Trasformiamo le statistiche in numeriche e deriviamo indicatori di efficienza
# e combinazioni (punti, rimbalzi e assist) che saranno anche target dei modelli.
dati_pul2 <- dati_pul %>%
  mutate(
    pts     = as.numeric(pts), 
    fga     = as.numeric(fga), 
    fta     = as.numeric(fta), 
    fgm     = as.numeric(fgm), 
    fg3m    = as.numeric(fg3m),
    fg3a    = as.numeric(fg3a), 
    ftm     = as.numeric(ftm), 
    ast     = as.numeric(ast), 
    tov     = as.numeric(tov), 
    minutes = as.numeric(minutes),
    treb    = as.numeric(treb), # Assicura che sia numerica per evitare errori di somma
    # Metriche Avanzate
    TS_PCT        = ifelse(fga + .44 * fta == 0, 0, pts / (2 * (fga + .44 * fta))),
    EFG_PCT       = ifelse(fga == 0, 0, (fgm + .5 * fg3m) / fga),
    PCT_FGA_3PT   = ifelse(fga == 0, 0, fg3a / fga),
    PCT_PTS_3PT   = ifelse(pts == 0, 0, ftm / pts),
    AST_TOV_RATIO = ast / (tov + 1),
    EST_USG_PCT   = ifelse(minutes == 0, 0, (fga + .44 * fta + tov) / minutes),
    # Combo
    pts_ast     = pts + ast,
    pts_treb    = pts + treb,
    treb_ast    = treb + ast,
    pts_reb_ast = pts + treb + ast
  )

metriche <- c(
  "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", "fg2m", "fg2a", "pctFG2", 
  "minutes", "ftm", "fta", "oreb", "dreb", "treb", "ast", "stl", "blk", "tov", "pf", 
  "pts", "plusminus", "fgContested", "fg2Contested", "boxOutsPlayerTeamRebound", 
  "screenAssist", "ptsScreenAssist", "deflections", "chargesDrawn", 
  "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
  "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts", 
  "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT", "AST_TOV_RATIO", "EST_USG_PCT",
  "pts_ast", "pts_treb", "treb_ast", "pts_reb_ast"
)

dati_prep <- dati_pul2 %>%
  mutate(across(all_of(metriche), ~as.numeric(as.character(.)))) %>%
  mutate(across(all_of(metriche), ~ifelse(is.na(.), 0, .))) %>%
  arrange(idPlayer, yearSeason, dateGame, idGame)

# Medie mobili storiche: .after = -1 esclude sempre la gara corrente,
# evitando leakage nella previsione.
dati_pul3 <- dati_prep %>%
  group_by(idPlayer, yearSeason) %>%
  mutate(
    across(
      all_of(metriche),
      list(
        L1         = ~ ifelse(is.na(lag(.x, n = 1)), 0, lag(.x, n = 1)),
        L3         = ~ ifelse(is.na(slide_dbl(.x, mean, .before = 3, .after = -1, .complete = FALSE)) | is.nan(slide_dbl(.x, mean, .before = 3, .after = -1, .complete = FALSE)), 0, slide_dbl(.x, mean, .before = 3, .after = -1, .complete = FALSE)),
        L5         = ~ ifelse(is.na(slide_dbl(.x, mean, .before = 5, .after = -1, .complete = FALSE)) | is.nan(slide_dbl(.x, mean, .before = 5, .after = -1, .complete = FALSE)), 0, slide_dbl(.x, mean, .before = 5, .after = -1, .complete = FALSE)),
        L10        = ~ ifelse(is.na(slide_dbl(.x, mean, .before = 10, .after = -1, .complete = FALSE)) | is.nan(slide_dbl(.x, mean, .before = 10, .after = -1, .complete = FALSE)), 0, slide_dbl(.x, mean, .before = 10, .after = -1, .complete = FALSE)),
        season_avg = ~ ifelse(is.na(slide_dbl(.x, mean, .before = Inf, .after = -1, .complete = FALSE)) | is.nan(slide_dbl(.x, mean, .before = Inf, .after = -1, .complete = FALSE)), 0, slide_dbl(.x, mean, .before = Inf, .after = -1, .complete = FALSE))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()

# Rapporto tra le medie delle ultime 3 e 10 gare per ogni metrica offensiva.
for (metrica in metriche) {
  nome_l3 <- paste0(metrica, "_L3")
  nome_l10 <- paste0(metrica, "_L10")
  nome_rapporto <- paste0(metrica, "_L3_L10_ratio")
  dati_pul3[[nome_rapporto]] <- ifelse(
    is.na(dati_pul3[[nome_l10]]) | dati_pul3[[nome_l10]] == 0,
    0,
    dati_pul3[[nome_l3]] / dati_pul3[[nome_l10]]
  )
}

# Storico player-avversario (head-to-head), calcolato solo sulle gare precedenti.
dati_h2h <- dati_pul3 %>%
  arrange(idPlayer, slugOpponent, dateGame, idGame) %>%
  group_by(idPlayer, slugOpponent) %>%
  mutate(
    across(
      all_of(metriche),
      ~{
        res <- slide_dbl(.x, mean, .before=3, .after=-1, .complete=F)
        ifelse(is.nan(res) | is.na(res),0, res)
      },
      .names="{.col}_H2H_L3"
    )
  ) %>%
  ungroup()

# Scarto tra storico H2H e media stagionale: misura un possibile matchup specifico.
dati_medie <- dati_h2h %>%
  arrange(idPlayer, dateGame, idGame) %>%
  mutate(
    across(
      .cols=all_of(metriche),
      .fns=~{
        h2h_val <- get(paste0(cur_column(), "_H2H_L3"))
        seasonal_val <- get(paste0(cur_column(),"_season_avg"))
        ifelse(seasonal_val==0, 0, h2h_val - seasonal_val)
      },
      .names="{.col}_diff_H2H"
    )
  )

# Indicatori aggiuntivi di continuità nei minuti e nei ritmi di gioco.
dati_off <- dati_medie %>%
  group_by(idPlayer, yearSeason) %>%
  arrange(dateGame) %>%
  mutate(
    minutes_sd_L10 = slide_dbl(minutes, sd, .before = 10, .after = -1, .complete = FALSE),
    pts_per_min_L5 = pts_L5 / ifelse(minutes_L5 == 0, 1, minutes_L5),
    fga_per_min_L5 = fga_L5 / ifelse(minutes_L5 == 0, 1, minutes_L5),
    pf_per_min_L5  = pf_L5 / ifelse(minutes_L5 == 0, 1, minutes_L5),
    minutes_sum_L3 = slide_dbl(minutes, sum, .before = 3, .after = -1, .complete = FALSE)
  ) %>%
  ungroup() %>%
  mutate(across(c(minutes_sd_L10, pts_per_min_L5, fga_per_min_L5, pf_per_min_L5, minutes_sum_L3), ~replace_na(., 0)))

# 2. Feature della difesa avversaria: trasformiamo i dati squadra in valori concessi.
if (!file.exists("dati_nba_t.rds")) {
  dati_nba_t <- game_logs(seasons = c(2024, 2025, 2026), result_types = "team", assign_to_environment = FALSE)
  saveRDS(dati_nba_t, "dati_nba_t.rds")
} else {
  dati_nba_t <- readRDS("dati_nba_t.rds")
}

# Possessi, pace ed efficienza della squadra per contestualizzare l'avversario.
dati_nba_t1 <- dati_nba_t %>%
  arrange(slugTeam, dateGame) %>%
  mutate(
    possessionsTeam = fgaTeam + (.44 * ftaTeam) + tovTeam - orebTeam,
    paceTeam = 48 * (possessionsTeam / (minutesTeam / 5)),
    efgPctTeam = (fgmTeam + (.5 * fg3mTeam)) / fgaTeam
  )

# Le metriche della squadra incontrata diventano metriche con suffisso Opp.
vars_team <- c(
  "fgmTeam", "fgaTeam", "pctFGTeam", "fg3mTeam", "fg3aTeam", "pctFG3Team", 
  "pctFTTeam", "fg2mTeam", "fg2aTeam", "pctFG2Team", "minutesTeam", "ftmTeam", 
  "ftaTeam", "orebTeam", "drebTeam", "trebTeam", "astTeam", "stlTeam", 
  "blkTeam", "tovTeam", "pfTeam", "ptsTeam", "plusminusTeam", "possessionsTeam", 
  "paceTeam", "efgPctTeam"
)

dati_opp <- dati_nba_t1 %>%
  select(idGame, slugTeam, all_of(vars_team)) %>%
  rename_with(~str_replace(., "Team$", "Opp"), all_of(vars_team))

dati_nba_t2 <- dati_nba_t1 %>%
  left_join(
    dati_opp,
    by=c("idGame"="idGame", "slugOpponent"="slugTeam")
  ) %>%
  mutate(
    defRatingOpp=(ptsOpp/possessionsOpp)*100
  )

metriche <- c("fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp", "pctFG3Opp", "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp", "ftmOpp", 
              "ftaOpp", "orebOpp", "drebOpp", "trebOpp", "astOpp", "stlOpp", "blkOpp", "tovOpp", "pfOpp", "ptsOpp", "plusminusOpp", "possessionsOpp", 
              "paceOpp", "efgPctOpp", "defRatingOpp")

# Anche per la difesa, tutte le finestre temporali escludono la partita corrente.
dati_def <- dati_nba_t2 %>%
  group_by(slugTeam, yearSeason) %>%
  arrange(dateGame, .by_group = TRUE) %>% # Fondamentale per garantire l'ordine temporale corretto
  mutate(
    across(
      all_of(metriche), 
      list(
        L1         = ~ ifelse(is.na(lag(.x, n = 1)), 0, lag(.x, n = 1)),
        L3         = ~ ifelse(is.na(slide_dbl(.x, mean, .before = 3, .after = -1, .complete = FALSE)) | is.nan(slide_dbl(.x, mean, .before = 3, .after = -1, .complete = FALSE)), 0, slide_dbl(.x, mean, .before = 3, .after = -1, .complete = FALSE)),
        L5         = ~ ifelse(is.na(slide_dbl(.x, mean, .before = 5, .after = -1, .complete = FALSE)) | is.nan(slide_dbl(.x, mean, .before = 5, .after = -1, .complete = FALSE)), 0, slide_dbl(.x, mean, .before = 5, .after = -1, .complete = FALSE)),
        L10        = ~ ifelse(is.na(slide_dbl(.x, mean, .before = 10, .after = -1, .complete = FALSE)) | is.nan(slide_dbl(.x, mean, .before = 10, .after = -1, .complete = FALSE)), 0, slide_dbl(.x, mean, .before = 10, .after = -1, .complete = FALSE)),
        season_avg = ~ ifelse(is.na(slide_dbl(.x, mean, .before = Inf, .after = -1, .complete = FALSE)) | is.nan(slide_dbl(.x, mean, .before = Inf, .after = -1, .complete = FALSE)), 0, slide_dbl(.x, mean, .before = Inf, .after = -1, .complete = FALSE))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()

# Rapporto tra le medie delle ultime 3 e 10 gare per ogni metrica difensiva.
for (metrica in metriche) {
  nome_l3 <- paste0(metrica, "_L3")
  nome_l10 <- paste0(metrica, "_L10")
  nome_rapporto <- paste0(metrica, "_L3_L10_ratio")
  dati_def[[nome_rapporto]] <- ifelse(
    is.na(dati_def[[nome_l10]]) | dati_def[[nome_l10]] == 0,
    0,
    dati_def[[nome_l3]] / dati_def[[nome_l10]]
  )
}

dati_def1 <- dati_def %>%
  select(
    idGame, numberGameTeamSeason, nameTeam, idTeam, 
    isB2BSecond, locationGame, slugTeam, countDaysRestTeam,
    fgmOpp, fgaOpp, pctFGOpp, fg3mOpp, fg3aOpp, pctFG3Opp, 
    pctFTOpp, fg2mOpp, fg2aOpp, pctFG2Opp, minutesOpp, ftmOpp, 
    ftaOpp, orebOpp, drebOpp, trebOpp, astOpp, stlOpp, 
    blkOpp, tovOpp, pfOpp, ptsOpp, plusminusOpp, possessionsOpp, 
    paceOpp, efgPctOpp, defRatingOpp,
    fgmOpp_L1, fgmOpp_L3, fgmOpp_L5, fgmOpp_L10, fgmOpp_season_avg,
    fgaOpp_L1, fgaOpp_L3, fgaOpp_L5, fgaOpp_L10, fgaOpp_season_avg,
    pctFGOpp_L1, pctFGOpp_L3, pctFGOpp_L5, pctFGOpp_L10, pctFGOpp_season_avg,
    fg3mOpp_L1, fg3mOpp_L3, fg3mOpp_L5, fg3mOpp_L10, fg3mOpp_season_avg,
    fg3aOpp_L1, fg3aOpp_L3, fg3aOpp_L5, fg3aOpp_L10, fg3aOpp_season_avg,
    pctFG3Opp_L1, pctFG3Opp_L3, pctFG3Opp_L5, pctFG3Opp_L10, pctFG3Opp_season_avg,
    pctFTOpp_L1, pctFTOpp_L3, pctFTOpp_L5, pctFTOpp_L10, pctFTOpp_season_avg,
    fg2mOpp_L1, fg2mOpp_L3, fg2mOpp_L5, fg2mOpp_L10, fg2mOpp_season_avg,
    fg2aOpp_L1, fg2aOpp_L3, fg2aOpp_L5, fg2aOpp_L10, fg2aOpp_season_avg,
    pctFG2Opp_L1, pctFG2Opp_L3, pctFG2Opp_L5, pctFG2Opp_L10, pctFG2Opp_season_avg,
    ftmOpp_L1, ftmOpp_L3, ftmOpp_L5, ftmOpp_L10, ftmOpp_season_avg,
    ftaOpp_L1, ftaOpp_L3, ftaOpp_L5, ftaOpp_L10, ftaOpp_season_avg,
    orebOpp_L1, orebOpp_L3, orebOpp_L5, orebOpp_L10, orebOpp_season_avg,
    drebOpp_L1, drebOpp_L3, drebOpp_L5, drebOpp_L10, drebOpp_season_avg,
    trebOpp_L1, trebOpp_L3, trebOpp_L5, trebOpp_L10, trebOpp_season_avg,
    astOpp_L1, astOpp_L3, astOpp_L5, astOpp_L10, astOpp_season_avg,
    stlOpp_L1, stlOpp_L3, stlOpp_L5, stlOpp_L10, stlOpp_season_avg,
    blkOpp_L1, blkOpp_L3, blkOpp_L5, blkOpp_L10, blkOpp_season_avg,
    tovOpp_L1, tovOpp_L3, tovOpp_L5, tovOpp_L10, tovOpp_season_avg,
    pfOpp_L1, pfOpp_L3, pfOpp_L5, pfOpp_L10, pfOpp_season_avg,
    ptsOpp_L1, ptsOpp_L3, ptsOpp_L5, ptsOpp_L10, ptsOpp_season_avg,
    plusminusOpp_L1, plusminusOpp_L3, plusminusOpp_L5, plusminusOpp_L10, plusminusOpp_season_avg,
    possessionsOpp_L1, possessionsOpp_L3, possessionsOpp_L5, possessionsOpp_L10, possessionsOpp_season_avg,
    paceOpp_L1, paceOpp_L3, paceOpp_L5, paceOpp_L10, paceOpp_season_avg,
    efgPctOpp_L1, efgPctOpp_L3, efgPctOpp_L5, efgPctOpp_L10, efgPctOpp_season_avg,
    defRatingOpp_L1, defRatingOpp_L3, defRatingOpp_L5, defRatingOpp_L10, defRatingOpp_season_avg,
    all_of(paste0(metriche, "_L3_L10_ratio"))
  )

#### Dataset ####
# 3. Dataset di modellazione: unione delle feature giocatore e avversario.
dati_modello <- dati_off %>%
  left_join(
    dati_def1,
    by=c("idGame"="idGame", "slugOpponent"="slugTeam"),
    suffix=c("", "_Opp")
  ) %>%
  arrange(dateGame, idGame, idPlayer) %>%
  mutate(row_id_master = row_number()) %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)

dati_modello <- dati_modello %>%
  filter(
    numberGamePlayerSeason > 5,
    minutes_season_avg >= 15,
    !is.na(pts),
    !is.na(minutes)
  ) %>%
  arrange(dateGame, idGame, idPlayer, row_id_master) %>%
  mutate(row_id = row_id_master)

# Confini globali, determinati sulle date (mai su singole righe): una data
# non può appartenere a due split diversi.
date_split <- dati_modello %>%
  distinct(dateGame) %>%
  arrange(dateGame)
n_date_split <- nrow(date_split)
cut_train_date <- date_split$dateGame[max(1L, floor(n_date_split * 0.70))]
cut_val_date <- date_split$dateGame[max(1L, floor(n_date_split * 0.85))]

dati_modello <- dati_modello %>%
  mutate(
    split_temporale = case_when(
      dateGame <= cut_train_date ~ "train",
      dateGame <= cut_val_date ~ "validation",
      TRUE ~ "test"
    ),
    split_temporale = factor(split_temporale, levels = c("train", "validation", "test"))
  ) %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)

# 4. Modelli previsivi. Ogni modello segue lo stesso schema: selezione feature,
# tuning su validation, calibrazione Q30 e verifica finale sul test separato.

#### Modello exp_min ####
# Minuti attesi, usati anche come filtro di eleggibilità.
escludere_min <- c(
  "row_id", "yearSeason", "dateGame", "idGame", "slugTeam", "slugOpponent",
  "namePlayer", "idPlayer", "nameTeam", "idTeam", "isWin",
  "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT",
  "fg2m", "fg2a", "pctFG2", "ftm", "fta", "oreb", "dreb",
  "treb", "ast", "stl", "blk", "tov", "pf", "plusminus", "pts",
  "fgContested", "fg2Contested", "fg3Contested",
  "boxOutsPlayerTeamRebound", "screenAssist", "ptsScreenAssist",
  "deflections", "chargesDrawn",
  "looseBallsRecoveredOffense", "looseBallsRecoveredDefense",
  "looseBallsRecovered", "boxOutsOffense", "boxOutsDefense",
  "boxOutsPlayerTREB", "boxOuts",
  "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT",
  "AST_TOV_RATIO", "EST_USG_PCT",
  # Nuove Target Combo
  "pts_ast", "pts_treb", "treb_ast", "pts_reb_ast",
  "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp",
  "pctFG3Opp", "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp",
  "minutesOpp", "ftmOpp", "ftaOpp", "orebOpp", "drebOpp",
  "trebOpp", "astOpp", "stlOpp", "blkOpp", "tovOpp", "pfOpp",
  "ptsOpp", "plusminusOpp", "possessionsOpp", "paceOpp",
  "efgPctOpp", "defRatingOpp"
)

df_min <- dati_modello %>%
  select(-all_of(c(escludere_min, "row_id_master", "split_temporale"))) %>%
  mutate(across(where(is.character), as.factor))

# Suddivisione cronologica: 70% train, 15% validation, 15% test.
# Il test set resta completamente separato durante selezione delle variabili,
# ricerca degli iperparametri e calibrazione.
idx_train <- which(dati_modello$split_temporale == "train")
idx_val <- which(dati_modello$split_temporale == "validation")
idx_test <- which(dati_modello$split_temporale == "test")

train_set <- df_min[idx_train,]
val_set <- df_min[idx_val,]
test_set <- df_min[idx_test,]

# Triage: una foresta rapida stima l'importanza delle feature; la lista finale
# è fissata esplicitamente per rendere l'esperimento riproducibile.
set.seed(123)
rf_quick <- ranger(
  formula = minutes~.,
  data = train_set,
  num.trees = 200,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 123,
  verbose = FALSE
)

top_vars <- importance(rf_quick) %>%
  enframe(name="Variabile", value="Importanza") %>%
  arrange(desc(Importanza)) %>%
  slice_head(n=40) %>%
  pull(Variabile)

top_vars <- c(
  "minutes_L10", "minutes_L3", "minutes_L5", "minutes_sum_L3", "minutes_season_avg", 
  "pts_season_avg", "pts_reb_ast_L10", "minutes_L1", "pts_ast_L10", "pts_L10", 
  "pts_ast_L5", "pts_ast_season_avg", "pts_treb_L10", "pts_reb_ast_season_avg", 
  "fga_L10", "fga_L5", "pts_L3", "fga_season_avg", "fgm_season_avg", 
  "pts_treb_season_avg", "fgm_L10", "pts_reb_ast_L5", "fga_L3", "pts_reb_ast_L3", 
  "pts_treb_L5", "pts_L5", "pts_ast_L3", "fgm_L5", "EST_USG_PCT_L10", 
  "pts_treb_L3", "EST_USG_PCT_season_avg", "pts_per_min_L5", "fg2a_L5", 
  "EST_USG_PCT_L5", "fg2m_season_avg", "fta_season_avg", "fg2a_season_avg", 
  "fg2m_L10", "fg2a_L10", "ftm_season_avg"
)

df_min1 <- df_min %>%
  select(minutes, all_of(top_vars)) %>%
  mutate(across(where(is.character), as.factor))

train_set1 <- df_min1[idx_train,]
val_set1 <- df_min1[idx_val,]
test_set1 <- df_min1[idx_test,]

# Ricerca iperparametri: il train serve per addestrare ciascun candidato,
# il validation per scegliere la combinazione con MAE minore.
p <- ncol(train_set1)-1
hyper_grid_min <- expand.grid(
  mtry=c(3, 5, 8, 10),
  min.node.size=c(300, 350, 400, 500),
  sample.fraction=c(.4, .5),
  num.trees=500,
  mae=NA,
  rmse=NA
)

set.seed(123)
for (i in 1:nrow(hyper_grid_min)) {
  modello_temp <- ranger(
    formula                   = minutes ~ .,
    data                      = train_set1,
    num.trees                 = hyper_grid_min$num.trees[i],
    mtry                      = hyper_grid_min$mtry[i],
    min.node.size             = hyper_grid_min$min.node.size[i],
    sample.fraction           = hyper_grid_min$sample.fraction[i],
    respect.unordered.factors = "order",
    seed                      = 123,
    verbose                   = FALSE,
    num.threads               = numero_thread_ranger()
  )
  pred_val <- predict(modello_temp, data = val_set1)$predictions
  hyper_grid_min$mae[i]  <- mean(abs(pred_val - val_set1$minutes), na.rm = TRUE)
  hyper_grid_min$rmse[i] <- sqrt(mean((pred_val - val_set1$minutes)^2, na.rm = TRUE))
}

hyper_grid_min <- hyper_grid_min %>% arrange(mae) %>% head(10)
best_params_min <- hyper_grid_min[1,]
#Migliore: 10-500-.4-500-4.492-5.866

# Addestramento finale con i migliori iperparametri selezionati sul validation set.
rf_minutes_ott <- ranger(
  formula = minutes ~ .,
  data = train_set1,
  num.trees = best_params_min$num.trees,
  mtry = best_params_min$mtry,
  min.node.size = best_params_min$min.node.size,
  sample.fraction = best_params_min$sample.fraction,
  importance = "impurity",
  respect.unordered.factors = "order",
  seed = 123,
  verbose = FALSE,
  num.threads = numero_thread_ranger()
)

# Validation: metriche usate per controllare il modello e calibrare Q30/Q70.
break_fasce_min <- c(-Inf, 15, 28, Inf)
nomi_fasce_min <- c("Bench (<15m)", "Rotation (15-28m)", "Starters (>28m)")
z_30_min <- abs(qnorm(.3))

val_set1 <- val_set1 %>%
  mutate(
    exp_min_raw = predict(rf_minutes_ott, data=val_set1)$predictions,
    exp_min = exp_min_raw, #Nessuna calibrazione
    fascia_min = cut(exp_min, breaks=break_fasce_min, labels=nomi_fasce_min, include.lowest=T)
    )

calibrazione_fasce_min <- val_set1 %>%
  group_by(fascia_min) %>%
  summarise(
    rmse_min=sqrt(mean((exp_min-minutes)^2, na.rm=T)),
    .groups="drop"
  )

calibrazione_q30_fasce_min <- calibra_q30_per_fascia(
  val_set1, "minutes", "exp_min", "fascia_min"
)
print(calibrazione_q30_fasce_min)

calibrazione_fasce_min <- calibrazione_fasce_min %>%
  left_join(calibrazione_q30_fasce_min %>% select(fascia, correzione_q30),
            by = c("fascia_min" = "fascia"))

val_set1 <- val_set1 %>%
  left_join(calibrazione_fasce_min, by="fascia_min") %>%
  mutate(
    exp_min_sd=rmse_min,
    exp_min_q30=pmax(0, exp_min + correzione_q30),
    exp_min_q70=exp_min+z_30_min*exp_min_sd
  )

res_val_min <- val_err(val_set1, "minutes", "exp_min", "Validation Minuti (Grezzo)")

metriche_validation_min <- val_set1 %>%
  summarise(
    N = n(),
    Bias_medio = mean(exp_min - minutes, na.rm = TRUE),
    Errore_medio_assoluto = mean(abs(exp_min - minutes), na.rm = TRUE),
    RMSE = sqrt(mean((exp_min - minutes)^2, na.rm = TRUE))
  )

print(metriche_validation_min)

val_set1 <- val_set1 %>%
  mutate(
    delta_min_q30 = minutes - exp_min_q30,
    hit_q30 = delta_min_q30 >= 0
  )

report_q30_min <- val_set1 %>%
  group_by(fascia_min) %>%
  summarise(
    N_Osservazioni = n(),
    MAE_Minuti = mean(abs(minutes-exp_min), na.rm=T),
    RMSE_Minuti = sqrt(mean((minutes-exp_min)^2, na.rm=T)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm=T)*100,
    .groups = "drop"
  )

print(report_q30_min)
cat(sprintf("\nHit Rate Globale Q30 Minuti: %.2f%%\n", mean(val_set1$hit_q30, na.rm = TRUE) * 100))
calibrazione_q30_min_validation_per_fascia <- curva_calibrazione_q30_per_fascia(
  val_set1, "minutes", "exp_min_q30", "fascia_min", "Minuti — Validation"
)

# Test set: valutazione finale, eseguita una sola volta dopo tuning e
# calibrazione sul validation set. Il Q30 punta a un hit rate vicino al 70%.
test_set1 <- test_set1 %>%
  mutate(
    exp_min_raw = predict(rf_minutes_ott, data = test_set1)$predictions,
    exp_min = exp_min_raw,
    fascia_min = cut(exp_min, breaks = break_fasce_min,
                     labels = nomi_fasce_min, include.lowest = TRUE)
  ) %>%
  left_join(calibrazione_fasce_min, by = "fascia_min") %>%
  mutate(
    exp_min_sd = rmse_min,
    exp_min_q30 = pmax(0, exp_min + correzione_q30),
    exp_min_q70 = exp_min + z_30_min * exp_min_sd,
    delta_min_q30 = minutes - exp_min_q30,
    hit_q30 = delta_min_q30 >= 0
  )

res_test_min <- val_err(test_set1, "minutes", "exp_min", "Test Minuti (Grezzo)")

metriche_test_min <- test_set1 %>%
  summarise(
    N = n(),
    Bias_medio = mean(exp_min - minutes, na.rm = TRUE),
    Errore_medio_assoluto = mean(abs(exp_min - minutes), na.rm = TRUE),
    RMSE = sqrt(mean((exp_min - minutes)^2, na.rm = TRUE)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm = TRUE) * 100
  )

print(metriche_test_min)
cat(sprintf("\nHit Rate Q30 sul test set: %.2f%% (obiettivo: circa 70%%)\n",
            metriche_test_min$Hit_Rate_Q30_pct))
calibrazione_q30_min_test_per_fascia <- curva_calibrazione_q30_per_fascia(
  test_set1, "minutes", "exp_min_q30", "fascia_min", "Minuti — Test"
)

# OOF per date e Q30 rolling: nessuna calibrazione del validation esterno.
oof_min <- genera_oof_temporali(train_set1, dati_modello$dateGame[idx_train],
  minutes ~ ., best_params_min, "minutes", break_fasce_min, nomi_fasce_min)
oof_pred_raw <- oof_min$raw
oof_pred_q30 <- oof_min$q30
log_oof_min <- oof_min$log
print(log_oof_min)

train_set1_oof <- train_set1 %>%
  select(-any_of(c("exp_min_raw", "fascia_min", "rmse_min", "exp_min", "exp_min_sd", "exp_min_q30", "exp_min_q70"))) %>%
  mutate(
    row_id_master = dati_modello$row_id_master[idx_train],
    exp_min_raw = oof_pred_raw,
    exp_min     = exp_min_raw, # Mantentiamo il modello Grezzo
    fascia_min  = cut(exp_min, breaks = break_fasce_min, labels = nomi_fasce_min, include.lowest = TRUE)
  ) %>%
  mutate(exp_min_sd = NA_real_, exp_min_q30 = oof_pred_q30, exp_min_q70 = NA_real_)

calibrazione_q30_min_train_per_fascia <- curva_calibrazione_q30_per_fascia(
  train_set1_oof, "minutes", "exp_min_q30", "fascia_min", "Minuti — Train OOF"
)

train_set1_oof$row_id_master <- dati_modello$row_id_master[idx_train]
val_set1$row_id_master <- dati_modello$row_id_master[idx_val]
test_set1$row_id_master <- dati_modello$row_id_master[idx_test]

colonne_minuti <- c("row_id_master", "exp_min", "exp_min_sd", "exp_min_q30", "exp_min_q70")
pred_minuti <- bind_rows(
  train_set1_oof %>% select(all_of(colonne_minuti)),
  val_set1        %>% select(all_of(colonne_minuti)),
  test_set1       %>% select(all_of(colonne_minuti))
)

print(analizza_sottostime_hit_q30(test_set1, "minutes", "exp_min_q30", "fascia_min", "Minuti"))

# Riuniamo le previsioni dei tre split sul dataset master tramite la chiave stabile.
dati_modello <- dati_modello %>%
  select(-any_of(c("exp_min_raw", "exp_min", "exp_min_sd", "exp_min_q30", "exp_min_q70"))) %>%
  left_join(pred_minuti, by = "row_id_master") %>%
  arrange(dateGame, idGame, idPlayer, row_id_master) %>%
  mutate(
    eligible_pts_model = !is.na(exp_min_q30),
    eligible_treb_model = !is.na(exp_min_q30),
    eligible_ast_model = !is.na(exp_min_q30)
  )

# I modelli successivi operano sui giocatori con Q30 minuti >= 12: il filtro
# evita di produrre stime per profili con probabilità bassa di minutaggio utile.
dati_com <- dati_modello
dati_com <- dati_com %>%
  filter(exp_min_q30 >= 12) %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)

dati_com1 <- dati_com

#### Modello exp_pts ####
# Punti attesi.
escludere <- c("row_id",
               "yearSeason", "dateGame", "idGame", "slugTeam", "slugOpponent", 
               "namePlayer", "idPlayer", "nameTeam", "idTeam", "isWin",
               "numberGameTeamSeason_Opp", "isB2BSecond_Opp", "locationGame_Opp", "countDaysRestTeam_Opp",
               "minutes",
               "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", 
               "fg2m", "fg2a", "pctFG2", "ftm", "fta", "oreb", "dreb", 
               "treb", "ast", "stl", "blk", "tov", "pf", "plusminus",
               "pts_ast", "pts_treb", "treb_ast", "pts_reb_ast",
               "fgContested", "fg2Contested", "fg3Contested", "boxOutsPlayerTeamRebound", 
               "screenAssist", "ptsScreenAssist", "deflections", "chargesDrawn", 
               "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
               "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts", 
               "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT", "AST_TOV_RATIO", "EST_USG_PCT",
               "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp", "pctFG3Opp", 
               "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp", "minutesOpp", "ftmOpp", 
               "ftaOpp", "orebOpp", "drebOpp", "trebOpp", "astOpp", "stlOpp", 
               "blkOpp", "tovOpp", "pfOpp", "ptsOpp", "plusminusOpp", "possessionsOpp", 
               "paceOpp", "efgPctOpp", "defRatingOpp", "exp_min", "exp_min_q70"
)

df_pts <- dati_com1 %>%
  select(-all_of(c(escludere, "row_id_master", "split_temporale")))


# Suddivisione cronologica: 70% train, 15% validation, 15% test.
idx_train <- which(dati_com1$split_temporale == "train" & dati_com1$eligible_pts_model)
idx_val <- which(dati_com1$split_temporale == "validation" & dati_com1$eligible_pts_model)
idx_test <- which(dati_com1$split_temporale == "test" & dati_com1$eligible_pts_model)

train_set_pts <- df_pts[idx_train, ]
val_set_pts   <- df_pts[idx_val, ]
test_set_pts  <- df_pts[idx_test, ]

#Triage per selezionare le migliori features
set.seed(123)
rf_quick_pts <- ranger(
  formula                   = pts ~ .,
  data                      = train_set_pts,
  num.trees                 = 200,
  importance                = "permutation",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE
)

top_vars_pts <- importance(rf_quick_pts) %>%
  enframe(name="Variabile", value="Importanza") %>%
  arrange(desc(Importanza)) %>%
  slice_head(n=40) %>%
  pull(Variabile)

top_vars_pts <- c(
  "fga_L10", "pts_season_avg", "fga_season_avg", "pts_ast_L10",
  "pts_L10", "pts_ast_season_avg", "fgm_season_avg", "pts_treb_season_avg",
  "fga_L5", "fgm_L10", "pts_treb_L10", "fg2a_season_avg",
  "pts_reb_ast_L10", "EST_USG_PCT_season_avg", "fgm_L5", "fg2m_season_avg",
  "pts_reb_ast_L5", "pts_L3", "pts_ast_L3", "pts_ast_L5",
  "exp_min_q30", "pts_L5", "pts_reb_ast_season_avg", "fg2a_L10",
  "pts_treb_L5", "fga_L3", "fgm_L3", "fta_season_avg",
  "ftm_season_avg", "pts_reb_ast_L3", "pts_treb_L3", "EST_USG_PCT_L5",
  "EST_USG_PCT_L10", "fg2a_L5", "minutes_season_avg", "pts_per_min_L5",
  "fga_per_min_L5", "fg3a_season_avg", "minutes_L10", "fg2m_L10"
)

df_pts1 <- df_pts %>%
  select(pts, all_of(top_vars_pts)) %>%
  mutate(across(where(is.character), as.factor))

train_set_pts1 <- df_pts1[idx_train, ]
val_set_pts1   <- df_pts1[idx_val, ]
test_set_pts1  <- df_pts1[idx_test, ]

p <- ncol(train_set_pts1) - 1
hyper_grid_pts <- expand.grid(
  mtry            = c(10, 13, 15),      
  min.node.size   = c(200, 250, 300, 350, 400),
  sample.fraction = c(0.4, 0.5),    
  num.trees       = 500,
  mae  = NA,
  rmse = NA
)

set.seed(123)
for (i in 1:nrow(hyper_grid_pts)) {
  modello_temp <- ranger(
    formula                   = pts ~ .,
    data                      = train_set_pts1,
    num.trees                 = hyper_grid_pts$num.trees[i],
    mtry                      = hyper_grid_pts$mtry[i],
    min.node.size             = hyper_grid_pts$min.node.size[i],
    sample.fraction           = hyper_grid_pts$sample.fraction[i],
    respect.unordered.factors = "order",
    seed                      = 123,
    verbose                   = FALSE,
    num.threads               = numero_thread_ranger()
  )
  pred_val <- predict(modello_temp, data = val_set_pts1)$predictions
  hyper_grid_pts$mae[i]  <- mean(abs(pred_val - val_set_pts1$pts), na.rm = TRUE)
  hyper_grid_pts$rmse[i] <- sqrt(mean((pred_val - val_set_pts1$pts)^2, na.rm = TRUE))
}

hyper_grid_pts <- hyper_grid_pts %>% arrange(mae) %>% head(10)
best_params_pts <- hyper_grid_pts[1, ]
#Migliore: 13-400-.4-500-4.961-6.359

#Addestriamo il modello
rf_pts_ott <- ranger(
  formula                   = pts ~ .,
  data                      = train_set_pts1,
  num.trees                 = best_params_pts$num.trees ,
  mtry                      = best_params_pts$mtry,
  min.node.size             = best_params_pts$min.node.size,
  sample.fraction           = best_params_pts$sample.fraction,
  importance                = "impurity",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE,
  num.threads               = numero_thread_ranger()
)

break_fasce_pts <- c(-Inf, 10, 20, Inf)
nomi_fasce_pts <- c("Low (<10pts)", "Mid (10-20pts)", "High (>20pts)")
z_30_pts <- abs(qnorm(.3))*1.13

val_set_pts1 <- val_set_pts1 %>%
  select(-any_of(c("exp_pts_raw", "fascia_pts", "rmse_pts", "exp_pts", "exp_pts_sd", "exp_pts_q30", "exp_pts_q70", "delta_pts_q30", "hit_q30", "residuo"))) %>%
  mutate(
    exp_pts_raw = predict(rf_pts_ott, data=val_set_pts1)$predictions,
    exp_pts = exp_pts_raw, #Nessuna calibrazione
    fascia_pts = cut(exp_pts, breaks=break_fasce_pts, labels=nomi_fasce_pts, include.lowest=T)
  )

calibrazione_fasce_pts <- val_set_pts1 %>%
  group_by(fascia_pts) %>%
  summarise(
    rmse_pts=sqrt(mean((exp_pts-pts)^2, na.rm=T)),
    .groups="drop"
  )

calibrazione_q30_fasce_pts <- calibra_q30_per_fascia(
  val_set_pts1, "pts", "exp_pts", "fascia_pts"
)
print(calibrazione_q30_fasce_pts)
calibrazione_fasce_pts <- calibrazione_fasce_pts %>%
  left_join(calibrazione_q30_fasce_pts %>% select(fascia, correzione_q30),
            by = c("fascia_pts" = "fascia"))

val_set_pts1 <- val_set_pts1 %>%
  left_join(calibrazione_fasce_pts, by="fascia_pts") %>%
  mutate(
    exp_pts_sd=rmse_pts,
    exp_pts_q30=pmax(0, exp_pts + correzione_q30),
    exp_pts_q70=exp_pts+z_30_pts*exp_pts_sd
  )

res_val_pts <- val_err(val_set_pts1, "pts", "exp_pts", "Validation Punti (Grezzo)")
metriche_validation_pts <- metriche_previsione(val_set_pts1, "pts", "exp_pts")
print(metriche_validation_pts)

val_set_pts1 <- val_set_pts1 %>%
  mutate(
    delta_pts_q30 = pts - exp_pts_q30,
    hit_q30 = delta_pts_q30 >= 0
  )

report_q30_pts <- val_set_pts1 %>%
  group_by(fascia_pts) %>%
  summarise(
    N_Osservazioni = n(),
    MAE_Punti = mean(abs(pts-exp_pts), na.rm=T),
    RMSE_Punti = sqrt(mean((pts-exp_pts)^2, na.rm=T)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm=T)*100,
    .groups = "drop"
  )

print(report_q30_pts)
cat(sprintf("\nHit Rate Globale Q30 Punti: %.2f%%\n", mean(val_set_pts1$hit_q30, na.rm = TRUE) * 100))
calibrazione_q30_pts_validation_per_fascia <- curva_calibrazione_q30_per_fascia(
  val_set_pts1, "pts", "exp_pts_q30", "fascia_pts", "Punti — Validation"
)

test_set_pts1 <- test_set_pts1 %>%
  mutate(exp_pts_raw = predict(rf_pts_ott, data = test_set_pts1)$predictions,
         exp_pts = exp_pts_raw,
         fascia_pts = cut(exp_pts, breaks = break_fasce_pts, labels = nomi_fasce_pts, include.lowest = TRUE)) %>%
  left_join(calibrazione_fasce_pts, by = "fascia_pts") %>%
  mutate(exp_pts_sd = rmse_pts,
         exp_pts_q30 = pmax(0, exp_pts + correzione_q30),
         exp_pts_q70 = exp_pts + z_30_pts * exp_pts_sd,
         delta_pts_q30 = pts - exp_pts_q30,
         hit_q30 = delta_pts_q30 >= 0)
res_test_pts <- val_err(test_set_pts1, "pts", "exp_pts", "Test Punti (Grezzo)")
metriche_test_pts <- metriche_previsione(test_set_pts1, "pts", "exp_pts") %>%
  mutate(Hit_Rate_Q30_pct = mean(test_set_pts1$hit_q30, na.rm = TRUE) * 100)
print(metriche_test_pts)
cat(sprintf("\nHit Rate Q30 sul test set Punti: %.2f%% (obiettivo: circa 70%%)\n", metriche_test_pts$Hit_Rate_Q30_pct))
calibrazione_q30_pts_test_per_fascia <- curva_calibrazione_q30_per_fascia(
  test_set_pts1, "pts", "exp_pts_q30", "fascia_pts", "Punti — Test"
)

# OOF temporali expanding-window.
oof_pts <- genera_oof_temporali(train_set_pts1, dati_com1$dateGame[idx_train],
  pts ~ ., best_params_pts, "pts", break_fasce_pts, nomi_fasce_pts)
oof_pred_pts_raw <- oof_pts$raw
oof_pred_pts_q30 <- oof_pts$q30
log_oof_pts <- oof_pts$log
print(log_oof_pts)

train_set_pts1_oof <- train_set_pts1 %>%
  select(-any_of(c("exp_pts_raw", "fascia_pts", "rmse_pts", "exp_pts", "exp_pts_sd", "exp_pts_q30", "exp_pts_q70"))) %>%
  mutate(
    row_id_master = dati_com1$row_id_master[idx_train],
    exp_pts_raw = oof_pred_pts_raw,
    exp_pts     = exp_pts_raw, # Modello Grezzo
    fascia_pts  = cut(exp_pts, breaks = break_fasce_pts, labels = nomi_fasce_pts, include.lowest = TRUE)
  ) %>%
  mutate(exp_pts_sd = NA_real_, exp_pts_q30 = oof_pred_pts_q30, exp_pts_q70 = NA_real_)

calibrazione_q30_pts_train_per_fascia <- curva_calibrazione_q30_per_fascia(
  train_set_pts1_oof, "pts", "exp_pts_q30", "fascia_pts", "Punti — Train OOF"
)

val_set_pts1$row_id_master  <- dati_com1$row_id_master[idx_val]
test_set_pts1$row_id_master <- dati_com1$row_id_master[idx_test]

colonne_punti <- c("row_id_master", "exp_pts", "exp_pts_sd", "exp_pts_q30", "exp_pts_q70")

pred_punti <- bind_rows(
  train_set_pts1_oof %>% select(all_of(colonne_punti)),
  val_set_pts1        %>% select(all_of(colonne_punti)),
  test_set_pts1       %>% select(all_of(colonne_punti))
)

print(analizza_sottostime_hit_q30(test_set_pts1, "pts", "exp_pts_q30", "fascia_pts", "Punti"))

dati_com <- dati_com %>%
  select(-any_of(c("exp_pts", "exp_pts_sd", "exp_pts_q30", "exp_pts_q70"))) %>%
  left_join(pred_punti, by = "row_id_master") %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)

#### Modello exp_treb ####
# Rimbalzi totali attesi.
escludere <- c("row_id",
               "yearSeason", "dateGame", "idGame", "slugTeam", "slugOpponent", 
               "namePlayer", "idPlayer", "nameTeam", "idTeam", "isWin",
               "numberGameTeamSeason_Opp", "isB2BSecond_Opp", "locationGame_Opp", "countDaysRestTeam_Opp",
               "minutes",
               "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", 
               "fg2m", "fg2a", "pctFG2", "ftm", "fta", "oreb", "dreb", "ast", "stl", "blk", "tov", "pf", "plusminus", "pts",
               "pts_ast", "pts_treb", "treb_ast", "pts_reb_ast",
               "fgContested", "fg2Contested", "fg3Contested", "boxOutsPlayerTeamRebound", 
               "screenAssist", "ptsScreenAssist", "deflections", "chargesDrawn", 
               "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
               "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts", 
               "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT", "AST_TOV_RATIO", "EST_USG_PCT",
               "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp", "pctFG3Opp", 
               "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp", "minutesOpp", "ftmOpp", 
               "ftaOpp", "orebOpp", "drebOpp", "trebOpp", "astOpp", "stlOpp", 
               "blkOpp", "tovOpp", "pfOpp", "ptsOpp", "plusminusOpp", "possessionsOpp", 
               "paceOpp", "efgPctOpp", "defRatingOpp", "exp_min", "exp_min_q70"
)

df_treb <- dati_com1 %>%
  select(-all_of(c(escludere, "row_id_master", "split_temporale")))

# Suddivisione cronologica: 70% train, 15% validation, 15% test.
idx_train <- which(dati_com1$split_temporale == "train" & dati_com1$eligible_treb_model)
idx_val <- which(dati_com1$split_temporale == "validation" & dati_com1$eligible_treb_model)
idx_test <- which(dati_com1$split_temporale == "test" & dati_com1$eligible_treb_model)

train_set_treb <- df_treb[idx_train,]
val_set_treb <- df_treb[idx_val,]
test_set_treb <- df_treb[idx_test,]

#Triage per selezionare le migliori features
set.seed(123)
rf_quick_treb <- ranger(
  formula                   = treb ~ .,
  data                      = train_set_treb,
  num.trees                 = 200,
  importance                = "permutation",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE
)

top_vars_treb <- importance(rf_quick_treb) %>%
  enframe(name="Variabile", value="Importanza") %>%
  arrange(desc(Importanza)) %>%
  slice_head(n=40) %>%
  pull(Variabile)

top_vars_treb <- c(
  "treb_L10", "treb_season_avg", "dreb_season_avg", "dreb_L10",
  "treb_L5", "treb_ast_L10", "screenAssist_season_avg", "treb_ast_season_avg",
  "oreb_season_avg", "dreb_L5", "treb_L3", "pts_treb_L10",
  "dreb_L3", "oreb_L10", "ptsScreenAssist_season_avg", "boxOutsPlayerTREB_season_avg",
  "fg2Contested_season_avg", "treb_ast_L5", "fg2m_season_avg", "pts_treb_L5",
  "pts_reb_ast_season_avg", "pts_treb_season_avg", "boxOutsOffense_season_avg", "fg2a_season_avg",
  "fgContested_season_avg", "fg2a_L10", "boxOuts_season_avg", "pts_reb_ast_L10",
  "treb_H2H_L3", "fg2a_L5", "boxOutsPlayerTREB_L10", "screenAssist_L10",
  "boxOutsPlayerTeamRebound_season_avg", "fgm_season_avg", "oreb_L5", "fga_season_avg",
  "pts_reb_ast_L5", "fg2Contested_L10", "exp_min_q30", "pts_reb_ast_L3"
)

df_treb1 <- df_treb %>%
  select(treb, all_of(top_vars_treb)) %>%
  mutate(across(where(is.character), as.factor))

train_set_treb1 <- df_treb1[idx_train, ]
val_set_treb1   <- df_treb1[idx_val, ]
test_set_treb1  <- df_treb1[idx_test, ]

p <- ncol(train_set_treb1) - 1
hyper_grid_treb <- expand.grid(
  mtry            = c(15, 20, 25, 30),
  min.node.size   = c(500, 550, 600, 650),  
  sample.fraction = c(0.4, 0.5),           
  num.trees       = 500,                  
  mae  = NA,
  rmse = NA
)

set.seed(123)
for (i in 1:nrow(hyper_grid_treb)) {
  modello_temp <- ranger(
    formula                   = treb ~ .,
    data                      = train_set_treb1,
    num.trees                 = hyper_grid_treb$num.trees[i],
    mtry                      = hyper_grid_treb$mtry[i],
    min.node.size             = hyper_grid_treb$min.node.size[i],
    sample.fraction           = hyper_grid_treb$sample.fraction[i],
    respect.unordered.factors = "order",
    seed                      = 123,
    verbose                   = FALSE,
    num.threads               = numero_thread_ranger()
  )
  pred_val <- predict(modello_temp, data = val_set_treb1)$predictions
  hyper_grid_treb$mae[i]  <- mean(abs(pred_val - val_set_treb1$treb), na.rm = TRUE)
  hyper_grid_treb$rmse[i] <- sqrt(mean((pred_val - val_set_treb1$treb)^2, na.rm = TRUE))
}

hyper_grid_treb  <- hyper_grid_treb %>% arrange(mae) %>% head(10)
best_params_treb <- hyper_grid_treb[1, ]
#Migliore: 20-550-.5-500-1.988-2.574

#Modello migliore
rf_treb_ott <- ranger(
  formula                   = treb ~ .,
  data                      = train_set_treb1,
  num.trees                 = best_params_treb$num.trees,
  mtry                      = best_params_treb$mtry,
  min.node.size             = best_params_treb$min.node.size,
  sample.fraction           = best_params_treb$sample.fraction,
  importance                = "impurity",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE,
  num.threads               = numero_thread_ranger()
)

break_fasce_treb <- c(-Inf, 4, 8, Inf)
nomi_fasce_treb <- c("Low (<4reb)", "Mid (4-8reb)", "High (>8reb)")
z_30_treb <- abs(qnorm(.3))*1.18

val_set_treb1 <- val_set_treb1 %>%
  select(-any_of(c("exp_treb_raw", "fascia_treb", "rmse_treb", "exp_treb", "exp_treb_sd", "exp_treb_q30", "exp_treb_q70", "delta_treb_q30", "hit_q30", "residuo"))) %>%
  mutate(
    exp_treb_raw = predict(rf_treb_ott, data=val_set_treb1)$predictions,
    exp_treb = exp_treb_raw, #Nessuna calibrazione
    fascia_treb = cut(exp_treb, breaks=break_fasce_treb, labels=nomi_fasce_treb, include.lowest=T)
  )

calibrazione_fasce_treb <- val_set_treb1 %>%
  group_by(fascia_treb) %>%
  summarise(
    rmse_treb=sqrt(mean((exp_treb-treb)^2, na.rm=T)),
    .groups="drop"
  )

calibrazione_q30_fasce_treb <- calibra_q30_per_fascia(
  val_set_treb1, "treb", "exp_treb", "fascia_treb"
)
print(calibrazione_q30_fasce_treb)
calibrazione_fasce_treb <- calibrazione_fasce_treb %>%
  left_join(calibrazione_q30_fasce_treb %>% select(fascia, correzione_q30),
            by = c("fascia_treb" = "fascia"))

val_set_treb1 <- val_set_treb1 %>%
  left_join(calibrazione_fasce_treb, by="fascia_treb") %>%
  mutate(
    exp_treb_sd=rmse_treb,
    exp_treb_q30=pmax(0, exp_treb + correzione_q30),
    exp_treb_q70=exp_treb+z_30_treb*exp_treb_sd
  )

res_val_treb <- val_err(val_set_treb1, "treb", "exp_treb", "Validation Rimbalzi (Grezzo)")
metriche_validation_treb <- metriche_previsione(val_set_treb1, "treb", "exp_treb")
print(metriche_validation_treb)

val_set_treb1 <- val_set_treb1 %>%
  mutate(
    delta_treb_q30 = treb - exp_treb_q30,
    hit_q30 = delta_treb_q30 >= 0
  )

report_q30_treb <- val_set_treb1 %>%
  group_by(fascia_treb) %>%
  summarise(
    N_Osservazioni = n(),
    MAE_Rimbalzi = mean(abs(treb-exp_treb), na.rm=T),
    RMSE_Rimbalzi = sqrt(mean((treb-exp_treb)^2, na.rm=T)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm=T)*100,
    .groups = "drop"
  )

print(report_q30_treb)
cat(sprintf("\nHit Rate Globale Q30 Rimbalzi: %.2f%%\n", mean(val_set_treb1$hit_q30, na.rm = TRUE) * 100))
calibrazione_q30_treb_validation_per_fascia <- curva_calibrazione_q30_per_fascia(
  val_set_treb1, "treb", "exp_treb_q30", "fascia_treb", "Rimbalzi — Validation"
)

test_set_treb1 <- test_set_treb1 %>%
  mutate(exp_treb_raw = predict(rf_treb_ott, data = test_set_treb1)$predictions,
         exp_treb = exp_treb_raw,
         fascia_treb = cut(exp_treb, breaks = break_fasce_treb, labels = nomi_fasce_treb, include.lowest = TRUE)) %>%
  left_join(calibrazione_fasce_treb, by = "fascia_treb") %>%
  mutate(exp_treb_sd = rmse_treb,
         exp_treb_q30 = pmax(0, exp_treb + correzione_q30),
         exp_treb_q70 = exp_treb + z_30_treb * exp_treb_sd,
         delta_treb_q30 = treb - exp_treb_q30,
         hit_q30 = delta_treb_q30 >= 0)
res_test_treb <- val_err(test_set_treb1, "treb", "exp_treb", "Test Rimbalzi (Grezzo)")
metriche_test_treb <- metriche_previsione(test_set_treb1, "treb", "exp_treb") %>%
  mutate(Hit_Rate_Q30_pct = mean(test_set_treb1$hit_q30, na.rm = TRUE) * 100)
print(metriche_test_treb)
cat(sprintf("\nHit Rate Q30 sul test set Rimbalzi: %.2f%% (obiettivo: circa 70%%)\n", metriche_test_treb$Hit_Rate_Q30_pct))
calibrazione_q30_treb_test_per_fascia <- curva_calibrazione_q30_per_fascia(
  test_set_treb1, "treb", "exp_treb_q30", "fascia_treb", "Rimbalzi — Test"
)

# OOF temporali expanding-window.
oof_treb <- genera_oof_temporali(train_set_treb1, dati_com1$dateGame[idx_train],
  treb ~ ., best_params_treb, "treb", break_fasce_treb, nomi_fasce_treb)
oof_pred_treb_raw <- oof_treb$raw
oof_pred_treb_q30 <- oof_treb$q30
log_oof_treb <- oof_treb$log
print(log_oof_treb)

train_set_treb1_oof <- train_set_treb1 %>%
  select(-any_of(c("exp_treb_raw", "fascia_treb", "rmse_treb", "exp_treb", "exp_treb_sd", "exp_treb_q30", "exp_treb_q70"))) %>%
  mutate(
    row_id_master = dati_com1$row_id_master[idx_train],
    exp_treb_raw = oof_pred_treb_raw,
    exp_treb     = exp_treb_raw, # Modello Grezzo
    fascia_treb  = cut(exp_treb, breaks = break_fasce_treb, labels = nomi_fasce_treb, include.lowest = TRUE)
  ) %>%
  mutate(exp_treb_sd = NA_real_, exp_treb_q30 = oof_pred_treb_q30, exp_treb_q70 = NA_real_)

calibrazione_q30_treb_train_per_fascia <- curva_calibrazione_q30_per_fascia(
  train_set_treb1_oof, "treb", "exp_treb_q30", "fascia_treb", "Rimbalzi — Train OOF"
)

val_set_treb1$row_id_master  <- dati_com1$row_id_master[idx_val]
test_set_treb1$row_id_master <- dati_com1$row_id_master[idx_test]

colonne_rimbalzi <- c("row_id_master", "exp_treb", "exp_treb_sd", "exp_treb_q30", "exp_treb_q70")

pred_rimbalzi <- bind_rows(
  train_set_treb1_oof %>% select(all_of(colonne_rimbalzi)),
  val_set_treb1        %>% select(all_of(colonne_rimbalzi)),
  test_set_treb1       %>% select(all_of(colonne_rimbalzi))
)

print(analizza_sottostime_hit_q30(test_set_treb1, "treb", "exp_treb_q30", "fascia_treb", "Rimbalzi"))

dati_com <- dati_com %>%
  select(-any_of(c("exp_treb", "exp_treb_sd", "exp_treb_q30", "exp_treb_q70"))) %>%
  left_join(pred_rimbalzi, by = "row_id_master") %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)

#### Modello exp_ast ####
# Assist attesi.
escludere <- c("row_id",
               "yearSeason", "dateGame", "idGame", "slugTeam", "slugOpponent", 
               "namePlayer", "idPlayer", "nameTeam", "idTeam", "isWin",
               "numberGameTeamSeason_Opp", "isB2BSecond_Opp", "locationGame_Opp", "countDaysRestTeam_Opp",
               "minutes", "pts",
               "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", 
               "fg2m", "fg2a", "pctFG2", "ftm", "fta", "treb", "oreb", "dreb", "stl", "blk", "tov", "pf", "plusminus",
               "fgContested", "fg2Contested", "fg3Contested", "boxOutsPlayerTeamRebound", 
               "screenAssist", "ptsScreenAssist", "deflections", "chargesDrawn", 
               "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
               "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts", 
               "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT", "AST_TOV_RATIO", "EST_USG_PCT",
               "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp", "pctFG3Opp", 
               "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp", "minutesOpp", "ftmOpp", 
               "ftaOpp", "orebOpp", "drebOpp", "trebOpp", "astOpp", "stlOpp", 
               "blkOpp", "tovOpp", "pfOpp", "ptsOpp", "plusminusOpp", "possessionsOpp", 
               "paceOpp", "efgPctOpp", "defRatingOpp", "exp_min", "exp_min_q70", "pts_ast", "pts_treb", "treb_ast", "pts_reb_ast"
)

df_ast <- dati_com1 %>%
  select(-all_of(c(escludere, "row_id_master", "split_temporale")))

# Suddivisione cronologica: 70% train, 15% validation, 15% test.
idx_train <- which(dati_com1$split_temporale == "train" & dati_com1$eligible_ast_model)
idx_val <- which(dati_com1$split_temporale == "validation" & dati_com1$eligible_ast_model)
idx_test <- which(dati_com1$split_temporale == "test" & dati_com1$eligible_ast_model)

train_set_ast <- df_ast[idx_train,]
val_set_ast <- df_ast[idx_val,]
test_set_ast <- df_ast[idx_test,]

#Triage per selezionare le migliori features
set.seed(123)
rf_quick_ast <- ranger(
  formula                   = ast ~ .,
  data                      = train_set_ast,
  num.trees                 = 200,
  importance                = "permutation",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE
)

top_vars_ast <- importance(rf_quick_ast) %>%
  enframe(name="Variabile", value="Importanza") %>%
  arrange(desc(Importanza)) %>%
  slice_head(n=40) %>%
  pull(Variabile)

top_vars_ast <- c(
  "ast_season_avg", "ast_L10", "ast_L5", "pts_ast_season_avg",
  "AST_TOV_RATIO_season_avg", "pts_ast_L10", "ast_L3", "pts_ast_L5",
  "tov_season_avg", "pts_ast_L3", "treb_ast_L10", "AST_TOV_RATIO_L10",
  "pts_reb_ast_season_avg", "treb_ast_season_avg", "fgm_season_avg", "ast_H2H_L3",
  "AST_TOV_RATIO_L5", "tov_L10", "pts_reb_ast_L10", "pts_reb_ast_L5",
  "fga_L10", "pts_season_avg", "fg2a_season_avg", "fga_season_avg",
  "pts_L10", "pts_treb_season_avg", "tov_L5", "pts_treb_L10",
  "exp_min_q30", "fga_L5", "treb_ast_L5", "pts_reb_ast_L3",
  "EST_USG_PCT_season_avg", "ftm_season_avg", "ast_L1", "minutes_season_avg",
  "treb_ast_L3", "AST_TOV_RATIO_L3", "pts_L5", "minutes_L5"
)

df_ast1 <- df_ast %>%
  select(ast, all_of(top_vars_ast)) %>%
  mutate(across(where(is.character), as.factor))

train_set_ast1 <- df_ast1[idx_train, ]
val_set_ast1   <- df_ast1[idx_val, ]
test_set_ast1  <- df_ast1[idx_test, ]

p <- ncol(train_set_ast1) - 1
hyper_grid_ast <- expand.grid(
  mtry            = c(5, 8, 10 ,13), 
  min.node.size   = c(150, 200, 250, 300),
  sample.fraction = c(0.4, 0.5),        
  num.trees       = 500,           
  mae  = NA,
  rmse = NA
)

set.seed(123)
for (i in 1:nrow(hyper_grid_ast)) {
  modello_temp <- ranger(
    formula                   = ast ~ .,
    data                      = train_set_ast1,
    num.trees                 = hyper_grid_ast$num.trees[i],
    mtry                      = hyper_grid_ast$mtry[i],
    min.node.size             = hyper_grid_ast$min.node.size[i],
    sample.fraction           = hyper_grid_ast$sample.fraction[i],
    respect.unordered.factors = "order",
    seed                      = 123,
    verbose                   = FALSE,
    num.threads               = numero_thread_ranger()
  )
  pred_val <- predict(modello_temp, data = val_set_ast1)$predictions
  hyper_grid_ast$mae[i]  <- mean(abs(pred_val - val_set_ast1$ast), na.rm = TRUE)
  hyper_grid_ast$rmse[i] <- sqrt(mean((pred_val - val_set_ast1$ast)^2, na.rm = TRUE))
}

hyper_grid_ast  <- hyper_grid_ast %>% arrange(mae) %>% head(10)
best_params_ast <- hyper_grid_ast[1, ]
#Migliore: 8-200-.4-500-1.478-1.941

#Modello migliore
rf_ast_ott <- ranger(
  formula                   = ast ~ .,
  data                      = train_set_ast1,
  num.trees                 = best_params_ast$num.trees,
  mtry                      = best_params_ast$mtry,
  min.node.size             = best_params_ast$min.node.size,
  sample.fraction           = best_params_ast$sample.fraction,
  importance                = "impurity",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE,
  num.threads               = numero_thread_ranger()
)

break_fasce_ast <- c(-Inf, 3, 6, Inf)
nomi_fasce_ast <- c("Low (<3ast)", "Mid (3-6ast)", "High (>6ast)")
z_30_ast <- abs(qnorm(.3))*1.25

val_set_ast1 <- val_set_ast1 %>%
  select(-any_of(c("exp_ast_raw", "fascia_ast", "rmse_ast", "exp_ast", "exp_ast_sd", "exp_ast_q30", "exp_ast_q70", "delta_ast_q30", "hit_q30", "residuo"))) %>%
  mutate(
    exp_ast_raw = predict(rf_ast_ott, data=val_set_ast1)$predictions,
    exp_ast = exp_ast_raw, #Nessuna calibrazione
    fascia_ast = cut(exp_ast, breaks=break_fasce_ast, labels=nomi_fasce_ast, include.lowest=T)
  )

calibrazione_fasce_ast <- val_set_ast1 %>%
  group_by(fascia_ast) %>%
  summarise(
    rmse_ast=sqrt(mean((exp_ast-ast)^2, na.rm=T)),
    .groups="drop"
  )

calibrazione_q30_fasce_ast <- calibra_q30_per_fascia(
  val_set_ast1, "ast", "exp_ast", "fascia_ast"
)
print(calibrazione_q30_fasce_ast)
calibrazione_fasce_ast <- calibrazione_fasce_ast %>%
  left_join(calibrazione_q30_fasce_ast %>% select(fascia, correzione_q30),
            by = c("fascia_ast" = "fascia"))

val_set_ast1 <- val_set_ast1 %>%
  left_join(calibrazione_fasce_ast, by="fascia_ast") %>%
  mutate(
    exp_ast_sd=rmse_ast,
    exp_ast_q30=pmax(0, exp_ast + correzione_q30),
    exp_ast_q70=exp_ast+z_30_ast*exp_ast_sd
  )

res_val_ast <- val_err(val_set_ast1, "ast", "exp_ast", "Validation Assist (Grezzo)")
metriche_validation_ast <- metriche_previsione(val_set_ast1, "ast", "exp_ast")
print(metriche_validation_ast)

val_set_ast1 <- val_set_ast1 %>%
  mutate(
    delta_ast_q30 = ast - exp_ast_q30,
    hit_q30 = delta_ast_q30 >= 0
  )

report_q30_ast <- val_set_ast1 %>%
  group_by(fascia_ast) %>%
  summarise(
    N_Osservazioni = n(),
    MAE_Assist = mean(abs(ast-exp_ast), na.rm=T),
    RMSE_Assist = sqrt(mean((ast-exp_ast)^2, na.rm=T)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm=T)*100,
    .groups = "drop"
  )

print(report_q30_ast)
cat(sprintf("\nHit Rate Globale Q30 Assist: %.2f%%\n", mean(val_set_ast1$hit_q30, na.rm = TRUE) * 100))
calibrazione_q30_ast_validation_per_fascia <- curva_calibrazione_q30_per_fascia(
  val_set_ast1, "ast", "exp_ast_q30", "fascia_ast", "Assist — Validation"
)

test_set_ast1 <- test_set_ast1 %>%
  mutate(exp_ast_raw = predict(rf_ast_ott, data = test_set_ast1)$predictions,
         exp_ast = exp_ast_raw,
         fascia_ast = cut(exp_ast, breaks = break_fasce_ast, labels = nomi_fasce_ast, include.lowest = TRUE)) %>%
  left_join(calibrazione_fasce_ast, by = "fascia_ast") %>%
  mutate(exp_ast_sd = rmse_ast,
         exp_ast_q30 = pmax(0, exp_ast + correzione_q30),
         exp_ast_q70 = exp_ast + z_30_ast * exp_ast_sd,
         delta_ast_q30 = ast - exp_ast_q30,
         hit_q30 = delta_ast_q30 >= 0)
res_test_ast <- val_err(test_set_ast1, "ast", "exp_ast", "Test Assist (Grezzo)")
metriche_test_ast <- metriche_previsione(test_set_ast1, "ast", "exp_ast") %>%
  mutate(Hit_Rate_Q30_pct = mean(test_set_ast1$hit_q30, na.rm = TRUE) * 100)
print(metriche_test_ast)
cat(sprintf("\nHit Rate Q30 sul test set Assist: %.2f%% (obiettivo: circa 70%%)\n", metriche_test_ast$Hit_Rate_Q30_pct))
calibrazione_q30_ast_test_per_fascia <- curva_calibrazione_q30_per_fascia(
  test_set_ast1, "ast", "exp_ast_q30", "fascia_ast", "Assist — Test"
)

# OOF temporali expanding-window.
oof_ast <- genera_oof_temporali(train_set_ast1, dati_com1$dateGame[idx_train],
  ast ~ ., best_params_ast, "ast", break_fasce_ast, nomi_fasce_ast)
oof_pred_ast_raw <- oof_ast$raw
oof_pred_ast_q30 <- oof_ast$q30
log_oof_ast <- oof_ast$log
print(log_oof_ast)

train_set_ast1_oof <- train_set_ast1 %>%
  select(-any_of(c("exp_ast_raw", "fascia_ast", "rmse_ast", "exp_ast", "exp_ast_sd", "exp_ast_q30", "exp_ast_q70"))) %>%
  mutate(
    row_id_master = dati_com1$row_id_master[idx_train],
    exp_ast_raw = oof_pred_ast_raw,
    exp_ast     = exp_ast_raw, # Modello Grezzo
    fascia_ast  = cut(exp_ast, breaks = break_fasce_ast, labels = nomi_fasce_ast, include.lowest = TRUE)
  ) %>%
  mutate(exp_ast_sd = NA_real_, exp_ast_q30 = oof_pred_ast_q30, exp_ast_q70 = NA_real_)

calibrazione_q30_ast_train_per_fascia <- curva_calibrazione_q30_per_fascia(
  train_set_ast1_oof, "ast", "exp_ast_q30", "fascia_ast", "Assist — Train OOF"
)

val_set_ast1$row_id_master  <- dati_com1$row_id_master[idx_val]
test_set_ast1$row_id_master <- dati_com1$row_id_master[idx_test]

colonne_assist <- c("row_id_master", "exp_ast", "exp_ast_sd", "exp_ast_q30", "exp_ast_q70")

pred_assist <- bind_rows(
  train_set_ast1_oof %>% select(all_of(colonne_assist)),
  val_set_ast1        %>% select(all_of(colonne_assist)),
  test_set_ast1       %>% select(all_of(colonne_assist))
)

print(analizza_sottostime_hit_q30(test_set_ast1, "ast", "exp_ast_q30", "fascia_ast", "Assist"))

dati_com <- dati_com %>%
  select(-any_of(c("exp_ast", "exp_ast_sd", "exp_ast_q30", "exp_ast_q70"))) %>%
  left_join(pred_assist, by = "row_id_master") %>%
  arrange(dateGame, idGame, idPlayer, row_id_master) %>%
  mutate(
    eligible_pts_ast_model = !is.na(exp_min_q30) & !is.na(exp_pts_q30) & !is.na(exp_ast_q30),
    eligible_pts_treb_model = !is.na(exp_min_q30) & !is.na(exp_pts_q30) & !is.na(exp_treb_q30),
    eligible_treb_ast_model = !is.na(exp_min_q30) & !is.na(exp_treb_q30) & !is.na(exp_ast_q30),
    eligible_pra_model = !is.na(exp_min_q30) & !is.na(exp_pts_q30) &
      !is.na(exp_treb_q30) & !is.na(exp_ast_q30)
  )

dati_com2 <- dati_com #Inseriamo le altre stime qui

#### Modello exp_pts_ast ####
# Somma punti + assist.
escludere <- c("row_id",
               "yearSeason", "dateGame", "idGame", "slugTeam", "slugOpponent", 
               "namePlayer", "idPlayer", "nameTeam", "idTeam", "isWin",
               "numberGameTeamSeason_Opp", "isB2BSecond_Opp", "locationGame_Opp", "countDaysRestTeam_Opp",
               "minutes", "pts", "ast",
               "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", 
               "fg2m", "fg2a", "pctFG2", "ftm", "fta", "treb", "oreb", "dreb", "stl", "blk", "tov", "pf", "plusminus",
               "fgContested", "fg2Contested", "fg3Contested", "boxOutsPlayerTeamRebound", 
               "screenAssist", "ptsScreenAssist", "deflections", "chargesDrawn", 
               "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
               "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts", 
               "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT", "AST_TOV_RATIO", "EST_USG_PCT",
               "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp", "pctFG3Opp", 
               "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp", "minutesOpp", "ftmOpp", 
               "ftaOpp", "orebOpp", "drebOpp", "trebOpp", "astOpp", "stlOpp", 
               "blkOpp", "tovOpp", "pfOpp", "ptsOpp", "plusminusOpp", "possessionsOpp", 
               "paceOpp", "efgPctOpp", "defRatingOpp", "exp_min", "exp_min_q70",
               "exp_pts_q70", "exp_pts", "exp_treb", "exp_treb_q70", "exp_ast_q70", "exp_ast", "pts_treb", "treb_ast", "pts_reb_ast"
)

df_pts_ast <- dati_com %>%
  select(-all_of(c(escludere, "row_id_master", "split_temporale")))

# Suddivisione cronologica: 70% train, 15% validation, 15% test.
idx_train <- which(dati_com$split_temporale == "train" & dati_com$eligible_pts_ast_model)
idx_val <- which(dati_com$split_temporale == "validation" & dati_com$eligible_pts_ast_model)
idx_test <- which(dati_com$split_temporale == "test" & dati_com$eligible_pts_ast_model)

train_set_pts_ast <- df_pts_ast[idx_train,]
val_set_pts_ast <- df_pts_ast[idx_val,]
test_set_pts_ast <- df_pts_ast[idx_test,]

#Triage per selezionare le migliori features
set.seed(123)
rf_quick_pts_ast <- ranger(
  formula                   = pts_ast ~ .,
  data                      = train_set_pts_ast,
  num.trees                 = 200,
  importance                = "permutation",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE
)

top_vars_pts_ast <- importance(rf_quick_pts_ast) %>%
  enframe(name="Variabile", value="Importanza") %>%
  arrange(desc(Importanza)) %>%
  slice_head(n=40) %>%
  pull(Variabile)

top_vars_pts_ast <- c(
  "pts_ast_L10", "pts_ast_season_avg", "exp_pts_q30", "fga_L10",
  "pts_season_avg", "fga_season_avg", "pts_L10", "pts_ast_L5",
  "pts_reb_ast_season_avg", "fgm_season_avg", "pts_reb_ast_L10", "pts_ast_L3",
  "pts_reb_ast_L5", "pts_treb_season_avg", "fga_L3", "exp_ast_q30",
  "fga_L5", "fgm_L10", "fgm_L5", "pts_L5",
  "pts_treb_L10", "exp_min_q30", "EST_USG_PCT_season_avg", "fg2a_season_avg",
  "ast_season_avg", "pts_reb_ast_L3", "EST_USG_PCT_L10", "pts_L3",
  "ftm_season_avg", "ast_L10", "pts_ast_H2H_L3", "EST_USG_PCT_L5",
  "fg2m_season_avg", "fgm_L3", "fg2a_L10", "fga_L1",
  "fta_season_avg", "ast_L5", "pts_per_min_L5", "fga_per_min_L5"
)

df_pts_ast1 <- df_pts_ast %>%
  select(pts_ast, all_of(top_vars_pts_ast)) %>%
  mutate(across(where(is.character), as.factor))

train_set_pts_ast1 <- df_pts_ast1[idx_train, ]
val_set_pts_ast1   <- df_pts_ast1[idx_val, ]
test_set_pts_ast1  <- df_pts_ast1[idx_test, ]

p <- ncol(train_set_pts_ast1) - 1
hyper_grid_pts_ast <- expand.grid(
  mtry            = c(3,5, 8, 10), 
  min.node.size   = c(150, 200, 250, 300, 350),
  sample.fraction = c(0.4, 0.5, 0.6),        
  num.trees       = 500,           
  mae  = NA,
  rmse = NA
)

set.seed(123)
for (i in 1:nrow(hyper_grid_pts_ast)) {
  modello_temp <- ranger(
    formula                   = pts_ast ~ .,
    data                      = train_set_pts_ast1,
    num.trees                 = hyper_grid_pts_ast$num.trees[i],
    mtry                      = hyper_grid_pts_ast$mtry[i],
    min.node.size             = hyper_grid_pts_ast$min.node.size[i],
    sample.fraction           = hyper_grid_pts_ast$sample.fraction[i],
    respect.unordered.factors = "order",
    seed                      = 123,
    verbose                   = FALSE,
    num.threads               = numero_thread_ranger()
  )
  pred_val <- predict(modello_temp, data = val_set_pts_ast1)$predictions
  hyper_grid_pts_ast$mae[i]  <- mean(abs(pred_val - val_set_pts_ast1$pts_ast), na.rm = TRUE)
  hyper_grid_pts_ast$rmse[i] <- sqrt(mean((pred_val - val_set_pts_ast1$pts_ast)^2, na.rm = TRUE))
}

hyper_grid_pts_ast  <- hyper_grid_pts_ast %>% arrange(mae) %>% head(10)
best_params_pts_ast <- hyper_grid_pts_ast[1, ]
#Migliore: 8-300-.6-500-5.402-6.892

#Validation
rf_pts_ast_ott <- ranger(
  formula                   = pts_ast ~ .,
  data                      = train_set_pts_ast1,
  num.trees                 = best_params_pts_ast$num.trees,
  mtry                      = best_params_pts_ast$mtry,
  min.node.size             = best_params_pts_ast$min.node.size,
  sample.fraction           = best_params_pts_ast$sample.fraction,
  importance                = "impurity",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE,
  num.threads               = numero_thread_ranger()
)

#validation
break_fasce_pts_ast <- c(-Inf, 15, 30, Inf)
nomi_fasce_pts_ast <- c("Low (<15 PA)", "Mid (15-30 PA)", "High (>30 PA)")
z_30_pts_ast <- abs(qnorm(.3))*1.08

val_set_pts_ast1 <- val_set_pts_ast1 %>%
  select(-any_of(c("exp_pts_ast_raw", "fascia_pts_ast", "bias_medio_pts_ast", "rmse_pts_ast",
                   "exp_pts_ast", "exp_pts_ast_sd", "exp_pts_ast_q30", "exp_pts_ast_q70"))) %>%
  mutate(
    exp_pts_ast_raw = predict(rf_pts_ast_ott, data = val_set_pts_ast1)$predictions,
    fascia_pts_ast = cut(exp_pts_ast_raw, breaks = break_fasce_pts_ast, labels = nomi_fasce_pts_ast, include.lowest = TRUE)
  )

calibrazione_fasce_pts_ast <- val_set_pts_ast1 %>%
  group_by(fascia_pts_ast) %>%
  summarise(
    bias_medio_pts_ast = mean(exp_pts_ast_raw - pts_ast, na.rm = TRUE),
    rmse_pts_ast       = sqrt(mean((exp_pts_ast_raw - pts_ast)^2, na.rm = TRUE)),
    .groups            = "drop"
  )

calibrazione_fasce_pts_ast

val_set_pts_ast1 <- val_set_pts_ast1 %>%
  left_join(calibrazione_fasce_pts_ast, by = "fascia_pts_ast") %>%
  mutate(
    exp_pts_ast = exp_pts_ast_raw - bias_medio_pts_ast, # Applicazione Correzione Bias
    exp_pts_ast_sd = rmse_pts_ast,
    exp_pts_ast_q70 = exp_pts_ast + z_30_pts_ast * exp_pts_ast_sd
  )

calibrazione_q30_fasce_pts_ast <- calibra_q30_per_fascia(
  val_set_pts_ast1, "pts_ast", "exp_pts_ast", "fascia_pts_ast"
)
print(calibrazione_q30_fasce_pts_ast)
calibrazione_fasce_pts_ast <- calibrazione_fasce_pts_ast %>%
  left_join(calibrazione_q30_fasce_pts_ast %>% select(fascia, correzione_q30),
            by = c("fascia_pts_ast" = "fascia"))

val_set_pts_ast1 <- val_set_pts_ast1 %>%
  left_join(calibrazione_q30_fasce_pts_ast %>% select(fascia, correzione_q30),
            by = c("fascia_pts_ast" = "fascia")) %>%
  mutate(
    exp_pts_ast_q30 = pmax(0, exp_pts_ast + correzione_q30),
    delta_pts_ast_q30 = pts_ast - exp_pts_ast_q30,
    hit_q30 = delta_pts_ast_q30 >= 0
  )

# Report Errori Validation: Grezzo vs Corretto
res_val_pts_ast_raw <- val_err(val_set_pts_ast1, "pts_ast", "exp_pts_ast_raw", "Validation Grezzo PTS+AST")
res_val_pts_ast_cal <- val_err(val_set_pts_ast1, "pts_ast", "exp_pts_ast", "Validation Corretto PTS+AST")
metriche_validation_pts_ast <- metriche_previsione(val_set_pts_ast1, "pts_ast", "exp_pts_ast")
print(metriche_validation_pts_ast)

report_q30_pts_ast <- val_set_pts_ast1 %>%
  group_by(fascia_pts_ast) %>%
  summarise(
    N_Osservazioni = n(),
    MAE_Punti_Assist = mean(abs(pts_ast-exp_pts_ast), na.rm=T),
    RMSE_Punti_Assist = sqrt(mean((pts_ast-exp_pts_ast)^2, na.rm=T)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm=T)*100,
    .groups = "drop"
  )

print(report_q30_pts_ast)
cat(sprintf("\nHit Rate Globale Q30 Punti + Assist: %.2f%%\n", mean(val_set_pts_ast1$hit_q30, na.rm = TRUE) * 100))
calibrazione_q30_pts_ast_validation_per_fascia <- curva_calibrazione_q30_per_fascia(
  val_set_pts_ast1, "pts_ast", "exp_pts_ast_q30", "fascia_pts_ast", "PTS+AST — Validation"
)

test_set_pts_ast1 <- test_set_pts_ast1 %>%
  mutate(exp_pts_ast_raw = predict(rf_pts_ast_ott, data = test_set_pts_ast1)$predictions,
         fascia_pts_ast = cut(exp_pts_ast_raw, breaks = break_fasce_pts_ast, labels = nomi_fasce_pts_ast, include.lowest = TRUE)) %>%
  left_join(calibrazione_fasce_pts_ast, by = "fascia_pts_ast") %>%
  mutate(exp_pts_ast = exp_pts_ast_raw - bias_medio_pts_ast,
         exp_pts_ast_sd = rmse_pts_ast,
         exp_pts_ast_q30 = pmax(0, exp_pts_ast + correzione_q30),
         exp_pts_ast_q70 = exp_pts_ast + z_30_pts_ast * exp_pts_ast_sd,
         delta_pts_ast_q30 = pts_ast - exp_pts_ast_q30,
         hit_q30 = delta_pts_ast_q30 >= 0)
res_test_pts_ast <- val_err(test_set_pts_ast1, "pts_ast", "exp_pts_ast", "Test PTS+AST")
metriche_test_pts_ast <- metriche_previsione(test_set_pts_ast1, "pts_ast", "exp_pts_ast") %>%
  mutate(Hit_Rate_Q30_pct = mean(test_set_pts_ast1$hit_q30, na.rm = TRUE) * 100)
print(metriche_test_pts_ast)
cat(sprintf("\nHit Rate Q30 sul test set Punti + Assist: %.2f%% (obiettivo: circa 70%%)\n", metriche_test_pts_ast$Hit_Rate_Q30_pct))
calibrazione_q30_pts_ast_test_per_fascia <- curva_calibrazione_q30_per_fascia(
  test_set_pts_ast1, "pts_ast", "exp_pts_ast_q30", "fascia_pts_ast", "PTS+AST — Test"
)


# OOF temporali expanding-window.
oof_pts_ast <- genera_oof_temporali(train_set_pts_ast1, dati_com$dateGame[idx_train],
  pts_ast ~ ., best_params_pts_ast, "pts_ast", break_fasce_pts_ast, nomi_fasce_pts_ast)
oof_pred_pts_ast_raw <- oof_pts_ast$raw
oof_pred_pts_ast_q30 <- oof_pts_ast$q30
log_oof_pts_ast <- oof_pts_ast$log
print(log_oof_pts_ast)

train_set_pts_ast1_oof <- train_set_pts_ast1 %>%
  select(-any_of(c("exp_pts_ast_raw", "fascia_pts_ast", "rmse_pts_ast", "exp_pts_ast", 
                   "exp_pts_ast_sd", "exp_pts_ast_q30", "exp_pts_ast_q70", "bias_medio_pts_ast"))) %>%
  mutate(
    row_id_master   = dati_com$row_id_master[idx_train],
    exp_pts_ast_raw = oof_pred_pts_ast_raw,
    fascia_pts_ast  = cut(exp_pts_ast_raw, breaks = break_fasce_pts_ast, labels = nomi_fasce_pts_ast, include.lowest = TRUE)
  ) %>%
  mutate(exp_pts_ast = exp_pts_ast_raw, exp_pts_ast_sd = NA_real_,
         exp_pts_ast_q30 = oof_pred_pts_ast_q30, exp_pts_ast_q70 = NA_real_)

calibrazione_q30_pts_ast_train_per_fascia <- curva_calibrazione_q30_per_fascia(
  train_set_pts_ast1_oof, "pts_ast", "exp_pts_ast_q30", "fascia_pts_ast", "PTS+AST — Train OOF"
)

val_set_pts_ast1$row_id_master <- dati_com$row_id_master[idx_val]
test_set_pts_ast1$row_id_master <- dati_com$row_id_master[idx_test]

colonne_pts_ast <- c("row_id_master", "exp_pts_ast", "exp_pts_ast_sd", "exp_pts_ast_q30", "exp_pts_ast_q70")

pred_pts_ast <- bind_rows(
  train_set_pts_ast1_oof %>% select(all_of(colonne_pts_ast)),
  val_set_pts_ast1       %>% select(all_of(colonne_pts_ast)),
  test_set_pts_ast1      %>% select(all_of(colonne_pts_ast))
)

print(analizza_sottostime_hit_q30(test_set_pts_ast1, "pts_ast", "exp_pts_ast_q30", "fascia_pts_ast", "PTS+AST"))

dati_com2 <- dati_com2 %>%
  select(-any_of(c("exp_pts_ast", "exp_pts_ast_sd", "exp_pts_ast_q30", "exp_pts_ast_q70"))) %>%
  left_join(pred_pts_ast, by = "row_id_master") %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)

#### Modello exp_pts_treb ####
# Somma punti + rimbalzi.
escludere <- c("row_id",
               "yearSeason", "dateGame", "idGame", "slugTeam", "slugOpponent", 
               "namePlayer", "idPlayer", "nameTeam", "idTeam", "isWin",
               "numberGameTeamSeason_Opp", "isB2BSecond_Opp", "locationGame_Opp", "countDaysRestTeam_Opp",
               "minutes", "pts", "ast",
               "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", 
               "fg2m", "fg2a", "pctFG2", "ftm", "fta", "treb", "oreb", "dreb", "stl", "blk", "tov", "pf", "plusminus",
               "fgContested", "fg2Contested", "fg3Contested", "boxOutsPlayerTeamRebound", 
               "screenAssist", "ptsScreenAssist", "deflections", "chargesDrawn", 
               "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
               "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts", 
               "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT", "AST_TOV_RATIO", "EST_USG_PCT",
               "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp", "pctFG3Opp", 
               "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp", "minutesOpp", "ftmOpp", 
               "ftaOpp", "orebOpp", "drebOpp", "trebOpp", "astOpp", "stlOpp", 
               "blkOpp", "tovOpp", "pfOpp", "ptsOpp", "plusminusOpp", "possessionsOpp", 
               "paceOpp", "efgPctOpp", "defRatingOpp", "exp_min", "exp_min_q70",
               "exp_pts_q70", "exp_pts", "exp_treb", "exp_treb_q70", "exp_ast_q70", "exp_ast", "treb_ast", "pts_reb_ast", "pts_ast"
)

df_pts_treb <- dati_com %>%
  select(-all_of(c(escludere, "row_id_master", "split_temporale")))

# Suddivisione cronologica: 70% train, 15% validation, 15% test.
idx_train <- which(dati_com$split_temporale == "train" & dati_com$eligible_pts_treb_model)
idx_val <- which(dati_com$split_temporale == "validation" & dati_com$eligible_pts_treb_model)
idx_test <- which(dati_com$split_temporale == "test" & dati_com$eligible_pts_treb_model)

train_set_pts_treb <- df_pts_treb[idx_train,]
val_set_pts_treb <- df_pts_treb[idx_val,]
test_set_pts_treb <- df_pts_treb[idx_test,]

#Triage per selezionare le migliori features
set.seed(123)
rf_quick_pts_treb <- ranger(
  formula                   = pts_treb ~ .,
  data                      = train_set_pts_treb,
  num.trees                 = 200,
  importance                = "permutation",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE
)

top_vars_pts_treb <- importance(rf_quick_pts_treb) %>%
  enframe(name="Variabile", value="Importanza") %>%
  arrange(desc(Importanza)) %>%
  slice_head(n=40) %>%
  pull(Variabile)

top_vars_pts_treb <- c(
  "pts_treb_L10", "pts_treb_season_avg", "exp_pts_q30", "pts_reb_ast_L10",
  "pts_season_avg", "fg2m_season_avg", "pts_reb_ast_season_avg", "pts_treb_L5",
  "fga_L10", "fg2a_season_avg", "pts_reb_ast_L5", "pts_ast_L10",
  "fgm_season_avg", "pts_L10", "fgm_L10", "pts_ast_season_avg",
  "fga_season_avg", "pts_reb_ast_L3", "pts_treb_L3", "fga_L5",
  "fg2a_L10", "exp_min_q30", "fg2m_L10", "pts_ast_L5",
  "fga_L3", "pts_L5", "fg2a_L5", "fgm_L5",
  "fgm_L3", "fg2m_L5", "ftm_season_avg", "pts_ast_L3",
  "treb_season_avg", "fta_season_avg", "exp_treb_q30", "pts_per_min_L5",
  "pts_L3", "dreb_season_avg", "dreb_L10", "EST_USG_PCT_season_avg"
)

df_pts_treb1 <- df_pts_treb %>%
  select(pts_treb, all_of(top_vars_pts_treb)) %>%
  mutate(across(where(is.character), as.factor))

train_set_pts_treb1 <- df_pts_treb1[idx_train, ]
val_set_pts_treb1   <- df_pts_treb1[idx_val, ]
test_set_pts_treb1  <- df_pts_treb1[idx_test, ]

p <- ncol(train_set_pts_treb1) - 1
hyper_grid_pts_treb <- expand.grid(
  mtry            = c(15, 20, 25, 30), 
  min.node.size   = c(250, 300, 350, 400),
  sample.fraction = c(0.4, 0.5),        
  num.trees       = 500,           
  mae  = NA,
  rmse = NA
)

set.seed(123)
for (i in 1:nrow(hyper_grid_pts_treb)) {
  modello_temp <- ranger(
    formula                   = pts_treb ~ .,
    data                      = train_set_pts_treb1,
    num.trees                 = hyper_grid_pts_treb$num.trees[i],
    mtry                      = hyper_grid_pts_treb$mtry[i],
    min.node.size             = hyper_grid_pts_treb$min.node.size[i],
    sample.fraction           = hyper_grid_pts_treb$sample.fraction[i],
    respect.unordered.factors = "order",
    seed                      = 123,
    verbose                   = FALSE,
    num.threads               = numero_thread_ranger()
  )
  pred_val <- predict(modello_temp, data = val_set_pts_treb1)$predictions
  hyper_grid_pts_treb$mae[i]  <- mean(abs(pred_val - val_set_pts_treb1$pts_treb), na.rm = TRUE)
  hyper_grid_pts_treb$rmse[i] <- sqrt(mean((pred_val - val_set_pts_treb1$pts_treb)^2, na.rm = TRUE))
}

hyper_grid_pts_treb  <- hyper_grid_pts_treb %>% arrange(mae) %>% head(10)
best_params_pts_treb <- hyper_grid_pts_treb[1, ]
#Migliore: 25-400-.4-500-5.872-7.444

#Validation
rf_pts_treb_ott <- ranger(
  formula                   = pts_treb ~ .,
  data                      = train_set_pts_treb1,
  num.trees                 = best_params_pts_treb$num.trees,
  mtry                      = best_params_pts_treb$mtry,
  min.node.size             = best_params_pts_treb$min.node.size,
  sample.fraction           = best_params_pts_treb$sample.fraction,
  importance                = "impurity",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE,
  num.threads               = numero_thread_ranger()
)

#validation
break_fasce_pts_treb <- c(-Inf, 15, 30, Inf)
nomi_fasce_pts_treb <- c("Low (<15 PT)", "Mid (15-30 PT)", "High (>30 PT)")
z_30_pts_treb <- abs(qnorm(.3))*1.05

val_set_pts_treb1 <- val_set_pts_treb1 %>%
  select(-any_of(c("exp_pts_treb_raw", "fascia_pts_treb", "bias_medio_pts_treb", "rmse_pts_treb",
                   "exp_pts_treb", "exp_pts_treb_sd", "exp_pts_treb_q30", "exp_pts_treb_q70"))) %>%
  mutate(
    exp_pts_treb_raw = predict(rf_pts_treb_ott, data = val_set_pts_treb1)$predictions,
    fascia_pts_treb = cut(exp_pts_treb_raw, breaks = break_fasce_pts_treb, labels = nomi_fasce_pts_treb, include.lowest = TRUE)
  )

calibrazione_fasce_pts_treb <- val_set_pts_treb1 %>%
  group_by(fascia_pts_treb) %>%
  summarise(
    bias_medio_pts_treb = mean(exp_pts_treb_raw - pts_treb, na.rm = TRUE),
    rmse_pts_treb       = sqrt(mean((exp_pts_treb_raw - pts_treb)^2, na.rm = TRUE)),
    .groups            = "drop"
  )

calibrazione_fasce_pts_treb

val_set_pts_treb1 <- val_set_pts_treb1 %>%
  left_join(calibrazione_fasce_pts_treb, by = "fascia_pts_treb") %>%
  mutate(
    exp_pts_treb = exp_pts_treb_raw - bias_medio_pts_treb, # Applicazione Correzione Bias
    exp_pts_treb_sd = rmse_pts_treb,
    exp_pts_treb_q70 = exp_pts_treb + z_30_pts_treb * exp_pts_treb_sd
  )

calibrazione_q30_fasce_pts_treb <- calibra_q30_per_fascia(
  val_set_pts_treb1, "pts_treb", "exp_pts_treb", "fascia_pts_treb"
)
print(calibrazione_q30_fasce_pts_treb)
calibrazione_fasce_pts_treb <- calibrazione_fasce_pts_treb %>%
  left_join(calibrazione_q30_fasce_pts_treb %>% select(fascia, correzione_q30),
            by = c("fascia_pts_treb" = "fascia"))
val_set_pts_treb1 <- val_set_pts_treb1 %>%
  left_join(calibrazione_q30_fasce_pts_treb %>% select(fascia, correzione_q30),
            by = c("fascia_pts_treb" = "fascia")) %>%
  mutate(
    exp_pts_treb_q30 = pmax(0, exp_pts_treb + correzione_q30),
    delta_pts_treb_q30 = pts_treb - exp_pts_treb_q30,
    hit_q30 = delta_pts_treb_q30 >= 0
  )

# Report Errori Validation: Grezzo vs Corretto
res_val_pts_treb_raw <- val_err(val_set_pts_treb1, "pts_treb", "exp_pts_treb_raw", "Validation Grezzo PTS+TREB")
res_val_pts_treb_cal <- val_err(val_set_pts_treb1, "pts_treb", "exp_pts_treb", "Validation Corretto PTS+TREB")
metriche_validation_pts_treb <- metriche_previsione(val_set_pts_treb1, "pts_treb", "exp_pts_treb")
print(metriche_validation_pts_treb)

report_q30_pts_treb <- val_set_pts_treb1 %>%
  group_by(fascia_pts_treb) %>%
  summarise(
    N_Osservazioni = n(),
    MAE_Punti_Rimbalzi = mean(abs(pts_treb-exp_pts_treb), na.rm=T),
    RMSE_Punti_Rimbalzi = sqrt(mean((pts_treb-exp_pts_treb)^2, na.rm=T)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm=T)*100,
    .groups = "drop"
  )

print(report_q30_pts_treb)
cat(sprintf("\nHit Rate Globale Q30 Punti + Rimbalzi: %.2f%%\n", mean(val_set_pts_treb1$hit_q30, na.rm = TRUE) * 100))
calibrazione_q30_pts_treb_validation_per_fascia <- curva_calibrazione_q30_per_fascia(
  val_set_pts_treb1, "pts_treb", "exp_pts_treb_q30", "fascia_pts_treb", "PTS+TREB — Validation"
)

test_set_pts_treb1 <- test_set_pts_treb1 %>%
  mutate(exp_pts_treb_raw = predict(rf_pts_treb_ott, data = test_set_pts_treb1)$predictions,
         fascia_pts_treb = cut(exp_pts_treb_raw, breaks = break_fasce_pts_treb, labels = nomi_fasce_pts_treb, include.lowest = TRUE)) %>%
  left_join(calibrazione_fasce_pts_treb, by = "fascia_pts_treb") %>%
  mutate(exp_pts_treb = exp_pts_treb_raw - bias_medio_pts_treb,
         exp_pts_treb_sd = rmse_pts_treb,
         exp_pts_treb_q30 = pmax(0, exp_pts_treb + correzione_q30),
         exp_pts_treb_q70 = exp_pts_treb + z_30_pts_treb * exp_pts_treb_sd,
         delta_pts_treb_q30 = pts_treb - exp_pts_treb_q30,
         hit_q30 = delta_pts_treb_q30 >= 0)
res_test_pts_treb <- val_err(test_set_pts_treb1, "pts_treb", "exp_pts_treb", "Test PTS+TREB")
metriche_test_pts_treb <- metriche_previsione(test_set_pts_treb1, "pts_treb", "exp_pts_treb") %>%
  mutate(Hit_Rate_Q30_pct = mean(test_set_pts_treb1$hit_q30, na.rm = TRUE) * 100)
print(metriche_test_pts_treb)
cat(sprintf("\nHit Rate Q30 sul test set Punti + Rimbalzi: %.2f%% (obiettivo: circa 70%%)\n", metriche_test_pts_treb$Hit_Rate_Q30_pct))
calibrazione_q30_pts_treb_test_per_fascia <- curva_calibrazione_q30_per_fascia(
  test_set_pts_treb1, "pts_treb", "exp_pts_treb_q30", "fascia_pts_treb", "PTS+TREB — Test"
)

# OOF temporali expanding-window.
oof_pts_treb <- genera_oof_temporali(train_set_pts_treb1, dati_com$dateGame[idx_train],
  pts_treb ~ ., best_params_pts_treb, "pts_treb", break_fasce_pts_treb, nomi_fasce_pts_treb)
oof_pred_pts_treb_raw <- oof_pts_treb$raw
oof_pred_pts_treb_q30 <- oof_pts_treb$q30
log_oof_pts_treb <- oof_pts_treb$log
print(log_oof_pts_treb)

train_set_pts_treb1_oof <- train_set_pts_treb1 %>%
  select(-any_of(c("exp_pts_treb_raw", "fascia_pts_treb", "rmse_pts_treb", "exp_pts_treb", 
                   "exp_pts_treb_sd", "exp_pts_treb_q30", "exp_pts_treb_q70", "bias_medio_pts_treb"))) %>%
  mutate(
    row_id_master    = dati_com$row_id_master[idx_train],
    exp_pts_treb_raw = oof_pred_pts_treb_raw,
    fascia_pts_treb  = cut(exp_pts_treb_raw, breaks = break_fasce_pts_treb, labels = nomi_fasce_pts_treb, include.lowest = TRUE)
  ) %>%
  mutate(exp_pts_treb = exp_pts_treb_raw, exp_pts_treb_sd = NA_real_,
         exp_pts_treb_q30 = oof_pred_pts_treb_q30, exp_pts_treb_q70 = NA_real_)

calibrazione_q30_pts_treb_train_per_fascia <- curva_calibrazione_q30_per_fascia(
  train_set_pts_treb1_oof, "pts_treb", "exp_pts_treb_q30", "fascia_pts_treb", "PTS+TREB — Train OOF"
)

val_set_pts_treb1$row_id_master <- dati_com$row_id_master[idx_val]
test_set_pts_treb1$row_id_master <- dati_com$row_id_master[idx_test]

colonne_pts_treb <- c("row_id_master", "exp_pts_treb", "exp_pts_treb_sd", "exp_pts_treb_q30", "exp_pts_treb_q70")

pred_pts_treb <- bind_rows(
  train_set_pts_treb1_oof %>% select(all_of(colonne_pts_treb)),
  val_set_pts_treb1       %>% select(all_of(colonne_pts_treb)),
  test_set_pts_treb1      %>% select(all_of(colonne_pts_treb))
)

print(analizza_sottostime_hit_q30(test_set_pts_treb1, "pts_treb", "exp_pts_treb_q30", "fascia_pts_treb", "PTS+TREB"))

dati_com2 <- dati_com2 %>%
  select(-any_of(c("exp_pts_treb", "exp_pts_treb_sd", "exp_pts_treb_q30", "exp_pts_treb_q70"))) %>%
  left_join(pred_pts_treb, by = "row_id_master") %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)

#### Modello exp_ast_treb ####
# Somma rimbalzi + assist.
escludere <- c("row_id",
               "yearSeason", "dateGame", "idGame", "slugTeam", "slugOpponent", 
               "namePlayer", "idPlayer", "nameTeam", "idTeam", "isWin",
               "numberGameTeamSeason_Opp", "isB2BSecond_Opp", "locationGame_Opp", "countDaysRestTeam_Opp",
               "minutes", "pts", "ast",
               "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", 
               "fg2m", "fg2a", "pctFG2", "ftm", "fta", "treb", "oreb", "dreb", "stl", "blk", "tov", "pf", "plusminus",
               "fgContested", "fg2Contested", "fg3Contested", "boxOutsPlayerTeamRebound", 
               "screenAssist", "ptsScreenAssist", "deflections", "chargesDrawn", 
               "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
               "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts", 
               "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT", "AST_TOV_RATIO", "EST_USG_PCT",
               "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp", "pctFG3Opp", 
               "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp", "minutesOpp", "ftmOpp", 
               "ftaOpp", "orebOpp", "drebOpp", "trebOpp", "astOpp", "stlOpp", 
               "blkOpp", "tovOpp", "pfOpp", "ptsOpp", "plusminusOpp", "possessionsOpp", 
               "paceOpp", "efgPctOpp", "defRatingOpp", "exp_min", "exp_min_q70",
               "exp_pts_q70", "exp_pts", "exp_treb", "exp_treb_q70", "exp_ast_q70", "exp_ast", "pts_reb_ast", "pts_ast", "pts_treb"
)

df_treb_ast <- dati_com %>%
  select(-all_of(c(escludere, "row_id_master", "split_temporale")))

# Suddivisione cronologica: 70% train, 15% validation, 15% test.
idx_train <- which(dati_com$split_temporale == "train" & dati_com$eligible_treb_ast_model)
idx_val <- which(dati_com$split_temporale == "validation" & dati_com$eligible_treb_ast_model)
idx_test <- which(dati_com$split_temporale == "test" & dati_com$eligible_treb_ast_model)

train_set_treb_ast <- df_treb_ast[idx_train,]
val_set_treb_ast <- df_treb_ast[idx_val,]
test_set_treb_ast <- df_treb_ast[idx_test,]

#Triage per selezionare le migliori features
set.seed(123)
rf_quick_treb_ast <- ranger(
  formula                   = treb_ast ~ .,
  data                      = train_set_treb_ast,
  num.trees                 = 200,
  importance                = "permutation",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE
)

top_vars_treb_ast <- importance(rf_quick_treb_ast) %>%
  enframe(name="Variabile", value="Importanza") %>%
  arrange(desc(Importanza)) %>%
  slice_head(n=40) %>%
  pull(Variabile)

top_vars_treb_ast <- c(
  "treb_ast_L10", "treb_ast_season_avg", "treb_ast_L5", "exp_treb_q30",
  "dreb_season_avg", "pts_reb_ast_L10", "pts_reb_ast_L5", "treb_L10",
  "treb_season_avg", "pts_reb_ast_season_avg", "exp_ast_q30", "ast_L10",
  "dreb_L10", "ast_season_avg", "pts_reb_ast_L3", "fg2m_season_avg",
  "treb_ast_L3", "treb_L5", "pts_treb_L10", "pts_treb_season_avg",
  "ast_L5", "fg2a_season_avg", "pts_treb_L5", "pts_ast_season_avg",
  "tov_season_avg", "fg2a_L10", "pts_ast_L10", "fg2a_L3",
  "dreb_L3", "fg2a_L5", "treb_ast_H2H_L3", "pts_ast_L5",
  "pts_season_avg", "fga_season_avg", "fg2m_L3", "dreb_L5",
  "ast_L3", "exp_min_q30", "tov_L10", "pts_L10"
)

df_treb_ast1 <- df_treb_ast %>%
  select(treb_ast, all_of(top_vars_treb_ast)) %>%
  mutate(across(where(is.character), as.factor))

train_set_treb_ast1 <- df_treb_ast1[idx_train, ]
val_set_treb_ast1   <- df_treb_ast1[idx_val, ]
test_set_treb_ast1  <- df_treb_ast1[idx_test, ]

p <- ncol(train_set_treb_ast1) - 1
hyper_grid_treb_ast <- expand.grid(
  mtry            = c(15, 20, 25, 30), 
  min.node.size   = c(300, 350, 400, 500),
  sample.fraction = c(0.4, 0.5),        
  num.trees       = 500,           
  mae  = NA,
  rmse = NA
)

set.seed(123)
for (i in 1:nrow(hyper_grid_treb_ast)) {
  modello_temp <- ranger(
    formula                   = treb_ast ~ .,
    data                      = train_set_treb_ast1,
    num.trees                 = hyper_grid_treb_ast$num.trees[i],
    mtry                      = hyper_grid_treb_ast$mtry[i],
    min.node.size             = hyper_grid_treb_ast$min.node.size[i],
    sample.fraction           = hyper_grid_treb_ast$sample.fraction[i],
    respect.unordered.factors = "order",
    seed                      = 123,
    verbose                   = FALSE,
    num.threads               = numero_thread_ranger()
  )
  pred_val <- predict(modello_temp, data = val_set_treb_ast1)$predictions
  hyper_grid_treb_ast$mae[i]  <- mean(abs(pred_val - val_set_treb_ast1$treb_ast), na.rm = TRUE)
  hyper_grid_treb_ast$rmse[i] <- sqrt(mean((pred_val - val_set_treb_ast1$treb_ast)^2, na.rm = TRUE))
}

hyper_grid_treb_ast  <- hyper_grid_treb_ast %>% arrange(mae) %>% head(10)
best_params_treb_ast <- hyper_grid_treb_ast[1, ]
#Migliore: 15-350-.4-500-2.721-3.488

#Validation
rf_treb_ast_ott <- ranger(
  formula                   = treb_ast ~ .,
  data                      = train_set_treb_ast1,
  num.trees                 = best_params_treb_ast$num.trees,
  mtry                      = best_params_treb_ast$mtry,
  min.node.size             = best_params_treb_ast$min.node.size,
  sample.fraction           = best_params_treb_ast$sample.fraction,
  importance                = "impurity",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE,
  num.threads               = numero_thread_ranger()
)

#validation
break_fasce_treb_ast <- c(-Inf, 6, 12, Inf)
nomi_fasce_treb_ast <- c("Low (<6 RA)", "Mid (6-12 RA)", "High (>12 RA)")
z_30_treb_ast <- abs(qnorm(.3))*1.13

val_set_treb_ast1 <- val_set_treb_ast1 %>%
  select(-any_of(c("exp_treb_ast_raw", "fascia_treb_ast", "bias_medio_treb_ast", "rmse_treb_ast",
                   "exp_treb_ast", "exp_treb_ast_sd", "exp_treb_ast_q30", "exp_treb_ast_q70"))) %>%
  mutate(
    exp_treb_ast_raw = predict(rf_treb_ast_ott, data = val_set_treb_ast1)$predictions,
    fascia_treb_ast = cut(exp_treb_ast_raw, breaks = break_fasce_treb_ast, labels = nomi_fasce_treb_ast, include.lowest = TRUE)
  )

calibrazione_fasce_treb_ast <- val_set_treb_ast1 %>%
  group_by(fascia_treb_ast) %>%
  summarise(
    bias_medio_treb_ast = mean(exp_treb_ast_raw - treb_ast, na.rm = TRUE),
    rmse_treb_ast       = sqrt(mean((exp_treb_ast_raw - treb_ast)^2, na.rm = TRUE)),
    .groups            = "drop"
  )

calibrazione_fasce_treb_ast

val_set_treb_ast1 <- val_set_treb_ast1 %>%
  left_join(calibrazione_fasce_treb_ast, by = "fascia_treb_ast") %>%
  mutate(
    exp_treb_ast = exp_treb_ast_raw - bias_medio_treb_ast, # Applicazione Correzione Bias
    exp_treb_ast_sd = rmse_treb_ast,
    exp_treb_ast_q70 = exp_treb_ast + z_30_treb_ast * exp_treb_ast_sd
  )

calibrazione_q30_fasce_treb_ast <- calibra_q30_per_fascia(
  val_set_treb_ast1, "treb_ast", "exp_treb_ast", "fascia_treb_ast"
)
print(calibrazione_q30_fasce_treb_ast)
calibrazione_fasce_treb_ast <- calibrazione_fasce_treb_ast %>%
  left_join(calibrazione_q30_fasce_treb_ast %>% select(fascia, correzione_q30),
            by = c("fascia_treb_ast" = "fascia"))
val_set_treb_ast1 <- val_set_treb_ast1 %>%
  left_join(calibrazione_q30_fasce_treb_ast %>% select(fascia, correzione_q30),
            by = c("fascia_treb_ast" = "fascia")) %>%
  mutate(
    exp_treb_ast_q30 = pmax(0, exp_treb_ast + correzione_q30),
    delta_treb_ast_q30 = treb_ast - exp_treb_ast_q30,
    hit_q30 = delta_treb_ast_q30 >= 0
  )

# Report Errori Validation: Grezzo vs Corretto
res_val_treb_ast_raw <- val_err(val_set_treb_ast1, "treb_ast", "exp_treb_ast_raw", "Validation Grezzo TREB+AST")
res_val_treb_ast_cal <- val_err(val_set_treb_ast1, "treb_ast", "exp_treb_ast", "Validation Corretto TREB+AST")
metriche_validation_treb_ast <- metriche_previsione(val_set_treb_ast1, "treb_ast", "exp_treb_ast")
print(metriche_validation_treb_ast)

report_q30_treb_ast <- val_set_treb_ast1 %>%
  group_by(fascia_treb_ast) %>%
  summarise(
    N_Osservazioni = n(),
    MAE_Rimbalzi_Assist = mean(abs(treb_ast-exp_treb_ast), na.rm=T),
    RMSE_Rimbalzi_Assist = sqrt(mean((treb_ast-exp_treb_ast)^2, na.rm=T)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm=T)*100,
    .groups = "drop"
  )

print(report_q30_treb_ast)
cat(sprintf("\nHit Rate Globale Q30 Rimbalzi + Assist: %.2f%%\n", mean(val_set_treb_ast1$hit_q30, na.rm = TRUE) * 100))
calibrazione_q30_treb_ast_validation_per_fascia <- curva_calibrazione_q30_per_fascia(
  val_set_treb_ast1, "treb_ast", "exp_treb_ast_q30", "fascia_treb_ast", "TREB+AST — Validation"
)

test_set_treb_ast1 <- test_set_treb_ast1 %>%
  mutate(exp_treb_ast_raw = predict(rf_treb_ast_ott, data = test_set_treb_ast1)$predictions,
         fascia_treb_ast = cut(exp_treb_ast_raw, breaks = break_fasce_treb_ast, labels = nomi_fasce_treb_ast, include.lowest = TRUE)) %>%
  left_join(calibrazione_fasce_treb_ast, by = "fascia_treb_ast") %>%
  mutate(exp_treb_ast = exp_treb_ast_raw - bias_medio_treb_ast,
         exp_treb_ast_sd = rmse_treb_ast,
         exp_treb_ast_q30 = pmax(0, exp_treb_ast + correzione_q30),
         exp_treb_ast_q70 = exp_treb_ast + z_30_treb_ast * exp_treb_ast_sd,
         delta_treb_ast_q30 = treb_ast - exp_treb_ast_q30,
         hit_q30 = delta_treb_ast_q30 >= 0)
res_test_treb_ast <- val_err(test_set_treb_ast1, "treb_ast", "exp_treb_ast", "Test TREB+AST")
metriche_test_treb_ast <- metriche_previsione(test_set_treb_ast1, "treb_ast", "exp_treb_ast") %>%
  mutate(Hit_Rate_Q30_pct = mean(test_set_treb_ast1$hit_q30, na.rm = TRUE) * 100)
print(metriche_test_treb_ast)
cat(sprintf("\nHit Rate Q30 sul test set Rimbalzi + Assist: %.2f%% (obiettivo: circa 70%%)\n", metriche_test_treb_ast$Hit_Rate_Q30_pct))
calibrazione_q30_treb_ast_test_per_fascia <- curva_calibrazione_q30_per_fascia(
  test_set_treb_ast1, "treb_ast", "exp_treb_ast_q30", "fascia_treb_ast", "TREB+AST — Test"
)

# OOF temporali expanding-window.
oof_treb_ast <- genera_oof_temporali(train_set_treb_ast1, dati_com$dateGame[idx_train],
  treb_ast ~ ., best_params_treb_ast, "treb_ast", break_fasce_treb_ast, nomi_fasce_treb_ast)
oof_pred_treb_ast_raw <- oof_treb_ast$raw
oof_pred_treb_ast_q30 <- oof_treb_ast$q30
log_oof_treb_ast <- oof_treb_ast$log
print(log_oof_treb_ast)

train_set_treb_ast1_oof <- train_set_treb_ast1 %>%
  select(-any_of(c("exp_treb_ast_raw", "fascia_treb_ast", "rmse_treb_ast", "exp_treb_ast", 
                   "exp_treb_ast_sd", "exp_treb_ast_q30", "exp_treb_ast_q70", "bias_medio_treb_ast"))) %>%
  mutate(
    row_id_master    = dati_com$row_id_master[idx_train],
    exp_treb_ast_raw = oof_pred_treb_ast_raw,
    fascia_treb_ast  = cut(exp_treb_ast_raw, breaks = break_fasce_treb_ast, labels = nomi_fasce_treb_ast, include.lowest = TRUE)
  ) %>%
  mutate(exp_treb_ast = exp_treb_ast_raw, exp_treb_ast_sd = NA_real_,
         exp_treb_ast_q30 = oof_pred_treb_ast_q30, exp_treb_ast_q70 = NA_real_)

calibrazione_q30_treb_ast_train_per_fascia <- curva_calibrazione_q30_per_fascia(
  train_set_treb_ast1_oof, "treb_ast", "exp_treb_ast_q30", "fascia_treb_ast", "TREB+AST — Train OOF"
)

val_set_treb_ast1$row_id_master <- dati_com$row_id_master[idx_val]
test_set_treb_ast1$row_id_master <- dati_com$row_id_master[idx_test]

colonne_treb_ast <- c("row_id_master", "exp_treb_ast", "exp_treb_ast_sd", "exp_treb_ast_q30", "exp_treb_ast_q70")

pred_treb_ast <- bind_rows(
  train_set_treb_ast1_oof %>% select(all_of(colonne_treb_ast)),
  val_set_treb_ast1       %>% select(all_of(colonne_treb_ast)),
  test_set_treb_ast1      %>% select(all_of(colonne_treb_ast))
)

print(analizza_sottostime_hit_q30(test_set_treb_ast1, "treb_ast", "exp_treb_ast_q30", "fascia_treb_ast", "TREB+AST"))

dati_com2 <- dati_com2 %>%
  select(-any_of(c("exp_treb_ast", "exp_treb_ast_sd", "exp_treb_ast_q30", "exp_treb_ast_q70"))) %>%
  left_join(pred_treb_ast, by = "row_id_master") %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)

# Il modello Punti + Rimbalzi + Assist (PRA) e' stato ritirato: bloccava il
# train e non fa parte delle sette stime del bot. Il blocco storico sottostante
# resta escluso dall'esecuzione finche' non verra' eliminato definitivamente
# in una pulizia meccanica del file esplorativo.
if (FALSE) {
escludere <- c("row_id",
               "yearSeason", "dateGame", "idGame", "slugTeam", "slugOpponent", 
               "namePlayer", "idPlayer", "nameTeam", "idTeam", "isWin",
               "numberGameTeamSeason_Opp", "isB2BSecond_Opp", "locationGame_Opp", "countDaysRestTeam_Opp",
               "minutes", "pts", "ast",
               "fgm", "fga", "pctFG", "fg3m", "fg3a", "pctFG3", "pctFT", 
               "fg2m", "fg2a", "pctFG2", "ftm", "fta", "treb", "oreb", "dreb", "stl", "blk", "tov", "pf", "plusminus",
               "fgContested", "fg2Contested", "fg3Contested", "boxOutsPlayerTeamRebound", 
               "screenAssist", "ptsScreenAssist", "deflections", "chargesDrawn", 
               "looseBallsRecoveredOffense", "looseBallsRecoveredDefense", "looseBallsRecovered", 
               "boxOutsOffense", "boxOutsDefense", "boxOutsPlayerTREB", "boxOuts", 
               "TS_PCT", "EFG_PCT", "PCT_FGA_3PT", "PCT_PTS_3PT", "AST_TOV_RATIO", "EST_USG_PCT",
               "fgmOpp", "fgaOpp", "pctFGOpp", "fg3mOpp", "fg3aOpp", "pctFG3Opp", 
               "pctFTOpp", "fg2mOpp", "fg2aOpp", "pctFG2Opp", "minutesOpp", "ftmOpp", 
               "ftaOpp", "orebOpp", "drebOpp", "trebOpp", "astOpp", "stlOpp", 
               "blkOpp", "tovOpp", "pfOpp", "ptsOpp", "plusminusOpp", "possessionsOpp", 
               "paceOpp", "efgPctOpp", "defRatingOpp", "treb_ast", "pts_ast", "pts_treb",
               "exp_min", "exp_min_q70", "exp_pts", "exp_pts_q70", "exp_treb", "exp_treb_q70", "exp_ast", "exp_ast_q70",
               "exp_pts_treb", "exp_pts_treb_q70", "exp_pts_ast", "exp_pts_ast_q70", "exp_treb_ast", "exp_treb_ast_q70"
)

df_pts_reb_ast <- dati_com2 %>%
  select(-all_of(c(escludere, "row_id_master", "split_temporale")))

# Suddivisione cronologica: 70% train, 15% validation, 15% test.
idx_train <- which(dati_com2$split_temporale == "train" & dati_com2$eligible_pra_model)
idx_val <- which(dati_com2$split_temporale == "validation" & dati_com2$eligible_pra_model)
idx_test <- which(dati_com2$split_temporale == "test" & dati_com2$eligible_pra_model)

train_set_pts_reb_ast <- df_pts_reb_ast[idx_train,]
val_set_pts_reb_ast <- df_pts_reb_ast[idx_val,]
test_set_pts_reb_ast <- df_pts_reb_ast[idx_test,]

#Triage per selezionare le migliori features
set.seed(123)
rf_quick_pts_reb_ast <- ranger(
  formula                   = pts_reb_ast ~ .,
  data                      = train_set_pts_reb_ast,
  num.trees                 = 200,
  importance                = "permutation",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE
)

top_vars_pts_reb_ast <- importance(rf_quick_pts_reb_ast) %>%
  enframe(name="Variabile", value="Importanza") %>%
  arrange(desc(Importanza)) %>%
  slice_head(n=40) %>%
  pull(Variabile)

top_vars_pts_reb_ast <- c(
  "exp_treb_ast_q30", "exp_pts_treb_q30", "exp_pts_ast_q30",
  "pts_reb_ast_L10", "pts_ast_L10", "pts_reb_ast_season_avg",
  "pts_treb_season_avg", "exp_pts_q30", "pts_treb_L10",
  "pts_ast_season_avg", "pts_ast_L5", "pts_treb_L5",
  "fgm_season_avg", "pts_reb_ast_L5", "pts_season_avg",
  "fgm_L10", "fga_L10", "pts_L10",
  "pts_reb_ast_L3", "treb_ast_season_avg", "fga_season_avg",
  "fg2a_L10", "fg2a_season_avg", "fg2m_season_avg",
  "fga_L5", "fg2m_L5", "fg2a_L5",
  "exp_min_q30", "ftm_season_avg", "pts_ast_L3",
  "fg2m_L10", "treb_ast_L10", "fga_L3",
  "pts_L5", "treb_ast_L5", "pts_L3",
  "EST_USG_PCT_L3", "fg2a_L3", "fta_season_avg",
  "EST_USG_PCT_season_avg"
)

df_pts_reb_ast1 <- df_pts_reb_ast %>%
  select(pts_reb_ast, all_of(top_vars_pts_reb_ast)) %>%
  mutate(across(where(is.character), as.factor))

train_set_pts_reb_ast1 <- df_pts_reb_ast1[idx_train, ]
val_set_pts_reb_ast1   <- df_pts_reb_ast1[idx_val, ]
test_set_pts_reb_ast1  <- df_pts_reb_ast1[idx_test, ]

p <- ncol(train_set_pts_reb_ast1) - 1
hyper_grid_pts_reb_ast <- expand.grid(
  mtry            = c(20, 30), 
  min.node.size   = c(200, 300),
  sample.fraction = c(0.4, 0.5),        
  num.trees       = 500,           
  mae  = NA,
  rmse = NA
)

set.seed(123)
for (i in 1:nrow(hyper_grid_pts_reb_ast)) {
  modello_temp <- ranger(
    formula                   = pts_reb_ast ~ .,
    data                      = train_set_pts_reb_ast1,
    num.trees                 = hyper_grid_pts_reb_ast$num.trees[i],
    mtry                      = hyper_grid_pts_reb_ast$mtry[i],
    min.node.size             = hyper_grid_pts_reb_ast$min.node.size[i],
    sample.fraction           = hyper_grid_pts_reb_ast$sample.fraction[i],
    respect.unordered.factors = "order",
    seed                      = 123,
    verbose                   = FALSE,
    num.threads               = numero_thread_ranger()
  )
  pred_val <- predict(modello_temp, data = val_set_pts_reb_ast1)$predictions
  hyper_grid_pts_reb_ast$mae[i]  <- mean(abs(pred_val - val_set_pts_reb_ast1$pts_reb_ast), na.rm = TRUE)
  hyper_grid_pts_reb_ast$rmse[i] <- sqrt(mean((pred_val - val_set_pts_reb_ast1$pts_reb_ast)^2, na.rm = TRUE))
}

hyper_grid_pts_reb_ast  <- hyper_grid_pts_reb_ast %>% arrange(mae) %>% head(10)
best_params_pts_reb_ast <- hyper_grid_pts_reb_ast[1, ]
#Migliore: 30-300-.5-500-6.351-8.108

#Validation
rf_pts_reb_ast_ott <- ranger(
  formula                   = pts_reb_ast ~ .,
  data                      = train_set_pts_reb_ast1,
  num.trees                 = best_params_pts_reb_ast$num.trees,
  mtry                      = best_params_pts_reb_ast$mtry,
  min.node.size             = best_params_pts_reb_ast$min.node.size,
  sample.fraction           = best_params_pts_reb_ast$sample.fraction,
  importance                = "impurity",
  respect.unordered.factors = "order",
  seed                      = 123,
  verbose                   = FALSE,
  num.threads               = numero_thread_ranger()
)

#validation
break_fasce_pts_reb_ast <- c(-Inf, 20, 35, Inf)
nomi_fasce_pts_reb_ast  <- c("Low (<20 PRA)", "Mid (20-35 PRA)", "High (>35 PRA)")
z_30_pts_reb_ast        <- abs(qnorm(.3))*1.08

val_set_pts_reb_ast1 <- val_set_pts_reb_ast1 %>%
  select(-any_of(c("exp_pts_reb_ast_raw", "fascia_pts_reb_ast", "bias_medio_pts_reb_ast", "rmse_pts_reb_ast",
                   "exp_pts_reb_ast", "exp_pts_reb_ast_sd", "exp_pts_reb_ast_q30", "exp_pts_reb_ast_q70"))) %>%
  mutate(
    exp_pts_reb_ast_raw = predict(rf_pts_reb_ast_ott, data = val_set_pts_reb_ast1)$predictions,
    fascia_pts_reb_ast = cut(exp_pts_reb_ast_raw, breaks = break_fasce_pts_reb_ast, labels = nomi_fasce_pts_reb_ast, include.lowest = TRUE)
  )

calibrazione_fasce_pts_reb_ast <- val_set_pts_reb_ast1 %>%
  group_by(fascia_pts_reb_ast) %>%
  summarise(
    bias_medio_pts_reb_ast = mean(exp_pts_reb_ast_raw - pts_reb_ast, na.rm = TRUE),
    rmse_pts_reb_ast       = sqrt(mean((exp_pts_reb_ast_raw - pts_reb_ast)^2, na.rm = TRUE)),
    .groups            = "drop"
  )

calibrazione_fasce_pts_reb_ast

val_set_pts_reb_ast1 <- val_set_pts_reb_ast1 %>%
  left_join(calibrazione_fasce_pts_reb_ast, by = "fascia_pts_reb_ast") %>%
  mutate(
    exp_pts_reb_ast = exp_pts_reb_ast_raw - bias_medio_pts_reb_ast, # Applicazione Correzione Bias
    exp_pts_reb_ast_sd = rmse_pts_reb_ast,
    exp_pts_reb_ast_q70 = exp_pts_reb_ast + z_30_pts_reb_ast * exp_pts_reb_ast_sd
  )

calibrazione_q30_fasce_pts_reb_ast <- calibra_q30_per_fascia(
  val_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast", "fascia_pts_reb_ast"
)
print(calibrazione_q30_fasce_pts_reb_ast)
calibrazione_fasce_pts_reb_ast <- calibrazione_fasce_pts_reb_ast %>%
  left_join(calibrazione_q30_fasce_pts_reb_ast %>% select(fascia, correzione_q30),
            by = c("fascia_pts_reb_ast" = "fascia"))
val_set_pts_reb_ast1 <- val_set_pts_reb_ast1 %>%
  left_join(calibrazione_q30_fasce_pts_reb_ast %>% select(fascia, correzione_q30),
            by = c("fascia_pts_reb_ast" = "fascia")) %>%
  mutate(
    exp_pts_reb_ast_q30 = pmax(0, exp_pts_reb_ast + correzione_q30),
    delta_pts_reb_ast_q30 = pts_reb_ast - exp_pts_reb_ast_q30,
    hit_q30 = delta_pts_reb_ast_q30 >= 0
  )

# Report Errori Validation: Grezzo vs Corretto
res_val_pts_reb_ast_raw <- val_err(val_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast_raw", "Validation Grezzo PTS+TREB+AST")
res_val_pts_reb_ast_cal <- val_err(val_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast", "Validation Corretto PTS+TREB+AST")
metriche_validation_pts_reb_ast <- metriche_previsione(val_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast")
print(metriche_validation_pts_reb_ast)

report_q30_pts_reb_ast <- val_set_pts_reb_ast1 %>%
  group_by(fascia_pts_reb_ast) %>%
  summarise(
    N_Osservazioni = n(),
    MAE_Punti_Rimbalzi_Assist = mean(abs(pts_reb_ast-exp_pts_reb_ast), na.rm=T),
    RMSE_Punti_Rimbalzi_Assist = sqrt(mean((pts_reb_ast-exp_pts_reb_ast)^2, na.rm=T)),
    Hit_Rate_Q30_pct = mean(hit_q30, na.rm=T)*100,
    .groups = "drop"
  )

print(report_q30_pts_reb_ast)
cat(sprintf("\nHit Rate Globale Q30 Punti + Rimbalzi + Assist: %.2f%%\n", mean(val_set_pts_reb_ast1$hit_q30, na.rm = TRUE) * 100))
calibrazione_q30_pts_reb_ast_validation_per_fascia <- curva_calibrazione_q30_per_fascia(
  val_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast_q30", "fascia_pts_reb_ast", "PTS+TREB+AST — Validation"
)

test_set_pts_reb_ast1 <- test_set_pts_reb_ast1 %>%
  mutate(exp_pts_reb_ast_raw = predict(rf_pts_reb_ast_ott, data = test_set_pts_reb_ast1)$predictions,
         fascia_pts_reb_ast = cut(exp_pts_reb_ast_raw, breaks = break_fasce_pts_reb_ast, labels = nomi_fasce_pts_reb_ast, include.lowest = TRUE)) %>%
  left_join(calibrazione_fasce_pts_reb_ast, by = "fascia_pts_reb_ast") %>%
  mutate(exp_pts_reb_ast = exp_pts_reb_ast_raw - bias_medio_pts_reb_ast,
         exp_pts_reb_ast_sd = rmse_pts_reb_ast,
         exp_pts_reb_ast_q30 = pmax(0, exp_pts_reb_ast + correzione_q30),
         exp_pts_reb_ast_q70 = exp_pts_reb_ast + z_30_pts_reb_ast * exp_pts_reb_ast_sd,
         delta_pts_reb_ast_q30 = pts_reb_ast - exp_pts_reb_ast_q30,
         hit_q30 = delta_pts_reb_ast_q30 >= 0)
res_test_pts_reb_ast <- val_err(test_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast", "Test PTS+TREB+AST")
metriche_test_pts_reb_ast <- metriche_previsione(test_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast") %>%
  mutate(Hit_Rate_Q30_pct = mean(test_set_pts_reb_ast1$hit_q30, na.rm = TRUE) * 100)
print(metriche_test_pts_reb_ast)
cat(sprintf("\nHit Rate Q30 sul test set Punti + Rimbalzi + Assist: %.2f%% (obiettivo: circa 70%%)\n", metriche_test_pts_reb_ast$Hit_Rate_Q30_pct))
calibrazione_q30_pts_reb_ast_test_per_fascia <- curva_calibrazione_q30_per_fascia(
  test_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast_q30", "fascia_pts_reb_ast", "PTS+TREB+AST — Test"
)

# OOF temporali expanding-window.
oof_pts_reb_ast <- genera_oof_temporali(train_set_pts_reb_ast1, dati_com2$dateGame[idx_train],
  pts_reb_ast ~ ., best_params_pts_reb_ast, "pts_reb_ast", break_fasce_pts_reb_ast, nomi_fasce_pts_reb_ast)
oof_pred_pts_reb_ast_raw <- oof_pts_reb_ast$raw
oof_pred_pts_reb_ast_q30 <- oof_pts_reb_ast$q30
log_oof_pts_reb_ast <- oof_pts_reb_ast$log
print(log_oof_pts_reb_ast)

train_set_pts_reb_ast1_oof <- train_set_pts_reb_ast1 %>%
  select(-any_of(c("exp_pts_reb_ast_raw", "fascia_pts_reb_ast", "rmse_pts_reb_ast", "exp_pts_reb_ast", 
                   "exp_pts_reb_ast_sd", "exp_pts_reb_ast_q30", "exp_pts_reb_ast_q70", "bias_medio_pts_reb_ast"))) %>%
  mutate(
    row_id_master    = dati_com2$row_id_master[idx_train],
    exp_pts_reb_ast_raw = oof_pred_pts_reb_ast_raw,
    fascia_pts_reb_ast  = cut(exp_pts_reb_ast_raw, breaks = break_fasce_pts_reb_ast, labels = nomi_fasce_pts_reb_ast, include.lowest = TRUE)
  ) %>%
  mutate(exp_pts_reb_ast = exp_pts_reb_ast_raw, exp_pts_reb_ast_sd = NA_real_,
         exp_pts_reb_ast_q30 = oof_pred_pts_reb_ast_q30, exp_pts_reb_ast_q70 = NA_real_)

calibrazione_q30_pts_reb_ast_train_per_fascia <- curva_calibrazione_q30_per_fascia(
  train_set_pts_reb_ast1_oof, "pts_reb_ast", "exp_pts_reb_ast_q30", "fascia_pts_reb_ast", "PTS+TREB+AST — Train OOF"
)

val_set_pts_reb_ast1$row_id_master <- dati_com2$row_id_master[idx_val]
test_set_pts_reb_ast1$row_id_master <- dati_com2$row_id_master[idx_test]

colonne_pts_reb_ast <- c("row_id_master", "exp_pts_reb_ast", "exp_pts_reb_ast_sd", "exp_pts_reb_ast_q30", "exp_pts_reb_ast_q70")

pred_pts_reb_ast <- bind_rows(
  train_set_pts_reb_ast1_oof %>% select(all_of(colonne_pts_reb_ast)),
  val_set_pts_reb_ast1       %>% select(all_of(colonne_pts_reb_ast)),
  test_set_pts_reb_ast1      %>% select(all_of(colonne_pts_reb_ast))
)

print(analizza_sottostime_hit_q30(val_set_pts_reb_ast1, "pts_reb_ast", "exp_pts_reb_ast_q30", "fascia_pts_reb_ast", "PTS+TREB+AST"))

dati_com2 <- dati_com2 %>%
  select(-any_of(c("exp_pts_reb_ast", "exp_pts_reb_ast_sd", "exp_pts_reb_ast_q30", "exp_pts_reb_ast_q70"))) %>%
  left_join(pred_pts_reb_ast, by = "row_id_master") %>%
  arrange(dateGame, idGame, idPlayer, row_id_master)
}

# Audit finale: disponibilita' meta-feature e righe realmente ammesse per
# ciascun modello e per ogni split esterno.
tabella_eligibilita_modelli <- bind_rows(
  riepilogo_eligibilita(dati_modello, "EXP_MIN"),
  riepilogo_eligibilita(dati_com1, "EXP_PTS", "eligible_pts_model"),
  riepilogo_eligibilita(dati_com1, "EXP_TREB", "eligible_treb_model"),
  riepilogo_eligibilita(dati_com1, "EXP_AST", "eligible_ast_model"),
  riepilogo_eligibilita(dati_com, "EXP_PTS_AST", "eligible_pts_ast_model"),
  riepilogo_eligibilita(dati_com, "EXP_PTS_TREB", "eligible_pts_treb_model"),
  riepilogo_eligibilita(dati_com, "EXP_TREB_AST", "eligible_treb_ast_model")
)
print(tabella_eligibilita_modelli)

# Pubblicazione dei sette modelli effettivamente letti dal bot. Ogni artefatto
# conserva le stesse fasce, il bias (per le combinazioni), RMSE e Q30 ottenuti
# sul validation set nello script corrente.
source(file.path(getwd(), "r", "esporta_modelli_calibrazione.R"))
configurazioni_bot <- list(
  minuti = crea_artefatto_modello_calibrato(rf_minutes_ott, "minutes", top_vars, break_fasce_min, nomi_fasce_min, calibrazione_fasce_min, "fascia_min", "rmse_min"),
  punti = crea_artefatto_modello_calibrato(rf_pts_ott, "pts", top_vars_pts, break_fasce_pts, nomi_fasce_pts, calibrazione_fasce_pts, "fascia_pts", "rmse_pts"),
  rimbalzi = crea_artefatto_modello_calibrato(rf_treb_ott, "treb", top_vars_treb, break_fasce_treb, nomi_fasce_treb, calibrazione_fasce_treb, "fascia_treb", "rmse_treb"),
  assist = crea_artefatto_modello_calibrato(rf_ast_ott, "ast", top_vars_ast, break_fasce_ast, nomi_fasce_ast, calibrazione_fasce_ast, "fascia_ast", "rmse_ast"),
  punti_assist = crea_artefatto_modello_calibrato(rf_pts_ast_ott, "pts_ast", top_vars_pts_ast, break_fasce_pts_ast, nomi_fasce_pts_ast, calibrazione_fasce_pts_ast, "fascia_pts_ast", "rmse_pts_ast", "bias_medio_pts_ast"),
  punti_rimbalzi = crea_artefatto_modello_calibrato(rf_pts_treb_ott, "pts_treb", top_vars_pts_treb, break_fasce_pts_treb, nomi_fasce_pts_treb, calibrazione_fasce_pts_treb, "fascia_pts_treb", "rmse_pts_treb", "bias_medio_pts_treb"),
  rimbalzi_assist = crea_artefatto_modello_calibrato(rf_treb_ast_ott, "treb_ast", top_vars_treb_ast, break_fasce_treb_ast, nomi_fasce_treb_ast, calibrazione_fasce_treb_ast, "fascia_treb_ast", "rmse_treb_ast", "bias_medio_treb_ast")
)
data_modelli <- Sys.getenv("NBA_DATA_MODELLI", as.character(Sys.Date()))
percorsi_modelli_bot <- pubblica_modelli_calibrazione(configurazioni_bot, as.Date(data_modelli), file.path(getwd(), "models"))
cat(sprintf("Modelli calibrati pubblicati: %s\n", paste(percorsi_modelli_bot, collapse = "; ")))
