# Orchestrazione sequenziale dei modelli. Non modifica RDS sorgenti.
sequenza_target_nba <- function() c("minuti","punti","rimbalzi","assist","punti_assist","punti_rimbalzi","rimbalzi_assist")
mappa_target_nba <- function() c(minuti="minutes",punti="pts",rimbalzi="treb",assist="ast",punti_assist="pts_ast",punti_rimbalzi="pts_treb",rimbalzi_assist="treb_ast")
dipendenze_meta_nba <- function() list(minuti=character(),punti=c("exp_min_q30","exp_min_sd"),rimbalzi=c("exp_min_q30"),assist=c("exp_min_q30"),punti_assist=c("exp_pts_q30","exp_min_q30","exp_ast_q30"),punti_rimbalzi=c("exp_pts_q30","exp_min_q30","exp_treb_q30"),rimbalzi_assist=c("exp_treb_q30","exp_ast_q30"))
configurazione_fasce_q30_nba <- function(target) {
  configurazioni <- list(
    minutes = list(breaks = c(-Inf, 15, 28, Inf), labels = c("Bench (<15m)", "Rotation (15-28m)", "Starters (>28m)")),
    pts = list(breaks = c(-Inf, 10, 20, Inf), labels = c("Low (<10pts)", "Mid (10-20pts)", "High (>20pts)")),
    treb = list(breaks = c(-Inf, 4, 8, Inf), labels = c("Low (<4reb)", "Mid (4-8reb)", "High (>8reb)")),
    ast = list(breaks = c(-Inf, 3, 6, Inf), labels = c("Low (<3ast)", "Mid (3-6ast)", "High (>6ast)")),
    pts_ast = list(breaks = c(-Inf, 15, 30, Inf), labels = c("Low (<15 PA)", "Mid (15-30 PA)", "High (>30 PA)")),
    pts_treb = list(breaks = c(-Inf, 15, 30, Inf), labels = c("Low (<15 PT)", "Mid (15-30 PT)", "High (>30 PT)")),
    treb_ast = list(breaks = c(-Inf, 6, 12, Inf), labels = c("Low (<6 RA)", "Mid (6-12 RA)", "High (>12 RA)"))
  )
  if (!target %in% names(configurazioni)) stop(sprintf("Fasce Q30 non definite per %s.", target), call. = FALSE)
  configurazioni[[target]]
}
calibra_q30_per_fasce_nba <- function(reale, predizione_grezza, configurazione) {
  fascia <- cut(predizione_grezza, breaks = configurazione$breaks, labels = configurazione$labels, include.lowest = TRUE)
  residui <- as.numeric(reale) - as.numeric(predizione_grezza)
  globale <- as.numeric(stats::quantile(residui, 0.30, na.rm = TRUE))
  correzioni <- vapply(configurazione$labels, function(etichetta) {
    x <- residui[as.character(fascia) == etichetta]
    if (length(x)) as.numeric(stats::quantile(x, 0.30, na.rm = TRUE)) else globale
  }, numeric(1))
  data.frame(fascia = configurazione$labels, correzione_q30 = correzioni, stringsAsFactors = FALSE)
}
applica_q30_per_fasce_nba <- function(predizione_grezza, calibrazione, configurazione) {
  fascia <- as.character(cut(predizione_grezza, breaks = configurazione$breaks, labels = configurazione$labels, include.lowest = TRUE))
  correzione <- calibrazione$correzione_q30[match(fascia, calibrazione$fascia)]
  if (anyNA(correzione)) stop("Calibrazione Q30 per fascia incompleta.", call. = FALSE)
  pmax(0, as.numeric(predizione_grezza) + correzione)
}
split_cronologico_nba <- function(dati) {
  date <- sort(unique(as.Date(dati$dateGame))); n<-length(date); if(n<3) stop("Servono almeno tre date.",call.=FALSE)
  a<-date[max(1,floor(n*.70))]; b<-date[max(1,floor(n*.85))]
  if(any(dati$dateGame<=a & dati$dateGame>b)) stop("Split non valido.",call.=FALSE)
  dati$split_temporale<-ifelse(as.Date(dati$dateGame)<=a,"train",ifelse(as.Date(dati$dateGame)<=b,"validation","test")); dati
}
valida_sequenza_feature <- function(liste) { if(!identical(names(liste),sequenza_target_nba())) stop("Le liste feature devono seguire la sequenza target NBA.",call.=FALSE); invisible(TRUE) }
verifica_dipendenze_meta <- function(target, colonne) { m<-setdiff(dipendenze_meta_nba()[[target]],colonne); if(length(m)) stop(sprintf("%s richiede meta-feature non disponibili: %s.",target,paste(m,collapse=", ")),call.=FALSE) }
configura_sequenza_modelli <- function(dati,liste_feature) {
  valida_sequenza_feature(liste_feature); dati<-split_cronologico_nba(dati); out<-list()
  for(t in sequenza_target_nba()) { verifica_dipendenze_meta(t,names(dati)); f<-liste_feature[[t]]; if(length(setdiff(f,names(dati)))) stop(sprintf("Feature assenti per %s: %s.",t,paste(setdiff(f,names(dati)),collapse=", ")),call.=FALSE); out[[t]]<-list(target=mappa_target_nba()[[t]],feature=f,dipendenze=dipendenze_meta_nba()[[t]]) }
  list(dati=dati,configurazione=out)
}
genera_oof_temporali_nba <- function(train_data, date_game, target, feature, parametri=list(num.trees=200L,mtry=NULL,min.node.size=5L,sample.fraction=.8), n_folds=6L, seed=123L) {
  if(!requireNamespace("ranger",quietly=TRUE)) stop("Serve ranger.",call.=FALSE)
  date_game<-as.Date(date_game); date<-sort(unique(date_game)); out<-rep(NA_real_,nrow(train_data)); audit<-data.frame(); if(length(date)<3) return(list(predizione=out,audit=audit))
  primo<-max(1L,floor(length(date)*.20)); rest<-date[-seq_len(primo)]; gruppi<-split(rest,cut(seq_along(rest),min(n_folds,length(rest)),labels=FALSE));
  for(i in seq_along(gruppi)) { pred_date<-gruppi[[i]]; train_date<-date[date<min(pred_date)]; it<-which(date_game%in%train_date); ip<-which(date_game%in%pred_date); if(!length(it)||!length(ip)) next; stopifnot(max(date_game[it])<min(date_game[ip])); d<-train_data[,c(target,feature),drop=FALSE]; mtry<-parametri$mtry %||% max(1L,floor(sqrt(length(feature)))); fit<-ranger::ranger(stats::as.formula(paste(target,"~ .")),data=d[it,,drop=FALSE],num.trees=parametri$num.trees,mtry=mtry,min.node.size=parametri$min.node.size,sample.fraction=parametri$sample.fraction,seed=seed,verbose=FALSE); out[ip]<-predict(fit,d[ip,,drop=FALSE])$predictions; audit<-rbind(audit,data.frame(fold=i,data_massima_train=max(date_game[it]),data_minima_oof=min(date_game[ip]),n_train=length(it),n_oof=length(ip))) }
  list(predizione=out,audit=audit)
}
`%||%` <- function(x,y) if(is.null(x)) y else x
verifica_audit_oof <- function(audit) { if(nrow(audit)&&any(audit$data_massima_train>=audit$data_minima_oof)) stop("Leakage temporale OOF rilevato.",call.=FALSE); TRUE }
aggiungi_meta_feature_nba <- function(dati, target, oof_train, pred_validation, pred_test, deviazione_train=NULL, deviazione_validation=NULL, deviazione_test=NULL, indici=NULL) {
  if(!"split_temporale"%in%names(dati)) stop("Split temporale assente.",call.=FALSE)
  if(is.null(indici)) {
    it<-which(dati$split_temporale=="train"); iv<-which(dati$split_temporale=="validation"); ie<-which(dati$split_temporale=="test")
  } else {
    if(!all(c("train","validation","test") %in% names(indici))) stop("Indici di split incompleti.",call.=FALSE)
    it<-indici$train; iv<-indici$validation; ie<-indici$test
    if(anyDuplicated(c(it,iv,ie)) || any(c(it,iv,ie) < 1L | c(it,iv,ie) > nrow(dati))) stop("Indici di split non validi.",call.=FALSE)
    if(any(dati$split_temporale[it]!="train") || any(dati$split_temporale[iv]!="validation") || any(dati$split_temporale[ie]!="test")) stop("Indici incoerenti con gli split temporali.",call.=FALSE)
  }
  if(length(oof_train)!=length(it)||length(pred_validation)!=length(iv)||length(pred_test)!=length(ie)) stop("Lunghezze predizioni incoerenti con gli split.",call.=FALSE)
  if(all(!is.finite(oof_train))) stop("Il train richiede almeno predizioni OOF: non usare predizioni in-sample.",call.=FALSE)
  nome<-paste0("exp_",target,"_q30"); dati[[nome]]<-NA_real_; dati[[nome]][it]<-oof_train; dati[[nome]][iv]<-pred_validation; dati[[nome]][ie]<-pred_test
  if(!is.null(deviazione_train)) { if(length(deviazione_train)!=length(it)||length(deviazione_validation)!=length(iv)||length(deviazione_test)!=length(ie)) stop("Deviazioni incoerenti.",call.=FALSE); sd_nome<-paste0("exp_",target,"_sd"); dati[[sd_nome]]<-NA_real_; dati[[sd_nome]][it]<-deviazione_train; dati[[sd_nome]][iv]<-deviazione_validation; dati[[sd_nome]][ie]<-deviazione_test }
  dati
}
addestra_target_ranger_nba <- function(dati, target, feature, parametri=list(num.trees=500L,mtry=NULL,min.node.size=5L,sample.fraction=.8), seed=123L) {
  if(!requireNamespace("ranger",quietly=TRUE)) stop("Serve ranger.",call.=FALSE)
  if(!all(c("split_temporale","dateGame",target,feature)%in%names(dati))) stop("Colonne target/feature/split mancanti.",call.=FALSE)
  ok<-complete.cases(dati[,c(target,feature),drop=FALSE]); tr<-which(dati$split_temporale=="train"&ok); va<-which(dati$split_temporale=="validation"&ok); te<-which(dati$split_temporale=="test"&ok)
  if(!length(tr)||!length(va)||!length(te)) stop("Split incompleti dopo il filtro feature.",call.=FALSE)
  mtry<-parametri$mtry %||% max(1L,floor(sqrt(length(feature)))); form<-stats::as.formula(paste(target,"~ .")); train<-dati[,c(target,feature),drop=FALSE]
  oof<-genera_oof_temporali_nba(train[tr,,drop=FALSE],dati$dateGame[tr],target,feature,parametri,seed=seed); verifica_audit_oof(oof$audit)
  modello<-ranger::ranger(form,data=train[tr,,drop=FALSE],num.trees=parametri$num.trees,mtry=mtry,min.node.size=parametri$min.node.size,sample.fraction=parametri$sample.fraction,importance="permutation",seed=seed,verbose=FALSE)
  raw_val<-predict(modello,train[va,,drop=FALSE])$predictions; raw_test<-predict(modello,train[te,,drop=FALSE])$predictions
  residui_val<-as.numeric(dati[[target]][va])-raw_val; bias<-mean(residui_val); q30<-as.numeric(stats::quantile(residui_val,.30,na.rm=TRUE)); configurazione_fasce<-configurazione_fasce_q30_nba(target); calibrazione_fasce<-calibra_q30_per_fasce_nba(dati[[target]][va],raw_val,configurazione_fasce)
  list(modello=modello,target=target,feature=feature,oof_raw=oof$predizione,oof_audit=oof$audit,pred_validation=pmax(0,raw_val+bias),pred_test=pmax(0,raw_test+bias),q30_validation=applica_q30_per_fasce_nba(raw_val,calibrazione_fasce,configurazione_fasce),q30_test=applica_q30_per_fasce_nba(raw_test,calibrazione_fasce,configurazione_fasce),bias_validation=bias,correzione_q30_validation=q30,calibrazione_q30_fasce_validation=calibrazione_fasce,configurazione_fasce_q30=configurazione_fasce,deviazione_validation=stats::sd(residui_val),metriche_test=c(MAE=mean(abs((raw_test+bias)-as.numeric(dati[[target]][te]))),RMSE=sqrt(mean(((raw_test+bias)-as.numeric(dati[[target]][te]))^2))),indici=list(train=tr,validation=va,test=te))
}
calibra_meta_oof_nba <- function(reale, raw, date) { q<-sdv<-rep(NA_real_,length(raw)); for(i in order(as.Date(date))) { idx<-which(as.Date(date)<as.Date(date)[i]&is.finite(raw)); if(length(idx)) { r<-reale[idx]-raw[idx]; q[i]<-pmax(0,raw[i]+as.numeric(stats::quantile(r,.30))); sdv[i]<-stats::sd(r) } }; list(q30=q,sd=sdv) }
addestra_sequenza_ranger_nba <- function(dati,liste_feature,parametri=list(num.trees=500L,mtry=NULL,min.node.size=5L,sample.fraction=.8),seed=123L) {
  z<-configura_sequenza_modelli(dati,liste_feature); d<-z$dati; risultati<-list()
  pref<-c(minuti="min",punti="pts",rimbalzi="treb",assist="ast",punti_assist="pts_ast",punti_rimbalzi="pts_treb",rimbalzi_assist="treb_ast")
  for(nome in sequenza_target_nba()) { cfg<-z$configurazione[[nome]]; fit<-addestra_target_ranger_nba(d,cfg$target,cfg$feature,parametri,seed); meta<-calibra_meta_oof_nba(d[[cfg$target]][fit$indici$train],fit$oof_raw,d$dateGame[fit$indici$train]); d<-aggiungi_meta_feature_nba(d,pref[[nome]],meta$q30,fit$q30_validation,fit$q30_test,meta$sd,rep(stats::sd(d[[cfg$target]][fit$indici$validation]-fit$pred_validation),length(fit$indici$validation)),rep(stats::sd(d[[cfg$target]][fit$indici$test]-fit$pred_test),length(fit$indici$test)),indici=fit$indici); risultati[[nome]]<-fit }
  list(dati=d,modelli=risultati)
}
