# Liste top-40 per target, aggiornate manualmente dopo il triage Ranger.
settimana_iso <- function(data) format(as.Date(data), "%G-W%V")
valida_feature_settimanali <- function(liste, colonne_disponibili = NULL, n = 40L) {
  if (!is.list(liste) || is.null(names(liste)) || any(!nzchar(names(liste)))) stop("Servono liste nominate per target.", call. = FALSE)
  for (target in names(liste)) {
    x <- as.character(liste[[target]])
    if (length(x) != n || any(!nzchar(x)) || anyDuplicated(x)) stop(sprintf("%s deve contenere %d feature uniche.", target, n), call. = FALSE)
    if (!is.null(colonne_disponibili) && length(setdiff(x, colonne_disponibili))) stop(sprintf("%s contiene feature assenti: %s.",target,paste(setdiff(x,colonne_disponibili),collapse=", ")),call.=FALSE)
  }
  invisible(TRUE)
}
pubblica_feature_settimanali <- function(liste, data, colonne_disponibili, cartella = "models") {
  valida_feature_settimanali(liste, colonne_disponibili); dir.create(cartella,recursive=TRUE,showWarnings=FALSE)
  settimana <- settimana_iso(data); oggetto <- list(settimana_iso=settimana, generato_il=format(Sys.time(),tz="Europe/Rome",usetz=TRUE), target=liste)
  rds <- file.path(cartella,paste0("features_",settimana,".rds")); tmp <- tempfile(".tmp_",dirname(rds),".rds"); saveRDS(oggetto,tmp); if(!file.rename(tmp,rds)) stop("Pubblicazione RDS fallita.",call.=FALSE)
  audit <- data.frame(settimana_iso=settimana,target=rep(names(liste),lengths(liste)),feature=unlist(liste,use.names=FALSE),ordine=unlist(lapply(liste,seq_along),use.names=FALSE)); csv<-sub("\\.rds$",".csv",rds); tmp<-tempfile(".tmp_",dirname(csv),".csv"); utils::write.csv(audit,tmp,row.names=FALSE); if(!file.rename(tmp,csv)) stop("Pubblicazione CSV fallita.",call.=FALSE); rds
}
aggiorna_feature_settimanali <- function(liste_parziali,data,colonne_disponibili,cartella="models") { valida_feature_settimanali(liste_parziali,colonne_disponibili); f<-file.path(cartella,paste0("features_",settimana_iso(data),".rds")); precedenti<-if(file.exists(f)) readRDS(f)$target else list(); pubblica_feature_settimanali(utils::modifyList(precedenti,liste_parziali),data,colonne_disponibili,cartella) }
leggi_feature_settimanali <- function(data, colonne_disponibili, cartella="models") { f<-file.path(cartella,paste0("features_",settimana_iso(data),".rds")); if(!file.exists(f)) stop(sprintf("Liste feature mancanti per %s: aggiornamento manuale richiesto.",settimana_iso(data)),call.=FALSE); x<-readRDS(f); valida_feature_settimanali(x$target,colonne_disponibili); x$target }
