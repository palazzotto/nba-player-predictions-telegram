#!/usr/bin/env Rscript
# Orchestratore locale: non scarica dati, non sovrascrive gli RDS sorgenti.
root <- getwd()
source(file.path(root,"r","prepara_dati.R")); source(file.path(root,"r","cache_feature_storiche.R")); source(file.path(root,"r","feature_settimanali.R")); source(file.path(root,"r","addestra_modelli.R")); source(file.path(root,"r","archivi_storici_nba.R"))
pubblica_modelli_bot <- function(risultato, data, cartella_modelli = "models") {
  modelli <- risultato$modelli
  target_bot <- setdiff(sequenza_target_nba(), "punti_rimbalzi_assist")
  if (!all(target_bot %in% names(modelli))) {
    stop("Il training non ha prodotto tutti i sette modelli del bot.", call. = FALSE)
  }
  dir.create(cartella_modelli, recursive = TRUE, showWarnings = FALSE)
  settimana <- settimana_iso(data)
  percorsi <- stats::setNames(
    file.path(cartella_modelli, paste0("modello_", target_bot, "_", settimana, ".rds")),
    target_bot
  )
  for (target in target_bot) scrivi_rds_atomico(modelli[[target]], percorsi[[target]])
  unname(percorsi)
}
esegui_training_locale <- function(data, dati, box_data, dati_nba_t, cartella_modelli="models", parametri=list(num.trees=500L,mtry=NULL,min.node.size=5L,sample.fraction=.8)) {
  dataset <- aggiorna_cache_feature_storiche(dati,box_data,dati_nba_t,file.path("data/cache","dataset_modello.rds"))$dati
  liste <- leggi_feature_settimanali(data,names(dataset),cartella_modelli)
  risultato <- addestra_sequenza_ranger_nba(dataset,liste,parametri)
  file <- file.path(cartella_modelli,paste0("modelli_",settimana_iso(data),".rds"))
  scrivi_rds_atomico(list(generato_il=format(Sys.time(),tz="Europe/Rome",usetz=TRUE),settimana_iso=settimana_iso(data),risultato=risultato),file)
  list(archivio_completo = file, modelli_bot = pubblica_modelli_bot(risultato, data, cartella_modelli))
}
if(sys.nframe()==0L) { a<-commandArgs(trailingOnly=TRUE); data<-sub("^--data=","",a[startsWith(a,"--data=")][1]); if(is.na(data)||!nzchar(data)) stop("Uso: Rscript r/aggiorna_e_prevedi.R --data=YYYY-MM-DD",call.=FALSE); file<-esegui_training_locale(as.Date(data),readRDS(file.path(root,"dati_nba.rds")),readRDS(file.path(root,"box_data.rds")),readRDS(file.path(root,"dati_nba_t.rds")),file.path(root,"models")); cat(sprintf("Archivio completo: %s\nModelli bot: %s\n",file$archivio_completo,paste(file$modelli_bot,collapse="; "))) }
