# Cache incrementale: il rebuild completo avviene una volta; dopo una nuova gara
# si ricalcolano solo giocatori e squadre coinvolti, poi si sostituiscono le chiavi.
source(file.path(getwd(),"r","prepara_dati.R"))
scrivi_cache_feature_atomica <- function(x,file) { dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE); tmp<-tempfile(".tmp_",dirname(file),".rds"); saveRDS(x,tmp); if(!file.rename(tmp,file)) stop("Pubblicazione cache fallita.",call.=FALSE); invisible(file) }
aggiorna_cache_feature_storiche <- function(dati,box_data,dati_nba_t,file_cache="data/cache/dataset_modello.rds",nuovi_giocatori=NULL,nuove_squadre=NULL) {
  if(!file.exists(file_cache)||is.null(nuovi_giocatori)||!nrow(nuovi_giocatori)) { x<-prepara_dataset_storico_modelli(dati,box_data,dati_nba_t); scrivi_cache_feature_atomica(x,file_cache); return(list(dati=x,modalita="completa")) }
  cache<-readRDS(file_cache); giocatori<-unique(as.character(nuovi_giocatori$idPlayer)); squadre<-unique(as.character(nuove_squadre$slugTeam %||% character()));
  # Ricostruzione limitata ai giocatori/squadre impattati; le loro righe vengono
  # sostituite integralmente, preservando il resto della cache senza ricalcolo.
  base_p<-dati[as.character(dati$idPlayer)%in%giocatori | as.character(dati$slugOpponent)%in%squadre,,drop=FALSE]; base_t<-dati_nba_t[as.character(dati_nba_t$slugTeam)%in%unique(c(squadre,as.character(base_p$slugOpponent))),,drop=FALSE]; x<-prepara_dataset_storico_modelli(base_p,box_data,dati_nba_t)
  chiavi<-paste(cache$idGame,cache$idPlayer); nuove_chiavi<-paste(x$idGame,x$idPlayer); cache<-cache[!chiavi%in%nuove_chiavi & !as.character(cache$idPlayer)%in%giocatori,,drop=FALSE]; out<-rbind(cache,x); out<-out[order(out$dateGame,out$idGame,out$idPlayer),,drop=FALSE]; scrivi_cache_feature_atomica(out,file_cache); list(dati=out,modalita="incrementale")
}
`%||%` <- function(x,y) if(is.null(x)) y else x
