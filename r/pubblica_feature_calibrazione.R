#!/usr/bin/env Rscript
# Estrae i vettori letterali top_vars* da uno script di calibrazione senza
# eseguirne training, download o altre istruzioni.
root <- getwd()
source(file.path(root, "r", "feature_settimanali.R"))

estrai_c_caratteri <- function(expr) {
  if (!is.call(expr) || !identical(as.character(expr[[1L]]), "c")) return(NULL)
  valori <- as.list(expr)[-1L]
  if (!length(valori) || !all(vapply(valori, is.character, logical(1)))) return(NULL)
  unname(unlist(valori))
}

estrai_top_vars_calibrazione <- function(file_script) {
  espressioni <- parse(file_script)
  assegnazioni <- list()
  for (expr in espressioni) {
    if (!is.call(expr) || !as.character(expr[[1L]]) %in% c("<-", "=")) next
    nome <- as.character(expr[[2L]])[[1L]]
    if (!startsWith(nome, "top_vars")) next
    valori <- estrai_c_caratteri(expr[[3L]])
    if (!is.null(valori)) assegnazioni[[nome]] <- valori
  }
  mappa <- c(top_vars="minuti",top_vars_pts="punti",top_vars_treb="rimbalzi",top_vars_ast="assist",top_vars_pts_ast="punti_assist",top_vars_pts_treb="punti_rimbalzi",top_vars_treb_ast="rimbalzi_assist")
  mancanti <- setdiff(names(mappa), names(assegnazioni)); if(length(mancanti)) stop(sprintf("Vettori top_vars mancanti: %s.",paste(mancanti,collapse=", ")),call.=FALSE)
  stats::setNames(unname(assegnazioni[names(mappa)]), unname(mappa))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly=TRUE)
  arg <- function(nome, default=NULL) { x<-args[startsWith(args,paste0("--",nome,"="))]; if(!length(x)) default else sub(paste0("--",nome,"="),"",x[[1L]],fixed=TRUE) }
file_script <- arg("script"); data <- arg("data",as.character(Sys.Date())); file_colonne <- arg("colonne"); target <- arg("target")
if(is.null(file_script)||is.null(file_colonne)||is.null(target)) stop("Uso: --script=FILE --colonne=FILE_RDS --target=TARGET [--data=YYYY-MM-DD].",call.=FALSE)
colonne <- names(readRDS(file_colonne)); liste <- estrai_top_vars_calibrazione(file_script); if(!target%in%names(liste)) stop("Target non valido.",call.=FALSE)
file <- aggiorna_feature_settimanali(liste[target],as.Date(data),colonne,file.path(root,"models"))
  cat(sprintf("Feature settimanali pubblicate: %s\n",file))
}
