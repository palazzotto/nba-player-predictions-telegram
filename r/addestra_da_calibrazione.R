#!/usr/bin/env Rscript
# Usa integralmente lo script canonico di calibrazione e ne pubblica i modelli.
args <- commandArgs(trailingOnly = TRUE)
data <- sub("^--data=", "", args[startsWith(args, "--data=")][1L])
if (is.na(data) || is.na(as.Date(data))) stop("Uso: Rscript r/addestra_da_calibrazione.R --data=YYYY-MM-DD", call. = FALSE)
Sys.setenv(NBA_DATA_MODELLI = data)
source(file.path(getwd(), "r", "prev1_exp_min_train_val_test_sottostime_q30_calibrazione.R"), local = globalenv())
