source(file.path("r", "archivi_storici_nba.R"))

storico_giocatori <- data.frame(idGame = c("1", "2"), idPlayer = c("a", "b"), punti = c(10, 20), stringsAsFactors = FALSE)
nuove_giocatori <- data.frame(idGame = c("2", "3"), idPlayer = c("b", "c"), punti = c(22, 30), stringsAsFactors = FALSE)
unito_giocatori <- unisci_storico_e_nuove(storico_giocatori, nuove_giocatori, "giocatori")
stopifnot(nrow(unito_giocatori) == 3L)
stopifnot(unito_giocatori$punti[unito_giocatori$idGame == "2"] == 22)

storico_squadre <- data.frame(idGame = c("1", "1"), slugTeam = c("atl", "bos"), valore = 1:2, stringsAsFactors = FALSE)
nuove_squadre <- data.frame(idGame = "1", slugTeam = "atl", valore = 9, stringsAsFactors = FALSE)
unito_squadre <- unisci_storico_e_nuove(storico_squadre, nuove_squadre, "squadre")
stopifnot(nrow(unito_squadre) == 2L)
stopifnot(unito_squadre$valore[unito_squadre$slugTeam == "atl"] == 9)

cat("OK: archivi incrementali\n")
