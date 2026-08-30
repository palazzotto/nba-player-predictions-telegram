source(file.path("r", "roster_nba_com.R"))
html <- '<html><head><script id="__NEXT_DATA__" type="application/json">{"props":{"pageProps":{"players":[{"PERSON_ID":1643411,"PLAYER_FIRST_NAME":"Darius","PLAYER_LAST_NAME":"Acuff Jr.","TEAM_ABBREVIATION":"SAC"},{"PERSON_ID":9999999,"PLAYER_FIRST_NAME":"Nuovo","PLAYER_LAST_NAME":"Rookie","TEAM_ABBREVIATION":"BOS"},{"PERSON_ID":8888888,"PLAYER_FIRST_NAME":"Senza","PLAYER_LAST_NAME":"Squadra","TEAM_ABBREVIATION":null}]}}}</script></head></html>'
roster <- estrai_roster_nba_com_da_html(html)
stopifnot(nrow(roster) == 3L, identical(roster$idPlayer, c("9999999", "1643411", "8888888")), sum(roster$esito == "confermato_nba") == 2L, sum(roster$esito == "revisione_squadra") == 1L, all(roster$TEAM[roster$esito == "confermato_nba"] == roster$sigla[roster$esito == "confermato_nba"]), all(roster$fonte == "NBA.com League Roster"))
duplicato <- rbind(roster, roster[1, ])
errore <- try(valida_roster_nba_com(duplicato), silent = TRUE)
stopifnot(inherits(errore, "try-error"))
cat("test_roster_nba_com: OK\n")
