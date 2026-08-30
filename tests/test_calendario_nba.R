source(file.path("r", "calendario_nba.R"))

fixture <- list(leagueSchedule = list(gameDates = list(
  list(gameDate = "2026-10-21", games = list(list(
    gameId = "0022600001", gameTimeUTC = "2026-10-22T00:00:00Z", gameType = 2,
    awayTeam = list(teamId = 1610612738, teamName = "Celtics", teamTricode = "BOS"),
    homeTeam = list(teamId = 1610612747, teamName = "Lakers", teamTricode = "LAL")
  ), list(
    gameId = "0022600002", gameTimeUTC = "2026-10-22T02:30:00Z", gameType = 2,
    awayTeam = list(teamId = 1610612744, teamName = "Warriors", teamTricode = "GSW"),
    homeTeam = list(teamId = 1610612746, teamName = "Clippers", teamTricode = "LAC")
  ))),
  list(gameDate = "2027-07-04", games = list(list(
    gameId = "0012700001", gameTimeUTC = "2027-07-04T18:00:00Z", gameType = 1,
    awayTeam = list(teamId = 1, teamName = "Fuori", teamTricode = "OUT"),
    homeTeam = list(teamId = 2, teamName = "Fuori", teamTricode = "OUT")
  )))
)))

calendario <- estrai_calendario_nba(fixture, anno_stagione = 2027L)
stopifnot(nrow(calendario) == 2L)
stopifnot(calendario$id_partita[[1]] == "0022600001")
stopifnot(calendario$data_partita[[1]] == as.Date("2026-10-21"))
stopifnot(identical(calendario$id_partita, c("0022600001", "0022600002")))
stopifnot(all(c("id_squadra_casa", "id_squadra_trasferta", "squadra_casa", "squadra_trasferta") %in% names(calendario)))

cat("OK: estrazione di tutte le gare NBA 2026/27 presenti nel feed\n")
