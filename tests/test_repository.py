import csv
import tempfile
import unittest
from datetime import date
from pathlib import Path

from bot.repository import CLUSTER_COLUMNS, OutputError, PredictionsRepository, REQUIRED_COLUMNS


def row(**overrides):
    values = {column: "1" for column in REQUIRED_COLUMNS}
    values.update({"data_partita": "2026-10-20", "id_partita": "22600001", "ora_locale": "2026-10-20T19:00:00Z", "squadra_casa": "Pistons", "squadra_trasferta": "Celtics", "id_squadra": "1", "squadra": "BOS", "id_giocatore": "10", "giocatore": "Player One", "eligible_bot": "TRUE", "generato_il": "2026-08-25"})
    values.update(overrides); return values


class RepositoryTests(unittest.TestCase):
    def write_rows(self, rows):
        temp = tempfile.TemporaryDirectory(); path = Path(temp.name) / "previsioni_2026-10-20.csv"
        with path.open("w", newline="") as f: writer = csv.DictWriter(f, fieldnames=REQUIRED_COLUMNS); writer.writeheader(); writer.writerows(rows)
        return temp, PredictionsRepository(Path(temp.name))

    def test_filters_and_navigates_eligible_rows(self):
        temp, repo = self.write_rows([row(), row(id_giocatore="11", giocatore="Hidden", eligible_bot="FALSE")]); self.addCleanup(temp.cleanup)
        rows = repo.load_day(date(2026, 10, 20))
        self.assertEqual([p.giocatore for p in repo.players(rows, "22600001", "BOS")], ["Player One"])
        self.assertEqual(rows[0].stile_cluster, "")
        self.assertEqual(repo.games(rows)[0].squadra_casa, "Pistons")

    def test_missing_and_duplicate_outputs_fail(self):
        with tempfile.TemporaryDirectory() as folder:
            with self.assertRaises(OutputError): PredictionsRepository(Path(folder)).load_day(date(2026, 10, 20))
        temp, repo = self.write_rows([row(), row()]); self.addCleanup(temp.cleanup)
        with self.assertRaises(OutputError): repo.load_day(date(2026, 10, 20))

    def test_loads_cluster_statistics_for_one_game(self):
        temp, repo = self.write_rows([row()]); self.addCleanup(temp.cleanup)
        path = Path(temp.name) / "confronti_cluster_2026-10-20.csv"
        values = {column: "1" for column in CLUSTER_COLUMNS}
        values.update({"data_partita": "2026-10-20", "id_partita": "22600001", "cluster": "1", "stile": "Perimetrali", "ordine": "1", "statistica": "pts", "etichetta": "Punti"})
        with path.open("w", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=CLUSTER_COLUMNS); writer.writeheader(); writer.writerow(values)
        stats = repo.load_cluster_game(date(2026, 10, 20), "22600001", 1)
        self.assertEqual(stats[0].etichetta, "Punti")
