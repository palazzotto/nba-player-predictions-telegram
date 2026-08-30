"""Lettura e validazione read-only degli export CSV della pipeline R."""
import csv
import math
from dataclasses import dataclass
from datetime import date
from pathlib import Path
from typing import Dict, List


REQUIRED_COLUMNS = (
    "data_partita", "id_partita", "ora_locale", "squadra_casa", "squadra_trasferta",
    "id_squadra", "squadra", "id_giocatore", "giocatore",
    "exp_min_q30", "exp_min", "exp_pts_q30", "exp_pts", "exp_ast_q30", "exp_ast",
    "exp_treb_q30", "exp_treb", "exp_pts_ast_q30", "exp_pts_ast",
    "exp_pts_treb_q30", "exp_pts_treb", "exp_treb_ast_q30", "exp_treb_ast",
    "eligible_bot", "generato_il",
)
OPTIONAL_COLUMNS = ("stile_cluster",)
# Contratto rigoroso: Q30 e stima centrale exp_*; nessun fallback a prev_*.
PREDICTION_COLUMNS = REQUIRED_COLUMNS[9:23]
CLUSTER_COLUMNS = (
    "data_partita", "id_partita", "cluster", "stile", "ordine", "statistica", "etichetta",
    "casa_attacco_media", "casa_attacco_rank", "trasferta_difesa_media", "trasferta_difesa_rank",
    "trasferta_attacco_media", "trasferta_attacco_rank", "casa_difesa_media", "casa_difesa_rank",
    "generato_il",
)
CLUSTER_VALUE_COLUMNS = CLUSTER_COLUMNS[7:15]


class OutputError(ValueError):
    """Export giornaliero assente, corrotto o non idoneo alla navigazione."""


@dataclass(frozen=True)
class Prediction:
    data_partita: str
    id_partita: str
    ora_locale: str
    squadra_casa: str
    squadra_trasferta: str
    id_squadra: str
    squadra: str
    id_giocatore: str
    giocatore: str
    values: Dict[str, float]
    stile_cluster: str = ""


@dataclass(frozen=True)
class ClusterStat:
    cluster: int
    stile: str
    ordine: int
    statistica: str
    etichetta: str
    values: Dict[str, float]


class PredictionsRepository:
    def __init__(self, output_dir: Path):
        self.output_dir = Path(output_dir)

    def load_day(self, requested_date: date) -> List[Prediction]:
        path = self.output_dir / ("previsioni_" + requested_date.isoformat() + ".csv")
        if not path.is_file():
            raise OutputError("Previsioni non disponibili per %s. Esegui prima la pipeline R." % requested_date.isoformat())
        try:
            with path.open("r", encoding="utf-8-sig", newline="") as handle:
                rows = list(csv.DictReader(handle))
        except (OSError, csv.Error) as exc:
            raise OutputError("Impossibile leggere l'output delle previsioni.") from exc
        if not rows:
            raise OutputError("L'output delle previsioni e' vuoto.")
        if not rows[0] or set(REQUIRED_COLUMNS) - set(rows[0]):
            raise OutputError("L'output delle previsioni non rispetta lo schema richiesto.")

        predictions, seen = [], set()
        for row in rows:
            if row.get("data_partita") != requested_date.isoformat():
                raise OutputError("L'output contiene una data non coerente con il file richiesto.")
            key = (row.get("id_partita"), row.get("id_giocatore"))
            if not all(key) or key in seen:
                raise OutputError("L'output contiene record duplicati o privi di identificativo.")
            seen.add(key)
            values = {}
            for column in PREDICTION_COLUMNS:
                try:
                    value = float(row[column])
                except (KeyError, TypeError, ValueError):
                    raise OutputError("L'output contiene una previsione non numerica.") from None
                if not math.isfinite(value):
                    raise OutputError("L'output contiene una previsione non valida.")
                values[column] = value
            eligible = str(row.get("eligible_bot", "")).strip().lower() == "true"
            if eligible:
                predictions.append(Prediction(
                    data_partita=row["data_partita"], id_partita=row["id_partita"],
                    ora_locale=row["ora_locale"], squadra_casa=row["squadra_casa"],
                    squadra_trasferta=row["squadra_trasferta"], id_squadra=row["id_squadra"],
                    squadra=row["squadra"], id_giocatore=row["id_giocatore"],
                    giocatore=row["giocatore"], values=values,
                    stile_cluster=str(row.get("stile_cluster") or "").strip(),
                ))
        return predictions

    def load_cluster_game(self, requested_date: date, game_id: str, cluster: int) -> List[ClusterStat]:
        path = self.output_dir / ("confronti_cluster_" + requested_date.isoformat() + ".csv")
        if not path.is_file():
            raise OutputError("Confronti per cluster non disponibili: esegui prima l'export R dei cluster.")
        try:
            with path.open("r", encoding="utf-8-sig", newline="") as handle:
                rows = list(csv.DictReader(handle))
        except (OSError, csv.Error) as exc:
            raise OutputError("Impossibile leggere i confronti per cluster.") from exc
        if not rows or set(CLUSTER_COLUMNS) - set(rows[0]):
            raise OutputError("L'output dei confronti cluster non rispetta lo schema richiesto.")

        result, seen = [], set()
        for row in rows:
            if row.get("data_partita") != requested_date.isoformat():
                raise OutputError("L'output cluster contiene una data non coerente.")
            if row.get("id_partita") != game_id or row.get("cluster") != str(cluster):
                continue
            try:
                order = int(row["ordine"])
                values = {column: float(row[column]) for column in CLUSTER_VALUE_COLUMNS}
            except (KeyError, TypeError, ValueError):
                raise OutputError("L'output cluster contiene valori non numerici.") from None
            if order < 1 or not row.get("etichetta") or any(not math.isfinite(value) for value in values.values()):
                raise OutputError("L'output cluster contiene valori non validi.")
            if order in seen:
                raise OutputError("L'output cluster contiene statistiche duplicate.")
            seen.add(order)
            result.append(ClusterStat(
                cluster=cluster, stile=row["stile"], ordine=order,
                statistica=row["statistica"], etichetta=row["etichetta"], values=values,
            ))
        if not result:
            raise OutputError("Non sono disponibili confronti cluster per questa partita.")
        return sorted(result, key=lambda row: row.ordine)

    @staticmethod
    def games(rows: List[Prediction]) -> List[Prediction]:
        unique = {row.id_partita: row for row in rows}
        return sorted(unique.values(), key=lambda row: (row.ora_locale, row.id_partita))

    @staticmethod
    def teams(rows: List[Prediction], game_id: str) -> List[Prediction]:
        unique = {row.squadra: row for row in rows if row.id_partita == game_id}
        return sorted(unique.values(), key=lambda row: row.squadra)

    @staticmethod
    def players(rows: List[Prediction], game_id: str, team: str) -> List[Prediction]:
        return sorted((row for row in rows if row.id_partita == game_id and row.squadra == team), key=lambda row: row.giocatore)

    @staticmethod
    def player(rows: List[Prediction], game_id: str, team: str, player_id: str) -> Prediction:
        for row in rows:
            if (row.id_partita, row.squadra, row.id_giocatore) == (game_id, team, player_id):
                return row
        raise OutputError("Il giocatore non e' piu' disponibile nell'output corrente.")
