import unittest

from bot.main import format_cluster_comparison, format_date_it, format_prediction, game_button_label, is_authorized, parse_callback, parse_requested_date, player_button_label
from bot.repository import ClusterStat, Prediction
from bot.settings import Settings


class NavigationTests(unittest.TestCase):
    def test_player_message_has_exactly_seven_estimates(self):
        player = Prediction("2026-10-20", "1", "", "A", "B", "1", "A", "2", "Test Player", {"exp_min_q30": 20, "exp_min": 22, "exp_pts_q30": 10, "exp_pts": 12, "exp_ast_q30": 2, "exp_ast": 3, "exp_treb_q30": 5, "exp_treb": 6, "exp_pts_ast_q30": 12, "exp_pts_ast": 15, "exp_pts_treb_q30": 15, "exp_pts_treb": 18, "exp_treb_ast_q30": 7, "exp_treb_ast": 9})
        lines = format_prediction(player).splitlines()
        self.assertEqual(len(lines), 17)
        self.assertIn("👤 <b>Test Player</b>", format_prediction(player))
        self.assertIn("<i>A</i>  •  B @ A", format_prediction(player))
        self.assertIn("⏱️ Minuti: <b>20.0</b>–<b>22.0</b>", format_prediction(player))
        self.assertIn("🎯 Punti + Assist", format_prediction(player))
        self.assertNotIn("PRA", format_prediction(player))

    def test_authorization_and_callback_validation(self):
        settings = Settings("unused", 9, __import__("pathlib").Path("output"))
        self.assertTrue(is_authorized(9, settings)); self.assertFalse(is_authorized(10, settings))
        self.assertEqual(parse_callback("g:2026-10-20:1"), ["g", "2026-10-20", "1"])
        self.assertEqual(parse_callback("c:2026-10-20:1:2"), ["c", "2026-10-20", "1", "2"])
        with self.assertRaises(ValueError): parse_callback("bad")

    def test_requested_date_requires_iso_format(self):
        self.assertEqual(str(parse_requested_date(["2026-10-20"])), "2026-10-20")
        self.assertEqual(format_date_it(__import__("datetime").date(2026, 10, 20)), "20 ottobre 2026")
        with self.assertRaises(ValueError): parse_requested_date(["20/10/2026"])

    def test_game_button_identifies_home_and_away(self):
        game = Prediction("2026-10-20", "1", "", "Pistons", "Celtics", "1", "BOS", "2", "Test Player", {})
        self.assertEqual(game_button_label(game), "🏠 Pistons  vs  Celtics ✈️")

    def test_player_button_includes_cluster_only_when_present(self):
        player = Prediction("2026-10-20", "1", "", "A", "B", "1", "A", "2", "LeBron James", {}, "Creatori")
        self.assertEqual(player_button_label(player), "👤 LeBron James — Creatori")
        self.assertEqual(player_button_label(Prediction("2026-10-20", "1", "", "A", "B", "1", "A", "2", "No Cluster", {})), "👤 No Cluster")

    def test_cluster_message_contains_both_directions_and_ranks(self):
        game = Prediction("2026-10-20", "1", "", "Pistons", "Celtics", "1", "DET", "2", "Test Player", {})
        stat = ClusterStat(1, "Perimetrali", 1, "pts", "Punti", {"casa_attacco_media": 50, "casa_attacco_rank": 4, "trasferta_difesa_media": 45, "trasferta_difesa_rank": 8, "trasferta_attacco_media": 52, "trasferta_attacco_rank": 2, "casa_difesa_media": 47, "casa_difesa_rank": 10})
        text = format_cluster_comparison(game, [stat])
        self.assertIn("Cluster · Perimetrali", text)
        self.assertIn("Pistons attacco → Celtics difesa", text)
        self.assertIn("Celtics attacco → Pistons difesa", text)
        self.assertIn("PTS   50.0 #04  45.0 #08", text)
        self.assertEqual(text.count("<pre>"), 2)
