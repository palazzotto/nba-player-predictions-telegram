import unittest
from pathlib import Path

from bot.settings import SettingsError, load_settings


class SettingsTests(unittest.TestCase):
    def test_valid_settings(self):
        settings = load_settings(Path("/project"), {"TELEGRAM_BOT_TOKEN": "secret", "ALLOWED_CHAT_ID": "42"})
        self.assertEqual(settings.allowed_chat_id, 42)
        self.assertEqual(settings.output_dir, Path("/project/output"))

    def test_invalid_chat_id_fails_without_showing_token(self):
        with self.assertRaises(SettingsError):
            load_settings(Path("/project"), {"TELEGRAM_BOT_TOKEN": "secret", "ALLOWED_CHAT_ID": "no"})
