"""Configurazione locale del bot, senza registrare segreti."""
from dataclasses import dataclass
import os
from pathlib import Path
from typing import Optional


class SettingsError(ValueError):
    """Configurazione mancante o non valida."""


@dataclass(frozen=True)
class Settings:
    telegram_bot_token: str
    allowed_chat_id: int
    output_dir: Path


def load_settings(project_root: Path, environ: Optional[dict] = None) -> Settings:
    """Carica .env se python-dotenv e' disponibile, poi valida l'ambiente."""
    try:
        from dotenv import load_dotenv
        load_dotenv(project_root / ".env")
    except ImportError:
        # Nei test si puo' passare l'ambiente esplicitamente senza dipendenze.
        pass

    values = os.environ if environ is None else environ
    token = (values.get("TELEGRAM_BOT_TOKEN") or "").strip()
    chat_id_text = (values.get("ALLOWED_CHAT_ID") or "").strip()
    if not token:
        raise SettingsError("TELEGRAM_BOT_TOKEN non configurato nel file .env.")
    if not chat_id_text:
        raise SettingsError("ALLOWED_CHAT_ID non configurato nel file .env.")
    try:
        allowed_chat_id = int(chat_id_text)
    except ValueError:
        raise SettingsError("ALLOWED_CHAT_ID deve essere un numero intero.") from None

    output_value = (values.get("OUTPUT_DIR") or "output").strip()
    output_dir = Path(output_value)
    if not output_dir.is_absolute():
        output_dir = project_root / output_dir
    return Settings(token, allowed_chat_id, output_dir)
