"""Avvio polling e navigazione Telegram per gli output R gia' pubblicati."""
from datetime import date, datetime
from html import escape
from pathlib import Path
from zoneinfo import ZoneInfo

from bot.repository import ClusterStat, OutputError, PredictionsRepository
from bot.settings import Settings, SettingsError, load_settings


ROME = ZoneInfo("Europe/Rome")
MONTHS_IT = (
    "gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno",
    "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre",
)
LABELS = (
    ("⏱️", "exp_min_q30", "exp_min", "Minuti"), ("🎯", "exp_pts_q30", "exp_pts", "Punti"),
    ("🏀", "exp_ast_q30", "exp_ast", "Assist"), ("🗑️", "exp_treb_q30", "exp_treb", "Rimbalzi"),
    ("🎯", "exp_pts_ast_q30", "exp_pts_ast", "Punti + Assist"),
    ("🏀", "exp_pts_treb_q30", "exp_pts_treb", "Punti + Rimbalzi"),
    ("🗑️", "exp_treb_ast_q30", "exp_treb_ast", "Rimbalzi + Assist"),
)


def today_rome() -> date:
    return datetime.now(ROME).date()


def is_authorized(chat_id, settings: Settings) -> bool:
    return chat_id is not None and int(chat_id) == settings.allowed_chat_id


def format_prediction(row) -> str:
    matchup = "%s @ %s" % (escape(row.squadra_trasferta), escape(row.squadra_casa))
    header = [
        "👤 <b>%s</b>" % escape(row.giocatore),
        "<i>%s</i>  •  %s" % (escape(row.squadra), matchup),
        "────────────",
    ]
    statistics = ["%s %s: <b>%.1f</b>–<b>%.1f</b>" % (emoji, label, row.values[q30], row.values[central]) for emoji, q30, central, label in LABELS]
    return "\n".join(header) + "\n\n" + "\n\n".join(statistics)


def format_date_it(value: date) -> str:
    return "%d %s %d" % (value.day, MONTHS_IT[value.month - 1], value.year)


def game_button_label(game) -> str:
    return "🏠 %s  vs  %s ✈️" % (game.squadra_casa, game.squadra_trasferta)


def player_button_label(player) -> str:
    label = player.giocatore
    if player.stile_cluster:
        label += " — " + player.stile_cluster
    return "👤 " + label


def format_cluster_comparison(game, stats: list[ClusterStat]) -> str:
    """Una schermata per cluster, compatta e leggibile anche su telefono."""
    style = escape(stats[0].stile)
    short_names = {
        "fgm": "FGM", "fga": "FGA", "fg3m": "3PM", "fg3a": "3PA",
        "fg2m": "2PM", "fg2a": "2PA", "ftm": "FTM", "fta": "FTA",
        "oreb": "OREB", "ast": "AST", "tov": "TOV", "pts": "PTS",
    }

    def table(attack_mean, attack_rank, defense_mean, defense_rank):
        rows = ["STAT    ATT     DIF"]
        for stat in stats:
            label = short_names.get(stat.statistica, stat.statistica.upper())
            rows.append("%-4s  %4.1f #%02d  %4.1f #%02d" % (
                label, stat.values[attack_mean], stat.values[attack_rank],
                stat.values[defense_mean], stat.values[defense_rank],
            ))
        return "<pre>%s</pre>" % "\n".join(rows)

    return "\n".join([
        "🏀 <b>%s @ %s</b>" % (escape(game.squadra_trasferta), escape(game.squadra_casa)),
        "<b>Cluster · %s</b>" % style,
        "<i>media · rank</i>",
        "",
        "📈 <b>%s attacco → %s difesa</b>" % (escape(game.squadra_casa), escape(game.squadra_trasferta)),
        table("casa_attacco_media", "casa_attacco_rank", "trasferta_difesa_media", "trasferta_difesa_rank"),
        "🛡 <b>%s attacco → %s difesa</b>" % (escape(game.squadra_trasferta), escape(game.squadra_casa)),
        table("trasferta_attacco_media", "trasferta_attacco_rank", "casa_difesa_media", "casa_difesa_rank"),
    ])


def parse_callback(data: str):
    parts = data.split(":")
    if len(parts) < 2 or parts[0] not in {"g", "c", "t", "p", "b"}:
        raise ValueError("Callback non valida.")
    return parts


def parse_requested_date(args) -> date:
    """Converte l'unico argomento di /data senza accettare formati ambigui."""
    if len(args) != 1:
        raise ValueError("Uso: /data YYYY-MM-DD")
    try:
        return date.fromisoformat(args[0])
    except ValueError:
        raise ValueError("Data non valida. Usa il formato YYYY-MM-DD.") from None


def build_application(settings: Settings):
    from telegram import InlineKeyboardButton, InlineKeyboardMarkup
    from telegram.constants import ParseMode
    from telegram.ext import Application, CallbackQueryHandler, CommandHandler, ContextTypes
    repository = PredictionsRepository(settings.output_dir)

    def keyboard(rows):
        return InlineKeyboardMarkup(rows)

    async def reject(update):
        chat = update.effective_chat
        if chat is not None and not is_authorized(chat.id, settings):
            if update.effective_message is not None:
                await update.effective_message.reply_text("Accesso non autorizzato.")
            return True
        return False

    async def start(update, context):
        if await reject(update): return
        await update.effective_message.reply_text(
            "🏀 <b>NBA Predict</b>\n\nUsa /oggi per vedere le previsioni disponibili.",
            parse_mode=ParseMode.HTML,
        )

    async def today(update, context):
        if await reject(update): return
        await show_games(update.effective_message, today_rome())

    async def selected_date(update, context):
        if await reject(update): return
        try:
            requested_date = parse_requested_date(context.args)
        except ValueError as exc:
            await update.effective_message.reply_text(str(exc))
            return
        await show_games(update.effective_message, requested_date)

    async def show_games(message, requested_date, edit=False):
        try:
            rows = repository.load_day(requested_date)
            games = repository.games(rows)
            if not games:
                text, markup = "🏀 <b>Nessuna previsione disponibile</b>\n\nNon ci sono giocatori idonei per il %s." % format_date_it(requested_date), None
            else:
                text = "🏀 <b>Previsioni NBA</b>\n\n📅 %s\n\nScegli una partita:" % format_date_it(requested_date)
                markup = keyboard([[InlineKeyboardButton(game_button_label(g), callback_data="g:%s:%s" % (g.data_partita, g.id_partita))] for g in games])
        except OutputError as exc:
            text, markup = str(exc), None
        if edit: await message.edit_text(text, reply_markup=markup, parse_mode=ParseMode.HTML)
        else: await message.reply_text(text, reply_markup=markup, parse_mode=ParseMode.HTML)

    async def callbacks(update, context):
        if await reject(update): return
        query = update.callback_query
        await query.answer()
        try:
            parts = parse_callback(query.data)
            if parts[0] == "b":
                await show_games(query.message, date.fromisoformat(parts[1]), edit=True); return
            requested_date = date.fromisoformat(parts[1])
            rows = repository.load_day(requested_date)
            if parts[0] == "g" and len(parts) == 3:
                teams = repository.teams(rows, parts[2])
                game = next(team for team in rows if team.id_partita == parts[2])
                buttons = [[InlineKeyboardButton("🏀 %s" % t.squadra, callback_data="t:%s:%s:%s" % (t.data_partita, t.id_partita, t.squadra))] for t in teams]
                buttons.insert(0, [InlineKeyboardButton("📊 Cluster: Perimetrali", callback_data="c:%s:%s:1" % (requested_date.isoformat(), parts[2]))])
                buttons.append([InlineKeyboardButton("‹ Indietro alle partite", callback_data="b:%s" % requested_date.isoformat())])
                text = "🏀 <b>%s @ %s</b>\n\n📅 %s\n\nScegli una squadra:" % (escape(game.squadra_trasferta), escape(game.squadra_casa), format_date_it(requested_date))
                await query.message.edit_text(text, reply_markup=keyboard(buttons), parse_mode=ParseMode.HTML); return
            if parts[0] == "c" and len(parts) == 4:
                cluster = int(parts[3])
                if cluster not in {1, 2, 3}: raise ValueError("Cluster non valido.")
                game = next(team for team in rows if team.id_partita == parts[2])
                try:
                    stats = repository.load_cluster_game(requested_date, parts[2], cluster)
                except OutputError as exc:
                    back = keyboard([[InlineKeyboardButton("‹ Torna alle squadre", callback_data="g:%s:%s" % (requested_date.isoformat(), parts[2]))]])
                    await query.message.edit_text("⚠️ %s" % escape(str(exc)), reply_markup=back, parse_mode=ParseMode.HTML)
                    return
                previous_cluster = 3 if cluster == 1 else cluster - 1
                next_cluster = 1 if cluster == 3 else cluster + 1
                buttons = [
                    [InlineKeyboardButton("‹ Cluster precedente", callback_data="c:%s:%s:%d" % (requested_date.isoformat(), parts[2], previous_cluster)), InlineKeyboardButton("Cluster successivo ›", callback_data="c:%s:%s:%d" % (requested_date.isoformat(), parts[2], next_cluster))],
                    [InlineKeyboardButton("🏀 Scegli una squadra", callback_data="g:%s:%s" % (requested_date.isoformat(), parts[2]))],
                ]
                await query.message.edit_text(format_cluster_comparison(game, stats), reply_markup=keyboard(buttons), parse_mode=ParseMode.HTML); return
            if parts[0] == "t" and len(parts) == 4:
                players = repository.players(rows, parts[2], parts[3])
                buttons = [[InlineKeyboardButton(player_button_label(p), callback_data="p:%s:%s:%s:%s" % (p.data_partita, p.id_partita, p.squadra, p.id_giocatore))] for p in players]
                buttons.append([InlineKeyboardButton("‹ Indietro alle squadre", callback_data="g:%s:%s" % (requested_date.isoformat(), parts[2]))])
                await query.message.edit_text("🏀 <b>%s</b>\n\nScegli un giocatore:" % escape(parts[3]), reply_markup=keyboard(buttons), parse_mode=ParseMode.HTML); return
            if parts[0] == "p" and len(parts) == 5:
                player = repository.player(rows, parts[2], parts[3], parts[4])
                back = InlineKeyboardMarkup([[InlineKeyboardButton("‹ Indietro ai giocatori", callback_data="t:%s:%s:%s" % (player.data_partita, player.id_partita, player.squadra))]])
                await query.message.edit_text(format_prediction(player), reply_markup=back, parse_mode=ParseMode.HTML); return
            raise ValueError("Callback non valida.")
        except (OutputError, ValueError):
            await query.message.edit_text("Questa selezione non e' piu' disponibile. Usa /oggi per ricaricare le previsioni.")

    app = Application.builder().token(settings.telegram_bot_token).build()
    app.add_handler(CommandHandler("start", start))
    app.add_handler(CommandHandler("oggi", today))
    app.add_handler(CommandHandler("data", selected_date))
    app.add_handler(CallbackQueryHandler(callbacks))
    return app


def main():
    project_root = Path(__file__).resolve().parents[1]
    try:
        settings = load_settings(project_root)
    except SettingsError as exc:
        raise SystemExit(str(exc))
    build_application(settings).run_polling()


if __name__ == "__main__":
    main()
