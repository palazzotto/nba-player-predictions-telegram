# NBA Player Predictions Telegram

Pipeline locale in R e bot Telegram in Python per consultare previsioni sui
giocatori NBA. Il progetto mantiene separata la parte statistica
dall'interfaccia: R aggiorna i dati, addestra e calibra i modelli, mentre il
bot legge esclusivamente CSV già pubblicati.

> Il repository non contiene token Telegram, chat ID, dataset NBA, modelli
> addestrati, cache o previsioni reali. Questi artefatti rimangono locali e sono
> esclusi da Git.

## Funzionalità

- split temporale train/validation/test 70%/15%/15%;
- modelli sequenziali Ranger per minuti, punti, assist, rimbalzi e combinazioni;
- correzione del bias e calibrazione Q30 calcolate sul validation set;
- feature costruite soltanto con informazioni precedenti alla partita;
- liste di 40 feature separate per target e settimana ISO;
- aggiornamento incrementale con deduplica degli archivi;
- roster ufficiale NBA.com basato su `PERSON_ID`;
- export CSV atomico e bot Telegram read-only;
- accesso Telegram limitato a un singolo chat ID configurato localmente.

Il bot espone sette statistiche:

1. minuti;
2. punti;
3. assist;
4. rimbalzi;
5. punti + assist;
6. punti + rimbalzi;
7. rimbalzi + assist.

Sono mostrati soltanto i giocatori dichiarati `eligible_bot=TRUE` dalla
pipeline R. Python non ricalcola né corregge le previsioni.

## Architettura

```text
NBA.com + nbastatR + archivi RDS locali
                  |
                  v
       pipeline e modelli R
                  |
                  v
 output/previsioni_YYYY-MM-DD.csv
                  |
                  v
       bot Telegram Python
```

```text
bot/       interfaccia Telegram e validazione degli export
r/         acquisizione, feature engineering, training ed export
tests/     test offline Python e R
config/    configurazione di esempio priva di segreti
data/      dati e cache locali, ignorati da Git
models/    modelli e feature settimanali, ignorati da Git
output/    CSV giornalieri letti dal bot, ignorati da Git
```

## Requisiti

- Python 3.9 o successivo;
- R 4.2 o successivo;
- un bot creato tramite BotFather;
- dati storici compatibili con i contratti descritti sotto.

Pacchetti R principali:

```r
install.packages(c(
  "dplyr", "slider", "ranger", "httr2", "jsonlite", "rvest",
  "tidyr", "stringr", "tidyverse", "ggplot2", "openxlsx"
))
```

La pipeline usa inoltre `nbastatR`, da installare secondo la documentazione
del progetto upstream.

## Configurazione del bot

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
cp config/.env.example .env
```

Compilare `.env` soltanto sul computer locale:

```dotenv
TELEGRAM_BOT_TOKEN=
ALLOWED_CHAT_ID=
OUTPUT_DIR=output
```

Il file `.env` è ignorato da Git. Non inserire mai questi valori nel codice,
nei test, nei log o nella documentazione.

## Dati locali richiesti

Collocare nella root, senza aggiungerli a Git:

- `dati_nba.rds`: game log dei giocatori;
- `dati_nba_t.rds`: game log delle squadre;
- `box_data.rds`: contenitore con `dataBoxScore[[1]]`.

Chiavi di deduplica:

- giocatori e box score: `idGame + idPlayer`;
- squadre: `idGame + slugTeam`.

Gli script conservano le stagioni passate. `yearSeason` identifica la stagione
NBA tramite l'anno finale: per esempio `2027` corrisponde alla stagione
2026/27.

## Flusso operativo

Eseguire i comandi dalla root del repository.

### 1. Aggiornare roster e calendario

```bash
Rscript r/scarica_roster_nba_com.R \
  --output=data/cache/roster_nba_com_2026_27.csv

Rscript r/scarica_calendario_2026_27.R
```

### 2. Incorporare le gare concluse

```bash
Rscript r/aggiorna_intervallo_con_box.R \
  --dal=2026-10-20 \
  --al=2026-10-24 \
  --stagione=2027
```

Il comando crea backup locali prima di pubblicare gli archivi deduplicati.

### 3. Addestrare e pubblicare i modelli

Il training completo è costoso e normalmente viene eseguito una volta alla
settimana, dopo l'aggiornamento degli archivi:

```bash
Rscript r/addestra_da_calibrazione.R --data=2026-10-21
```

La data determina la settimana ISO degli artefatti in `models/`. Feature,
iperparametri e calibrazione sono selezionati senza utilizzare il test set.

### 4. Creare le feature pre-partita

```bash
DATA_STIMA=2026-10-21

Rscript r/crea_feature_prepartita.R \
  --data="$DATA_STIMA" \
  --output="data/cache/feature_prepartita_${DATA_STIMA}.csv"
```

### 5. Pubblicare le previsioni

```bash
Rscript r/genera_previsioni_bot.R \
  --data="$DATA_STIMA" \
  --settimana=2026-W43
```

Il risultato è `output/previsioni_YYYY-MM-DD.csv`. L'integrazione degli stili
e dei confronti cluster è facoltativa: se il relativo dataset locale non è
presente, le previsioni standard continuano a essere pubblicate.

### 6. Avviare il bot

```bash
source .venv/bin/activate
python3 -m bot.main
```

Comandi Telegram:

- `/start`: mostra la guida essenziale;
- `/oggi`: carica l'export della data corrente;
- `/data YYYY-MM-DD`: consulta un export per una data specifica.

Il bot usa il polling e funziona soltanto mentre il processo locale e il
computer sono accesi.

## Contratto dell'export

Ogni riga del CSV identifica un giocatore e una partita. Sono richiesti dati
partita/squadra/giocatore, le coppie `exp_*_q30` e `exp_*`, `eligible_bot` e
la data di generazione. Il repository Python rifiuta file vuoti, date
incoerenti, ID duplicati e valori non finiti.

## Test

Test Python:

```bash
python3 -m unittest discover -s tests -p 'test_*.py'
```

Test R:

```bash
for test_file in tests/test_*.R; do Rscript "$test_file"; done
```

`test_prepara_dati.R` esegue anche l'integrazione con gli RDS reali quando
questi sono disponibili; in una clone priva di dati viene saltato in modo
esplicito.

## Sicurezza e privacy

- mantenere privata la repository se contiene logica o feature proprietarie;
- verificare `git status` prima di ogni commit;
- non forzare mai l'aggiunta di file ignorati;
- ruotare immediatamente un token se compare accidentalmente nella cronologia;
- non pubblicare output reali senza avere verificato che gli identificativi e
  i dati contenuti siano adatti alla condivisione.

## Avvertenza

Le previsioni sono stime statistiche sperimentali, non garanzie e non consigli
finanziari o di scommessa.
