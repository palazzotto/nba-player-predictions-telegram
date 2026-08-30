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

## Cosa mostra davvero il bot

Il percorso principale dell'interfaccia è:

```text
/oggi o /data → partita → squadra → giocatore → previsioni
```

Per ciascuna statistica il bot mostra una coppia nel formato:

```text
stima cautelativa Q30 – stima centrale
```

Per esempio, `Punti: 16.4–20.1` significa che `16.4` è la soglia
cautelativa Q30 e `20.1` è la previsione centrale già corretta dal bias. Non
è un intervallo di confidenza classico, non è una linea bookmaker e non
esprime una probabilità calcolata dal bot.

### Stime cautelative Q30

La Q30 viene ottenuta in R dalla distribuzione dei residui
`valore osservato - previsione grezza` del validation set. La correzione è
calcolata separatamente per fasce di previsione — bassa, media e alta, con
soglie specifiche per ciascun target — e poi applicata alle nuove stime.

In termini operativi, la Q30 mira a essere una soglia che il valore reale
raggiunga o superi in circa il 70% dei casi comparabili osservati in
validazione. È un obiettivo empirico di calibrazione, non una garanzia sul
singolo giocatore o sulla singola partita. Il test set resta esterno alla
calibrazione e serve soltanto a misurare le prestazioni finali.

### Modelli sequenziali e controllo del leakage

I sette modelli Ranger non sono indipendenti: vengono eseguiti in sequenza e
alcune stime cautelative già prodotte diventano meta-feature dei target
successivi. In particolare:

- punti, assist e rimbalzi dipendono anche dai minuti previsti;
- punti + assist usa le stime di punti, minuti e assist;
- punti + rimbalzi usa le stime di punti, minuti e rimbalzi;
- rimbalzi + assist usa le stime di rimbalzi e assist.

Per evitare che questa sequenza introduca informazioni future, nel train le
meta-feature sono prodotte con predizioni out-of-fold cronologiche a finestra
espansiva. Ogni fold è addestrato soltanto su partite antecedenti alle
osservazioni da prevedere. Validation e test rimangono successivi al train e
separati nel tempo nella proporzione 70% / 15% / 15%.

La selezione conserva 40 feature distinte per ciascun target. Gli artefatti
sono associati alla settimana ISO e vengono riutilizzati nelle esecuzioni
giornaliere della stessa settimana.

### Idoneità dei giocatori

La pipeline mantiene una previsione soltanto se il giocatore appartiene al
roster NBA.com corrente e possiede lo storico minimo richiesto dal modello.
Un giocatore diventa selezionabile nel bot quando:

- ha almeno 6 partite nella stagione usate come storico;
- la stima cautelativa dei minuti, `exp_min_q30`, è almeno 12;
- tutte le sette stime Q30 e tutte le sette stime centrali sono finite e
  disponibili.

Lo stato di infortunio non viene usato per escludere automaticamente un
giocatore. La decisione viene esportata da R nel campo `eligible_bot`; il bot
Python si limita a rispettarla.

## Cluster e stili di gioco

I cluster sono un'integrazione facoltativa e non modificano le sette
previsioni del giocatore. Il progetto gestisce tre stili:

1. `Perimetrali`;
2. `Lunghi`;
3. `Creatori`.

Un dataset RDS locale può associare `idPlayer` a `Stile_gioco`. Quando viene
fornito alla generazione giornaliera, il nome dello stile compare accanto al
giocatore nei pulsanti Telegram. Senza questo file, le previsioni standard
continuano a funzionare e la colonna `stile_cluster` resta vuota.

Per ogni partita è inoltre disponibile una schermata di confronto per
cluster. Il bot mette a confronto:

- attacco della squadra di casa contro difesa della squadra in trasferta;
- attacco della squadra in trasferta contro difesa della squadra di casa.

Per entrambe le direzioni mostra media e ranking per 12 metriche: FGM, FGA,
3PM, 3PA, 2PM, 2PA, FTM, FTA, rimbalzi offensivi, assist, palle perse e punti.
I pulsanti permettono di passare dal cluster precedente al successivo senza
abbandonare la partita.

R prepara questi confronti a partire da un workbook locale con esattamente i
sei fogli `Offensivo C1`, `Difensivo C1`, `Offensivo C2`, `Difensivo C2`,
`Offensivo C3` e `Difensivo C3`. Python legge soltanto il CSV risultante: non
apre l'Excel e non calcola medie o ranking. Il dataset cluster, il modello che
lo ha generato e il workbook con medie/rank non sono inclusi nel repository.

## Architettura

```text
NBA.com + nbastatR + archivi RDS locali
                  |
                  v
       pipeline e modelli R
             /          \
            v            v
 previsioni giornaliere  confronti cluster opzionali
            \            /
             v          v
       CSV atomici in output/
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

Integrazioni cluster facoltative, anch'esse solo locali:

- un RDS con le colonne univoche `idPlayer` e `Stile_gioco`;
- `medie_cluster_con_rank.xlsx`, con i sei fogli offensivi e difensivi
  descritti nella sezione dedicata.

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

Per aggiungere lo stile ai giocatori, indicare esplicitamente il dataset
locale:

```bash
Rscript r/genera_previsioni_bot.R \
  --data="$DATA_STIMA" \
  --settimana=2026-W43 \
  --cluster=Cluster/dati_cluster_completi.rds
```

### 6. Pubblicare i confronti cluster facoltativi

Dopo avere creato le previsioni della giornata:

```bash
Rscript r/esporta_confronti_cluster.R \
  --data "$DATA_STIMA" \
  --workbook Cluster/medie_cluster_con_rank.xlsx \
  --output-dir output
```

Il risultato è `output/confronti_cluster_YYYY-MM-DD.csv`. Se questo file
manca, il bot mostra un messaggio specifico soltanto quando si apre la
schermata cluster; la navigazione delle previsioni dei giocatori resta
disponibile.

### 7. Avviare il bot

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

Le coppie obbligatorie sono:

| Statistica | Q30 cautelativa | Stima centrale |
|---|---|---|
| Minuti | `exp_min_q30` | `exp_min` |
| Punti | `exp_pts_q30` | `exp_pts` |
| Assist | `exp_ast_q30` | `exp_ast` |
| Rimbalzi | `exp_treb_q30` | `exp_treb` |
| Punti + assist | `exp_pts_ast_q30` | `exp_pts_ast` |
| Punti + rimbalzi | `exp_pts_treb_q30` | `exp_pts_treb` |
| Rimbalzi + assist | `exp_treb_ast_q30` | `exp_treb_ast` |

`PTS+REB+AST` può esistere nelle analisi esplorative, ma non viene esportata
né mostrata nell'interfaccia Telegram.

Gli export delle previsioni, dei confronti cluster e degli artefatti vengono
scritti prima in un file temporaneo, validati e poi rinominati. Un file
mancante o obsoleto non fa partire automaticamente download o training dal
bot.

## Cosa non è incluso

La repository contiene il codice e i test, ma non è un pacchetto pronto a
produrre risultati identici senza gli artefatti locali. Restano esclusi:

- game log e box score storici;
- cache di calendario e roster;
- modelli Ranger addestrati e liste di feature settimanali;
- dataset e workbook dei cluster;
- previsioni giornaliere e registro delle stime emesse;
- token Telegram, chat ID e qualsiasi altra configurazione privata.

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
