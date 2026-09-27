# Esegue in ordine tutti gli script del Capitolo 3.
# Ogni script legge i risultati di quelli precedenti dalla cartella "risultati".

# Prima di eseguire impostare come directory di lavoro la cartella principale del repository

dir.create("risultati", showWarnings = FALSE)
dir.create("grafici", showWarnings = FALSE)

# I dati sono gia nella cartella "dati". Per scaricarli di nuovo dalle fonti
# originali togliere il commento alla riga seguente (richiede una connessione).
# source("script/00_scarica_dati.R")

source("script/01_dati_temperatura.R")
source("script/02_indice_hdd.R")
source("script/03_index_modelling.R")
source("script/04_daily_modelling.R")
source("script/05_prezzi.R")
source("script/06_backtest.R")
source("script/07_copertura_basis_risk.R")

cat("Elaborazione completata.\n")
