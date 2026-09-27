# 00 - Download dei dati pubblici utilizzati nel capitolo
#
# Non serve rieseguirlo: i file scaricati sono gia nella cartella dati.
# Tutte le fonti sono gratuite e non richiedono registrazione o chiavi di accesso.
#
# Output: dati/era5_milano.csv, dati/era5_torino.csv, dati/era5_verona.csv, dati/era5_bologna.csv
#         dati/euribor_6m_bce.csv
#         dati/ghcn_ITE00100554.csv (Milano Brera), dati/ghcn_ITM00016064.csv (Cameri)

# directory di lavoro: la cartella codice_R (vedi LEGGIMI)
cartella <- "dati"

# ---- 1. Temperature ERA5 tramite Open-Meteo ---------------------------------
# models=era5 impone la reanalisi ERA5 (griglia 0,25 gradi); il servizio restituisce
# il punto di griglia piu vicino alle coordinate richieste.
# timezone=Europe/Berlin: il giorno va dalla mezzanotte alla mezzanotte ora locale.
citta <- data.frame(nome = c("milano", "torino", "verona", "bologna"),
                    lat = c(45.45, 45.07, 45.40, 44.53),
                    lon = c(9.28, 7.68, 10.88, 11.30))

for (i in 1:nrow(citta)) {
  indirizzo <- paste0("https://archive-api.open-meteo.com/v1/archive",
                      "?latitude=", citta$lat[i], "&longitude=", citta$lon[i],
                      "&start_date=1974-01-01&end_date=2026-08-31",
                      "&daily=temperature_2m_max,temperature_2m_min",
                      "&timezone=Europe%2FBerlin&models=era5&format=csv")
  # le prime tre righe del file contengono coordinate e quota del punto di griglia
  d <- read.csv(indirizzo, skip = 3)
  names(d) <- c("date", "tmax", "tmin")
  write.csv(d, paste0(cartella, "/era5_", citta$nome[i], ".csv"), row.names = FALSE)
  Sys.sleep(90)  # pausa per rispettare i limiti del servizio gratuito (richieste al minuto)
}

# ---- 2. Euribor a 6 mesi, media mensile (portale dati BCE) ------------------
bce <- read.csv(paste0("https://data-api.ecb.europa.eu/service/data/FM/",
                       "M.U2.EUR.RT.MM.EURIBOR6MD_.HSTA?format=csvdata&startPeriod=2000-01"))
euribor <- data.frame(mese = bce$TIME_PERIOD, tasso = bce$OBS_VALUE)
write.csv(euribor, paste0(cartella, "/euribor_6m_bce.csv"), row.names = FALSE)

# ---- 3. Stazioni meteorologiche GHCN-Daily (NOAA) ---------------------------
# servono solo per verificare la completezza delle serie di stazione
for (stazione in c("ITE00100554", "ITM00016064")) {
  download.file(paste0("https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/",
                       stazione, ".csv"),
                paste0(cartella, "/ghcn_", stazione, ".csv"), mode = "wb")
}
