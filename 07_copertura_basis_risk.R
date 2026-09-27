# 07 - Efficacia della copertura e basis risk geografico
#
# Un'impresa a Milano, Torino, Verona o Bologna acquista la stessa put sull'indice
# di Milano. Per ogni citta si confronta la variabilita del margine senza e con la put.
#
# Input:  risultati/indice_hdd_stima.csv (prodotto dallo script 02)
#         dati/era5_milano.csv, dati/era5_torino.csv,
#         dati/era5_verona.csv, dati/era5_bologna.csv
# Output: risultati/tab_copertura.csv
#         risultati/07_copertura_basis_risk.txt

# directory di lavoro: la cartella codice_R (vedi LEGGIMI)

tick <- 20
sconto <- exp(-0.0211 * 151 / 365)

payoff_put <- function(indice_hdd, strike_put) {
  tick * pmax(strike_put - indice_hdd, 0)
}

# stessa procedura degli script 01 e 02, ripetuta per ogni citta:
# indice stagionale 1974/75-2024/25 e serie corretta per il trend
indice_corretto <- function(file) {
  d <- read.csv(file)
  d$date <- as.Date(d$date)
  d$DAT <- (d$tmax + d$tmin) / 2
  d$HDD <- pmax(18 - d$DAT, 0)
  anno <- as.numeric(format(d$date, "%Y"))
  mese <- as.numeric(format(d$date, "%m"))
  d$stagione <- NA
  d$stagione[mese >= 11] <- anno[mese >= 11]
  d$stagione[mese <= 3] <- anno[mese <= 3] - 1
  d <- subset(d, !is.na(stagione) & stagione >= 1974 & stagione <= 2024)
  indice <- aggregate(HDD ~ stagione, data = d, FUN = sum)
  indice$t <- indice$stagione - 1973
  trend <- lm(HDD ~ t, data = indice)
  indice$HDD_detrend <- indice$HDD - fitted(trend) + coef(trend)[1] + coef(trend)[2] * 52
  indice
}

# contratto: put sull'indice di Milano, strike e premio come nello script 05
stima <- read.csv("risultati/indice_hdd_stima.csv")
H_milano <- stima$HDD_detrend
K <- round(mean(H_milano) / 10) * 10
premio <- sconto * mean(payoff_put(H_milano, K))
payoff_milano <- payoff_put(H_milano, K)

citta <- c("milano", "torino", "verona", "bologna")
copertura <- data.frame()
correlazioni_senza_correzione <- c()

for (c in citta) {
  indice_citta <- indice_corretto(paste0("dati/era5_", c, ".csv"))
  H <- indice_citta$HDD_detrend

  # ipotesi: il margine dell'impresa varia di 20 euro per ogni punto dell'indice
  # della propria citta rispetto a un inverno medio
  margine_scoperto <- tick * (H - mean(H))
  # con la put su Milano: si aggiunge il payoff e si toglie il premio (portato a scadenza)
  margine_coperto <- margine_scoperto + payoff_milano - premio / sconto

  copertura <- rbind(copertura, data.frame(
    citta = c,
    correlazione_con_milano = cor(H, H_milano),
    dev_standard_scoperto = sd(margine_scoperto),
    dev_standard_coperto = sd(margine_coperto),
    riduzione_percentuale = 100 * (1 - sd(margine_coperto) / sd(margine_scoperto)),
    peggiore_scoperto = min(margine_scoperto),
    peggiore_coperto = min(margine_coperto)))

  correlazioni_senza_correzione[c] <- cor(indice_citta$HDD, stima$HDD)
}
write.csv(copertura, "risultati/tab_copertura.csv", row.names = FALSE)

capture.output(
  cat("Strike:", K, "| premio:", premio, "\n"),
  cat("Controllo: la serie corretta di Milano coincide con quella dello script 02:",
      all.equal(indice_corretto("dati/era5_milano.csv")$HDD_detrend, H_milano), "\n\n"),
  print(copertura),
  cat("\nCorrelazione con Milano calcolata sugli indici non corretti per il trend:\n"),
  print(correlazioni_senza_correzione),
  file = "risultati/07_copertura_basis_risk.txt"
)
