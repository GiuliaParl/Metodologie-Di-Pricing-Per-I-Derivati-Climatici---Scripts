# 06 - Verifica fuori campione: stagioni dal 2000/01 al 2025/26
#
# Per ogni stagione uso solo le stagioni precedenti e confronto la burn analysis
# senza correzione del trend con quella con correzione del trend.
# Il premio e attualizzato con l'Euribor a 6 mesi di ottobre di ciascun anno.
#
# Input:  risultati/indice_hdd.csv (prodotto dallo script 02)
#         dati/euribor_6m_bce.csv  (portale dati BCE)
# Output: risultati/tab_backtest.csv
#         risultati/06_backtest.txt
#         grafici/figura13_backtest.png

# directory di lavoro: la cartella codice_R (vedi LEGGIMI)
library(ggplot2)

indice <- read.csv("risultati/indice_hdd.csv")
euribor <- read.csv("dati/euribor_6m_bce.csv")

tick <- 20

payoff_put <- function(indice_hdd, strike_put) {
  tick * pmax(strike_put - indice_hdd, 0)
}

risultati <- data.frame()

for (s in 2000:2025) {
  passato <- subset(indice, stagione < s)
  realizzato <- indice$HDD[indice$stagione == s]

  # tasso e fattore di sconto disponibili al 31 ottobre dell'anno s
  r <- euribor$tasso[euribor$mese == paste0(s, "-10")] / 100
  sconto <- exp(-r * 151 / 365)

  # strike: media delle ultime dieci stagioni, arrotondata alla decina
  K <- round(mean(tail(passato$HDD, 10)) / 10) * 10

  # senza correzione del trend
  previsione_grezza <- mean(passato$HDD)
  premio_grezzo <- sconto * mean(payoff_put(passato$HDD, K))

  # con correzione del trend
  passato$t <- passato$stagione - 1973
  trend <- lm(HDD ~ t, data = passato)
  previsione_detrend <- coef(trend)[1] + coef(trend)[2] * (s - 1973)
  detrend <- passato$HDD - fitted(trend) + previsione_detrend
  premio_detrend <- sconto * mean(payoff_put(detrend, K))

  # risultato di chi vende la put: premio incassato meno payoff pagato (attualizzato)
  pagato <- sconto * payoff_put(realizzato, K)

  riga <- data.frame(stagione = s, tasso = r, strike = K, realizzato = realizzato,
                     previsione_grezza = previsione_grezza,
                     previsione_detrend = previsione_detrend,
                     premio_grezzo = premio_grezzo, premio_detrend = premio_detrend,
                     payoff = pagato,
                     risultato_grezzo = premio_grezzo - pagato,
                     risultato_detrend = premio_detrend - pagato)
  risultati <- rbind(risultati, riga)
}

risultati$errore_grezzo <- risultati$previsione_grezza - risultati$realizzato
risultati$errore_detrend <- risultati$previsione_detrend - risultati$realizzato
write.csv(risultati, "risultati/tab_backtest.csv", row.names = FALSE)

sintesi <- data.frame(
  impostazione = c("Senza correzione del trend", "Con correzione del trend"),
  errore_medio = c(mean(risultati$errore_grezzo), mean(risultati$errore_detrend)),
  errore_assoluto_medio = c(mean(abs(risultati$errore_grezzo)), mean(abs(risultati$errore_detrend))),
  stagioni_sovrastimate = c(sum(risultati$errore_grezzo > 0), sum(risultati$errore_detrend > 0)),
  premio_medio = c(mean(risultati$premio_grezzo), mean(risultati$premio_detrend)),
  risultato_cumulato = c(sum(risultati$risultato_grezzo), sum(risultati$risultato_detrend)),
  stagioni_in_perdita = c(sum(risultati$risultato_grezzo < 0), sum(risultati$risultato_detrend < 0))
)

capture.output(
  print(sintesi),
  cat("\nPayoff medio pagato:", mean(risultati$payoff), "\n"),
  cat("Stagioni in cui la put e stata esercitata:", sum(risultati$payoff > 0), "su", nrow(risultati), "\n"),
  cat("Strike minimo e massimo:", range(risultati$strike), "\n"),
  cat("Euribor di ottobre minimo e massimo (%):", 100 * range(risultati$tasso), "\n"),
  file = "risultati/06_backtest.txt"
)

# Figura 13: risultato cumulato del venditore della put
risultati$cumulato_grezzo <- cumsum(risultati$risultato_grezzo)
risultati$cumulato_detrend <- cumsum(risultati$risultato_detrend)

grafico <- ggplot(risultati, aes(x = stagione)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(aes(y = cumulato_grezzo, colour = "Senza correzione del trend"), linewidth = 0.9) +
  geom_line(aes(y = cumulato_detrend, colour = "Con correzione del trend"), linewidth = 0.9) +
  scale_colour_manual(values = c("Senza correzione del trend" = "#1f4e79",
                                 "Con correzione del trend" = "#c00000")) +
  labs(x = "Stagione invernale (anno di inizio)",
       y = "Risultato cumulato del venditore (euro)", colour = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("grafici/figura13_backtest.png", grafico, width = 16, height = 9, units = "cm", dpi = 300)
