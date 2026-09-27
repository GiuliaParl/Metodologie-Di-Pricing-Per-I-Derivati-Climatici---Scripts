# 02 - Costruzione dell'indice HDD stagionale e analisi del trend
#
# Input:  risultati/temperatura_giornaliera.csv (prodotto dallo script 01)
# Output: risultati/indice_hdd.csv          (tutte le 52 stagioni)
#         risultati/indice_hdd_stima.csv    (51 stagioni, con serie senza trend)
#         risultati/livello_trend.csv       (livello previsto per il 2025/26 e suo errore)
#         risultati/02_indice_trend.txt
#         grafici/figura9_indice_hdd.png

# directory di lavoro: la cartella principale del repository 
library(ggplot2)

dati <- read.csv("risultati/temperatura_giornaliera.csv")
dati$date <- as.Date(dati$date)
anno <- as.numeric(format(dati$date, "%Y"))
mese <- as.numeric(format(dati$date, "%m"))

# gradi giorno di riscaldamento: di quanto la temperatura resta sotto i 18 gradi
dati$HDD <- pmax(18 - dati$DAT, 0)

# stagione invernale: novembre e dicembre appartengono alla stagione che inizia
# in quell'anno, gennaio-marzo alla stagione iniziata l'anno precedente
dati$stagione <- NA
dati$stagione[mese >= 11] <- anno[mese >= 11]
dati$stagione[mese <= 3] <- anno[mese <= 3] - 1
inverno <- subset(dati, !is.na(stagione))

# indice stagionale: somma degli HDD dal 1 novembre al 31 marzo
indice <- aggregate(HDD ~ stagione, data = inverno, FUN = sum)
conta_giorni <- aggregate(HDD ~ stagione, data = inverno, FUN = length)
indice$giorni <- conta_giorni$HDD

# tengo solo le stagioni complete (151 giorni, 152 negli anni bisestili):
# restano escluse la stagione 1973/74 (mancano novembre e dicembre 1973)
indice <- subset(indice, giorni >= 151)
write.csv(indice, "risultati/indice_hdd.csv", row.names = FALSE)

# 51 stagioni per la stima, la stagione 2025/26 per la verifica
stima <- subset(indice, stagione <= 2024)
verifica <- subset(indice, stagione == 2025)

# trend lineare dell'indice: t = 1 per la stagione 1974/75
stima$t <- stima$stagione - 1973
trend <- lm(HDD ~ t, data = stima)

# controllo: aggiungo un termine quadratico per vedere se il trend accelera
trend_quadratico <- lm(HDD ~ t + I(t^2), data = stima)

# controllo: autocorrelazione dei residui (le stagioni dovrebbero essere indipendenti)
autocorrelazione_residui <- acf(residuals(trend), lag.max = 1, plot = FALSE)$acf[2]

# livello previsto dalla retta per la stagione da valutare (2025/26, t = 52)
# e suo errore standard: misura quanto e incerta la posizione della retta
previsione <- predict(trend, newdata = data.frame(t = 52), se.fit = TRUE)
livello_2025 <- as.numeric(previsione$fit)
errore_livello <- as.numeric(previsione$se.fit)
write.csv(data.frame(livello = livello_2025, errore_standard = errore_livello),
          "risultati/livello_trend.csv", row.names = FALSE)

# rimozione del trend: ogni stagione conserva il suo scostamento dalla retta
# e viene riportata al livello previsto per il 2025/26
stima$HDD_detrend <- stima$HDD - fitted(trend) + livello_2025
write.csv(stima, "risultati/indice_hdd_stima.csv", row.names = FALSE)

descrivi <- function(x) {
  c(media = mean(x), dev_standard = sd(x), minimo = min(x), massimo = max(x),
    coeff_variazione = 100 * sd(x) / mean(x))
}

capture.output(
  cat("Stagioni complete:", nrow(indice), "| giorni per stagione:", unique(indice$giorni), "\n"),
  cat("Indice realizzato nella stagione 2025/26:", verifica$HDD, "\n\n"),
  cat("Indice osservato (51 stagioni):\n"), print(descrivi(stima$HDD)),
  cat("Stagione piu fredda:", stima$stagione[which.max(stima$HDD)],
      "| stagione piu mite:", stima$stagione[which.min(stima$HDD)], "\n"),
  cat("\nMedia prime 26 stagioni (1974/75-1999/00):", mean(stima$HDD[stima$stagione <= 1999]), "\n"),
  cat("Media ultime 25 stagioni (2000/01-2024/25):", mean(stima$HDD[stima$stagione >= 2000]), "\n"),
  cat("Media ultime 30 / 20 / 10 stagioni:", mean(tail(stima$HDD, 30)),
      mean(tail(stima$HDD, 20)), mean(tail(stima$HDD, 10)), "\n\n"),
  summary(trend),
  summary(trend_quadratico),
  cat("Autocorrelazione dei residui del trend (primo ritardo):", autocorrelazione_residui, "\n"),
  cat("Soglia approssimata al 95% (2/radice di 51):", 2 / sqrt(51), "\n\n"),
  cat("Livello previsto per il 2025/26:", livello_2025, "| errore standard:", errore_livello,
      "| intervallo al 95%:", livello_2025 - 1.96 * errore_livello, livello_2025 + 1.96 * errore_livello, "\n\n"),
  cat("Indice senza trend:\n"), print(descrivi(stima$HDD_detrend)),
  file = "risultati/02_indice_trend.txt"
)

# Figura 9: indice per stagione (blu), retta di tendenza (rosso),
# stagione di verifica 2025/26 (rombo verde)
grafico <- ggplot(stima, aes(x = stagione, y = HDD)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#c00000", fill = "grey85") +
  geom_line(colour = "#1f4e79") +
  geom_point(colour = "#1f4e79", size = 1.5) +
  geom_point(data = verifica, aes(x = stagione, y = HDD), colour = "#2e7d32", size = 3, shape = 18) +
  labs(x = "Stagione invernale (anno di inizio)", y = "Indice HDD stagionale (°C·giorno)") +
  theme_minimal()

ggsave("grafici/figura9_indice_hdd.png", grafico, width = 18, height = 9, units = "cm", dpi = 300)
