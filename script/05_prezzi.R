# 05 - Prezzo della put con le tre tecniche di pricing e sensibilita al trend
#
# Input:  risultati/indice_hdd.csv, risultati/indice_hdd_stima.csv,
#         risultati/livello_trend.csv                                (script 02)
#         risultati/tab_distribuzioni.csv                            (script 03)
#         risultati/simulazioni_hdd_2025.csv                         (script 04)
# Output: risultati/tab_prezzi.csv
#         risultati/tab_sensibilita.csv
#         risultati/05_prezzi.txt
#         grafici/figura12_montecarlo.png

# directory di lavoro: la cartella principale del repository 
library(ggplot2)

indice <- read.csv("risultati/indice_hdd.csv")
stima <- read.csv("risultati/indice_hdd_stima.csv")
livello <- read.csv("risultati/livello_trend.csv")
distribuzioni <- read.csv("risultati/tab_distribuzioni.csv")
simulazioni <- read.csv("risultati/simulazioni_hdd_2025.csv")

# parametri del contratto e di mercato
tick <- 20                     # euro per punto indice (contratti europei CME)
r <- 0.0211                    # Euribor 6 mesi, media di ottobre 2025 (BCE): 2,1068%
tau <- 151 / 365               # dal 31/10/2025 al 31/03/2026
sconto <- exp(-r * tau)

# strike at the money: valore atteso dell'indice senza trend, arrotondato alla decina
K <- round(mean(stima$HDD_detrend) / 10) * 10
strike <- c(K - 50, K, K + 50)

payoff_put <- function(indice_hdd, strike_put) {
  tick * pmax(strike_put - indice_hdd, 0)
}

# prezzo = media dei payoff attualizzata; errore standard = dev. standard / radice di n
prezzo <- function(valori, k) sconto * mean(payoff_put(valori, k))
errore <- function(valori, k) sconto * sd(payoff_put(valori, k)) / sqrt(length(valori))

# per l'index modelling estraggo 100.000 valori da ciascuna distribuzione stimata
set.seed(2026)
normale <- rnorm(100000, distribuzioni$parametro_1[1], distribuzioni$parametro_2[1])
lognormale <- rlnorm(100000, distribuzioni$parametro_1[2], distribuzioni$parametro_2[2])

prezzi <- data.frame(strike = strike, burn_grezza = NA, burn_detrend = NA,
                     index_normale = NA, index_lognormale = NA, daily = NA)

for (i in 1:3) {
  k <- strike[i]
  prezzi$burn_grezza[i] <- prezzo(stima$HDD, k)
  prezzi$burn_detrend[i] <- prezzo(stima$HDD_detrend, k)
  prezzi$index_normale[i] <- prezzo(normale, k)
  prezzi$index_lognormale[i] <- prezzo(lognormale, k)
  prezzi$daily[i] <- prezzo(simulazioni$HDD, k)
}
write.csv(prezzi, "risultati/tab_prezzi.csv", row.names = FALSE)

# valore atteso dell'indice secondo ciascuna tecnica
valore_atteso <- c(
  burn_grezza = mean(stima$HDD),
  burn_detrend = mean(stima$HDD_detrend),
  index_normale = distribuzioni$parametro_1[1],
  index_lognormale = exp(distribuzioni$parametro_1[2] + distribuzioni$parametro_2[2]^2 / 2),
  daily = mean(simulazioni$HDD)
)

# incertezza: errore standard per la burn analysis (51 stagioni)
# ed errore di simulazione per il daily modelling (10.000 stagioni)
errore_burn <- errore(stima$HDD_detrend, K)
intervallo_burn <- prezzi$burn_detrend[2] + c(-1.96, 1.96) * errore_burn
errore_montecarlo <- errore(simulazioni$HDD, K)

# caricamento per il rischio: P = D * (media + alfa * dev. standard)
payoff_storici <- payoff_put(stima$HDD_detrend, K)
alfa <- c(0, 0.25, 0.5)
prezzo_caricato <- sconto * (mean(payoff_storici) + alfa * sd(payoff_storici))

# probabilita che la put venga esercitata
probabilita <- c(burn_grezza = mean(stima$HDD < K),
                 burn_detrend = mean(stima$HDD_detrend < K),
                 daily = mean(simulazioni$HDD < K))

# sensibilita al trattamento del trend (stesso strike K = 1.520)
# a) burn analysis sulle sole stagioni recenti, come nella prassi (10-30 anni)
# b) serie corretta con il livello della retta spostato di un errore standard
serie_alternative <- list(
  "Tutte le 51 stagioni osservate" = stima$HDD,
  "Ultime 30 stagioni osservate" = tail(stima$HDD, 30),
  "Ultime 20 stagioni osservate" = tail(stima$HDD, 20),
  "Ultime 10 stagioni osservate" = tail(stima$HDD, 10),
  "Serie corretta, livello centrale" = stima$HDD_detrend,
  "Serie corretta, livello +1 errore standard" = stima$HDD_detrend + livello$errore_standard,
  "Serie corretta, livello -1 errore standard" = stima$HDD_detrend - livello$errore_standard
)

sensibilita <- data.frame()
for (nome in names(serie_alternative)) {
  x <- serie_alternative[[nome]]
  sensibilita <- rbind(sensibilita, data.frame(
    impostazione = nome, stagioni = length(x), valore_atteso = mean(x),
    prezzo_put = prezzo(x, K), errore_standard = errore(x, K)))
}
write.csv(sensibilita, "risultati/tab_sensibilita.csv", row.names = FALSE)

# verifica sulla stagione 2025/26
realizzato <- indice$HDD[indice$stagione == 2025]

capture.output(
  cat("Fattore di sconto:", sconto, "| strike at the money:", K, "\n\n"),
  print(prezzi),
  cat("\nValore atteso dell'indice:\n"), print(valore_atteso),
  cat("\nProbabilita di esercizio:\n"), print(probabilita),
  cat("\nBurn analysis corretta, strike", K, ": errore standard =", errore_burn,
      "| intervallo al 95% =", intervallo_burn, "\n"),
  cat("Daily modelling, strike", K, ": errore di simulazione =", errore_montecarlo, "\n"),
  cat("\nPrezzo con caricamento, alfa = 0 / 0.25 / 0.5:", prezzo_caricato, "\n\n"),
  print(sensibilita),
  cat("\nStagione 2025/26: indice realizzato =", realizzato,
      "| payoff della put =", payoff_put(realizzato, K), "\n"),
  cat("Scarto tra valore atteso e realizzato:\n"), print(valore_atteso - realizzato),
  file = "risultati/05_prezzi.txt"
)

# Figura 12: stagioni simulate (istogramma), distribuzione storica senza trend
# (curva rossa), strike (linea punteggiata), valore realizzato (linea verde)
grafico <- ggplot() +
  geom_histogram(data = simulazioni, aes(x = HDD, y = after_stat(density)),
                 bins = 40, fill = "#8faadc", colour = "white") +
  geom_density(data = stima, aes(x = HDD_detrend), colour = "#c00000", linewidth = 0.9) +
  geom_vline(xintercept = K, linetype = "dotted") +
  geom_vline(xintercept = realizzato, colour = "#2e7d32", linewidth = 0.9) +
  labs(x = "Indice HDD stagionale (°C·giorno)", y = "Densità") +
  theme_minimal()

ggsave("grafici/figura12_montecarlo.png", grafico, width = 16, height = 9, units = "cm", dpi = 300)
