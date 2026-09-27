# 04 - Daily modelling con il modello di Alaton et al. (2002)
#
# Il modello ha tre componenti:
#   1) media stagionale Lambda(t) = a + b*t + c*sin(omega*t) + d*cos(omega*t)
#   2) ritorno verso la media: Y(t) = phi * Y(t-1) + errore,  con Y = T - S
#   3) volatilita costante all'interno di ogni mese
#
# Input:  risultati/temperatura_giornaliera.csv (prodotto dallo script 01)
# Output: risultati/tab_volatilita_mensile.csv
#         risultati/simulazioni_hdd_2025.csv   (10.000 stagioni simulate)
#         risultati/04_modello_giornaliero.txt
#         grafici/figura11_media_stagionale.png

# directory di lavoro: la cartella codice_R 
library(tseries)
library(moments)
library(ggplot2)

dati <- read.csv("risultati/temperatura_giornaliera.csv")
dati$date <- as.Date(dati$date)

# uso solo i dati disponibili alla data di valutazione (31 ottobre 2025)
stima <- subset(dati, date <= as.Date("2025-10-31"))
stima$t <- 1:nrow(stima)
stima$mese <- as.numeric(format(stima$date, "%m"))

# 1) media stagionale: trend lineare + una sinusoide con periodo di un anno
omega <- 2 * pi / 365.25
stima$seno <- sin(omega * stima$t)
stima$coseno <- cos(omega * stima$t)
modello_media <- lm(DAT ~ t + seno + coseno, data = stima)
stima$S <- fitted(modello_media)
ampiezza <- sqrt(coef(modello_media)["seno"]^2 + coef(modello_media)["coseno"]^2)

# 2) scostamenti dalla media stagionale e verifica di stazionarieta (test ADF)
stima$Y <- stima$DAT - stima$S
test_adf <- adf.test(stima$Y)

# ritorno verso la media: regressione di Y(t) su Y(t-1), senza intercetta
n <- nrow(stima)
Y_oggi <- stima$Y[2:n]
Y_ieri <- stima$Y[1:(n - 1)]
modello_ar <- lm(Y_oggi ~ Y_ieri - 1)
phi <- coef(modello_ar)[1]
kappa <- 1 - phi
emivita <- log(0.5) / log(phi)   # giorni necessari per dimezzare uno scostamento

# 3) volatilita mensile: deviazione standard degli errori mese per mese
errori <- residuals(modello_ar)
mese_errori <- stima$mese[2:n]
volatilita <- aggregate(errori ~ mese_errori, FUN = sd)
names(volatilita) <- c("mese", "sigma")
write.csv(volatilita, "risultati/tab_volatilita_mensile.csv", row.names = FALSE)

# controllo dei residui: li divido per la volatilita del loro mese
residui_std <- errori / volatilita$sigma[mese_errori]
autocorrelazione <- acf(residui_std, lag.max = 1, plot = FALSE)$acf[2]

# controllo della media stagionale: scostamento medio Y per mese
# (se la sinusoide descrive bene il ciclo annuale dovrebbe essere vicino a zero)
scostamento_mese <- aggregate(Y ~ mese, data = stima, FUN = mean)

# 4) simulazione Monte Carlo della stagione 2025/26
set.seed(2025)
n_sim <- 10000
giorni <- seq(as.Date("2025-11-01"), as.Date("2026-03-31"), by = "day")
t_futuro <- as.numeric(giorni - min(stima$date)) + 1

S_futuro <- coef(modello_media)[1] + coef(modello_media)[2] * t_futuro +
  coef(modello_media)[3] * sin(omega * t_futuro) +
  coef(modello_media)[4] * cos(omega * t_futuro)
sigma_futuro <- volatilita$sigma[as.numeric(format(giorni, "%m"))]

# tutte le traiettorie partono dallo scostamento osservato il 31 ottobre 2025
Y <- rep(stima$Y[n], n_sim)
HDD_simulato <- rep(0, n_sim)

for (g in 1:length(giorni)) {
  Y <- phi * Y + sigma_futuro[g] * rnorm(n_sim)
  temperatura <- S_futuro[g] + Y
  HDD_simulato <- HDD_simulato + pmax(18 - temperatura, 0)
}

write.csv(data.frame(HDD = HDD_simulato), "risultati/simulazioni_hdd_2025.csv", row.names = FALSE)

capture.output(
  cat("Osservazioni usate per la stima:", n, "\n\n"),
  summary(modello_media),
  cat("Trend per decennio (gradi C):", 3652.5 * coef(modello_media)["t"], "\n"),
  cat("Ampiezza del ciclo annuale (gradi C):", ampiezza, "\n\n"),
  test_adf,
  summary(modello_ar),
  cat("phi =", phi, "| kappa =", kappa, "| emivita (giorni) =", emivita, "\n\n"),
  print(volatilita),
  cat("\nResidui standardizzati: asimmetria =", skewness(residui_std),
      "| curtosi =", kurtosis(residui_std),
      "| autocorrelazione al primo ritardo =", autocorrelazione, "\n\n"),
  cat("Scostamento medio dalla media stagionale, per mese:\n"), print(scostamento_mese),
  cat("\n"),
  cat("Indice HDD simulato 2025/26: media =", mean(HDD_simulato),
      "| dev. standard =", sd(HDD_simulato), "\n"),
  cat("Quantili 5% e 95%:", quantile(HDD_simulato, c(0.05, 0.95)), "\n"),
  file = "risultati/04_modello_giornaliero.txt"
)

# Figura 11: temperatura osservata negli ultimi tre anni (grigio)
# e media stagionale stimata Lambda(t) (blu)
periodo <- subset(stima, date >= as.Date("2022-11-01"))

grafico <- ggplot(periodo, aes(x = date)) +
  geom_line(aes(y = DAT), colour = "grey65", linewidth = 0.3) +
  geom_line(aes(y = S), colour = "#1f4e79", linewidth = 1) +
  labs(x = NULL, y = "Temperatura media giornaliera (°C)") +
  theme_minimal()

ggsave("grafici/figura11_media_stagionale.png", grafico, width = 18, height = 8, units = "cm", dpi = 300)
