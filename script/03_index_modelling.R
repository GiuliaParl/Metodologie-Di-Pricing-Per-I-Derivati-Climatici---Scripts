# 03 - Index modelling: scelta della distribuzione dell'indice
#
# Input:  risultati/indice_hdd_stima.csv (prodotto dallo script 02)
# Output: risultati/tab_distribuzioni.csv
#         risultati/03_distribuzioni.txt
#         grafici/figura10_distribuzioni.png

# directory di lavoro: la cartella codice_R (vedi LEGGIMI)
library(MASS)
library(ggplot2)

stima <- read.csv("risultati/indice_hdd_stima.csv")
x <- stima$HDD_detrend

# stima di massima verosimiglianza delle due distribuzioni candidate
fit_normale <- fitdistr(x, "normal")
fit_lognormale <- fitdistr(x, "lognormal")

# criterio di Akaike: AIC = 2k - 2 log-verosimiglianza (k = 2 parametri)
# il valore piu basso indica l'adattamento migliore
aic_normale <- 2 * 2 - 2 * fit_normale$loglik
aic_lognormale <- 2 * 2 - 2 * fit_lognormale$loglik

tab <- data.frame(
  distribuzione = c("Normale", "Lognormale"),
  parametro_1 = c(fit_normale$estimate[1], fit_lognormale$estimate[1]),
  parametro_2 = c(fit_normale$estimate[2], fit_lognormale$estimate[2]),
  log_verosimiglianza = c(fit_normale$loglik, fit_lognormale$loglik),
  AIC = c(aic_normale, aic_lognormale)
)
write.csv(tab, "risultati/tab_distribuzioni.csv", row.names = FALSE)

# test di Shapiro-Wilk: ipotesi nulla = i dati provengono da una normale
test_normalita <- shapiro.test(x)

capture.output(
  print(tab),
  test_normalita,
  file = "risultati/03_distribuzioni.txt"
)

# Figura 10: istogramma dell'indice senza trend e le due densita stimate
griglia <- seq(min(x) - 100, max(x) + 100, length.out = 300)
curve <- data.frame(
  hdd = c(griglia, griglia),
  densita = c(dnorm(griglia, fit_normale$estimate[1], fit_normale$estimate[2]),
              dlnorm(griglia, fit_lognormale$estimate[1], fit_lognormale$estimate[2])),
  distribuzione = rep(c("Normale", "Lognormale"), each = length(griglia))
)

grafico <- ggplot() +
  geom_histogram(data = data.frame(x = x), aes(x = x, y = after_stat(density)),
                 bins = 12, fill = "grey85", colour = "white") +
  geom_line(data = curve, aes(x = hdd, y = densita, colour = distribuzione,
                              linetype = distribuzione), linewidth = 0.8) +
  scale_colour_manual(values = c("Normale" = "#1f4e79", "Lognormale" = "#c00000")) +
  labs(x = "Indice HDD corretto per il trend (punti indice)", y = "Densità",
       colour = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("grafici/figura10_distribuzioni.png", grafico, width = 16, height = 9, units = "cm", dpi = 300)
