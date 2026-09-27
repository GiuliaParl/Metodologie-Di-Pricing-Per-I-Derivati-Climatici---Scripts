# 01 - Dati di temperatura: controlli, statistiche descrittive e trend
#
# Input:  dati/era5_milano.csv
#         (ERA5 scaricato da Open-Meteo, punto di griglia 45,5 N - 9,25 E,
#          dal 1/1/1974 al 31/8/2026, colonne: date, tmax, tmin)
#         dati/ghcn_ITE00100554.csv, dati/ghcn_ITM00016064.csv (stazioni NOAA)
# Output: risultati/temperatura_giornaliera.csv
#         risultati/tab1_medie_mensili.csv
#         risultati/01_statistiche.txt
#         grafici/figura8_temperatura.png

# directory di lavoro: la cartella codice_R (vedi LEGGIMI)
library(ggplot2)

dati <- read.csv("dati/era5_milano.csv")
dati$date <- as.Date(dati$date)

# controlli sul file: giorni mancanti, date ripetute, valori assenti o incoerenti
tutti_i_giorni <- seq(min(dati$date), max(dati$date), by = "day")
controlli <- c(osservazioni = nrow(dati),
               giorni_nel_periodo = length(tutti_i_giorni),
               date_ripetute = sum(duplicated(dati$date)),
               valori_mancanti = sum(is.na(dati$tmax) | is.na(dati$tmin)),
               giorni_con_tmax_minore_di_tmin = sum(dati$tmax < dati$tmin, na.rm = TRUE))

# temperatura media giornaliera: media tra massima e minima
dati$DAT <- (dati$tmax + dati$tmin) / 2

dati$anno <- as.numeric(format(dati$date, "%Y"))
dati$mese <- as.numeric(format(dati$date, "%m"))

# statistiche sull'intero periodo
statistiche <- data.frame(
  media = mean(dati$DAT),
  dev_standard = sd(dati$DAT),
  minimo = min(dati$DAT),
  massimo = max(dati$DAT)
)

# Tabella 1: media e deviazione standard per mese, solo anni completi (1974-2025)
anni_completi <- subset(dati, anno <= 2025)
media_mese <- aggregate(DAT ~ mese, data = anni_completi, FUN = mean)
dev_mese <- aggregate(DAT ~ mese, data = anni_completi, FUN = sd)
tab1 <- data.frame(mese = media_mese$mese,
                   media = round(media_mese$DAT, 2),
                   dev_standard = round(dev_mese$DAT, 2))
write.csv(tab1, "risultati/tab1_medie_mensili.csv", row.names = FALSE)

# trend di lungo periodo: regressione della media annua sull'anno (1974-2025)
medie_annue <- aggregate(DAT ~ anno, data = anni_completi, FUN = mean)
trend <- lm(DAT ~ anno, data = medie_annue)

# completezza delle stazioni GHCN nel periodo 2005-2024:
# quota di giorni in cui sono presenti sia la massima sia la minima
copertura <- function(file) {
  s <- read.csv(file)
  presenti <- !is.na(as.numeric(s$TMAX)) & !is.na(as.numeric(s$TMIN))
  giorni <- s$DATE[presenti]
  c(ultimo_giorno_completo = max(giorni),
    copertura_2005_2024 = round(100 * sum(giorni >= "2005-01-01" & giorni <= "2024-12-31") / 7305, 1))
}

capture.output(
  cat("Controlli sul file ERA5:\n"), print(controlli),
  cat("\nStatistiche della temperatura media giornaliera:\n"), print(statistiche),
  cat("\n"), print(tab1),
  summary(trend),
  cat("Riscaldamento stimato per decennio (gradi C):", 10 * coef(trend)["anno"], "\n\n"),
  cat("Stazione Milano Brera (ITE00100554):\n"), print(copertura("dati/ghcn_ITE00100554.csv")),
  cat("Stazione Cameri (ITM00016064):\n"), print(copertura("dati/ghcn_ITM00016064.csv")),
  file = "risultati/01_statistiche.txt"
)

# dati giornalieri per gli script successivi
write.csv(dati[, c("date", "tmax", "tmin", "DAT")],
          "risultati/temperatura_giornaliera.csv", row.names = FALSE)

# Figura 8: temperatura giornaliera (grigio), media annua (blu), trend (rosso)
medie_annue$data <- as.Date(paste0(medie_annue$anno, "-07-01"))

et_gio <- "Temperatura media giornaliera"
et_ann <- "Media annua"
et_trend <- "Retta di tendenza"
ordine <- c(et_gio, et_ann, et_trend)

grafico <- ggplot() +
  geom_line(data = dati, aes(x = date, y = DAT, colour = et_gio, linetype = et_gio),
            linewidth = 0.2) +
  geom_line(data = medie_annue, aes(x = data, y = DAT, colour = et_ann, linetype = et_ann),
            linewidth = 0.9) +
  geom_smooth(data = medie_annue, aes(x = data, y = DAT, colour = et_trend, linetype = et_trend),
              method = "lm", formula = y ~ x, se = FALSE) +
  scale_colour_manual(values = setNames(c("grey75", "#1f4e79", "#c00000"), ordine),
                      breaks = ordine) +
  scale_linetype_manual(values = setNames(c("solid", "solid", "dashed"), ordine),
                        breaks = ordine) +
  labs(x = NULL, y = "Temperatura media giornaliera (°C)",
       colour = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("grafici/figura8_temperatura.png", grafico, width = 18, height = 9, units = "cm", dpi = 300)
