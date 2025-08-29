# ===========================================
# FX Live - Pilot (versión clara, sin funciones auxiliares)
# ===========================================

options(pillar.sigfig = 4)

library(tidyverse)
library(readxl)
library(modelsummary)
library(yaml)
library(lubridate)
library(zoo)
library(TTR)

# -------------------------------------------
# Parámetros
# -------------------------------------------

print(getwd())

params     <- read_yaml("../scripts/params.yaml")
start_date <- as.Date(params$start_date, format = "%Y-%m-%d")

# -------------------------------------------
# Carga de datos
# -------------------------------------------
df_data <- read_excel("../input/daily_data.xlsx", sheet = "GT") |>
  select(Fecha, D_Close) |>
  mutate(
    Fecha   = as.Date(Fecha),
    D_Close = signif(D_Close, 5)
  ) |>
  filter(Fecha >= start_date) |>
  arrange(Fecha)

# -------------------------------------------
# Métricas básicas (%D, %W) y YoY
# -------------------------------------------
# %D y %W con lag para no mirar al futuro
df_data <- df_data |>
  mutate(
    `%D` = D_Close / lag(D_Close, 1) - 1,
    `%W` = D_Close / lag(D_Close, 5) - 1
  )

# Assuming df_data is sorted by Fecha (ascending)
fecha_target <- df_data$Fecha %m-% years(1)

# Method 1: Using findInterval correctly
# findInterval gives the index of the last element <= target
# We need to ensure we get valid indices
idx_prior <- findInterval(fecha_target, df_data$Fecha, rightmost.closed = TRUE)
idx_prior <- pmax(idx_prior, 1)  # Ensure minimum index of 1
idx_prior <- ifelse(df_data$Fecha[idx_prior] <= fecha_target, idx_prior, NA_integer_)

D_Close_prev <- ifelse(!is.na(idx_prior), df_data$D_Close[idx_prior], NA_real_)

df_data <- df_data |>
  mutate(YoY = D_Close / D_Close_prev - 1)

# -------------------------------------------
# SMA, Desviaciones y Bandas de Bollinger (20/50/200)
# -------------------------------------------
df_data <- df_data |>
  mutate(
    SMA20  = TTR::SMA(D_Close, n = 20),
    SMA50  = TTR::SMA(D_Close, n = 50),
    SMA200 = TTR::SMA(D_Close, n = 200),

    SD20   = zoo::rollapply(D_Close, width = 20,  FUN = sd, fill = NA, align = "right"),
    SD50   = zoo::rollapply(D_Close, width = 50,  FUN = sd, fill = NA, align = "right"),
    SD200  = zoo::rollapply(D_Close, width = 200, FUN = sd, fill = NA, align = "right"),

    Lower20  = SMA20  - SD20,   Higher20  = SMA20  + SD20,
    Lower50  = SMA50  - SD50,   Higher50  = SMA50  + SD50,
    Lower200 = SMA200 - SD200,  Higher200 = SMA200 + SD200
  )

# -------------------------------------------
# NUEVOS INDICADORES SOLICITADOS
# -------------------------------------------

# 1) Salida de banda (TRUE/FALSE) para cada ventana
df_data <- df_data |>
  mutate(
    Outside20  = (D_Close < Lower20  | D_Close > Higher20),
    Outside50  = (D_Close < Lower50  | D_Close > Higher50),
    Outside200 = (D_Close < Lower200 | D_Close > Higher200)
  )

# 2) Percentil de D_Close respecto a su propia distribución (0–100)
#    percent_rank() da 0..1; lo llevamos a 0..100
df_data <- df_data |>
  mutate(
    Percentile = dplyr::percent_rank(D_Close) * 100
  )

# 3) Location = D_Close - SMA50
df_data <- df_data |>
  mutate(
    Location = D_Close - SMA50
  )

# 4) Clasificaciones por horizonte:
#    - CP ±: comparación con SMA20
#    - INT ±: comparación con SMA50
#    - LT  ±: comparación con SMA200
df_data <- df_data |>
  mutate(
    CP  = if_else(D_Close > SMA20,  "CP+",  "CP-"),
    INT = if_else(D_Close > SMA50,  "INT+", "INT-"),
    LT  = if_else(D_Close > SMA200, "LT+",  "LT-")
  )

# 5) Generación de Métricas de Riesgo y Clasificación de Tendencia

df_data %>%
  arrange(Fecha) %>%
  mutate(
    # RSI (14)
    price_change = D_Close - dplyr::lag(D_Close),
    gains = ifelse(price_change > 0, price_change, 0),
    losses = ifelse(price_change < 0, abs(price_change), 0),
    avg_gains = zoo::rollmean(gains, k = 14, fill = NA, align = "right"),
    avg_losses = zoo::rollmean(losses, k = 14, fill = NA, align = "right"),
    rsi = dplyr::if_else(avg_losses == 0, 100, 100 - (100 / (1 + avg_gains / avg_losses))),

    # Volatilidad (20 días, anualizada %)
    volatility_20 = zoo::rollapply(`%D`, width = 20, FUN = sd, fill = NA, align = "right") * sqrt(252) * 100,

    # Clasificación de tendencia
    trend_short = dplyr::case_when(
      D_Close > SMA20 & SMA20 > dplyr::lag(SMA20, 5) ~ "Depreciación",
      D_Close < SMA20 & SMA20 < dplyr::lag(SMA20, 5) ~ "Apreciación",
      TRUE ~ "Lateral"
    ),
    trend_medium = dplyr::case_when(
      SMA20 > SMA50 & SMA50 > dplyr::lag(SMA50, 10) ~ "Depreciación",
      SMA20 < SMA50 & SMA50 < dplyr::lag(SMA50, 10) ~ "Apreciación",
      TRUE ~ "Lateral"
    ),
    trend_long = dplyr::case_when(
      SMA50 > SMA200 ~ "Depreciación",
      SMA50 < SMA200 ~ "Apreciación",
      TRUE ~ "Lateral"
    ),

    # Señal Bandas Bollinger
    bb_signal = dplyr::case_when(
      D_Close > Higher20 ~ "Sobrecompra",
      D_Close < Lower20 ~ "Sobreventa",
      TRUE ~ "Neutral"
    )
  )-> df_data
 