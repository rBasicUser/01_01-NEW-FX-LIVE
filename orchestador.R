# ====================================================================
# SISTEMA DE GENERACIÓN AUTOMÁTICA DE REPORTES FX LIVE
# Central American Business Intelligence
# ====================================================================

# Cargar librerías necesarias
# install.packages("rmarkdown")
# install.packages("extrafont")
# install.packages("kableExtra")
# install.packages("DT")
# install.packages("zoo")
# install.packages("moments")
# install.packages("modelsummary")
# install.packages("TTR")
# install.packages("patchwork")


library(rmarkdown)
library(here)
library(glue)
library(purrr)
library(fs)

# Configuración de países
paises <- c("GT", "HN", "CR", "DO", "PE", "CO", "MX")

# Nombres completos para los títulos (opcional)
nombres_paises <- list(
  "GT" = "Guatemala",
  "HN" = "Honduras", 
  "CR" = "Costa Rica",
  "DO" = "República Dominicana",
  "PE" = "Perú",
  "CO" = "Colombia",
  "MX" = "México"
)

# ====================================================================
# FUNCIÓN PARA CREAR TEMPLATE RMD POR PAÍS
# ====================================================================

crear_rmd_pais <- function(codigo_pais) {
  
  # Template del documento RMD (tu documento original con modificaciones mínimas)
  template_rmd <- glue('
---
title: "FX Live - Análisis Técnico y de Riesgo - {nombres_paises[[codigo_pais]]}"
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float: 
      collapsed: false
      smooth_scroll: true
    theme: flatly
    highlight: tango
    css: "styles.css"
    code_folding: hide
editor_options: 
  markdown: 
    wrap: 72
---

```{{=html}}
<style>
.alert{{
  padding: 15px;
  margin-bottom: 20px;
  border: 1px solid transparent;
  border-radius: 4px;
}}
.alert-warning{{
  color: #8a6d3b;
  background-color: #fcf8e3;
  border-color: #faebcc;
}}
.alert-success{{
  color: #3c763d;
  background-color: #dff0d8;
  border-color: #d6e9c6;
}}
.alert-danger{{
  color: #a94442;
  background-color: #f2dede;
  border-color: #ebccd1;
}}
.metric-card{{
  background: #f8f9fa;
  border-radius: 8px;
  padding: 15px;
  margin: 10px 0;
  border-left: 4px solid #007bff;
}}
.trend-up{{ color: #28a745; font-weight: bold; }}
.trend-down{{ color: #dc3545; font-weight: bold; }}
.trend-neutral{{ color: #6c757d; font-weight: bold; }}
.side-by-side{{
  display: flex;
  flex-wrap: wrap;
  gap: 20px;
  align-items: flex-start;
}}
.side-by-side > div{{
  flex: 1;
  min-width: 300px;
}}
</style>
```
  
  
```{{r setup, include=FALSE}}
library(extrafont)
library(scales)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(DT)
library(zoo)      # rollmean / rollapply
library(moments)  # skewness / kurtosis

knitr::opts_chunk$set(
  echo = FALSE, 
  message = FALSE, 
  warning = FALSE,
  fig.width = 10,
  fig.height = 6,
  dpi = 300,
  comment = NA
)

# Parámetros
lookback_months <- 12
confidence_level <- 0.95

# Tema visual
theme_fx <- function() {{
  theme_minimal(base_family="Century Gothic") +
    theme(
      plot.title = element_text(size = 18, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 13, color = "gray60"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
    )
}}
```

```{{r data-load, include=FALSE}}

# Cargar datos y preparar Métricas
source("../scripts/pilot {codigo_pais}.R")

# Observaciones actuales
current_data <- df_data %>% dplyr::slice_tail(n = 1)
previous_data <- df_data %>% dplyr::slice_tail(n = 2) %>% dplyr::slice_head(n = 1)
```

# 📊 Resumen Ejecutivo

::: metric-card
**Fecha del Análisis:** `r format(current_data$Fecha, \'%d de %B, %Y\')`

**Precio Actual:**
`r number(current_data$D_Close, accuracy = 0.01, big.mark = ",")`
(`r if_else(current_data$D_Close > previous_data$D_Close, "↗️", "↘️")`
`r percent((current_data$D_Close / previous_data$D_Close) - 1, accuracy = 0.01)`
vs día anterior)

**Tendencia General:** `r current_data$trend_long` (Largo Plazo)

**Bandas de Normalidad indican:** `r current_data$bb_signal`

**Percentil Historico:** `r round(current_data$Percentile, 1)`/100
:::

```{{r alerts, results="asis"}}
# Alertas
alerts <- list()

if (current_data$Outside20) {{
  alert_type <- if_else(current_data$D_Close > current_data$Higher20, "warning", "danger")
  alert_msg <- if_else(
    current_data$D_Close > current_data$Higher20, 
    "⚠️ Precio fuera de banda superior (20 períodos) - Posible sobredepreciación",
    "🚨 Precio fuera de banda inferior (20 períodos) - Posible sobreapreciación"
  )
  alerts <- append(alerts, list(list(type = alert_type, message = alert_msg)))
}}

if (!is.na(current_data$rsi)) {{
  if (current_data$rsi > 70) {{
    alerts <- append(alerts, list(list(type = "warning", message = "⚠️ RSI indica condición de sobredepreciación")))
  }} else if (current_data$rsi < 30) {{
    alerts <- append(alerts, list(list(type = "danger", message = "🚨 RSI indica condición de sobreapreciación")))
  }}
}}

if (abs(current_data$`%D`) > quantile(abs(df_data$`%D`), 0.95, na.rm = TRUE)) {{
  alerts <- append(alerts, list(list(type = "warning", message = "⚠️ Movimiento diario inusual detectado")))
}}

if (length(alerts) > 0) {{
  for (alert in alerts) {{
    cat(paste0(\'<div class="alert alert-\', alert$type, \'">\', alert$message, \'</div>\'))
  }}
}} else {{
  cat(\'<div class="alert alert-success">✅ No se detectaron señales de alerta inmediatas</div>\')
}}
```

# 📈 Posición Actual

```{{r current-position}}
# Construcción explícita (evita rename/select/pivot_longer con tipos mixtos)
library(tibble)


current_formatted <- tibble(
  Métrica = c(
    "Precio",
    "Cambio respecto al día anterior (%)",
    "Cambio Semanal (%)",
    "Cambio Anual (%)",
    "Volatilidad (20d)",
    "Tendencia Corto",
    "Tendencia Medio",
    "Tendencia Largo"
  ),
  Valor = c(
    scales::number(current_data$D_Close, accuracy = 0.01, big.mark = ","),
    scales::percent(current_data$`%D`, accuracy = 0.01),
    scales::percent(current_data$`%W`, accuracy = 0.01),
    scales::percent(current_data$YoY, accuracy = 0.01),
    paste0("Q ", round((current_data$volatility_20 / 100 * current_data$D_Close), 3)),
    as.character(current_data$trend_short),
    as.character(current_data$trend_medium),
    as.character(current_data$trend_long)
  )
)

current_formatted %>%
  kable("html", align = c("l", "r")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"), position = "center") %>%
  row_spec(0, bold = TRUE, background = "#f8f9fa") %>%
  column_spec(1, bold = TRUE, width = "200px") %>%
  column_spec(2, width = "150px")

```

# 📊 Análisis Técnico

## Evolución del Precio

```{{r bollinger-enhanced, fig.height=8}}
library(patchwork)
recent_data <- df_data %>% dplyr::filter(Fecha >= (Sys.Date() - months(18)))

# Ventanas de tiempo
end_date <- max(df_data$Fecha, na.rm = TRUE)
data_60m <- df_data %>% filter(Fecha >= (end_date %m-% months(60)))
data_24m <- df_data %>% filter(Fecha >= (end_date %m-% months(18)))

# --- Gráfico superior: Precio + SMA200 (60 meses) ---
p_top <- ggplot(data_60m, aes(x = Fecha)) +
  geom_line(aes(y = D_Close), color = "black", linewidth = 0.9) +
  geom_line(aes(y = SMA200), color = "#1e567c", linewidth = 0.9, alpha = 0.9) +
  labs(
    title = "Largo Plazo (5 años)",
    x = NULL, y = "Nivel del Tipo de Cambio",
    caption = "SMA200: tendencia de largo plazo"
  ) +
  theme_fx()

print(p_top)

# --- Gráfico inferior: SMA50 y SMA20 (24 meses) ---
p_bottom <- ggplot(data_24m, aes(x = Fecha)) +
  # Si tienes bandas de Bollinger ya calculadas:
  geom_line(aes(y = D_Close), color = "black", linewidth = 0.9) +
  geom_point(data = current_data, aes(x= Fecha, y = D_Close), color = "red", size = 3, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = Lower20, ymax = Higher20), alpha = 0.15, fill = "#76bcebff") +
  geom_ribbon(aes(ymin = Lower50, ymax = Higher50), alpha = 0.10, fill = "#12649bff") +
  geom_line(aes(y = SMA50), color = "#12649bff", linewidth = 0.9) +
  geom_line(aes(y = SMA20), color = "#76bcebff", linewidth = 0.9) +
  labs(
    title = "Corto y Mediano Plazo (año y medio)",
    x = NULL, y = NULL,
    caption = "Bandas: Corto Plazo (oscura), Mediano Plazo (Clara)"
  ) +
  theme_fx()

# --- Ensamble con patchwork: uno sobre otro ---
print(p_bottom)
```

## Análisis de Volatilidad

```{{r volatility-analysis}}
# Paso 1: transformar la serie en dinero (precio del día)
recent_data <- recent_data %>%
  mutate(vol20_money = volatility_20 * D_Close / 100)

# Paso 2: calcular la mediana de ese periodo en dinero
mediana_money <- median(recent_data$vol20_money, na.rm = TRUE)

# Paso 3: graficar
p3 <- ggplot(recent_data, aes(x = Fecha)) +
  geom_area(aes(y = vol20_money), alpha = 0.3) +
  geom_line(aes(y = vol20_money), linewidth = 0.8) +
  geom_hline(yintercept = mediana_money, linetype = "dashed", alpha = 0.7) +
  labs(
    title = "Volatilidad Histórica (20 días)",
    subtitle = "Anualizada, expresada en moneda",
    y = NULL,
    x = "Fecha",
    caption = "Línea discontinua: mediana del periodo"
  ) +
  theme_fx()

print(p3)

```

## Retorno y Riesgo  

::: {{.side-by-side}}
::: {{}}
```{{r risk-metrics}}
calculate_var <- function(returns, confidence = 0.95) {{
  as.numeric(quantile(returns, confidence, na.rm = TRUE))
}}

calculate_max_drawdown <- function(prices) {{
  log_p <- log(prices)
  cummax_log <- cummax(log_p)
  drawdown <- exp(log_p - cummax_log) - 1
  max(drawdown, na.rm = TRUE)
}}

daily_returns <- df_data$`%D`
weekly_returns <- df_data$`%W`
annual_returns <- df_data$YoY

# Calcular VaR en dinero
var_daily_pct <- calculate_var(daily_returns, 0.95)
var_weekly_pct <- calculate_var(weekly_returns, 0.95)
var_annual_pct <- calculate_var(annual_returns, 0.95)
var_daily_money <- var_daily_pct * current_data$D_Close
var_weekly_money <- var_weekly_pct * current_data$D_Close
var_annual_money <- var_annual_pct * current_data$D_Close

risk_metrics <- tibble(
  Métrica = c(
    "VaR Diario (95%) - En dinero",
    "VaR Diario (95%) - En %",
    "VaR Semanal (95%) - En dinero",
    "VaR Semanal (95%) - En %",
    "VaR Anual (95%) - En dinero",
    "VaR Anual (95%) - En %",
    "Máxima Pérdida Histórica (Max Drawdown)",
    "Volatilidad Actual (20d)",
    "Volatilidad Histórica (Anual)",
    "Sharpe Ratio (aprox.)"
  ),
  Valor = c(
    abs(var_daily_money),
    abs(var_daily_pct) * 100,
    abs(var_weekly_money),
    abs(var_weekly_pct) * 100,
    abs(var_annual_money),
    abs(var_annual_pct) * 100,
    calculate_max_drawdown(df_data$D_Close) * 100,
    current_data$volatility_20,
    sd(daily_returns, na.rm = TRUE) * sqrt(252) * 100,
    (mean(daily_returns, na.rm = TRUE) / sd(daily_returns, na.rm = TRUE)) * sqrt(252)
  ),
  Unidad = c("Q", "%", "Q", "%", "Q", "%", "%", "%", "%", "ratio")
) %>%
  mutate(
    Valor_fmt = dplyr::case_when(
      Unidad == "%" ~ paste0(scales::number(Valor, accuracy = 0.01), " %"),
      Unidad == "Q" ~ paste0("Q ", scales::number(Valor, accuracy = 0.01)),
      TRUE ~ scales::number(Valor, accuracy = 0.01)
    )
  )

risk_metrics %>%
  dplyr::select(Métrica, Valor_fmt) %>%
  kable("html", caption = "Métricas de Riesgo y Performance", col.names = c("Métrica", "Valor")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, width = "300px") %>%
  column_spec(2, width = "175px") %>%
  pack_rows("Value at Risk", 1, 6) %>%
  pack_rows("Métricas de Volatilidad", 7, 9) %>%
  pack_rows("Ratio de Performance", 10, 10)
```
:::

::: {{}}
```{{r trend-analysis}}
trend_summary <- df_data %>%
  dplyr::filter(Fecha >= (Sys.Date() - months(6))) %>%
  dplyr::mutate(Mes = lubridate::floor_date(Fecha, "month")) %>%
  dplyr::group_by(Mes) %>%
  dplyr::summarise(
    Retorno_Mensual = (dplyr::last(D_Close) / dplyr::first(D_Close) - 1) * 100,
    .groups = "drop_last"
  ) %>%
  dplyr::ungroup()

trend_summary %>%
  dplyr::select(Mes, Retorno_Mensual) %>%
  dplyr::mutate(
    Mes = format(Mes, "%B %Y"),
    Retorno_Mensual = paste0(round(Retorno_Mensual, 2), "%")
  ) %>%
  kable("html", caption = "Rendimientos Mensuales (Últimos 6 meses)",
        col.names = c("Mes", "Retorno Mensual")) %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = TRUE)
```
:::
:::

# 📈 Análisis de Distribuciones

```{{r distributions, fig.height=10}}
p4 <- df_data %>%
  dplyr::select(Fecha, `%D`, `%W`, YoY) %>%
  tidyr::pivot_longer(cols = c(`%D`, `%W`, YoY), names_to = "Periodo", values_to = "Retorno") %>%
  dplyr::mutate(
    Retorno_pct = Retorno * 100,
    Periodo = dplyr::case_when(
      Periodo == "%D" ~ "Diario",
      Periodo == "%W" ~ "Semanal", 
      TRUE ~ "Anual"
    )
  ) %>%
  ggplot(aes(x = Retorno_pct)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.7) +
  geom_density(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Periodo, scales = "free", ncol = 1) +
  labs(
    title = "Distribución de Retornos por Período",
    subtitle = "Histograma con curva de densidad estimada",
    x = "Retorno (%)",
    y = "Densidad"
  ) +
  theme_fx()

print(p4)
```

# 📊 Dashboard de Señales

```{{r signals-dashboard}}
signals_data <- tibble(
  Indicador = c(
    "Precio en Corto Plazo",
    "Precio en Mediano Plazo", 
    "Precio en Largo Plazo",
    "Corto vs Mediano Plazo",
    "Mediano vs Largo Plazo",
    "Bandas de Normalidad",
    "RSI (14)",
    "Volatilidad vs Mediana"
  ),
  Señal = c(
    if_else(current_data$D_Close > current_data$SMA20, "Depreciación", "Apreciación"),
    if_else(current_data$D_Close > current_data$SMA50, "Depreciación", "Apreciación"),
    if_else(current_data$D_Close > current_data$SMA200, "Depreciación", "Apreciación"),
    if_else(current_data$SMA20 > current_data$SMA50, "Depreciación", "Apreciación"),
    if_else(current_data$SMA50 > current_data$SMA200, "Depreciación", "Apreciación"),
    current_data$bb_signal,
    case_when(
      is.na(current_data$rsi) ~ "N/A",
      current_data$rsi > 70 ~ "Sobrecompra",
      current_data$rsi < 30 ~ "Sobreventa",
      TRUE ~ "Neutral"
    ),
    if_else(current_data$volatility_20 > median(df_data$volatility_20, na.rm = TRUE), "Alta", "Normal")
  ),
) %>%
  mutate(
    Color = case_when(
      Señal %in% c("Depreciación", "Alta") ~ "success",
      Señal %in% c("Apreciación", "Sobreventa") ~ "danger", 
      Señal %in% c("Sobrecompra") ~ "warning",
      TRUE ~ "secondary"
    )
  )

signals_data %>%
  dplyr::select(Indicador, Señal) %>%
  kable("html", caption = "Dashboard de Señales Técnicas", col.names = c("Indicador", "Señal")) %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows("Señales de Precio", 1, 3) %>%
  pack_rows("Señales de Tendencia", 4, 5) %>%
  pack_rows("Señales de Momentum", 6, 7) %>%
  pack_rows("Señales de Volatilidad", 8, 8)
```

------------------------------------------------------------------------

::: {{style="margin-top: 30px; padding: 20px; background-color: #f8f9fa; border-radius: 5px;"}}
<h4>📝 Notas Metodológicas</h4>

<ul>

<li><strong>Bandas de Normalidad:</strong> 
  Un valor fuera de las bandas de normalidad es un movimiento inusual en el respectivo corto, largo o mediano plazo
  </li>

<li><strong>RSI:</strong> Índice de Fuerza Relativa: compara caídas y subidas en los últimos 14 días para identificar si puede venir un cambio significativo
<li><strong>VaR/CVaR:</strong> Value at Risk indica la cantidad que puedo llegar perder ante un evento poco usual
</li>

<li><strong>Volatilidad:</strong> 
mide qué tanto ha variado el precio en los últimos 20 días y lo proyecta a lo que sería en un año</li>

<li><strong>Tendencias:</strong> Trayectoria más probable del Tipo de Cambio</li>

</ul>
:::

::: {{style="text-align: center; margin-top: 20px; color: #6c757d; font-size: 0.9em;"}}
Reporte generado automáticamente el
`r format(Sys.time(), \'%d de %B, %Y a las %H:%M\')` | © Equipo de
Inteligencia de Negocios
:::')
  
  # Crear nombre del archivo
  nombre_archivo <- glue("FX_Live_{codigo_pais}.Rmd")
  
  # Escribir el archivo
  writeLines(template_rmd, nombre_archivo)
  
  cat(glue("✅ Archivo generado: {nombre_archivo}\n"))
  
  return(nombre_archivo)
}

# ====================================================================
# FUNCIÓN PARA RENDERIZAR UN REPORTE
# ====================================================================

renderizar_reporte <- function(archivo_rmd) {
  getwd()
  tryCatch({
    cat(glue("🔄 Renderizando: {archivo_rmd}...\n"))
    
    rmarkdown::render(
      archivo_rmd,
      output_format = "html_document",
      quiet = TRUE,
      encoding = "UTF-8"
    )
    
    archivo_html <- gsub(".Rmd$", ".html", archivo_rmd)
    cat(glue("✅ Completado: {archivo_html}\n"))
    
    return(archivo_html)
  }, error = function(e) {
    cat(glue("❌ Error renderizando {archivo_rmd}: {e$message}\n"))
    return(NULL)
  })
}

# ====================================================================
# FUNCIÓN PRINCIPAL - GENERAR TODOS LOS REPORTES
# ====================================================================

generar_reportes_fx <- function(paises_a_procesar = paises, renderizar = TRUE) {
  
  cat("====================================================================\n")
  cat("🚀 INICIANDO GENERACIÓN DE REPORTES FX LIVE\n")
  cat("====================================================================\n")
  
  # Crear directorio de salida si no existe
  
  # Cambiar al directorio de reportes
  setwd("reportes_FX")
  
  # Generar archivos RMD
  cat("\n📝 GENERANDO ARCHIVOS RMD...\n")
  archivos_generados <- map_chr(paises_a_procesar, crear_rmd_pais)
  
  # Renderizar si es solicitado
  if (renderizar) {
    cat("\n🔄 RENDERIZANDO REPORTES HTML...\n")
    reportes_html <- map(archivos_generados, renderizar_reporte)
    reportes_exitosos <- reportes_html[!map_lgl(reportes_html, is.null)]
    
    cat("\n====================================================================\n")
    cat(glue("✅ PROCESO COMPLETADO: {length(reportes_exitosos)}/{length(paises_a_procesar)} reportes generados exitosamente\n"))
    
    if (length(reportes_exitosos) > 0) {
      cat("\n📋 REPORTES GENERADOS:\n")
      walk(reportes_exitosos, ~ cat(glue("   • {.x}\n")))
    }
    
    if (length(reportes_exitosos) < length(paises_a_procesar)) {
      cat("\n⚠️  ALGUNOS REPORTES FALLARON - Revise los mensajes de error arriba\n")
    }
  } else {
    cat("\n📝 Archivos RMD generados (sin renderizar)\n")
    walk(archivos_generados, ~ cat(glue("   • {.x}\n")))
  }
  
  # Regresar al directorio original
  setwd("..")
  
  cat("\n🎯 Proceso finalizado\n")
  return(archivos_generados)
}

# ====================================================================
# FUNCIÓN DE EJECUCIÓN RÁPIDA
# ====================================================================

# Generar solo archivos RMD (sin renderizar)
generar_solo_rmd <- function() {
  generar_reportes_fx(renderizar = FALSE)
}

# Generar y renderizar todos los reportes
generar_todo <- function() {
  generar_reportes_fx(renderizar = TRUE)
}

# Generar y renderizar todos los reportes
generar_todo <- function() {
  generar_reportes_fx(renderizar = TRUE)
}

renderizar_solo_html <- function(paises_a_procesar = paises) {
  # Asegura entrar/salir del directorio correcto
  old <- setwd("reportes_FX"); on.exit(setwd(old), add = TRUE)

  archivos <- glue::glue("FX_Live_{paises_a_procesar}.Rmd")
  existentes <- archivos[fs::file_exists(archivos)]

  if (length(existentes) == 0) {
    stop("No hay archivos Rmd para renderizar en 'reportes_FX'.")
  }

  cat("\n🔄 RENDERIZANDO REPORTES HTML...\n")
  res <- purrr::map(existentes, renderizar_reporte)

  invisible(res)
}

# ====================================================================
# INSTRUCCIONES DE USO
# ====================================================================

cat("====================================================================\n")
cat("🔧 SISTEMA DE GENERACIÓN DE REPORTES FX LIVE CARGADO\n")
cat("====================================================================\n")
cat("\nFunciones disponibles:\n")
cat("  • generar_todo()           - Genera RMD y renderiza HTML para todos los países\n")
cat("  • generar_solo_rmd()       - Solo genera archivos RMD\n")
cat("  • generar_reportes_fx()    - Función principal con opciones\n")
cat("\nPaíses configurados: ", paste(paises, collapse = ", "), "\n")
cat("\n💡 Ejecuta generar_todo() para empezar\n")

# Si hay necesidad de volver a generar los reportes por un cambio demasiado brusco en la estructura general del reporte, volver a generarlos
#   
# generar_todo()

#generar_todo()

renderizar_solo_html()    
