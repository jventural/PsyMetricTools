library(devtools)
load_all("D:/14. LIBRERIAS/PsyMetricTools", quiet = TRUE)
library(ggplot2)

# Verificar disponibilidad de paquetes circulares
if (!requireNamespace("circlize", quietly = TRUE))
  install.packages("circlize", repos = "https://cloud.r-project.org", quiet = TRUE)
if (!requireNamespace("patchwork", quietly = TRUE))
  install.packages("patchwork", repos = "https://cloud.r-project.org", quiet = TRUE)

res <- readRDS("D:/1. INVESTIGACIONES/4. BASES DE DATOS PROCESADAS/11_ASESORIAS/Raquel Garcia/reportes/data/sem_sin_DERS_resultados.rds")
fit <- res$fit

# ===== Figura 3 (chord diagram) =====
predictors <- c("sueno_num","alim_num","ejerc_si","fuma_si","alc_si","vape_si",
                "sustancias_si","sexo_mujer","edad_num","imc")
mediators  <- c("DASS_dep","DASS_anx","DASS_str","bien")
outcome    <- "suic"

node_groups <- c(
  sueno_num = "Hábito", alim_num = "Hábito", ejerc_si = "Hábito",
  fuma_si = "Sustancia", alc_si = "Sustancia", vape_si = "Sustancia",
  sustancias_si = "Sustancia",
  sexo_mujer = "Demogr", edad_num = "Demogr", imc = "Demogr",
  DASS_dep = "DASS", DASS_anx = "DASS", DASS_str = "DASS",
  bien = "Bienestar",
  suic = "Outcome"
)
node_labels <- c(
  sueno_num = "Sueño", alim_num = "Alimentación", ejerc_si = "Ejercicio",
  fuma_si = "Tabaco", alc_si = "Alcohol", vape_si = "Vapeador",
  sustancias_si = "Otras sust.",
  sexo_mujer = "Sexo (M)", edad_num = "Edad", imc = "IMC",
  DASS_dep = "DASS Dep", DASS_anx = "DASS Anx", DASS_str = "DASS Str",
  bien = "Bienestar",
  suic = "Conducta\nsuicida"
)
paleta <- c("Hábito" = "#FFE082", "Sustancia" = "#FFAB91", "Demogr" = "#B0BEC5",
            "DASS" = "#F4A8A8", "Bienestar" = "#A5D6A7", "Outcome" = "#90CAF9")

png("D:/1. INVESTIGACIONES/4. BASES DE DATOS PROCESADAS/11_ASESORIAS/Raquel Garcia/reportes/figures/fig_chord_a4.png",
    width = 11, height = 11, units = "in", res = 300, bg = "white")
plot_mediation_chord(
  fit,
  predictors = predictors, mediators = mediators, outcome = outcome,
  node_groups = node_groups, node_labels = node_labels,
  palette = paleta,
  chord_width_range = c(0.5, 8),
  big_gap = 12, gap_degree = 1.5,
  label_cex = 0.95,
  title = "Asociaciones bivariadas en el modelo de mediación múltiple paralela (N = 1,091)"
)
dev.off()
cat("OK fig_chord_a4.png\n")

# ===== Figura 4 (donuts de mediación) =====
mediadores_named <- c("Depresión" = "DASS_dep",
                      "Ansiedad" = "DASS_anx",
                      "Estrés"   = "DASS_str",
                      "Bienestar" = "bien")
colors_med <- c("Depresión" = "#F4A8A8",
                "Ansiedad"  = "#FFAB91",
                "Estrés"    = "#FFCC80",
                "Bienestar" = "#A5D6A7")

p_donuts <- plot_mediation_donuts(
  fit,
  predictor = "sueno",
  mediators = mediadores_named,
  outcome = "suic",
  predictor_regression = "sueno_num",
  mediator_colors = colors_med,
  show_total = TRUE,
  title = "Proporción de mediación de la calidad del sueño sobre la conducta suicida",
  subtitle = "Cada panel muestra |a × b| / Σ|a × b| por mediador paralelo (N = 1,091)"
)
ggsave("D:/1. INVESTIGACIONES/4. BASES DE DATOS PROCESADAS/11_ASESORIAS/Raquel Garcia/reportes/figures/fig_donuts_sueno.png",
       p_donuts, width = 11, height = 3.5, dpi = 300, bg = "white")
cat("OK fig_donuts_sueno.png\n")

# Donuts para alimentación
p_donuts2 <- plot_mediation_donuts(
  fit,
  predictor = "alim",
  mediators = mediadores_named,
  outcome = "suic",
  predictor_regression = "alim_num",
  mediator_colors = colors_med,
  show_total = TRUE,
  title = "Proporción de mediación de la calidad de la alimentación sobre la conducta suicida",
  subtitle = "Cada panel muestra |a × b| / Σ|a × b| por mediador paralelo (N = 1,091)"
)
ggsave("D:/1. INVESTIGACIONES/4. BASES DE DATOS PROCESADAS/11_ASESORIAS/Raquel Garcia/reportes/figures/fig_donuts_alim.png",
       p_donuts2, width = 11, height = 3.5, dpi = 300, bg = "white")
cat("OK fig_donuts_alim.png\n")

# Donuts para sustancias
p_donuts3 <- plot_mediation_donuts(
  fit,
  predictor = "sustancias",
  mediators = mediadores_named,
  outcome = "suic",
  predictor_regression = "sustancias_si",
  mediator_colors = colors_med,
  show_total = TRUE,
  title = "Proporción de mediación del consumo de otras sustancias sobre la conducta suicida",
  subtitle = "Cada panel muestra |a × b| / Σ|a × b| por mediador paralelo (N = 1,091)"
)
ggsave("D:/1. INVESTIGACIONES/4. BASES DE DATOS PROCESADAS/11_ASESORIAS/Raquel Garcia/reportes/figures/fig_donuts_sustancias.png",
       p_donuts3, width = 11, height = 3.5, dpi = 300, bg = "white")
cat("OK fig_donuts_sustancias.png\n")
