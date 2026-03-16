# ============================================================================
# PROJETO: Tendência Temporal e Padrões Espaciais da Sífilis Gestacional
#          e Congênita no Brasil
# SCRIPT:  00_setup.R — Configuração do ambiente, pacotes e caminhos
# AUTOR:   [Equipe de Pesquisa]
# DATA:    2026-03
# ============================================================================

# --- Caminhos do projeto ---
proj_dir   <- here::here()
data_raw   <- file.path(proj_dir, "data", "raw")
data_proc  <- file.path(proj_dir, "data", "processed")
out_tables <- file.path(proj_dir, "output", "tables")
out_figs   <- file.path(proj_dir, "output", "figures")
out_maps   <- file.path(proj_dir, "output", "maps")
out_models <- file.path(proj_dir, "output", "models")
out_suppl  <- file.path(proj_dir, "output", "supplementary")

# --- Instalar e carregar pacotes ---
required_packages <- c(
  # Infraestrutura
  "here", "tidyverse", "data.table", "janitor", "writexl", "readxl",
  "openxlsx", "lubridate", "glue",


  # Dados DataSUS
  "read.dbc", "httr", "curl",

  # Análise descritiva e tabelas
  "gtsummary", "gt", "flextable", "kableExtra",

  # Análise de tendência temporal
  "segmented",   # Joinpoint-like segmented regression
  "prais",       # Prais-Winsten para autocorrelação serial
  "MASS",        # Binomial negativa
  "lmtest",      # Testes de diagnóstico
  "sandwich",    # Erros robustos
  "tseries",     # Testes de série temporal

  # Análise espacial
  "sf", "spdep", "tmap", "tmaptools",
  "spatialreg",  # Modelos espaciais (SAR, SEM)
  "INLA",        # Modelos BYM (opcional — ver nota abaixo)
  "geobr",       # Malhas cartográficas do Brasil
  "rgeoda",      # GeoDa em R — LISA, Moran

  # Visualização

  "ggplot2", "ggrepel", "patchwork", "viridis", "RColorBrewer",
  "scales", "cowplot",

  # Modelagem multivariada
  "lme4",        # Modelos multinível
  "broom", "broom.mixed",
  "performance", # Diagnósticos de modelos
  "DHARMa"       # Diagnósticos para GLMMs
)

# Pacotes que precisam de instalação especial
# INLA: install.packages("INLA", repos = c(getOption("repos"),
#        INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  # INLA requer repositório especial
  missing_std  <- setdiff(missing, "INLA")
  missing_inla <- intersect(missing, "INLA")

  if (length(missing_std) > 0) {
    message("Instalando pacotes: ", paste(missing_std, collapse = ", "))
    install.packages(missing_std, dependencies = TRUE)
  }
  if (length(missing_inla) > 0) {
    message("Instalando INLA...")
    install.packages("INLA",
                     repos = c(getOption("repos"),
                               INLA = "https://inla.r-inla-download.org/R/stable"),
                     dep = TRUE)
  }
}

install_if_missing(required_packages)

# Carregar todos
suppressPackageStartupMessages({
  for (pkg in required_packages) {
    if (pkg %in% installed.packages()[, "Package"]) {
      library(pkg, character.only = TRUE)
    } else {
      warning(glue("Pacote '{pkg}' não disponível — algumas funções podem falhar."))
    }
  }
})

# --- Parâmetros globais do estudo ---
ANOS_ESTUDO   <- 2012:2023
ANO_INICIO    <- min(ANOS_ESTUDO)
ANO_FIM       <- max(ANOS_ESTUDO)
CRS_BRASIL    <- 4674   # SIRGAS 2000

# Códigos UF
UF_CODES <- c(
  "AC" = 12, "AL" = 27, "AP" = 16, "AM" = 13, "BA" = 29,

  "CE" = 23, "DF" = 53, "ES" = 32, "GO" = 52, "MA" = 21,
  "MT" = 51, "MS" = 50, "MG" = 31, "PA" = 15, "PB" = 25,
  "PR" = 41, "PE" = 26, "PI" = 22, "RJ" = 33, "RN" = 24,
  "RS" = 43, "RO" = 11, "RR" = 14, "SC" = 42, "SP" = 35,
  "SE" = 28, "TO" = 17
)

# Macrorregiões
MACRO_REGIOES <- list(
  Norte     = c(11, 12, 13, 14, 15, 16, 17),
  Nordeste  = c(21, 22, 23, 24, 25, 26, 27, 28, 29),
  Sudeste   = c(31, 32, 33, 35),
  Sul       = c(41, 42, 43),
  `Centro-Oeste` = c(50, 51, 52, 53)
)

# Rótulos raça/cor — SINAN
RACA_COR_LABELS <- c(
  "1" = "Branca", "2" = "Preta", "3" = "Amarela",
  "4" = "Parda",  "5" = "Indígena", "9" = "Ignorado"
)

# --- Tema ggplot padrão para publicação ---
theme_pub <- theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 10, color = "grey30"),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 9),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 10),
    plot.caption     = element_text(size = 7, color = "grey50")
  )
theme_set(theme_pub)

# Paleta de cores para raça/cor
cores_raca <- c(
  "Branca"   = "#4575B4",
  "Preta"    = "#D73027",
  "Parda"    = "#FC8D59",
  "Amarela"  = "#FEE090",
  "Indígena" = "#91BFDB",
  "Ignorado" = "#BDBDBD"
)

# Paleta regiões
cores_regiao <- c(
  "Norte"        = "#1B9E77",
  "Nordeste"     = "#D95F02",
  "Sudeste"      = "#7570B3",
  "Sul"          = "#E7298A",
  "Centro-Oeste" = "#66A61E"
)

message("=== Setup concluído. Período: ", ANO_INICIO, "-", ANO_FIM, " ===")
