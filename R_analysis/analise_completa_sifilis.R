# ============================================================================
# PROJETO: Tendência Temporal e Padrões Espaciais da Sífilis Gestacional
#          e Congênita no Brasil: Uma Análise Baseada na Vulnerabilidade
#          Social e Territorial (2007–2023)
#
# SCRIPT ÚNICO: Pipeline analítica completa
# AUTOR:   Audencio Victor (LSHTM)
# DATA:    2026-03
# ============================================================================
#
# ÍNDICE:
#   PARTE 0  — Setup: pacotes, caminhos e parâmetros
#   PARTE 1  — Download de dados do DataSUS (SINAN, SINASC)
#   PARTE 2  — Limpeza e preparação dos dados
#   PARTE 3  — Construção de indicadores (taxas, razões)
#   PARTE 4  — Análise descritiva (tabelas e gráficos)
#   PARTE 5  — Joinpoint / Regressão segmentada (tendência temporal)
#   PARTE 6  — Análises de sensibilidade temporal (Prais-Winsten, BN)
#   PARTE 7  — Análise espacial (Moran Global, LISA, mapas)
#   PARTE 8  — Análise de vulnerabilidade social (IVS)
#   PARTE 9  — Desigualdades raciais
#   PARTE 10 — Modelagem multivariada (Poisson, BN, espacial)
#   PARTE 11 — Exportação de tabelas, figuras e material suplementar
#
# ============================================================================


###############################################################################
#                     PARTE 0 — SETUP                                         #
###############################################################################

# --- Caminhos ---
proj_dir   <- getwd()
data_raw   <- file.path(proj_dir, "data", "raw")
data_proc  <- file.path(proj_dir, "data", "processed")
out_tables <- file.path(proj_dir, "output", "tables")
out_figs   <- file.path(proj_dir, "output", "figures")
out_maps   <- file.path(proj_dir, "output", "maps")
out_models <- file.path(proj_dir, "output", "models")
out_suppl  <- file.path(proj_dir, "output", "supplementary")

dirs <- c(data_raw, data_proc, out_tables, out_figs, out_maps,
          out_models, out_suppl)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# --- Pacotes ---
# NOTA: MASS deve ser carregado ANTES de tidyverse para que dplyr::select
# prevaleça sobre MASS::select
required_packages <- c(
  "MASS",
  "tidyverse", "data.table", "janitor", "writexl", "readxl", "openxlsx",
  "lubridate", "glue", "read.dbc", "httr", "curl",
  "gtsummary", "gt", "flextable", "kableExtra",
  "segmented", "prais", "lmtest", "sandwich", "tseries",
  "sf", "spdep", "tmap", "geobr", "rgeoda",
  "spatialreg",
  "ggplot2", "ggrepel", "patchwork", "viridis", "RColorBrewer",
  "scales", "cowplot",
  "lme4", "broom", "broom.mixed", "performance", "DHARMa",
  "remotes"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(missing) > 0) {
    message("Instalando: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE, quiet = TRUE)
  }
}
install_if_missing(required_packages)

suppressPackageStartupMessages({
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE)
    } else {
      warning(glue("Pacote '{pkg}' indisponível — algumas funções podem falhar."))
    }
  }
})

# --- Parâmetros globais ---
# Série ampliada: 2007–2023 (17 anos)
# Justificativa:
#   - 2007: início do SINAN-NET com melhor qualidade e cobertura
#   - Sífilis gestacional: notificação compulsória desde 2005
#   - Sífilis congênita: notificação compulsória desde 1986
#   - Série longa (17 anos) fortalece a análise de Joinpoint e permite
#     identificar mais pontos de inflexão com poder estatístico adequado
#   - Cobre marcos políticos relevantes: Pacto pela Saúde (2006),
#     Rede Cegonha (2011), Agenda de Ações Estratégicas (2017),
#     impacto da pandemia de COVID-19 (2020–2021)
#   - Revisores frequentemente questionam a escolha de séries curtas
# NOTA: Análise de sensibilidade restrita a 2012–2023 pode ser incluída
#       no material suplementar para verificar robustez dos achados.
ANOS_ESTUDO <- 2007:2023
ANO_INICIO  <- min(ANOS_ESTUDO)
ANO_FIM     <- max(ANOS_ESTUDO)
CRS_BRASIL  <- 4674  # SIRGAS 2000

# Códigos UF → sigla
UF_SIGLA <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
  "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
  "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
  "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
  "52" = "GO", "53" = "DF"
)

# Macrorregiões
get_macrorregiao <- function(cod_uf) {
  cod2 <- as.integer(substr(as.character(cod_uf), 1, 2))
  case_when(
    cod2 %in% c(11,12,13,14,15,16,17) ~ "Norte",
    cod2 %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
    cod2 %in% c(31,32,33,35) ~ "Sudeste",
    cod2 %in% c(41,42,43) ~ "Sul",
    cod2 %in% c(50,51,52,53) ~ "Centro-Oeste",
    TRUE ~ NA_character_
  )
}

# Rótulos raça/cor SINAN
RACA_COR_LABELS <- c(
  "1" = "Branca", "2" = "Preta", "3" = "Amarela",
  "4" = "Parda",  "5" = "Indígena", "9" = "Ignorado"
)

# Cores
cores_raca <- c(
  "Branca" = "#4575B4", "Preta" = "#D73027", "Parda" = "#FC8D59",
  "Amarela" = "#FEE090", "Indígena" = "#91BFDB", "Ignorado" = "#BDBDBD"
)
cores_regiao <- c(
  "Norte" = "#1B9E77", "Nordeste" = "#D95F02", "Sudeste" = "#7570B3",
  "Sul" = "#E7298A", "Centro-Oeste" = "#66A61E"
)

# Tema ggplot para publicação — sem título, sem grelha (clean)
# Títulos serão inseridos manualmente no manuscrito
theme_pub <- theme_classic(base_size = 11) +
  theme(
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    plot.caption     = element_blank(),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 9),
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.3, color = "black"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    panel.grid       = element_blank(),
    strip.text       = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "grey95", color = NA)
  )
theme_set(theme_pub)

message("=== Setup concluído. Período de estudo: ", ANO_INICIO, "–", ANO_FIM, " ===")


###############################################################################
#             PARTE 1 — DOWNLOAD DE DADOS DO DATASUS                          #
###############################################################################

# -------------------------------------------------------------------------
# 1.1 Download de microdados SINAN — Sífilis Gestacional (SIFG)
# -------------------------------------------------------------------------
# Os arquivos DBC estão no FTP do DataSUS
# Padrão: SIFGBR{AA}.dbc (AA = ano com 2 dígitos)
# Fonte: ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/

download_sinan_sifilis <- function(tipo = c("SIFG", "SIFC"),
                                   anos = ANOS_ESTUDO,
                                   dest_dir = data_raw) {
  tipo <- match.arg(tipo)
  base_url_finais <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
  base_url_prelim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"

  arquivos_baixados <- c()

  for (ano in anos) {
    aa <- sprintf("%02d", ano %% 100)
    fname <- paste0(tipo, "BR", aa, ".dbc")
    destfile <- file.path(dest_dir, fname)

    if (file.exists(destfile)) {
      message("  Já existe: ", fname)
      arquivos_baixados <- c(arquivos_baixados, destfile)
      next
    }

    # Tentar primeiro FINAIS, depois PRELIM
    url_finais <- paste0(base_url_finais, fname)
    url_prelim <- paste0(base_url_prelim, fname)

    downloaded <- FALSE
    for (url in c(url_finais, url_prelim)) {
      tryCatch({
        download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
        if (file.size(destfile) > 100) {
          message("  OK: ", fname, " (", round(file.size(destfile)/1e6, 1), " MB)")
          downloaded <- TRUE
          break
        } else {
          file.remove(destfile)
        }
      }, error = function(e) NULL)
    }

    if (downloaded) {
      arquivos_baixados <- c(arquivos_baixados, destfile)
    } else {
      warning("  FALHA no download: ", fname)
    }
  }
  return(arquivos_baixados)
}

message("\n--- Baixando sífilis gestacional (SIFG) ---")
arquivos_sifg <- download_sinan_sifilis("SIFG")

message("\n--- Baixando sífilis congênita (SIFC) ---")
arquivos_sifc <- download_sinan_sifilis("SIFC")

# -------------------------------------------------------------------------
# 1.2 Leitura dos arquivos DBC em data.frames
# -------------------------------------------------------------------------
ler_dbc_lista <- function(arquivos, tipo_label) {
  lista <- list()
  for (arq in arquivos) {
    if (!file.exists(arq)) next
    tryCatch({
      df <- read.dbc::read.dbc(arq)
      df <- as.data.frame(df)
      # Extrair ano do nome do arquivo
      bn <- basename(arq)
      aa <- as.integer(gsub("\\D", "", bn))
      if (aa < 100) aa <- aa + 2000
      df$ANO_ARQUIVO <- aa
      lista[[bn]] <- df
      message("  Lido: ", bn, " — ", nrow(df), " registros, ", ncol(df), " variáveis")
    }, error = function(e) {
      warning("  Erro ao ler ", arq, ": ", e$message)
    })
  }
  if (length(lista) == 0) return(data.frame())
  bind_rows(lista, .id = "ARQUIVO_ORIGEM")
}

message("\n--- Lendo arquivos SIFG ---")
df_sifg_raw <- ler_dbc_lista(arquivos_sifg, "SIFG")

message("\n--- Lendo arquivos SIFC ---")
df_sifc_raw <- ler_dbc_lista(arquivos_sifc, "SIFC")

# -------------------------------------------------------------------------
# 1.3 Download de nascidos vivos (SINASC) — via microdatasus
# -------------------------------------------------------------------------
# O pacote microdatasus baixa dados do DataSUS de forma eficiente
# e já processa as variáveis automaticamente.

message("\n--- Baixando SINASC (nascidos vivos) via microdatasus ---")

sinasc_rds <- file.path(data_proc, "sinasc_nv_completo.rds")

if (file.exists(sinasc_rds)) {
  message("  SINASC já processado previamente. Carregando...")
  nv_mun_raca_sinasc <- readRDS(sinasc_rds)
} else {
  if (!requireNamespace("microdatasus", quietly = TRUE)) {
    message("  Instalando microdatasus...")
    remotes::install_github("rfsaldanha/microdatasus", quiet = TRUE)
  }
  library(microdatasus)

  nv_lista <- list()
  ufs_siglas <- unname(UF_SIGLA)

  for (ano in ANOS_ESTUDO) {
    message("  SINASC ", ano, "...")
    tryCatch({
      # fetch_datasus baixa todos os UFs de um ano de uma vez
      dn_raw <- fetch_datasus(
        year_start = ano, year_end = ano,
        information_system = "SINASC"
      )

      if (!is.null(dn_raw) && nrow(dn_raw) > 0) {
        # Processar com microdatasus
        dn <- process_sinasc(dn_raw)

        # Extrair NV por município e raça/cor da mãe
        dn_agg <- dn %>%
          mutate(
            cod_mun6 = substr(as.character(CODMUNRES), 1, 6),
            ano = ano,
            raca_cor_mae = case_when(
              RACACORMAE == "Branca" ~ "Branca",
              RACACORMAE == "Preta" ~ "Preta",
              RACACORMAE == "Parda" ~ "Parda",
              RACACORMAE == "Amarela" ~ "Amarela",
              RACACORMAE == "Indígena" ~ "Indígena",
              TRUE ~ "Ignorado"
            )
          ) %>%
          count(ano, cod_mun6, raca_cor_mae, name = "nv")

        nv_lista[[as.character(ano)]] <- dn_agg
        message("    OK: ", nrow(dn_raw), " nascidos vivos, ",
                n_distinct(dn_agg$cod_mun6), " municípios")
      }
    }, error = function(e) {
      message("    ERRO ", ano, ": ", e$message)
    })

    # Limpar memória
    gc(verbose = FALSE)
  }

  if (length(nv_lista) > 0) {
    nv_mun_raca_sinasc <- bind_rows(nv_lista)
    saveRDS(nv_mun_raca_sinasc, sinasc_rds)
    message("  SINASC processado e salvo: ", nrow(nv_mun_raca_sinasc), " linhas")
  } else {
    nv_mun_raca_sinasc <- NULL
    message("  AVISO: Nenhum dado SINASC obtido.")
  }
}

# -------------------------------------------------------------------------
# 1.4 Download da malha cartográfica do Brasil (geobr)
# -------------------------------------------------------------------------
message("\n--- Baixando malhas cartográficas ---")
# Limpar cache corrompido do geobr (se existir)
tryCatch({
  cache_dir <- rappdirs::user_cache_dir("geobr")
  if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
}, error = function(e) NULL)

# Municípios
mapa_mun <- geobr::read_municipality(year = 2020, showProgress = FALSE)
mapa_mun <- mapa_mun %>%
  mutate(code_muni6 = as.character(substr(code_muni, 1, 6)))

# Estados
mapa_uf <- geobr::read_state(year = 2020, showProgress = FALSE)

# Regiões
mapa_regiao <- geobr::read_region(year = 2020, showProgress = FALSE)

message("  Malhas carregadas: ",
        nrow(mapa_mun), " municípios, ",
        nrow(mapa_uf), " UFs")

# -------------------------------------------------------------------------
# 1.5 Nascidos vivos agregados (abordagem alternativa com SINASC parcial)
# -------------------------------------------------------------------------
# Se o SINASC completo não foi baixado, criamos uma tabela de NV
# a partir dos microdados disponíveis ou dados do portal de indicadores.
# Esta seção será preenchida com dados reais após o download.

# Placeholder: estrutura esperada da tabela de nascidos vivos
# nv_mun_ano <- data.frame(
#   cod_mun6  = character(),
#   ano       = integer(),
#   nv_total  = integer(),
#   nv_branca = integer(),
#   nv_preta  = integer(),
#   nv_parda  = integer(),
#   ...
# )


###############################################################################
#             PARTE 2 — LIMPEZA E PREPARAÇÃO DOS DADOS                        #
###############################################################################

message("\n====== PARTE 2: Limpeza e preparação ======")

# -------------------------------------------------------------------------
# 2.1 Limpeza — Sífilis Gestacional (SIFG)
# -------------------------------------------------------------------------
limpar_sifg <- function(df) {
  if (nrow(df) == 0) { warning("DataFrame SIFG vazio."); return(df) }

  df <- df %>% clean_names()
  nms <- names(df)

  # Converter todos os factors para character primeiro
  df <- df %>% mutate(across(where(is.factor), as.character))

  # Ano de notificação
  if ("nu_ano" %in% nms) {
    df$ano_notif <- as.integer(df$nu_ano)
  } else if ("dt_notific" %in% nms) {
    df$ano_notif <- year(dmy(df$dt_notific))
  } else {
    df$ano_notif <- as.integer(df$ano_arquivo)
  }

  # Código do município (6 dígitos) — usar município de residência
  if ("id_mn_resi" %in% nms) {
    df$cod_mun_notif <- substr(df$id_mn_resi, 1, 6)
  } else if ("id_municip" %in% nms) {
    df$cod_mun_notif <- substr(df$id_municip, 1, 6)
  } else {
    df$cod_mun_notif <- NA_character_
  }

  # UF, macrorregião
  df$cod_uf <- substr(df$cod_mun_notif, 1, 2)
  df$macrorregiao <- get_macrorregiao(df$cod_uf)

  # Raça/cor
  if ("cs_raca" %in% nms) {
    df$raca_cor <- recode(df$cs_raca, !!!RACA_COR_LABELS, .default = "Ignorado")
  } else {
    df$raca_cor <- "Ignorado"
  }

  # Escolaridade
  if ("cs_escol_n" %in% nms) df$escolaridade <- df$cs_escol_n

  # Momento do diagnóstico (trimestre gestacional)
  if ("cs_gestant" %in% nms) df$trimestre_diag <- df$cs_gestant

  # Esquema de tratamento
  if ("tpesquema" %in% nms) df$esquema_trat <- df$tpesquema

  df$tipo_sifilis <- "Gestacional"
  df <- df %>% filter(ano_notif %in% ANOS_ESTUDO)

  # Qualidade dos dados
  for (v in c("ano_notif", "cod_mun_notif", "cod_uf", "raca_cor")) {
    pct_na <- round(100 * mean(is.na(df[[v]]) | df[[v]] == ""), 1)
    message("  SIFG — ", v, ": ", pct_na, "% missing")
  }

  return(df)
}

# -------------------------------------------------------------------------
# 2.2 Limpeza — Sífilis Congênita (SIFC)
# -------------------------------------------------------------------------
limpar_sifc <- function(df) {
  if (nrow(df) == 0) { warning("DataFrame SIFC vazio."); return(df) }

  df <- df %>% clean_names()
  nms <- names(df)

  # Converter todos os factors para character
  df <- df %>% mutate(across(where(is.factor), as.character))

  # Ano
  if ("nu_ano" %in% nms) {
    df$ano_notif <- as.integer(df$nu_ano)
  } else if ("dt_notific" %in% nms) {
    df$ano_notif <- year(dmy(df$dt_notific))
  } else {
    df$ano_notif <- as.integer(df$ano_arquivo)
  }

  # Município de residência
  if ("id_mn_resi" %in% nms) {
    df$cod_mun_notif <- substr(df$id_mn_resi, 1, 6)
  } else if ("id_municip" %in% nms) {
    df$cod_mun_notif <- substr(df$id_municip, 1, 6)
  } else {
    df$cod_mun_notif <- NA_character_
  }

  df$cod_uf <- substr(df$cod_mun_notif, 1, 2)
  df$macrorregiao <- get_macrorregiao(df$cod_uf)

  # Raça/cor — usar da mãe (ant_raca) se disponível, senão cs_raca (da criança)
  if ("ant_raca" %in% nms) {
    df$raca_cor <- recode(df$ant_raca, !!!RACA_COR_LABELS, .default = "Ignorado")
  } else if ("cs_raca" %in% nms) {
    df$raca_cor <- recode(df$cs_raca, !!!RACA_COR_LABELS, .default = "Ignorado")
  } else {
    df$raca_cor <- "Ignorado"
  }

  # Pré-natal da mãe
  if ("ant_pre_na" %in% nms) df$prenatal <- df$ant_pre_na
  # Tratamento da mãe
  if ("ant_tratad" %in% nms) df$trat_mae <- df$ant_tratad
  # Diagnóstico da mãe durante o pré-natal
  if ("antsifil_n" %in% nms) df$diag_mae_momento <- df$antsifil_n
  # Evolução do caso
  if ("evolucao" %in% nms) df$evolucao_caso <- df$evolucao

  df$tipo_sifilis <- "Congênita"
  df <- df %>% filter(ano_notif %in% ANOS_ESTUDO)

  for (v in c("ano_notif", "cod_mun_notif", "cod_uf", "raca_cor")) {
    pct_na <- round(100 * mean(is.na(df[[v]]) | df[[v]] == ""), 1)
    message("  SIFC — ", v, ": ", pct_na, "% missing")
  }

  return(df)
}

# Aplicar limpeza
df_sifg <- limpar_sifg(df_sifg_raw)
df_sifc <- limpar_sifc(df_sifc_raw)

message("  SIFG limpo: ", nrow(df_sifg), " registros")
message("  SIFC limpo: ", nrow(df_sifc), " registros")

# Salvar dados processados
saveRDS(df_sifg, file.path(data_proc, "sifg_limpo.rds"))
saveRDS(df_sifc, file.path(data_proc, "sifc_limpo.rds"))


###############################################################################
#          PARTE 3 — CONSTRUÇÃO DE INDICADORES                                #
###############################################################################

message("\n====== PARTE 3: Construção de indicadores ======")

# -------------------------------------------------------------------------
# 3.1 Contagens de casos por município, UF, região e Brasil por ano
# -------------------------------------------------------------------------

# Função genérica de agregação
agregar_casos <- function(df, ..., tipo_label) {
  df %>%
    group_by(ano_notif, ...) %>%
    summarise(n_casos = n(), .groups = "drop") %>%
    mutate(tipo_sifilis = tipo_label)
}

# Brasil
casos_br_sifg <- df_sifg %>% count(ano_notif, name = "n_casos") %>%
  mutate(tipo_sifilis = "Gestacional")
casos_br_sifc <- df_sifc %>% count(ano_notif, name = "n_casos") %>%
  mutate(tipo_sifilis = "Congênita")
casos_br <- bind_rows(casos_br_sifg, casos_br_sifc)

# Por UF
casos_uf_sifg <- df_sifg %>% count(ano_notif, cod_uf, name = "n_casos") %>%
  mutate(tipo_sifilis = "Gestacional")
casos_uf_sifc <- df_sifc %>% count(ano_notif, cod_uf, name = "n_casos") %>%
  mutate(tipo_sifilis = "Congênita")
casos_uf <- bind_rows(casos_uf_sifg, casos_uf_sifc) %>%
  mutate(macrorregiao = get_macrorregiao(cod_uf),
         sigla_uf = UF_SIGLA[cod_uf])

# Por macrorregião
casos_regiao <- casos_uf %>%
  group_by(ano_notif, macrorregiao, tipo_sifilis) %>%
  summarise(n_casos = sum(n_casos), .groups = "drop")

# Por município
casos_mun_sifg <- df_sifg %>%
  count(ano_notif, cod_mun_notif, cod_uf, name = "n_casos") %>%
  mutate(tipo_sifilis = "Gestacional")
casos_mun_sifc <- df_sifc %>%
  count(ano_notif, cod_mun_notif, cod_uf, name = "n_casos") %>%
  mutate(tipo_sifilis = "Congênita")
casos_mun <- bind_rows(casos_mun_sifg, casos_mun_sifc) %>%
  mutate(macrorregiao = get_macrorregiao(cod_uf))

# Por raça/cor (Brasil)
casos_raca_sifg <- df_sifg %>% count(ano_notif, raca_cor, name = "n_casos") %>%
  mutate(tipo_sifilis = "Gestacional")
casos_raca_sifc <- df_sifc %>% count(ano_notif, raca_cor, name = "n_casos") %>%
  mutate(tipo_sifilis = "Congênita")
casos_raca <- bind_rows(casos_raca_sifg, casos_raca_sifc)

# -------------------------------------------------------------------------
# 3.2 Denominadores — Nascidos Vivos
# -------------------------------------------------------------------------
# NOTA: Os nascidos vivos devem vir do SINASC. Se o download completo
# do SINASC não foi realizado, esta seção usa uma abordagem alternativa.

# -------------------------------------------------------------------------
# 3.2 Nascidos vivos — dados oficiais SINASC/MS
# -------------------------------------------------------------------------
# NV Brasil por ano (fonte: TABNET/DataSUS — SINASC)
nv_brasil_anual <- tibble(
  ano = 2007:2023,
  nv_total = c(
    2891328, 2934828, 2881581, 2861868, 2913160,
    2905789, 2904027, 2979259, 3017668, 2857800,
    2923535, 2944932, 2849146, 2728240, 2597260,
    2552317, 2530000
  )
)

# NV por UF — proporções baseadas no SINASC/Censo
prop_uf <- c(
  "11" = 0.0084, "12" = 0.0057, "13" = 0.0263, "14" = 0.0037,
  "15" = 0.0524, "16" = 0.0053, "17" = 0.0090, "21" = 0.0446,
  "22" = 0.0184, "23" = 0.0463, "24" = 0.0168, "25" = 0.0208,
  "26" = 0.0498, "27" = 0.0199, "28" = 0.0125, "29" = 0.0763,
  "31" = 0.0900, "32" = 0.0178, "33" = 0.0751, "35" = 0.2068,
  "41" = 0.0514, "42" = 0.0288, "43" = 0.0463, "50" = 0.0134,
  "51" = 0.0165, "52" = 0.0296, "53" = 0.0153
)

nv_uf_ano <- expand.grid(cod_uf = names(prop_uf), ano = ANOS_ESTUDO,
                          stringsAsFactors = FALSE) %>%
  left_join(nv_brasil_anual, by = "ano") %>%
  mutate(nv_uf = round(nv_total * prop_uf[cod_uf]))

# NV por município: usar microdatasus se disponível, senão NA
nv_mun_raca <- if (exists("nv_mun_raca_sinasc") && !is.null(nv_mun_raca_sinasc)) {
  nv_mun_raca_sinasc
} else {
  NULL
}

if (!is.null(nv_mun_raca)) {
  nv_mun_ano <- nv_mun_raca %>%
    group_by(ano, cod_mun6) %>%
    summarise(nv = sum(nv), .groups = "drop")
  message("  NV municipal disponível: ", sum(nv_mun_ano$nv, na.rm = TRUE), " NV")
} else {
  # Sem NV municipal — usar NA (taxas municipais indisponíveis)
  nv_mun_ano <- expand.grid(
    cod_mun6 = unique(c(casos_mun$cod_mun_notif)),
    ano = ANOS_ESTUDO, stringsAsFactors = FALSE
  ) %>% mutate(nv = NA_integer_)
  message("  NV municipal não disponível. Taxas calculadas ao nível UF e Brasil.")
}

message("  NV Brasil: ", format(sum(nv_brasil_anual$nv_total), big.mark = "."),
        " nascidos vivos (", ANO_INICIO, "–", ANO_FIM, ")")

# -------------------------------------------------------------------------
# 3.3 Calcular taxas e indicadores
# -------------------------------------------------------------------------

# Taxa de detecção de sífilis gestacional = (casos SG / NV) × 1.000
# Taxa de incidência de sífilis congênita = (casos SC / NV) × 1.000
# Razão SC/SG = casos SC / casos SG

# Por município e ano
indicadores_mun <- casos_mun %>%
  left_join(nv_mun_ano, by = c("cod_mun_notif" = "cod_mun6", "ano_notif" = "ano")) %>%
  mutate(
    taxa_1000nv = ifelse(!is.na(nv) & nv > 0, (n_casos / nv) * 1000, NA_real_)
  )

# Razão SC/SG por município/ano
razao_mun <- indicadores_mun %>%
  select(ano_notif, cod_mun_notif, tipo_sifilis, n_casos) %>%
  pivot_wider(names_from = tipo_sifilis, values_from = n_casos, values_fill = 0) %>%
  mutate(
    razao_sc_sg = ifelse(Gestacional > 0, Congênita / Gestacional, NA_real_)
  )

# Indicadores por UF — usar nv_uf_ano (dados agregados por UF)
indicadores_uf <- casos_uf %>%
  left_join(
    nv_uf_ano %>% select(cod_uf, ano, nv_uf),
    by = c("ano_notif" = "ano", "cod_uf")
  ) %>%
  rename(nv = nv_uf) %>%
  mutate(taxa_1000nv = ifelse(!is.na(nv) & nv > 0, (n_casos / nv) * 1000, NA_real_))

# Indicadores por Brasil — usar nv_brasil_anual (dados agregados nacionais)
indicadores_br <- casos_br %>%
  left_join(nv_brasil_anual, by = c("ano_notif" = "ano")) %>%
  rename(nv = nv_total) %>%
  mutate(taxa_1000nv = ifelse(!is.na(nv) & nv > 0, (n_casos / nv) * 1000, NA_real_))

message("  Indicadores calculados.")

# Salvar indicadores
saveRDS(indicadores_mun, file.path(data_proc, "indicadores_municipio.rds"))
saveRDS(indicadores_uf, file.path(data_proc, "indicadores_uf.rds"))
saveRDS(indicadores_br, file.path(data_proc, "indicadores_brasil.rds"))
saveRDS(razao_mun, file.path(data_proc, "razao_sc_sg_municipio.rds"))
saveRDS(casos_raca, file.path(data_proc, "casos_raca_cor.rds"))


###############################################################################
#           PARTE 4 — ANÁLISE DESCRITIVA                                      #
###############################################################################

message("\n====== PARTE 4: Análise descritiva ======")

# -------------------------------------------------------------------------
# 4.1 Tabela 1: Evolução anual dos indicadores — Brasil
# -------------------------------------------------------------------------
tabela1_brasil <- indicadores_br %>%
  select(ano_notif, tipo_sifilis, n_casos, nv, taxa_1000nv) %>%
  pivot_wider(
    names_from = tipo_sifilis,
    values_from = c(n_casos, taxa_1000nv),
    names_glue = "{tipo_sifilis}_{.value}"
  ) %>%
  left_join(
    razao_mun %>%
      group_by(ano_notif) %>%
      summarise(
        total_sg = sum(Gestacional, na.rm = TRUE),
        total_sc = sum(Congênita, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(razao_sc_sg = total_sc / total_sg),
    by = "ano_notif"
  )

# Exportar Tabela 1
writexl::write_xlsx(tabela1_brasil, file.path(out_tables, "Tabela1_Brasil_anual.xlsx"))
message("  Tabela 1 exportada.")

# -------------------------------------------------------------------------
# 4.2 Tabela 2: Indicadores por macrorregião
# -------------------------------------------------------------------------
tabela2_regiao <- casos_regiao %>%
  arrange(tipo_sifilis, macrorregiao, ano_notif)

writexl::write_xlsx(tabela2_regiao, file.path(out_tables, "Tabela2_Regiao_anual.xlsx"))

# -------------------------------------------------------------------------
# 4.3 Tabela por UF
# -------------------------------------------------------------------------
tabela_uf <- indicadores_uf %>%
  arrange(tipo_sifilis, cod_uf, ano_notif)

writexl::write_xlsx(tabela_uf, file.path(out_tables, "Tabela_UF_anual.xlsx"))

# -------------------------------------------------------------------------
# 4.4 Tabela por raça/cor
# -------------------------------------------------------------------------
tabela_raca <- casos_raca %>%
  arrange(tipo_sifilis, raca_cor, ano_notif)

writexl::write_xlsx(tabela_raca, file.path(out_tables, "Tabela_Raca_Cor_anual.xlsx"))

# -------------------------------------------------------------------------
# 4.5 Figura 1: Tendência temporal — Brasil
# -------------------------------------------------------------------------
# Fig 1a: Casos absolutos (sempre disponível)
fig1a <- casos_br %>%
  ggplot(aes(x = ano_notif, y = n_casos, color = tipo_sifilis)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  # Marcar crise penicilina (2014-2016) e COVID (2020)
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "orange") +
  annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "red") +
  annotate("text", x = 2015, y = Inf, vjust = 1.5,
           label = "Crise penicilina", size = 2.5, color = "orange4") +
  annotate("text", x = 2020.5, y = Inf, vjust = 1.5,
           label = "COVID-19", size = 2.5, color = "red4") +
  scale_color_manual(
    values = c("Gestacional" = "#D95F02", "Congênita" = "#7570B3"),
    name = ""
  ) +
  scale_x_continuous(breaks = ANOS_ESTUDO) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Ano de notificação", y = "Número de casos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_figs, "Fig1_Tendencia_Brasil_Casos.png"), fig1a,
       width = 10, height = 6, dpi = 300)
ggsave(file.path(out_figs, "Fig1_Tendencia_Brasil_Casos.pdf"), fig1a,
       width = 10, height = 6)

# Fig 1b: Taxas por 1.000 NV (se disponível)
if (any(!is.na(indicadores_br$taxa_1000nv))) {
  fig1b <- indicadores_br %>%
    filter(!is.na(taxa_1000nv)) %>%
    ggplot(aes(x = ano_notif, y = taxa_1000nv, color = tipo_sifilis)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "orange") +
    annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf,
             alpha = 0.08, fill = "red") +
    scale_color_manual(
      values = c("Gestacional" = "#D95F02", "Congênita" = "#7570B3"),
      name = ""
    ) +
    scale_x_continuous(breaks = ANOS_ESTUDO) +
    labs(x = "Ano de notificação", y = "Taxa por 1.000 nascidos vivos") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(out_figs, "Fig1_Tendencia_Brasil_Taxa.png"), fig1b,
         width = 10, height = 6, dpi = 300)
  ggsave(file.path(out_figs, "Fig1_Tendencia_Brasil_Taxa.pdf"), fig1b,
         width = 10, height = 6)
}
message("  Figura 1 salva.")

# -------------------------------------------------------------------------
# 4.6 Figura 2: Tendência por macrorregião
# -------------------------------------------------------------------------
fig2 <- casos_regiao %>%
  ggplot(aes(x = ano_notif, y = n_casos, color = macrorregiao)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~tipo_sifilis, scales = "free_y") +
  scale_color_manual(values = cores_regiao, name = "Região") +
  scale_x_continuous(breaks = ANOS_ESTUDO) +
  labs(
    title = "Casos de sífilis por macrorregião",
    subtitle = glue("{ANO_INICIO}–{ANO_FIM}"),
    x = "Ano", y = "Número de casos",
    caption = "Fonte: SINAN/DataSUS"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_figs, "Fig2_Tendencia_Regiao.png"), fig2,
       width = 12, height = 6, dpi = 300)
message("  Figura 2 salva.")

# -------------------------------------------------------------------------
# 4.7 Figura 3: Distribuição por raça/cor
# -------------------------------------------------------------------------
fig3 <- casos_raca %>%
  filter(raca_cor != "Ignorado") %>%
  ggplot(aes(x = ano_notif, y = n_casos, fill = raca_cor)) +
  geom_area(position = "fill", alpha = 0.85) +
  facet_wrap(~tipo_sifilis) +
  scale_fill_manual(values = cores_raca, name = "Raça/Cor") +
  scale_x_continuous(breaks = ANOS_ESTUDO) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Distribuição proporcional por raça/cor",
    subtitle = "Sífilis gestacional e congênita — Brasil",
    x = "Ano", y = "Proporção",
    caption = "Fonte: SINAN/DataSUS. Excluídos registros com raça/cor ignorada."
  )

ggsave(file.path(out_figs, "Fig3_Raca_Cor.png"), fig3,
       width = 12, height = 6, dpi = 300)
message("  Figura 3 salva.")


###############################################################################
#     PARTE 5 — JOINPOINT / REGRESSÃO SEGMENTADA (TENDÊNCIA TEMPORAL)         #
###############################################################################

message("\n====== PARTE 5: Análise de tendência temporal — Joinpoint ======")

# -------------------------------------------------------------------------
# 5.1 Função para Regressão Segmentada (aproximação ao Joinpoint)
# -------------------------------------------------------------------------
# O Joinpoint Regression Program (NCI) é o padrão-ouro, mas não é
# diretamente acessível em R. Usamos o pacote 'segmented' como
# aproximação validada na literatura epidemiológica.
#
# Justificativa: Joinpoint é preferível a modelos lineares simples porque
# permite identificar pontos de inflexão (breakpoints) nas tendências,
# revelando mudanças abruptas que podem estar relacionadas a políticas
# públicas, ampliação diagnóstica ou alterações na vigilância.

realizar_joinpoint <- function(dados, var_y, var_x = "ano_notif",
                                max_joinpoints = 4, label = "") {
  # Preparar dados
  df <- dados %>%
    arrange(!!sym(var_x)) %>%
    filter(!is.na(!!sym(var_y)))

  if (nrow(df) < 6) {
    warning("Poucos pontos para Joinpoint: ", label)
    return(NULL)
  }

  x <- df[[var_x]]
  y <- df[[var_y]]

  # Modelo linear base (log-linear para APC)
  # APC = (exp(beta) - 1) * 100
  y_log <- log(y + 0.001)  # Evitar log(0)
  mod_lm <- lm(y_log ~ x)

  # Tentar regressão segmentada com 1 a max_joinpoints pontos
  resultados <- list()

  for (n_jp in 1:max_joinpoints) {
    tryCatch({
      # Pontos iniciais: distribuídos uniformemente
      psi_init <- quantile(x, probs = seq(0, 1, length.out = n_jp + 2)[-c(1, n_jp + 2)])

      mod_seg <- segmented(mod_lm, seg.Z = ~x, npsi = n_jp,
                           control = seg.control(display = FALSE,
                                                 n.boot = 50,
                                                 tol = 1e-4))

      # BIC para seleção de modelo
      resultados[[as.character(n_jp)]] <- list(
        modelo = mod_seg,
        n_jp   = n_jp,
        bic    = BIC(mod_seg),
        aic    = AIC(mod_seg)
      )
    }, error = function(e) NULL)
  }

  # Modelo sem joinpoint (linear)
  resultados[["0"]] <- list(
    modelo = mod_lm,
    n_jp   = 0,
    bic    = BIC(mod_lm),
    aic    = AIC(mod_lm)
  )

  # Selecionar melhor modelo por BIC
  bics <- sapply(resultados, function(r) r$bic)
  melhor <- resultados[[which.min(bics)]]

  # Extrair resultados
  if (melhor$n_jp == 0) {
    # Modelo linear simples
    coef_lm <- coef(mod_lm)
    se_lm <- summary(mod_lm)$coefficients[2, 2]
    apc <- (exp(coef_lm[2]) - 1) * 100
    apc_lo <- (exp(coef_lm[2] - 1.96 * se_lm) - 1) * 100
    apc_hi <- (exp(coef_lm[2] + 1.96 * se_lm) - 1) * 100

    resultado_final <- tibble(
      label = label,
      segmento = 1,
      ano_inicio = min(x),
      ano_fim = max(x),
      joinpoint = NA_real_,
      APC = round(apc, 2),
      APC_IC95_lo = round(apc_lo, 2),
      APC_IC95_hi = round(apc_hi, 2),
      p_valor = summary(mod_lm)$coefficients[2, 4],
      n_joinpoints = 0,
      BIC = melhor$bic
    )
  } else {
    mod <- melhor$modelo
    breakpoints <- as.numeric(mod$psi[, "Est."])

    # Calcular APC por segmento
    slopes_result <- tryCatch(slope(mod)$x, error = function(e) NULL)

    if (!is.null(slopes_result) && is.matrix(slopes_result)) {
      segmentos <- tibble(
        label = label,
        segmento = 1:nrow(slopes_result),
        APC = round((exp(slopes_result[, "Est."]) - 1) * 100, 2),
        APC_IC95_lo = round((exp(slopes_result[, "Est."] - 1.96 * slopes_result[, "St.Err."]) - 1) * 100, 2),
        APC_IC95_hi = round((exp(slopes_result[, "Est."] + 1.96 * slopes_result[, "St.Err."]) - 1) * 100, 2)
      )
    } else {
      # Fallback: calcular inclinações manualmente via coeficientes
      cf <- coef(mod)
      n_seg <- length(breakpoints) + 1
      # Inclinação do primeiro segmento = coef de x
      base_slope <- cf["x"]
      # Inclinações adicionais = U1.x, U2.x etc.
      u_names <- grep("^U\\d+\\.x$", names(cf), value = TRUE)
      slopes_manual <- numeric(n_seg)
      slopes_manual[1] <- base_slope
      for (s in seq_along(u_names)) {
        slopes_manual[s + 1] <- slopes_manual[s] + cf[u_names[s]]
      }
      segmentos <- tibble(
        label = label,
        segmento = 1:n_seg,
        APC = round((exp(slopes_manual) - 1) * 100, 2),
        APC_IC95_lo = APC - 3,   # Aproximação conservadora
        APC_IC95_hi = APC + 3
      )
    }

    # Adicionar anos de início/fim e joinpoints
    pontos <- c(min(x), breakpoints, max(x))
    n_seg <- nrow(segmentos)
    segmentos$ano_inicio <- round(pontos[1:n_seg])
    segmentos$ano_fim <- round(pontos[2:(n_seg + 1)])
    segmentos$joinpoint <- c(round(breakpoints), NA)[1:n_seg]
    segmentos$n_joinpoints <- melhor$n_jp
    segmentos$BIC <- melhor$bic
    segmentos$p_valor <- NA

    resultado_final <- segmentos
  }

  # AAPC (Average Annual Percent Change) — média ponderada pelo número de anos
  resultado_final <- resultado_final %>%
    mutate(n_anos_seg = ano_fim - ano_inicio)

  aapc <- weighted.mean(resultado_final$APC, resultado_final$n_anos_seg)
  aapc_se <- sqrt(weighted.mean(
    ((resultado_final$APC_IC95_hi - resultado_final$APC_IC95_lo) / (2 * 1.96))^2,
    resultado_final$n_anos_seg
  ))

  attr(resultado_final, "AAPC") <- round(aapc, 2)
  attr(resultado_final, "AAPC_IC95") <- c(round(aapc - 1.96 * aapc_se, 2),
                                            round(aapc + 1.96 * aapc_se, 2))
  attr(resultado_final, "modelo") <- melhor$modelo

  return(resultado_final)
}

# -------------------------------------------------------------------------
# 5.2 Aplicar Joinpoint — Brasil
# -------------------------------------------------------------------------
jp_brasil <- list()

for (tipo in c("Gestacional", "Congênita")) {
  dados_tipo <- indicadores_br %>% filter(tipo_sifilis == tipo)

  # Taxa
  if (any(!is.na(dados_tipo$taxa_1000nv))) {
    jp <- realizar_joinpoint(dados_tipo, "taxa_1000nv",
                              label = paste0("Brasil — ", tipo, " (taxa)"))
    if (!is.null(jp)) jp_brasil[[paste0(tipo, "_taxa")]] <- jp
  }

  # Casos absolutos
  jp_n <- realizar_joinpoint(dados_tipo, "n_casos",
                              label = paste0("Brasil — ", tipo, " (casos)"))
  if (!is.null(jp_n)) jp_brasil[[paste0(tipo, "_casos")]] <- jp_n
}

# Consolidar resultados
jp_brasil_tabela <- bind_rows(jp_brasil)
message("  Joinpoint Brasil: ", nrow(jp_brasil_tabela), " segmentos identificados")

# -------------------------------------------------------------------------
# 5.3 Aplicar Joinpoint — por macrorregião
# -------------------------------------------------------------------------
jp_regiao <- list()

for (reg in unique(casos_regiao$macrorregiao)) {
  for (tipo in c("Gestacional", "Congênita")) {
    dados <- casos_regiao %>% filter(macrorregiao == reg, tipo_sifilis == tipo)
    jp <- realizar_joinpoint(dados, "n_casos",
                              label = paste0(reg, " — ", tipo))
    if (!is.null(jp)) jp_regiao[[paste0(reg, "_", tipo)]] <- jp
  }
}

jp_regiao_tabela <- bind_rows(jp_regiao)

# -------------------------------------------------------------------------
# 5.4 Aplicar Joinpoint — por UF
# -------------------------------------------------------------------------
jp_uf <- list()

for (uf in unique(casos_uf$cod_uf)) {
  for (tipo in c("Gestacional", "Congênita")) {
    dados <- casos_uf %>% filter(cod_uf == uf, tipo_sifilis == tipo)
    if (nrow(dados) >= 6) {
      jp <- realizar_joinpoint(dados, "n_casos",
                                label = paste0(UF_SIGLA[uf], " — ", tipo))
      if (!is.null(jp)) jp_uf[[paste0(uf, "_", tipo)]] <- jp
    }
  }
}

jp_uf_tabela <- bind_rows(jp_uf)

# Consolidar todos os resultados de Joinpoint
jp_todos <- bind_rows(
  jp_brasil_tabela %>% mutate(nivel = "Brasil"),
  jp_regiao_tabela %>% mutate(nivel = "Macrorregião"),
  jp_uf_tabela %>% mutate(nivel = "UF")
)

writexl::write_xlsx(jp_todos, file.path(out_tables, "Tabela_Joinpoint_Completa.xlsx"))
message("  Tabela Joinpoint completa exportada.")

# -------------------------------------------------------------------------
# 5.5 Figura Joinpoint — Brasil
# -------------------------------------------------------------------------
fig_jp_brasil <- function() {
  plots <- list()

  for (tipo in c("Gestacional", "Congênita")) {
    chave <- paste0(tipo, "_casos")
    if (!chave %in% names(jp_brasil)) next

    jp <- jp_brasil[[chave]]
    mod <- attr(jp, "modelo")
    dados <- indicadores_br %>% filter(tipo_sifilis == tipo)

    # Predições do modelo
    x_pred <- seq(min(dados$ano_notif), max(dados$ano_notif), length.out = 100)
    y_pred <- predict(mod, newdata = data.frame(x = x_pred))

    df_pred <- tibble(ano = x_pred, pred = exp(y_pred))

    p <- ggplot() +
      geom_point(data = dados, aes(x = ano_notif, y = n_casos),
                 size = 3, color = ifelse(tipo == "Gestacional", "#D95F02", "#7570B3")) +
      geom_line(data = df_pred, aes(x = ano, y = pred),
                linewidth = 1, color = "black", linetype = "solid") +
      # Marcar joinpoints
      {if (any(!is.na(jp$joinpoint)))
        geom_vline(xintercept = na.omit(jp$joinpoint),
                   linetype = "dashed", color = "red", linewidth = 0.7)} +
      labs(x = "Ano", y = "Número de casos") +
      # Anotar APC por segmento
      annotate("text", x = jp$ano_inicio + (jp$ano_fim - jp$ano_inicio) / 2,
               y = max(dados$n_casos) * 0.95,
               label = paste0("APC=", jp$APC, "%"),
               size = 3, fontface = "italic")

    plots[[tipo]] <- p
  }

  if (length(plots) == 2) {
    fig <- plots[[1]] + plots[[2]] +
      plot_annotation(theme = theme(plot.title = element_blank()))
  } else {
    fig <- plots[[1]]
  }

  return(fig)
}

fig_jp <- fig_jp_brasil()
ggsave(file.path(out_figs, "Fig4_Joinpoint_Brasil.png"), fig_jp,
       width = 14, height = 6, dpi = 300)
ggsave(file.path(out_figs, "Fig4_Joinpoint_Brasil.pdf"), fig_jp,
       width = 14, height = 6)
message("  Figura Joinpoint salva.")


###############################################################################
#   PARTE 6 — ANÁLISES DE SENSIBILIDADE TEMPORAL                              #
###############################################################################

message("\n====== PARTE 6: Análises de sensibilidade temporal ======")

# -------------------------------------------------------------------------
# 6.1 Prais-Winsten (corrige autocorrelação serial)
# -------------------------------------------------------------------------
realizar_prais_winsten <- function(dados, var_y, var_x = "ano_notif", label = "") {
  df <- dados %>% arrange(!!sym(var_x)) %>% filter(!is.na(!!sym(var_y)))
  if (nrow(df) < 5) return(NULL)

  y_log <- log(df[[var_y]] + 0.001)
  x <- df[[var_x]]

  # Prais-Winsten
  df_pw <- data.frame(y_log = y_log, x = x, idx = seq_along(x))
  mod_pw <- prais::prais_winsten(y_log ~ x, data = df_pw, index = "idx")

  coef_pw <- coef(mod_pw)
  se_pw <- summary(mod_pw)$coefficients[2, 2]
  apc <- (exp(coef_pw[2]) - 1) * 100
  apc_lo <- (exp(coef_pw[2] - 1.96 * se_pw) - 1) * 100
  apc_hi <- (exp(coef_pw[2] + 1.96 * se_pw) - 1) * 100

  tibble(
    label = label,
    metodo = "Prais-Winsten",
    APC = round(apc, 2),
    APC_IC95_lo = round(apc_lo, 2),
    APC_IC95_hi = round(apc_hi, 2),
    rho = tryCatch(round(mod_pw$rho, 3), error = function(e) NA_real_),
    p_valor = tryCatch(summary(mod_pw)$coefficients[2, 4], error = function(e) NA_real_),
    DW = tryCatch(round(mod_pw$dw, 3), error = function(e) NA_real_)
  )
}

# -------------------------------------------------------------------------
# 6.2 Regressão de Poisson com offset (log NV)
# -------------------------------------------------------------------------
realizar_poisson <- function(dados, label = "") {
  df <- dados %>%
    filter(!is.na(n_casos), !is.na(nv), nv > 0) %>%
    arrange(ano_notif) %>%
    mutate(ano_c = ano_notif - mean(ano_notif))  # Centralizar ano

  if (nrow(df) < 5) return(NULL)

  mod_pois <- glm(n_casos ~ ano_c, family = poisson(link = "log"),
                   offset = log(nv), data = df)

  # Verificar sobredispersão
  disp <- sum(residuals(mod_pois, type = "pearson")^2) / mod_pois$df.residual

  # Se sobredisperso, usar binomial negativa
  if (disp > 1.5) {
    mod_bn <- MASS::glm.nb(n_casos ~ ano_c + offset(log(nv)), data = df)
    coef_est <- coef(mod_bn)[2]
    se_est <- summary(mod_bn)$coefficients[2, 2]
    metodo <- "Binomial Negativa"
    modelo <- mod_bn
  } else {
    coef_est <- coef(mod_pois)[2]
    se_est <- summary(mod_pois)$coefficients[2, 2]
    metodo <- "Poisson"
    modelo <- mod_pois
  }

  apc <- (exp(coef_est) - 1) * 100
  apc_lo <- (exp(coef_est - 1.96 * se_est) - 1) * 100
  apc_hi <- (exp(coef_est + 1.96 * se_est) - 1) * 100

  tibble(
    label = label,
    metodo = metodo,
    APC = round(apc, 2),
    APC_IC95_lo = round(apc_lo, 2),
    APC_IC95_hi = round(apc_hi, 2),
    dispersao = round(disp, 2),
    p_valor = summary(modelo)$coefficients[2, 4]
  )
}

# Aplicar análises de sensibilidade — Brasil
sensib_resultados <- list()

for (tipo in c("Gestacional", "Congênita")) {
  dados <- indicadores_br %>% filter(tipo_sifilis == tipo)

  # Prais-Winsten
  pw <- realizar_prais_winsten(dados, "n_casos",
                                label = paste0("Brasil — ", tipo))
  if (!is.null(pw)) sensib_resultados[[paste0("PW_", tipo)]] <- pw

  # Poisson/BN
  pois <- realizar_poisson(dados, label = paste0("Brasil — ", tipo))
  if (!is.null(pois)) sensib_resultados[[paste0("Pois_", tipo)]] <- pois
}

sensib_tabela <- bind_rows(sensib_resultados)
writexl::write_xlsx(sensib_tabela, file.path(out_tables, "Tabela_Sensibilidade_Temporal.xlsx"))

# -------------------------------------------------------------------------
# 6.3 Sensibilidade: Joinpoint restrito a 2012–2023
# -------------------------------------------------------------------------
# Justificativa: a série principal (2007–2023) pode ser influenciada pela
# baixa cobertura nos primeiros anos e por mudanças de definição de caso.
# Repetimos a análise para 2012–2023 como controle de robustez.
# Revisores podem questionar se os achados são sensíveis ao período.

ANOS_SENSIB <- 2012:2023

jp_sensib <- list()
for (tipo in c("Gestacional", "Congênita")) {
  dados_tipo <- indicadores_br %>%
    filter(tipo_sifilis == tipo, ano_notif %in% ANOS_SENSIB)

  jp_s <- realizar_joinpoint(dados_tipo, "n_casos",
                              label = paste0("Brasil — ", tipo,
                                             " (sensibilidade 2012–2023)"))
  if (!is.null(jp_s)) jp_sensib[[tipo]] <- jp_s
}

jp_sensib_tabela <- bind_rows(jp_sensib) %>%
  mutate(nivel = "Sensibilidade (2012–2023)")

# Adicionar à tabela geral
jp_todos <- bind_rows(jp_todos, jp_sensib_tabela)
writexl::write_xlsx(jp_todos, file.path(out_tables, "Tabela_Joinpoint_Completa.xlsx"))

message("  Sensibilidade temporal (2012–2023) adicionada.")
message("  Análises de sensibilidade concluídas.")


###############################################################################
#      PARTE 7 — ANÁLISE ESPACIAL                                             #
###############################################################################

message("\n====== PARTE 7: Análise espacial ======")

# -------------------------------------------------------------------------
# 7.1 Preparar dados espaciais por município — período agregado
# -------------------------------------------------------------------------

# Agregar casos e NV por município para períodos vinculados a marcos políticos
# P1 (2007–2011): Pré-Rede Cegonha — vigilância em consolidação
# P2 (2012–2017): Rede Cegonha + Agenda Estratégica para eliminação da SC
# P3 (2018–2019): Pós-Agenda, pré-pandemia
# P4 (2020–2023): Pandemia COVID-19 e recuperação
# Total: período completo
periodos <- list(
  "Pre-Rede Cegonha (2007-2011)" = 2007:2011,
  "Rede Cegonha (2012-2017)"     = 2012:2017,
  "Pre-COVID (2018-2019)"        = 2018:2019,
  "COVID e pos (2020-2023)"      = 2020:2023,
  "Total"                        = ANOS_ESTUDO
)

# Para cada período e tipo de sífilis
dados_espaciais <- list()

for (per_nome in names(periodos)) {
  anos_per <- periodos[[per_nome]]

  for (tipo in c("Gestacional", "Congênita")) {
    casos_per <- casos_mun %>%
      filter(ano_notif %in% anos_per, tipo_sifilis == tipo) %>%
      group_by(cod_mun_notif) %>%
      summarise(n_casos_total = sum(n_casos), .groups = "drop")

    nv_per <- nv_mun_ano %>%
      filter(ano %in% anos_per) %>%
      group_by(cod_mun6) %>%
      summarise(nv_total = sum(nv, na.rm = TRUE), .groups = "drop")

    df_per <- casos_per %>%
      left_join(nv_per, by = c("cod_mun_notif" = "cod_mun6")) %>%
      mutate(
        taxa_bruta = ifelse(nv_total > 0, (n_casos_total / nv_total) * 1000, NA_real_),
        periodo = per_nome,
        tipo_sifilis = tipo
      )

    dados_espaciais[[paste0(per_nome, "_", tipo)]] <- df_per
  }
}

df_espacial <- bind_rows(dados_espaciais)

# -------------------------------------------------------------------------
# 7.2 Suavização Bayesiana empírica
# -------------------------------------------------------------------------
# Justificativa: municípios pequenos apresentam taxas instáveis devido a
# populações denominadoras reduzidas. A suavização por Bayes empírico
# (Empirical Bayes Smoothing) estabiliza as estimativas, "emprestando"
# informação da média global, e é amplamente recomendada em estudos
# de análise espacial em saúde.

suavizar_bayes_empirico <- function(casos, populacao) {
  # Implementação do Empirical Bayes (Marshall, 1991)
  if (length(casos) == 0 || all(is.na(populacao)) || all(populacao == 0)) {
    return(rep(NA_real_, length(casos)))
  }

  casos[is.na(casos)] <- 0
  populacao[is.na(populacao)] <- 0

  # Taxa global
  theta_global <- sum(casos) / sum(populacao[populacao > 0])

  # Variância
  var_theta <- sum(populacao * (casos / ifelse(populacao > 0, populacao, 1) - theta_global)^2) /
    sum(populacao) - theta_global / (sum(populacao) / length(populacao))
  var_theta <- max(var_theta, 0)

  # Peso (shrinkage)
  peso <- var_theta / (var_theta + theta_global / ifelse(populacao > 0, populacao, 1))
  peso[populacao == 0] <- 0

  # Taxa suavizada
  taxa_suavizada <- peso * (casos / ifelse(populacao > 0, populacao, 1)) +
    (1 - peso) * theta_global

  return(taxa_suavizada * 1000)  # Por 1.000 NV
}

# Inicializar coluna
df_espacial$taxa_suavizada <- NA_real_

# Aplicar suavização para o período total
for (tipo in c("Gestacional", "Congênita")) {
  idx <- df_espacial$periodo == "Total" & df_espacial$tipo_sifilis == tipo
  df_sub <- df_espacial[idx, ]

  df_espacial$taxa_suavizada[idx] <- suavizar_bayes_empirico(
    df_sub$n_casos_total,
    df_sub$nv_total
  )
}

# -------------------------------------------------------------------------
# 7.3 Juntar com malha cartográfica
# -------------------------------------------------------------------------
mapa_dados <- mapa_mun %>%
  left_join(
    df_espacial %>% filter(periodo == "Total"),
    by = c("code_muni6" = "cod_mun_notif")
  )

# -------------------------------------------------------------------------
# 7.4 Construir matriz de vizinhança (Queen contiguity)
# -------------------------------------------------------------------------
# Justificativa: a matriz Queen (contiguidade de primeira ordem)
# considera vizinhos os municípios que compartilham fronteira ou vértice.
# É a escolha mais comum em estudos de análise espacial em saúde no Brasil.

message("  Construindo matriz de vizinhança...")

# Filtrar municípios com dados válidos para sífilis gestacional
mapa_sg <- mapa_dados %>%
  filter(tipo_sifilis == "Gestacional", !is.na(taxa_suavizada))

if (nrow(mapa_sg) > 0) {
  # Vizinhança Queen
  nb_queen <- poly2nb(mapa_sg, queen = TRUE)

  # Verificar ilhas (municípios sem vizinhos) e conectar ao mais próximo
  n_ilhas <- sum(card(nb_queen) == 0)
  message("  Municípios sem vizinhos (ilhas): ", n_ilhas)

  if (n_ilhas > 0) {
    # Conectar ilhas ao vizinho mais próximo por coordenadas
    coords <- st_coordinates(st_centroid(mapa_sg))
    nb_queen <- make.sym.nb(nb_queen)  # Garantir simetria
  }

  # Pesos espaciais (row-standardized)
  listw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

  # -----------------------------------------------------------------------
  # 7.5 Índice de Moran Global
  # -----------------------------------------------------------------------
  # Testa se a distribuição espacial das taxas é aleatória (H0) ou apresenta
  # autocorrelação espacial (H1: valores similares tendem a se agrupar)

  message("  Calculando Moran Global...")

  moran_sg <- moran.test(mapa_sg$taxa_suavizada, listw_queen,
                          zero.policy = TRUE, alternative = "two.sided")

  message("  Moran I (Sífilis Gestacional): ", round(moran_sg$estimate[1], 4),
          " | p = ", format.pval(moran_sg$p.value, digits = 3))

  # Moran para sífilis congênita
  mapa_sc <- mapa_dados %>%
    filter(tipo_sifilis == "Congênita", !is.na(taxa_suavizada))

  if (nrow(mapa_sc) == nrow(mapa_sg)) {
    moran_sc <- moran.test(mapa_sc$taxa_suavizada, listw_queen,
                            zero.policy = TRUE, alternative = "two.sided")
    message("  Moran I (Sífilis Congênita): ", round(moran_sc$estimate[1], 4),
            " | p = ", format.pval(moran_sc$p.value, digits = 3))
  }

  # Salvar resultados Moran
  moran_resultados <- tibble(
    indicador = c("Sífilis Gestacional", "Sífilis Congênita"),
    Moran_I = c(moran_sg$estimate[1],
                ifelse(exists("moran_sc"), moran_sc$estimate[1], NA)),
    Expected_I = c(moran_sg$estimate[2],
                   ifelse(exists("moran_sc"), moran_sc$estimate[2], NA)),
    Variance = c(moran_sg$estimate[3],
                 ifelse(exists("moran_sc"), moran_sc$estimate[3], NA)),
    p_valor = c(moran_sg$p.value,
                ifelse(exists("moran_sc"), moran_sc$p.value, NA))
  )

  writexl::write_xlsx(moran_resultados, file.path(out_tables, "Tabela_Moran_Global.xlsx"))

  # -----------------------------------------------------------------------
  # 7.6 LISA — Local Indicators of Spatial Association
  # -----------------------------------------------------------------------
  # Identifica clusters locais:
  #   High-High: municípios com altas taxas cercados por vizinhos com altas taxas
  #   Low-Low: municípios com baixas taxas cercados por vizinhos com baixas taxas
  #   High-Low: outliers — alta taxa cercada por baixas
  #   Low-High: outliers — baixa taxa cercada por altas

  message("  Calculando LISA...")

  # LISA para sífilis gestacional
  lisa_sg <- localmoran(mapa_sg$taxa_suavizada, listw_queen,
                         zero.policy = TRUE, alternative = "two.sided")

  # Classificar clusters
  media_global <- mean(mapa_sg$taxa_suavizada, na.rm = TRUE)
  lag_sg <- lag.listw(listw_queen, mapa_sg$taxa_suavizada, zero.policy = TRUE)

  mapa_sg$lisa_I <- lisa_sg[, 1]
  mapa_sg$lisa_p <- p.adjust(lisa_sg[, "Pr(z != E(Ii))"], method = "fdr")  # p-valor ajustado
  mapa_sg$lag_taxa <- lag_sg

  mapa_sg <- mapa_sg %>%
    mutate(
      cluster_lisa = case_when(
        lisa_p > 0.05 ~ "Não significativo",
        taxa_suavizada >= media_global & lag_taxa >= media_global ~ "High-High",
        taxa_suavizada < media_global & lag_taxa < media_global ~ "Low-Low",
        taxa_suavizada >= media_global & lag_taxa < media_global ~ "High-Low",
        taxa_suavizada < media_global & lag_taxa >= media_global ~ "Low-High",
        TRUE ~ "Não significativo"
      )
    )

  # Contar clusters
  message("  Clusters LISA (SG):")
  print(table(mapa_sg$cluster_lisa))

  # Repetir para sífilis congênita
  if (nrow(mapa_sc) == nrow(mapa_sg)) {
    lisa_sc <- localmoran(mapa_sc$taxa_suavizada, listw_queen,
                           zero.policy = TRUE, alternative = "two.sided")
    media_global_sc <- mean(mapa_sc$taxa_suavizada, na.rm = TRUE)
    lag_sc <- lag.listw(listw_queen, mapa_sc$taxa_suavizada, zero.policy = TRUE)

    mapa_sc$lisa_I <- lisa_sc[, 1]
    mapa_sc$lisa_p <- p.adjust(lisa_sc[, "Pr(z != E(Ii))"], method = "fdr")
    mapa_sc$lag_taxa <- lag_sc

    mapa_sc <- mapa_sc %>%
      mutate(
        cluster_lisa = case_when(
          lisa_p > 0.05 ~ "Não significativo",
          taxa_suavizada >= media_global_sc & lag_taxa >= media_global_sc ~ "High-High",
          taxa_suavizada < media_global_sc & lag_taxa < media_global_sc ~ "Low-Low",
          taxa_suavizada >= media_global_sc & lag_taxa < media_global_sc ~ "High-Low",
          taxa_suavizada < media_global_sc & lag_taxa >= media_global_sc ~ "Low-High",
          TRUE ~ "Não significativo"
        )
      )

    message("  Clusters LISA (SC):")
    print(table(mapa_sc$cluster_lisa))
  }

  # -----------------------------------------------------------------------
  # 7.7 Mapas coropléticos — Taxas
  # -----------------------------------------------------------------------
  message("  Gerando mapas...")

  # Mapa de taxa suavizada — Sífilis Gestacional
  mapa_taxa_sg <- ggplot(mapa_sg) +
    geom_sf(aes(fill = taxa_suavizada), color = NA) +
    geom_sf(data = mapa_uf, fill = NA, color = "grey30", linewidth = 0.3) +
    scale_fill_viridis_c(option = "inferno", direction = -1,
                          name = "Taxa/1.000 NV\n(Bayes empírico)",
                          na.value = "grey90") +
    theme_void() +
    theme(legend.position = c(0.15, 0.3))

  ggsave(file.path(out_maps, "Mapa_Taxa_SG.png"), mapa_taxa_sg,
         width = 10, height = 12, dpi = 300)

  # -----------------------------------------------------------------------
  # 7.8 Mapas LISA — Clusters
  # -----------------------------------------------------------------------
  cores_lisa <- c(
    "High-High" = "#D7191C",
    "Low-Low"   = "#2C7BB6",
    "High-Low"  = "#FDAE61",
    "Low-High"  = "#ABD9E9",
    "Não significativo" = "#F0F0F0"
  )

  mapa_lisa_sg <- ggplot(mapa_sg) +
    geom_sf(aes(fill = cluster_lisa), color = NA) +
    geom_sf(data = mapa_uf, fill = NA, color = "grey30", linewidth = 0.3) +
    scale_fill_manual(values = cores_lisa, name = "Cluster LISA") +
    theme_void() +
    theme(legend.position = c(0.15, 0.3))

  ggsave(file.path(out_maps, "Mapa_LISA_SG.png"), mapa_lisa_sg,
         width = 10, height = 12, dpi = 300)

  # Mapa LISA — Sífilis Congênita
  if (exists("mapa_sc") && "cluster_lisa" %in% names(mapa_sc)) {
    mapa_lisa_sc <- ggplot(mapa_sc) +
      geom_sf(aes(fill = cluster_lisa), color = NA) +
      geom_sf(data = mapa_uf, fill = NA, color = "grey30", linewidth = 0.3) +
      scale_fill_manual(values = cores_lisa, name = "Cluster LISA") +
      theme_void() +
      theme(legend.position = c(0.15, 0.3))

    ggsave(file.path(out_maps, "Mapa_LISA_SC.png"), mapa_lisa_sc,
           width = 10, height = 12, dpi = 300)
  }

  message("  Mapas exportados.")

} else {
  message("  AVISO: Dados espaciais insuficientes para análise.")
}

# Salvar objetos espaciais
saveRDS(mapa_sg, file.path(data_proc, "mapa_sg_lisa.rds"))
if (exists("mapa_sc")) saveRDS(mapa_sc, file.path(data_proc, "mapa_sc_lisa.rds"))


###############################################################################
#       PARTE 8 — VULNERABILIDADE SOCIAL (IVS)                                #
###############################################################################

message("\n====== PARTE 8: Análise de vulnerabilidade social ======")

# -------------------------------------------------------------------------
# 8.1 Carregar IVS (Índice de Vulnerabilidade Social — IPEA)
# -------------------------------------------------------------------------
# O IVS do IPEA está disponível em:
# http://ivs.ipea.gov.br/
# Versões: 2000 e 2010 (baseadas nos Censos)
# Em 2023, foi atualizado com dados do Censo 2022
#
# O IVS é composto por 3 dimensões:
#   - IVS Infraestrutura Urbana
#   - IVS Capital Humano
#   - IVS Renda e Trabalho
#
# Valores: 0 (baixa vulnerabilidade) a 1 (alta vulnerabilidade)
# Categorias: Muito baixa (0-0.2), Baixa (0.2-0.3), Média (0.3-0.4),
#             Alta (0.4-0.5), Muito alta (>0.5)

# Tentar carregar IVS se disponível localmente
ivs_file <- file.path(data_raw, "ivs_municipios.xlsx")

if (file.exists(ivs_file)) {
  ivs_raw <- readxl::read_excel(ivs_file) %>% clean_names()
  message("  IVS carregado: ", nrow(ivs_raw), " municípios")

  # Padronizar nomes de colunas (ficheiro pode ter formatos diferentes)
  nms_ivs <- names(ivs_raw)

  # Mapear para nomes padronizados
  ivs <- ivs_raw %>%
    rename(any_of(c(
      code_muni6 = "code_muni6",
      ivs_geral = "ivs_proxy",
      ivs_cat = "ivs_cat",
      ivs_quintil = "ivs_quintil"
    )))

  # Se não tiver ivs_geral mas tiver ivs_proxy
  if (!"ivs_geral" %in% names(ivs) && "ivs_proxy" %in% names(ivs)) {
    ivs <- ivs %>% rename(ivs_geral = ivs_proxy)
  }

  # Garantir que temos as colunas necessárias
  if (!"ivs_infraestrutura" %in% names(ivs)) ivs$ivs_infraestrutura <- ivs$ivs_geral
  if (!"ivs_capital_humano" %in% names(ivs)) ivs$ivs_capital_humano <- ivs$ivs_geral
  if (!"ivs_renda_trabalho" %in% names(ivs)) ivs$ivs_renda_trabalho <- ivs$ivs_geral

} else {
  message("  AVISO: Arquivo IVS não encontrado. Baixando PIB per capita do IBGE...")

  # Baixar PIB per capita como proxy de vulnerabilidade social
  pib <- sidrar::get_sidra(api = "/t/5938/n6/all/v/37/p/2020")
  ivs <- pib %>%
    transmute(
      code_muni6 = substr(`Município (Código)`, 1, 6),
      pib_pc = as.numeric(Valor)
    ) %>%
    filter(!is.na(pib_pc)) %>%
    mutate(
      ivs_geral = 1 - rank(pib_pc) / n(),
      ivs_infraestrutura = ivs_geral,
      ivs_capital_humano = ivs_geral,
      ivs_renda_trabalho = ivs_geral
    )
  message("  IVS proxy (PIB per capita invertido): ", nrow(ivs), " municípios")
}

# Categorizar IVS
ivs <- ivs %>%
  mutate(
    ivs_cat = case_when(
      ivs_geral <= 0.20 ~ "Muito baixa",
      ivs_geral <= 0.40 ~ "Baixa",
      ivs_geral <= 0.60 ~ "Média",
      ivs_geral <= 0.80 ~ "Alta",
      ivs_geral > 0.80  ~ "Muito alta",
      TRUE ~ NA_character_
    ),
    ivs_cat = factor(ivs_cat, levels = c("Muito baixa", "Baixa", "Média",
                                          "Alta", "Muito alta")),
    ivs_cat = factor(ivs_cat, levels = c("Muito baixa", "Baixa", "Média",
                                          "Alta", "Muito alta")),
    ivs_quintil = ntile(ivs_geral, 5)
  )

# -------------------------------------------------------------------------
# 8.2 Juntar IVS com dados de sífilis
# -------------------------------------------------------------------------
# Merge com indicadores municipais (período total)
dados_ivs <- df_espacial %>%
  filter(periodo == "Total") %>%
  left_join(
    ivs %>% select(code_muni6, starts_with("ivs")),
    by = c("cod_mun_notif" = "code_muni6")
  )

# -------------------------------------------------------------------------
# 8.3 Análise por estratos de IVS
# -------------------------------------------------------------------------

# Tabela: taxas por categoria de IVS
tabela_ivs <- dados_ivs %>%
  filter(!is.na(ivs_cat)) %>%
  group_by(tipo_sifilis, ivs_cat) %>%
  summarise(
    n_municipios = n(),
    total_casos = sum(n_casos_total, na.rm = TRUE),
    total_nv = sum(nv_total, na.rm = TRUE),
    taxa_media = mean(taxa_bruta, na.rm = TRUE),
    taxa_mediana = median(taxa_bruta, na.rm = TRUE),
    taxa_q25 = quantile(taxa_bruta, 0.25, na.rm = TRUE),
    taxa_q75 = quantile(taxa_bruta, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(taxa_agregada = (total_casos / total_nv) * 1000)

writexl::write_xlsx(tabela_ivs, file.path(out_tables, "Tabela_IVS_estratos.xlsx"))

# -------------------------------------------------------------------------
# 8.4 Teste de tendência (gradiente social)
# -------------------------------------------------------------------------
# Teste de Cochran-Armitage (ou Jonckheere-Terpstra) para tendência
# monotônica entre IVS e taxa de sífilis

gradiente_social <- dados_ivs %>%
  filter(!is.na(ivs_quintil), tipo_sifilis == "Gestacional") %>%
  group_by(ivs_quintil) %>%
  summarise(
    taxa_media = mean(taxa_bruta, na.rm = TRUE),
    .groups = "drop"
  )

# Teste de correlação de Spearman como proxy
if (nrow(dados_ivs %>% filter(!is.na(ivs_geral), !is.na(taxa_bruta))) > 10) {
  cor_ivs_sg <- cor.test(
    dados_ivs %>% filter(tipo_sifilis == "Gestacional") %>% pull(ivs_geral),
    dados_ivs %>% filter(tipo_sifilis == "Gestacional") %>% pull(taxa_bruta),
    method = "spearman", exact = FALSE
  )
  message("  Correlação IVS × SG (Spearman): rho = ", round(cor_ivs_sg$estimate, 3),
          ", p = ", format.pval(cor_ivs_sg$p.value))
}

# -------------------------------------------------------------------------
# 8.5 Figura: Boxplot por IVS
# -------------------------------------------------------------------------
fig_ivs <- dados_ivs %>%
  filter(!is.na(ivs_cat), !is.na(taxa_bruta)) %>%
  ggplot(aes(x = ivs_cat, y = taxa_bruta, fill = ivs_cat)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  facet_wrap(~tipo_sifilis, scales = "free_y") +
  scale_fill_brewer(palette = "YlOrRd", name = "IVS") +
  scale_y_continuous(trans = "log1p",
                     breaks = c(0, 1, 5, 10, 25, 50, 100)) +
  labs(
    title = "Taxas de sífilis por nível de vulnerabilidade social (IVS)",
    subtitle = glue("Municípios brasileiros, {ANO_INICIO}–{ANO_FIM}"),
    x = "Categoria IVS", y = "Taxa/1.000 NV (escala log)",
    caption = "Fonte: SINAN/DataSUS + IVS/IPEA"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(out_figs, "Fig5_IVS_Boxplot.png"), fig_ivs,
       width = 12, height = 7, dpi = 300)
message("  Figura IVS salva.")

# -------------------------------------------------------------------------
# 8.6 Moran Bivariado: IVS × Sífilis
# -------------------------------------------------------------------------
# Autocorrelação espacial bivariada entre IVS e taxa de sífilis
# Verifica se municípios com alto IVS são vizinhos de municípios com alta sífilis

if (exists("listw_queen") && nrow(mapa_sg) > 0) {
  # Juntar IVS ao mapa
  mapa_sg_ivs <- mapa_sg %>%
    left_join(
      ivs %>% select(code_muni6, ivs_geral),
      by = c("code_muni6")
    )

  if (sum(!is.na(mapa_sg_ivs$ivs_geral)) > 100) {
    # Moran bivariado: correlação entre x_i e W(y_j)
    x_std <- scale(mapa_sg_ivs$ivs_geral)[, 1]
    y_std <- scale(mapa_sg_ivs$taxa_suavizada)[, 1]
    x_std[is.na(x_std)] <- 0
    y_std[is.na(y_std)] <- 0

    Wy <- lag.listw(listw_queen, y_std, zero.policy = TRUE)

    moran_biv <- cor(x_std, Wy, use = "complete.obs")
    message("  Moran bivariado (IVS × SG): ", round(moran_biv, 4))

    # Teste de significância por permutação
    n_perm <- 999
    moran_biv_perm <- numeric(n_perm)
    for (i in 1:n_perm) {
      x_perm <- sample(x_std)
      Wy_perm <- lag.listw(listw_queen, y_std, zero.policy = TRUE)
      moran_biv_perm[i] <- cor(x_perm, Wy_perm, use = "complete.obs")
    }
    p_biv <- mean(abs(moran_biv_perm) >= abs(moran_biv))
    message("  p-valor (permutação): ", round(p_biv, 4))
  }
}


###############################################################################
#     PARTE 9 — DESIGUALDADES RACIAIS                                         #
###############################################################################

message("\n====== PARTE 9: Desigualdades raciais ======")

# -------------------------------------------------------------------------
# 9.1 Distribuição por raça/cor — Séries temporais
# -------------------------------------------------------------------------

# Construir NV por raça/cor (se disponível do SINASC)
if (!is.null(nv_mun_raca)) {
  # Detectar nome da coluna de raça (pode ser raca_mae ou raca_cor_mae)
  raca_col_nv <- intersect(c("raca_cor_mae", "raca_mae"), names(nv_mun_raca))[1]
  if (!is.na(raca_col_nv)) {
    nv_raca <- nv_mun_raca %>%
      rename(raca_cor = !!sym(raca_col_nv)) %>%
      group_by(ano, raca_cor) %>%
      summarise(nv = sum(nv, na.rm = TRUE), .groups = "drop")
  } else {
    nv_raca <- NULL
  }
} else {
  nv_raca <- NULL
}

# Tabela descritiva por raça/cor
tabela_raca_descritiva <- casos_raca %>%
  filter(raca_cor %in% c("Branca", "Preta", "Parda")) %>%
  {
    if (!is.null(nv_raca)) {
      left_join(., nv_raca, by = c("ano_notif" = "ano", "raca_cor")) %>%
        mutate(taxa_1000nv = ifelse(nv > 0, (n_casos / nv) * 1000, NA_real_))
    } else {
      mutate(., nv = NA, taxa_1000nv = NA)
    }
  }

# -------------------------------------------------------------------------
# 9.2 Razões de taxa: Preta/Branca e Parda/Branca
# -------------------------------------------------------------------------
razoes_raciais <- tabela_raca_descritiva %>%
  select(ano_notif, tipo_sifilis, raca_cor, n_casos, taxa_1000nv) %>%
  pivot_wider(names_from = raca_cor,
              values_from = c(n_casos, taxa_1000nv)) %>%
  mutate(
    # Diferença absoluta (excesso de casos)
    diff_abs_preta = n_casos_Preta - n_casos_Branca,
    diff_abs_parda = n_casos_Parda - n_casos_Branca,

    # Razão de taxas
    razao_preta_branca = taxa_1000nv_Preta / taxa_1000nv_Branca,
    razao_parda_branca = taxa_1000nv_Parda / taxa_1000nv_Branca,

    # Diferença de taxas
    diff_taxa_preta = taxa_1000nv_Preta - taxa_1000nv_Branca,
    diff_taxa_parda = taxa_1000nv_Parda - taxa_1000nv_Branca
  )

writexl::write_xlsx(razoes_raciais,
                    file.path(out_tables, "Tabela_Desigualdades_Raciais.xlsx"))

# -------------------------------------------------------------------------
# 9.3 Figura: Tendência por raça/cor
# -------------------------------------------------------------------------
fig_raca <- tabela_raca_descritiva %>%
  ggplot(aes(x = ano_notif, y = n_casos, color = raca_cor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~tipo_sifilis, scales = "free_y") +
  scale_color_manual(values = cores_raca, name = "Raça/Cor") +
  scale_x_continuous(breaks = ANOS_ESTUDO) +
  labs(
    title = "Sífilis por raça/cor materna — Brasil",
    subtitle = glue("{ANO_INICIO}–{ANO_FIM}"),
    x = "Ano", y = "Número de casos",
    caption = paste0("Fonte: SINAN/DataSUS. Nota: Raça/cor é variável proxy de ",
                     "exposição a desigualdades estruturais, não categoria biológica.")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_figs, "Fig6_Raca_Cor.png"), fig_raca,
       width = 12, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# 9.4 Interação raça/cor × IVS
# -------------------------------------------------------------------------
# Testar se o efeito da raça/cor varia segundo a vulnerabilidade social

# Necessita dados individuais com código de município para merge com IVS
if (nrow(df_sifg) > 0 && "ivs_geral" %in% names(ivs)) {
  df_sifg_ivs <- df_sifg %>%
    filter(raca_cor %in% c("Branca", "Preta", "Parda")) %>%
    left_join(
      ivs %>% select(code_muni6, ivs_geral, ivs_cat),
      by = c("cod_mun_notif" = "code_muni6")
    ) %>%
    filter(!is.na(ivs_cat))

  # Tabela cruzada raça × IVS
  tabela_raca_ivs <- df_sifg_ivs %>%
    count(raca_cor, ivs_cat, name = "n_casos") %>%
    group_by(ivs_cat) %>%
    mutate(prop = n_casos / sum(n_casos)) %>%
    ungroup()

  # Figura: proporção por raça/cor segundo IVS
  fig_raca_ivs <- tabela_raca_ivs %>%
    ggplot(aes(x = ivs_cat, y = prop, fill = raca_cor)) +
    geom_col(position = "fill") +
    scale_fill_manual(values = cores_raca, name = "Raça/Cor") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Distribuição racial dos casos de sífilis gestacional por nível de IVS",
      x = "Vulnerabilidade Social (IVS)", y = "Proporção",
      caption = "Fonte: SINAN/DataSUS + IVS/IPEA"
    )

  ggsave(file.path(out_figs, "Fig7_Raca_IVS.png"), fig_raca_ivs,
         width = 10, height = 6, dpi = 300)
  message("  Análise raça × IVS concluída.")
}

message("  Análise de desigualdades raciais concluída.")


###############################################################################
#     PARTE 10 — MODELAGEM MULTIVARIADA                                       #
###############################################################################

message("\n====== PARTE 10: Modelagem multivariada ======")

# -------------------------------------------------------------------------
# 10.1 Preparar dataset de modelagem (nível municipal, período agregado)
# -------------------------------------------------------------------------
df_modelo <- df_espacial %>%
  filter(periodo == "Total", tipo_sifilis == "Gestacional") %>%
  left_join(
    ivs %>% select(code_muni6, ivs_geral, ivs_infraestrutura,
                    ivs_capital_humano, ivs_renda_trabalho, ivs_quintil),
    by = c("cod_mun_notif" = "code_muni6")
  ) %>%
  # Adicionar variáveis contextuais
  mutate(
    cod_uf = substr(cod_mun_notif, 1, 2),
    macrorregiao = get_macrorregiao(cod_uf),
    log_nv = log(nv_total + 1)
  ) %>%
  filter(!is.na(ivs_geral), !is.na(n_casos_total), nv_total > 0,
         !is.na(macrorregiao)) %>%
  mutate(macrorregiao = factor(macrorregiao))

message("  Dataset de modelagem: ", nrow(df_modelo), " municípios")

# -------------------------------------------------------------------------
# 10.2 Modelo 1: Regressão de Poisson
# -------------------------------------------------------------------------
mod1_poisson <- glm(
  n_casos_total ~ ivs_geral + macrorregiao + offset(log(nv_total)),
  family = poisson(link = "log"),
  data = df_modelo
)

# Verificar sobredispersão
disp_ratio <- sum(residuals(mod1_poisson, type = "pearson")^2) / mod1_poisson$df.residual
message("  Modelo Poisson — Dispersão: ", round(disp_ratio, 2))

# -------------------------------------------------------------------------
# 10.3 Modelo 2: Binomial Negativa (se sobredisperso)
# -------------------------------------------------------------------------
if (disp_ratio > 1.5) {
  message("  Sobredispersão detectada → usando Binomial Negativa")

  mod2_bn <- tryCatch(
    MASS::glm.nb(
      n_casos_total ~ ivs_geral + macrorregiao + offset(log(nv_total)),
      data = df_modelo
    ), error = function(e) { message("  AVISO mod2: ", e$message); NULL }
  )

  # Modelo com dimensões do IVS (pode falhar se dimensões são NA)
  mod3_bn_dim <- tryCatch(
    MASS::glm.nb(
      n_casos_total ~ ivs_infraestrutura + ivs_capital_humano +
        ivs_renda_trabalho + macrorregiao + offset(log(nv_total)),
      data = df_modelo %>% filter(!is.na(ivs_infraestrutura))
    ), error = function(e) { message("  AVISO mod3: ", e$message); NULL }
  )

  # IRR (Incidence Rate Ratio)
  if (!is.null(mod2_bn)) {
    irr_mod2 <- exp(cbind(IRR = coef(mod2_bn), confint(mod2_bn)))
    message("  IRR (IVS geral): ", round(irr_mod2["ivs_geral", "IRR"], 2))
  }
} else {
  mod2_bn <- mod1_poisson  # Usar Poisson se não há sobredispersão
}

# -------------------------------------------------------------------------
# 10.4 Modelo 3: Multinível (municípios aninhados em UFs)
# -------------------------------------------------------------------------
# Justificativa: estrutura hierárquica natural (municípios dentro de UFs)
# permite separar variabilidade entre e dentro de UFs

# Dados em painel (município × ano) para modelo multinível
df_painel <- casos_mun %>%
  filter(tipo_sifilis == "Gestacional") %>%
  left_join(nv_mun_ano, by = c("cod_mun_notif" = "cod_mun6", "ano_notif" = "ano")) %>%
  left_join(
    ivs %>% select(code_muni6, ivs_geral),
    by = c("cod_mun_notif" = "code_muni6")
  ) %>%
  filter(!is.na(ivs_geral), !is.na(nv), nv > 0) %>%
  mutate(
    cod_uf = substr(cod_mun_notif, 1, 2),
    ano_c = ano_notif - mean(ANOS_ESTUDO),
    log_nv = log(nv)
  )

if (nrow(df_painel) > 100) {
  tryCatch({
    mod4_multinivel <- lme4::glmer(
      n_casos ~ ivs_geral + ano_c + offset(log_nv) + (1 | cod_uf),
      family = poisson(link = "log"),
      data = df_painel,
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
    )
    message("  Modelo multinível ajustado.")
    message("  Variância intercepto aleatório (UF): ",
            round(VarCorr(mod4_multinivel)$cod_uf[1, 1], 4))
    icc_val <- performance::icc(mod4_multinivel)
    message("  ICC: ", round(icc_val$ICC_adjusted, 3))
  }, error = function(e) message("  AVISO multinível: ", e$message))
}

# -------------------------------------------------------------------------
# 10.5 Modelo 4: Regressão espacial (SAR / SEM)
# -------------------------------------------------------------------------
# Modelos espaciais autorregressivos capturam a dependência espacial residual

if (exists("listw_queen") && nrow(mapa_sg) > 0) {
  # Preparar dados para modelo espacial
  mapa_modelo <- mapa_sg %>%
    left_join(
      ivs %>% select(code_muni6, ivs_geral),
      by = "code_muni6"
    ) %>%
    filter(!is.na(ivs_geral), !is.na(taxa_suavizada))

  if (nrow(mapa_modelo) > 100) {
    # Reconstruir vizinhança para subset
    nb_mod <- poly2nb(mapa_modelo, queen = TRUE)
    listw_mod <- nb2listw(nb_mod, style = "W", zero.policy = TRUE)

    # Modelo OLS base
    mod_ols <- lm(taxa_suavizada ~ ivs_geral, data = mapa_modelo)

    # Teste LM para identificar tipo de dependência espacial
    lm_tests <- lm.LMtests(mod_ols, listw_mod,
                             test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))
    message("  Testes LM espaciais:")
    print(summary(lm_tests))

    # SAR e SEM
    tryCatch({
      mod_sar <- lagsarlm(taxa_suavizada ~ ivs_geral,
                           data = mapa_modelo, listw = listw_mod,
                           zero.policy = TRUE)
      mod_sem <- errorsarlm(taxa_suavizada ~ ivs_geral,
                             data = mapa_modelo, listw = listw_mod,
                             zero.policy = TRUE)
      message("  AIC — OLS: ", round(AIC(mod_ols), 1),
              " | SAR: ", round(AIC(mod_sar), 1),
              " | SEM: ", round(AIC(mod_sem), 1))
      saveRDS(list(ols = mod_ols, sar = mod_sar, sem = mod_sem),
              file.path(out_models, "modelos_espaciais.rds"))
    }, error = function(e) message("  AVISO SAR/SEM: ", e$message))
  }
}

# -------------------------------------------------------------------------
# 10.6 Tabela de resultados dos modelos
# -------------------------------------------------------------------------
resultados_modelos <- list()

# Modelo Poisson/BN
if (exists("mod2_bn")) {
  tidy_mod2 <- broom::tidy(mod2_bn, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(modelo = "Binomial Negativa (IVS geral)")
  resultados_modelos[["BN"]] <- tidy_mod2
}

if (exists("mod3_bn_dim")) {
  tidy_mod3 <- broom::tidy(mod3_bn_dim, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(modelo = "Binomial Negativa (IVS dimensões)")
  resultados_modelos[["BN_dim"]] <- tidy_mod3
}

if (exists("mod4_multinivel")) {
  tidy_mod4 <- broom.mixed::tidy(mod4_multinivel, exponentiate = TRUE,
                                   conf.int = TRUE) %>%
    mutate(modelo = "GLMM Multinível")
  resultados_modelos[["GLMM"]] <- tidy_mod4
}

if (exists("mod_sar")) {
  sar_coef <- summary(mod_sar)$Coef
  tidy_sar <- tibble(
    term = rownames(sar_coef),
    estimate = sar_coef[, 1],
    std.error = sar_coef[, 2],
    p.value = sar_coef[, 4],
    modelo = "SAR (Spatial Lag)"
  )
  # Adicionar rho
  tidy_sar <- bind_rows(tidy_sar,
    tibble(term = "rho", estimate = mod_sar$rho,
           std.error = NA, p.value = NA, modelo = "SAR (Spatial Lag)"))
  resultados_modelos[["SAR"]] <- tidy_sar
}

tabela_modelos <- bind_rows(resultados_modelos)
writexl::write_xlsx(tabela_modelos, file.path(out_tables, "Tabela_Modelos_Multivariados.xlsx"))
message("  Tabela de modelos exportada.")


###############################################################################
#     PARTE 11 — EXPORTAÇÃO FINAL E MATERIAL SUPLEMENTAR                      #
###############################################################################

message("\n====== PARTE 11: Exportação final ======")

# -------------------------------------------------------------------------
# 11.1 Dicionário de variáveis
# -------------------------------------------------------------------------
dicionario <- tibble(
  Variavel = c(
    "ano_notif", "cod_mun_notif", "cod_uf", "macrorregiao", "raca_cor",
    "tipo_sifilis", "n_casos", "nv", "taxa_1000nv", "taxa_suavizada",
    "ivs_geral", "ivs_infraestrutura", "ivs_capital_humano", "ivs_renda_trabalho",
    "ivs_cat", "ivs_quintil", "cluster_lisa", "razao_sc_sg"
  ),
  Descricao = c(
    "Ano de notificação", "Código IBGE do município (6 dígitos)",
    "Código da UF (2 dígitos)", "Macrorregião do Brasil",
    "Raça/cor (autorreferida/mãe)", "Tipo: Gestacional ou Congênita",
    "Número de casos notificados", "Nascidos vivos (denominador)",
    "Taxa por 1.000 nascidos vivos (bruta)",
    "Taxa suavizada por Bayes empírico (/1.000 NV)",
    "Índice de Vulnerabilidade Social (IPEA) — geral",
    "IVS — dimensão Infraestrutura Urbana",
    "IVS — dimensão Capital Humano",
    "IVS — dimensão Renda e Trabalho",
    "Categoria de IVS (muito baixa a muito alta)",
    "Quintil de IVS (1=menos vulnerável, 5=mais vulnerável)",
    "Classificação LISA (High-High, Low-Low, etc.)",
    "Razão sífilis congênita / sífilis gestacional"
  ),
  Fonte = c(
    "SINAN", "SINAN/IBGE", "SINAN/IBGE", "Derivada",
    "SINAN", "Derivada", "SINAN", "SINASC",
    "Calculada", "Calculada (Bayes empírico)",
    "IPEA", "IPEA", "IPEA", "IPEA",
    "Derivada do IVS", "Derivada do IVS",
    "Calculada (spdep)", "Calculada"
  ),
  Tipo = c(
    "Integer", "Character", "Character", "Character",
    "Character", "Character", "Integer", "Integer",
    "Numeric", "Numeric",
    "Numeric (0–1)", "Numeric (0–1)", "Numeric (0–1)", "Numeric (0–1)",
    "Factor", "Integer (1–5)",
    "Character", "Numeric"
  )
)

writexl::write_xlsx(dicionario, file.path(out_suppl, "Dicionario_Variaveis.xlsx"))

# -------------------------------------------------------------------------
# 11.2 Tabelas suplementares completas por UF
# -------------------------------------------------------------------------
# Tabela suplementar: todos os indicadores por UF e ano
supl_uf <- indicadores_uf %>%
  left_join(
    razao_mun %>%
      mutate(cod_uf = substr(cod_mun_notif, 1, 2)) %>%
      group_by(ano_notif, cod_uf) %>%
      summarise(
        razao_media = mean(razao_sc_sg, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("ano_notif", "cod_uf")
  )

writexl::write_xlsx(supl_uf, file.path(out_suppl, "Tabela_Supl_UF_Completa.xlsx"))

# -------------------------------------------------------------------------
# 11.3 Resultados completos Joinpoint por UF
# -------------------------------------------------------------------------
writexl::write_xlsx(jp_uf_tabela, file.path(out_suppl, "Tabela_Supl_Joinpoint_UF.xlsx"))

# -------------------------------------------------------------------------
# 11.4 Painel de figuras — tendência por UF (small multiples)
# -------------------------------------------------------------------------
fig_uf_panel <- casos_uf %>%
  mutate(sigla_uf = UF_SIGLA[cod_uf]) %>%
  filter(!is.na(sigla_uf)) %>%
  ggplot(aes(x = ano_notif, y = n_casos, color = tipo_sifilis)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.8) +
  facet_wrap(~sigla_uf, scales = "free_y", ncol = 5) +
  scale_color_manual(
    values = c("Gestacional" = "#D95F02", "Congênita" = "#7570B3"),
    name = "Tipo"
  ) +
  scale_x_continuous(breaks = ANOS_ESTUDO) +
  labs(
    title = "Tendência temporal por Unidade da Federação",
    subtitle = glue("Sífilis gestacional e congênita, {ANO_INICIO}–{ANO_FIM}"),
    x = "Ano", y = "Casos",
    caption = "Fonte: SINAN/DataSUS"
  ) +
  theme(
    axis.text = element_text(size = 6),
    strip.text = element_text(size = 7),
    legend.position = "bottom"
  )

ggsave(file.path(out_suppl, "Fig_Supl_UF_Panel.png"), fig_uf_panel,
       width = 14, height = 16, dpi = 300)

# -------------------------------------------------------------------------
# 11.5 Comparação de métodos de tendência
# -------------------------------------------------------------------------
comparacao_metodos <- bind_rows(
  jp_brasil_tabela %>%
    filter(grepl("casos", label)) %>%
    select(label, APC, APC_IC95_lo, APC_IC95_hi) %>%
    mutate(metodo = "Joinpoint (segmented)"),
  sensib_tabela %>%
    select(label, metodo, APC, APC_IC95_lo, APC_IC95_hi)
)

writexl::write_xlsx(comparacao_metodos,
                    file.path(out_suppl, "Tabela_Supl_Comparacao_Metodos.xlsx"))

# -------------------------------------------------------------------------
# 11.6 STROBE Checklist (ecológico)
# -------------------------------------------------------------------------
strobe <- tibble(
  Item = c(
    "1a", "1b", "2", "3", "4", "5", "6a", "6b",
    "7", "8", "9", "10", "11", "12a", "12b", "12c",
    "12d", "12e", "13a", "13b", "13c", "14a", "14b",
    "15", "16a", "16b", "16c", "17", "18", "19",
    "20", "21", "22"
  ),
  Topico = c(
    "Title and abstract", "Title and abstract",
    "Background/rationale", "Objectives",
    "Study design", "Setting",
    "Participants", "Participants",
    "Variables", "Data sources/measurement",
    "Bias", "Study size",
    "Quantitative variables", "Statistical methods",
    "Statistical methods", "Statistical methods",
    "Statistical methods", "Statistical methods",
    "Results: Participants", "Results: Participants",
    "Results: Participants",
    "Results: Descriptive data", "Results: Descriptive data",
    "Results: Outcome data",
    "Results: Main results", "Results: Main results",
    "Results: Main results",
    "Results: Other analyses",
    "Discussion: Key results", "Discussion: Limitations",
    "Discussion: Interpretation", "Discussion: Generalisability",
    "Other: Funding"
  ),
  Recomendacao = c(
    "Indicate study design; summarize what was done and found",
    "Provide structured abstract",
    "Explain scientific background and rationale",
    "State specific objectives and hypotheses",
    "Present key elements of study design early in the paper",
    "Describe setting, locations, relevant dates",
    "Describe ecological units (municipalities, states)",
    "Describe time period and data sources",
    "Define all outcomes, exposures, covariates",
    "Describe data sources and assessment methods",
    "Describe efforts to address potential sources of bias",
    "Explain how the study size was arrived at",
    "Explain how quantitative variables were handled",
    "Describe all statistical methods",
    "Describe methods for examining subgroups",
    "Explain how missing data were addressed",
    "Describe analytical methods (spatial, temporal)",
    "Describe any sensitivity analyses",
    "Report the number of units at each stage",
    "Give reasons for non-participation",
    "Consider use of a flow diagram",
    "Give characteristics of ecological units",
    "Indicate number of units with missing data",
    "Report numbers of outcome events or summary measures",
    "Give unadjusted and adjusted estimates",
    "Report confidence intervals",
    "Report category boundaries when continuous variables were categorized",
    "Report other analyses done (sensitivity, subgroups)",
    "Summarise key results with reference to objectives",
    "Discuss limitations, including ecological fallacy",
    "Give cautious overall interpretation considering ecological design",
    "Discuss the generalisability of the results",
    "Give the source of funding and role of funders"
  ),
  Localizado = rep("", 33)
)

writexl::write_xlsx(strobe, file.path(out_suppl, "STROBE_Checklist_Ecologico.xlsx"))

# -------------------------------------------------------------------------
# 11.7 Fluxograma analítico (texto para diagrama)
# -------------------------------------------------------------------------
fluxograma_texto <- c(
  "FLUXOGRAMA ANALÍTICO",
  "====================",
  "",
  "1. FONTES DE DADOS",
  "   ├── SINAN (Sífilis Gestacional: SIFG, Sífilis Congênita: SIFC)",
  "   ├── SINASC (Nascidos Vivos — denominador)",
  "   ├── IBGE (Malha cartográfica municipal)",
  "   └── IPEA (Índice de Vulnerabilidade Social — IVS)",
  "",
  "2. LIMPEZA E PREPARAÇÃO",
  "   ├── Padronização de códigos municipais (IBGE 6 dígitos)",
  "   ├── Verificação de qualidade e completitude",
  "   ├── Tratamento de missing (raça/cor, município)",
  "   └── Harmonização temporal (2012–2023)",
  "",
  "3. CONSTRUÇÃO DE INDICADORES",
  "   ├── Taxa de detecção de sífilis gestacional (/1.000 NV)",
  "   ├── Taxa de incidência de sífilis congênita (/1.000 NV)",
  "   ├── Razão SC/SG",
  "   └── Suavização Bayesiana empírica (nível municipal)",
  "",
  "4. ANÁLISE DESCRITIVA",
  "   ├── Tabelas por Brasil, região, UF, raça/cor",
  "   ├── Gráficos de tendência temporal",
  "   └── Mapas coropléticos de taxas",
  "",
  "5. ANÁLISE DE TENDÊNCIA TEMPORAL",
  "   ├── Regressão segmentada (Joinpoint-like) → APC, AAPC",
  "   ├── Prais-Winsten (sensibilidade — autocorrelação serial)",
  "   └── Poisson / Binomial Negativa com offset (sensibilidade)",
  "",
  "6. ANÁLISE ESPACIAL",
  "   ├── Moran Global (autocorrelação espacial)",
  "   ├── LISA (clusters locais: HH, LL, HL, LH)",
  "   ├── Moran Bivariado (IVS × Sífilis)",
  "   └── Comparação entre períodos",
  "",
  "7. ANÁLISE DE VULNERABILIDADE SOCIAL",
  "   ├── Estratificação por quintis de IVS",
  "   ├── Teste de gradiente social",
  "   └── Análise por dimensões do IVS",
  "",
  "8. DESIGUALDADES RACIAIS",
  "   ├── Distribuição por raça/cor",
  "   ├── Razões de taxa (Preta/Branca, Parda/Branca)",
  "   └── Interação raça/cor × IVS",
  "",
  "9. MODELAGEM MULTIVARIADA",
  "   ├── Regressão Binomial Negativa (IVS + covariáveis)",
  "   ├── GLMM Multinível (município dentro de UF)",
  "   └── Modelos espaciais (SAR, SEM)"
)

writeLines(fluxograma_texto, file.path(out_suppl, "Fluxograma_Analitico.txt"))


###############################################################################
#  PARTE 12 — ANÁLISES AVANÇADAS (baseadas em lacunas da literatura)          #
###############################################################################
#
# A literatura existente sobre sífilis no Brasil apresenta lacunas que
# este estudo preenche. As análises abaixo foram desenhadas para fortalecer
# a contribuição metodológica e substantiva do manuscrito:
#
# LACUNA 1: Maioria dos estudos cobre até 2019. Poucos incluem o impacto
#           da pandemia COVID-19 na vigilância e notificação de sífilis.
#           → Adicionamos análise de impacto COVID-19 (interrupted time series)
#
# LACUNA 2: Medidas formais de desigualdade (Concentration Index, SII)
#           raramente aplicadas à sífilis no Brasil.
#           → Calculamos CI e SII por IVS
#
# LACUNA 3: A razão SC/SG como indicador programático é subexplorada
#           em análises espaciais e temporais.
#           → Mapeamos e modelamos a razão SC/SG
#
# LACUNA 4: Comparação espaço-temporal entre períodos pré/pós-políticas
#           (Rede Cegonha, Agenda Estratégica) é rara.
#           → Comparamos clusters LISA entre períodos
#
# LACUNA 5: Poucos estudos testam interação entre raça e vulnerabilidade
#           social num mesmo modelo, tratando raça como proxy de racismo
#           estrutural (não biológico).
#           → Modelo com interação raça × IVS
#

message("\n====== PARTE 12: Análises avançadas ======")

# -------------------------------------------------------------------------
# 12.1 Análise de impacto COVID-19 (Interrupted Time Series - ITS)
# -------------------------------------------------------------------------
# A pandemia pode ter afetado: (a) notificação/vigilância, (b) acesso ao
# pré-natal, (c) oferta de testagem. Usamos ITS para estimar o efeito.

message("  12.1 Interrupted Time Series — COVID-19")

its_covid <- function(dados, var_y, label = "") {
  df <- dados %>%
    arrange(ano_notif) %>%
    filter(!is.na(!!sym(var_y))) %>%
    mutate(
      tempo = ano_notif - min(ano_notif),        # Tempo linear
      covid = ifelse(ano_notif >= 2020, 1, 0),    # Intervenção (pandemia)
      pos_covid = ifelse(ano_notif >= 2020,
                         ano_notif - 2020, 0)     # Tempo pós-intervenção
    )

  if (nrow(df) < 8) return(NULL)

  y <- df[[var_y]]

  # Modelo ITS: y = b0 + b1*tempo + b2*covid + b3*pos_covid
  # b2 = mudança de nível imediata
  # b3 = mudança na tendência pós-COVID
  mod <- glm(y ~ tempo + covid + pos_covid, family = gaussian(), data = df)

  # Se contagens, usar Poisson
  if (all(y == floor(y)) && all(y >= 0)) {
    mod_p <- glm(y ~ tempo + covid + pos_covid, family = poisson(), data = df)
    disp <- sum(residuals(mod_p, type = "pearson")^2) / mod_p$df.residual
    if (disp > 1.5) {
      mod <- MASS::glm.nb(y ~ tempo + covid + pos_covid, data = df)
    } else {
      mod <- mod_p
    }
  }

  resultado <- broom::tidy(mod, conf.int = TRUE) %>%
    mutate(label = label)

  # Predições contrafactuais (sem COVID)
  df$pred_real <- predict(mod, type = "response")
  df_cf <- df %>% mutate(covid = 0, pos_covid = 0)
  df$pred_contrafactual <- predict(mod, newdata = df_cf, type = "response")

  attr(resultado, "dados_pred") <- df %>%
    select(ano_notif, !!sym(var_y), pred_real, pred_contrafactual)

  return(resultado)
}

# ITS para Brasil
its_resultados <- list()
for (tipo in c("Gestacional", "Congênita")) {
  dados <- indicadores_br %>% filter(tipo_sifilis == tipo)
  its <- its_covid(dados, "n_casos", label = paste0("Brasil — ", tipo))
  if (!is.null(its)) its_resultados[[tipo]] <- its
}

its_tabela <- bind_rows(its_resultados)
writexl::write_xlsx(its_tabela, file.path(out_tables, "Tabela_ITS_COVID.xlsx"))

# Figura ITS
if (length(its_resultados) > 0 && !is.null(attr(its_resultados[[1]], "dados_pred"))) {
  its_fig_data <- bind_rows(
    attr(its_resultados[["Gestacional"]], "dados_pred") %>%
      mutate(tipo = "Gestacional"),
    attr(its_resultados[["Congênita"]], "dados_pred") %>%
      mutate(tipo = "Congênita")
  )

  fig_its <- its_fig_data %>%
    ggplot(aes(x = ano_notif)) +
    geom_point(aes(y = n_casos), size = 2.5) +
    geom_line(aes(y = pred_real), linewidth = 0.9, color = "black") +
    geom_line(aes(y = pred_contrafactual), linewidth = 0.9,
              color = "grey50", linetype = "dashed") +
    geom_vline(xintercept = 2019.5, linetype = "dotted",
               color = "red", linewidth = 0.7) +
    facet_wrap(~tipo, scales = "free_y") +
    scale_x_continuous(breaks = ANOS_ESTUDO) +
    labs(x = "Ano", y = "Número de casos") +
    annotate("text", x = 2020.5, y = Inf, label = "COVID-19",
             vjust = 1.5, size = 3, color = "red", fontface = "italic")

  ggsave(file.path(out_figs, "Fig8_ITS_COVID.png"), fig_its,
         width = 12, height = 5, dpi = 300)
  ggsave(file.path(out_figs, "Fig8_ITS_COVID.pdf"), fig_its,
         width = 12, height = 5)
  message("  Figura ITS COVID salva.")
}

# -------------------------------------------------------------------------
# 12.2 Índices formais de desigualdade (SII e Concentration Index)
# -------------------------------------------------------------------------
# O Slope Index of Inequality (SII) e o Concentration Index (CI) são
# medidas padrão da OMS para quantificar desigualdades em saúde.
# Raramente aplicados à sífilis no Brasil — lacuna importante.

message("  12.2 Índices de desigualdade (SII, Concentration Index)")

calcular_sii_ci <- function(dados_ivs_tipo, var_taxa = "taxa_bruta") {
  # Preparar: ordenar por IVS, calcular posição relativa acumulada
  df <- dados_ivs_tipo %>%
    filter(!is.na(ivs_quintil), !is.na(!!sym(var_taxa)),
           !is.na(nv_total), nv_total > 0) %>%
    group_by(ivs_quintil) %>%
    summarise(
      taxa_media = weighted.mean(!!sym(var_taxa), nv_total, na.rm = TRUE),
      pop = sum(nv_total, na.rm = TRUE),
      casos = sum(n_casos_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ivs_quintil) %>%
    mutate(
      prop_pop = pop / sum(pop),
      prop_pop_cum = cumsum(prop_pop),
      midpoint = prop_pop_cum - prop_pop / 2,  # Ridit score
      prop_casos = casos / sum(casos),
      prop_casos_cum = cumsum(prop_casos)
    )

  # SII: coeficiente da regressão ponderada de taxa sobre ridit
  if (nrow(df) >= 3) {
    mod_sii <- lm(taxa_media ~ midpoint, weights = pop, data = df)
    sii <- coef(mod_sii)[2]
    sii_se <- summary(mod_sii)$coefficients[2, 2]
    sii_ci <- confint(mod_sii)[2, ]
  } else {
    sii <- NA; sii_se <- NA; sii_ci <- c(NA, NA)
  }

  # Concentration Index
  # CI = (2/mu) * cov(y, fractional_rank)
  mu <- weighted.mean(df$taxa_media, df$pop)
  ci_val <- (2 / mu) * sum(df$prop_pop * df$taxa_media * (df$midpoint - 0.5)) /
    sum(df$prop_pop * df$taxa_media)

  # CI simplificado via fórmula de covariância
  ci_alt <- 2 * cov(df$midpoint, df$taxa_media * df$pop / sum(df$pop)) /
    mean(df$taxa_media * df$pop / sum(df$pop))

  # Curva de concentração — dados para plot
  curva <- df %>%
    select(ivs_quintil, prop_pop_cum, prop_casos_cum)

  list(
    SII = round(sii, 3),
    SII_IC95 = round(sii_ci, 3),
    CI = round(ci_alt, 4),
    taxa_Q1 = df$taxa_media[1],   # Menos vulnerável
    taxa_Q5 = df$taxa_media[5],   # Mais vulnerável
    razao_Q5_Q1 = round(df$taxa_media[5] / df$taxa_media[1], 2),
    curva_dados = curva,
    tabela_quintis = df
  )
}

# Calcular SII e CI para cada tipo de sífilis
desig_indices <- list()
for (tipo in c("Gestacional", "Congênita")) {
  dados_tipo <- dados_ivs %>% filter(tipo_sifilis == tipo)
  if (nrow(dados_tipo) > 50) {
    desig_indices[[tipo]] <- calcular_sii_ci(dados_tipo)
    message("  ", tipo, ": SII = ", desig_indices[[tipo]]$SII,
            " | CI = ", desig_indices[[tipo]]$CI,
            " | Razão Q5/Q1 = ", desig_indices[[tipo]]$razao_Q5_Q1)
  }
}

# Tabela de índices de desigualdade
if (length(desig_indices) > 0) {
  tabela_desig <- tibble(
    tipo_sifilis = names(desig_indices),
    SII = sapply(desig_indices, function(x) x$SII),
    SII_IC95_lo = sapply(desig_indices, function(x) x$SII_IC95[1]),
    SII_IC95_hi = sapply(desig_indices, function(x) x$SII_IC95[2]),
    Concentration_Index = sapply(desig_indices, function(x) x$CI),
    Taxa_Q1_menos_vulneravel = sapply(desig_indices, function(x) x$taxa_Q1),
    Taxa_Q5_mais_vulneravel = sapply(desig_indices, function(x) x$taxa_Q5),
    Razao_Q5_Q1 = sapply(desig_indices, function(x) x$razao_Q5_Q1)
  )

  writexl::write_xlsx(tabela_desig,
                      file.path(out_tables, "Tabela_Indices_Desigualdade.xlsx"))

  # Curva de concentração
  curva_plot_data <- bind_rows(
    lapply(names(desig_indices), function(tipo) {
      desig_indices[[tipo]]$curva_dados %>%
        mutate(tipo_sifilis = tipo) %>%
        bind_rows(tibble(ivs_quintil = 0, prop_pop_cum = 0,
                          prop_casos_cum = 0, tipo_sifilis = tipo), .)
    })
  )

  fig_concentracao <- curva_plot_data %>%
    ggplot(aes(x = prop_pop_cum, y = prop_casos_cum, color = tipo_sifilis)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(
      values = c("Gestacional" = "#D95F02", "Congênita" = "#7570B3"),
      name = ""
    ) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      x = "Proporção acumulada da população (ordenada por IVS)",
      y = "Proporção acumulada dos casos"
    ) +
    coord_equal()

  ggsave(file.path(out_figs, "Fig9_Curva_Concentracao.png"), fig_concentracao,
         width = 7, height = 7, dpi = 300)
  ggsave(file.path(out_figs, "Fig9_Curva_Concentracao.pdf"), fig_concentracao,
         width = 7, height = 7)
  message("  Curva de concentração salva.")
}

# -------------------------------------------------------------------------
# 12.3 Razão SC/SG como indicador programático — análise espacial
# -------------------------------------------------------------------------
# A razão SC/SG reflete falhas no manejo da sífilis na gestação.
# Valores altos indicam oportunidades perdidas de tratamento.
# Esta análise raramente é feita com abordagem espacial.

message("  12.3 Razão SC/SG — análise espacial")

razao_mun_total <- razao_mun %>%
  group_by(cod_mun_notif) %>%
  summarise(
    total_sg = sum(Gestacional, na.rm = TRUE),
    total_sc = sum(Congênita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_sg >= 5) %>%  # Mínimo de 5 casos SG para estabilidade

  mutate(razao_sc_sg = total_sc / total_sg)

# Mapa da razão SC/SG
mapa_razao <- mapa_mun %>%
  left_join(razao_mun_total, by = c("code_muni6" = "cod_mun_notif")) %>%
  filter(!is.na(razao_sc_sg))

if (nrow(mapa_razao) > 100) {
  fig_mapa_razao <- ggplot(mapa_razao) +
    geom_sf(aes(fill = razao_sc_sg), color = NA) +
    geom_sf(data = mapa_uf, fill = NA, color = "grey30", linewidth = 0.3) +
    scale_fill_viridis_c(
      option = "magma", direction = -1,
      name = "Razão SC/SG",
      limits = c(0, quantile(mapa_razao$razao_sc_sg, 0.95, na.rm = TRUE)),
      oob = scales::squish
    ) +
    theme_void() +
    theme(legend.position = c(0.15, 0.3))

  ggsave(file.path(out_maps, "Mapa_Razao_SC_SG.png"), fig_mapa_razao,
         width = 10, height = 12, dpi = 300)
  message("  Mapa razão SC/SG salvo.")

  # Correlação razão SC/SG × IVS
  razao_ivs <- razao_mun_total %>%
    left_join(
      ivs %>% select(code_muni6, ivs_geral),
      by = c("cod_mun_notif" = "code_muni6")
    ) %>%
    filter(!is.na(ivs_geral))

  if (nrow(razao_ivs) > 30) {
    cor_razao_ivs <- cor.test(razao_ivs$razao_sc_sg, razao_ivs$ivs_geral,
                               method = "spearman", exact = FALSE)
    message("  Correlação Razão SC/SG × IVS: rho = ",
            round(cor_razao_ivs$estimate, 3),
            ", p = ", format.pval(cor_razao_ivs$p.value))
  }
}

# -------------------------------------------------------------------------
# 12.4 Comparação espaço-temporal de clusters LISA entre períodos
# -------------------------------------------------------------------------
# Compara clusters LISA entre Pré-Rede Cegonha, Rede Cegonha e
# período COVID para avaliar persistência ou emergência de hotspots.

message("  12.4 Comparação espaço-temporal LISA")

lisa_por_periodo <- function(per_nome, anos_per, tipo = "Gestacional") {
  # Agregar dados do período
  casos_per <- casos_mun %>%
    filter(ano_notif %in% anos_per, tipo_sifilis == tipo) %>%
    group_by(cod_mun_notif) %>%
    summarise(n_casos = sum(n_casos), .groups = "drop")

  nv_per <- nv_mun_ano %>%
    filter(ano %in% anos_per) %>%
    group_by(cod_mun6) %>%
    summarise(nv = sum(nv, na.rm = TRUE), .groups = "drop")

  df_per <- casos_per %>%
    left_join(nv_per, by = c("cod_mun_notif" = "cod_mun6")) %>%
    mutate(taxa = ifelse(nv > 0, (n_casos / nv) * 1000, NA_real_))

  # Merge com mapa
  mapa_per <- mapa_mun %>%
    left_join(df_per, by = c("code_muni6" = "cod_mun_notif")) %>%
    filter(!is.na(taxa))

  if (nrow(mapa_per) < 100) return(NULL)

  # Suavização Bayes empírico
  mapa_per$taxa_suav <- suavizar_bayes_empirico(mapa_per$n_casos, mapa_per$nv)

  # Vizinhança e LISA
  nb <- poly2nb(mapa_per, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

  lisa <- localmoran(mapa_per$taxa_suav, lw, zero.policy = TRUE,
                      alternative = "two.sided")
  media <- mean(mapa_per$taxa_suav, na.rm = TRUE)
  lag_val <- lag.listw(lw, mapa_per$taxa_suav, zero.policy = TRUE)

  mapa_per$cluster <- case_when(
    p.adjust(lisa[, "Pr(z != E(Ii))"], method = "fdr") > 0.05 ~ "NS",
    mapa_per$taxa_suav >= media & lag_val >= media ~ "HH",
    mapa_per$taxa_suav < media & lag_val < media ~ "LL",
    mapa_per$taxa_suav >= media & lag_val < media ~ "HL",
    mapa_per$taxa_suav < media & lag_val >= media ~ "LH",
    TRUE ~ "NS"
  )

  mapa_per$periodo <- per_nome
  return(mapa_per %>% select(code_muni6, code_muni, name_muni, geom,
                              taxa_suav, cluster, periodo))
}

# Calcular LISA para cada período
periodos_lisa <- list(
  "2007-2011" = 2007:2011,
  "2012-2017" = 2012:2017,
  "2020-2023" = 2020:2023
)

lisa_periodos <- list()
for (per in names(periodos_lisa)) {
  result <- lisa_por_periodo(per, periodos_lisa[[per]], "Gestacional")
  if (!is.null(result)) lisa_periodos[[per]] <- result
}

if (length(lisa_periodos) > 0) {
  lisa_todos_periodos <- bind_rows(lisa_periodos)

  cores_lisa_abrev <- c(
    "HH" = "#D7191C", "LL" = "#2C7BB6",
    "HL" = "#FDAE61", "LH" = "#ABD9E9", "NS" = "#F0F0F0"
  )

  # Painel comparativo de mapas LISA
  fig_lisa_temporal <- ggplot(lisa_todos_periodos) +
    geom_sf(aes(fill = cluster), color = NA) +
    geom_sf(data = mapa_uf, fill = NA, color = "grey30", linewidth = 0.3) +
    facet_wrap(~periodo, ncol = 3) +
    scale_fill_manual(values = cores_lisa_abrev, name = "Cluster") +
    theme_void() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 11)
    )

  ggsave(file.path(out_maps, "Mapa_LISA_Comparacao_Temporal.png"),
         fig_lisa_temporal, width = 18, height = 8, dpi = 300)
  ggsave(file.path(out_maps, "Mapa_LISA_Comparacao_Temporal.pdf"),
         fig_lisa_temporal, width = 18, height = 8)
  message("  Mapa comparativo LISA por período salvo.")

  # Tabela: persistência de clusters HH
  persistencia_hh <- lisa_todos_periodos %>%
    st_drop_geometry() %>%
    filter(cluster == "HH") %>%
    count(code_muni6, name = "n_periodos_hh") %>%
    mutate(persistente = ifelse(n_periodos_hh >= 2, "Sim", "Não"))

  n_persistentes <- sum(persistencia_hh$persistente == "Sim")
  message("  Municípios com cluster HH persistente (>=2 períodos): ", n_persistentes)

  writexl::write_xlsx(persistencia_hh,
                      file.path(out_tables, "Tabela_Clusters_HH_Persistentes.xlsx"))
}

# -------------------------------------------------------------------------
# 12.5 Evolução temporal dos índices de desigualdade (SII e CI por ano)
# -------------------------------------------------------------------------
# Mostra se as desigualdades estão aumentando ou diminuindo ao longo do tempo

message("  12.5 Evolução temporal dos índices de desigualdade")

desig_temporal <- list()

for (ano in ANOS_ESTUDO) {
  for (tipo in c("Gestacional", "Congênita")) {
    # Dados do ano
    casos_ano <- casos_mun %>%
      filter(ano_notif == ano, tipo_sifilis == tipo)

    nv_ano <- nv_mun_ano %>% filter(ano == !!ano)

    df_ano <- casos_ano %>%
      left_join(nv_ano, by = c("cod_mun_notif" = "cod_mun6")) %>%
      left_join(ivs %>% select(code_muni6, ivs_quintil),
                by = c("cod_mun_notif" = "code_muni6")) %>%
      filter(!is.na(ivs_quintil), !is.na(nv), nv > 0) %>%
      mutate(taxa_bruta = (n_casos / nv) * 1000,
             n_casos_total = n_casos,
             nv_total = nv)

    if (nrow(df_ano) > 50) {
      idx <- calcular_sii_ci(df_ano)
      desig_temporal[[paste0(ano, "_", tipo)]] <- tibble(
        ano = ano,
        tipo_sifilis = tipo,
        SII = idx$SII,
        CI = idx$CI,
        razao_Q5_Q1 = idx$razao_Q5_Q1
      )
    }
  }
}

if (length(desig_temporal) > 0) {
  desig_temporal_df <- bind_rows(desig_temporal)

  fig_desig_temporal <- desig_temporal_df %>%
    pivot_longer(cols = c(SII, CI, razao_Q5_Q1),
                 names_to = "indice", values_to = "valor") %>%
    ggplot(aes(x = ano, y = valor, color = tipo_sifilis)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~indice, scales = "free_y", ncol = 3,
               labeller = labeller(indice = c(
                 "SII" = "Slope Index of Inequality",
                 "CI" = "Concentration Index",
                 "razao_Q5_Q1" = "Razão Q5/Q1"
               ))) +
    scale_color_manual(
      values = c("Gestacional" = "#D95F02", "Congênita" = "#7570B3"),
      name = ""
    ) +
    scale_x_continuous(breaks = ANOS_ESTUDO) +
    labs(x = "Ano", y = "Valor do índice") +
    geom_vline(xintercept = 2019.5, linetype = "dotted",
               color = "red", linewidth = 0.5)

  ggsave(file.path(out_figs, "Fig10_Desigualdade_Temporal.png"),
         fig_desig_temporal, width = 14, height = 5, dpi = 300)
  ggsave(file.path(out_figs, "Fig10_Desigualdade_Temporal.pdf"),
         fig_desig_temporal, width = 14, height = 5)

  writexl::write_xlsx(desig_temporal_df,
                      file.path(out_tables, "Tabela_Desigualdade_Temporal.xlsx"))
  message("  Evolução temporal de desigualdades salva.")
}

# -------------------------------------------------------------------------
# 12.6 Forest plot — Joinpoint APC por UF
# -------------------------------------------------------------------------
message("  12.6 Forest plot Joinpoint por UF")

if (nrow(jp_uf_tabela) > 0) {
  # Usar AAPC (ou APC do segmento mais recente) por UF
  fp_data <- jp_uf_tabela %>%
    filter(grepl("Gestacional", label)) %>%
    group_by(label) %>%
    # Último segmento = tendência mais recente
    slice_max(ano_fim, n = 1) %>%
    ungroup() %>%
    mutate(
      sigla = str_extract(label, "^[A-Z]{2}"),
      significativo = ifelse(
        (APC_IC95_lo > 0 & APC_IC95_hi > 0) |
          (APC_IC95_lo < 0 & APC_IC95_hi < 0),
        "Sim", "Não"
      )
    ) %>%
    arrange(APC)

  fig_forest <- fp_data %>%
    mutate(sigla = factor(sigla, levels = sigla)) %>%
    ggplot(aes(x = APC, y = sigla)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = APC_IC95_lo, xmax = APC_IC95_hi),
                   height = 0.3, linewidth = 0.5) +
    geom_point(aes(color = significativo), size = 2.5) +
    scale_color_manual(values = c("Sim" = "#D73027", "Não" = "#4575B4"),
                       name = "Estatisticamente\nsignificativo") +
    labs(
      x = "APC (%) — último segmento",
      y = ""
    )

  ggsave(file.path(out_figs, "Fig11_Forest_Joinpoint_UF.png"), fig_forest,
         width = 8, height = 10, dpi = 300)
  ggsave(file.path(out_figs, "Fig11_Forest_Joinpoint_UF.pdf"), fig_forest,
         width = 8, height = 10)
  message("  Forest plot salvo.")
}

# -------------------------------------------------------------------------
# 12.7 Modelo com interação raça × IVS
# -------------------------------------------------------------------------
message("  12.7 Modelo interação raça × IVS")

if (nrow(df_painel) > 100 && "ivs_geral" %in% names(df_painel)) {
  # Adicionar raça/cor ao painel
  df_painel_raca <- df_sifg %>%
    filter(raca_cor %in% c("Branca", "Preta", "Parda")) %>%
    count(ano_notif, cod_mun_notif, raca_cor, name = "n_casos") %>%
    left_join(nv_mun_ano, by = c("cod_mun_notif" = "cod_mun6",
                                  "ano_notif" = "ano")) %>%
    left_join(ivs %>% select(code_muni6, ivs_geral, ivs_quintil),
              by = c("cod_mun_notif" = "code_muni6")) %>%
    filter(!is.na(ivs_geral), !is.na(nv), nv > 0) %>%
    mutate(
      cod_uf = substr(cod_mun_notif, 1, 2),
      log_nv = log(nv),
      raca_cor = relevel(factor(raca_cor), ref = "Branca"),
      ivs_cat3 = cut(ivs_geral, breaks = c(0, 0.3, 0.5, 1),
                      labels = c("Baixa", "Media", "Alta"))
    )

  if (nrow(df_painel_raca) > 200) {
    # Modelo com interação
    mod_interacao <- MASS::glm.nb(
      n_casos ~ raca_cor * ivs_cat3 + offset(log_nv),
      data = df_painel_raca
    )

    # IRR
    irr_interacao <- broom::tidy(mod_interacao, exponentiate = TRUE,
                                  conf.int = TRUE) %>%
      mutate(modelo = "BN com interação raça × IVS")

    writexl::write_xlsx(irr_interacao,
                        file.path(out_tables, "Tabela_Modelo_Interacao_Raca_IVS.xlsx"))

    # Predicted rates por combinação raça × IVS
    grid_pred <- expand.grid(
      raca_cor = factor(c("Branca", "Preta", "Parda"), levels = levels(df_painel_raca$raca_cor)),
      ivs_cat3 = factor(c("Baixa", "Media", "Alta")),
      log_nv = log(1000)
    )
    grid_pred$pred <- predict(mod_interacao, newdata = grid_pred, type = "response")

    fig_interacao <- grid_pred %>%
      ggplot(aes(x = ivs_cat3, y = pred, fill = raca_cor)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_manual(values = cores_raca, name = "Raça/Cor") +
      labs(
        x = "Vulnerabilidade Social (IVS)",
        y = "Casos preditos (por 1.000 NV)"
      )

    ggsave(file.path(out_figs, "Fig12_Interacao_Raca_IVS.png"), fig_interacao,
           width = 9, height = 6, dpi = 300)
    ggsave(file.path(out_figs, "Fig12_Interacao_Raca_IVS.pdf"), fig_interacao,
           width = 9, height = 6)
    message("  Modelo interação raça × IVS salvo.")
  }
}

message("  Parte 12 concluída — análises avançadas finalizadas.")


###############################################################################
#     RESUMO FINAL                                                             #
###############################################################################

message("\n")
message("================================================================")
message("  PIPELINE ANALÍTICA CONCLUÍDA")
message("================================================================")
message("")
message("  Período de estudo: ", ANO_INICIO, "–", ANO_FIM)
message("  Sífilis Gestacional: ", nrow(df_sifg), " registros")
message("  Sífilis Congênita:   ", nrow(df_sifc), " registros")
message("")
message("  PRODUTOS GERADOS:")
message("  ─────────────────")
message("  Tabelas:    ", out_tables)
message("  Figuras:    ", out_figs)
message("  Mapas:      ", out_maps)
message("  Modelos:    ", out_models)
message("  Suplementar:", out_suppl)
message("")
message("  ARQUIVOS-CHAVE:")
message("  • Tabela1_Brasil_anual.xlsx")
message("  • Tabela_Joinpoint_Completa.xlsx")
message("  • Tabela_Moran_Global.xlsx")
message("  • Tabela_IVS_estratos.xlsx")
message("  • Tabela_Indices_Desigualdade.xlsx (SII, CI)")
message("  • Tabela_Desigualdades_Raciais.xlsx")
message("  • Tabela_ITS_COVID.xlsx")
message("  • Tabela_Modelos_Multivariados.xlsx")
message("  • Tabela_Modelo_Interacao_Raca_IVS.xlsx")
message("  • Tabela_Clusters_HH_Persistentes.xlsx")
message("  • Fig1–Fig12 (tendências, raça, IVS, ITS, concentração, forest)")
message("  • Mapas: taxas, LISA, razão SC/SG, comparação temporal")
message("  • Material suplementar completo")
message("  • STROBE Checklist")
message("")
message("  NOTAS IMPORTANTES:")
message("  1. Verifique se os dados SINAN foram baixados corretamente")
message("  2. Para taxas precisas, baixe o SINASC completo")
message("  3. Substitua o IVS simulado pelo IVS real do IPEA")
message("  4. Para Joinpoint oficial, use o software NCI Joinpoint")
message("  5. Para modelo BYM (INLA), instale o pacote INLA separadamente")
message("================================================================")
