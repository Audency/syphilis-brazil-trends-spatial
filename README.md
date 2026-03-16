# Temporal Trends, Spatial Patterns, and Social Inequalities in Gestational and Congenital Syphilis in Brazil, 2007-2023

## Overview

Nationwide ecological study analysing temporal trends, spatial patterns, and social inequalities in gestational and congenital syphilis across all 5,570 Brazilian municipalities from 2007 to 2023.

## Data Sources

- **SINAN** (Sistema de Informacao de Agravos de Notificacao): gestational and congenital syphilis notifications
- **SINASC** (Sistema de Informacoes sobre Nascidos Vivos): live births (denominators)
- **IBGE/geobr**: municipal boundary shapefiles
- **IPEA**: Social Vulnerability Index (IVS)

## Methods

- Joinpoint/segmented regression (temporal trends)
- Global Moran's I and LISA (spatial analysis)
- Interrupted time series (COVID-19 impact)
- Slope Index of Inequality and Concentration Index
- Negative binomial, multilevel, and spatial regression models

## Structure

```
R_analysis/
  analise_completa_sifilis.R    # Single-script full pipeline
  data/raw/                      # Downloaded DBC files (not tracked)
  data/processed/                # Processed RDS files (not tracked)
  output/
    tables/                      # Excel tables
    figures/                     # Publication-ready figures
    maps/                        # Choropleth and LISA maps
    supplementary/               # Supplementary material
manuscript/
  manuscript_syphilis_brazil.md  # Full manuscript draft
```

## Author

Audencio Victor, London School of Hygiene and Tropical Medicine (LSHTM)
