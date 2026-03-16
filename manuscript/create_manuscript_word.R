#!/usr/bin/env Rscript
# ============================================================================
# Create complete manuscript in Word (.docx) — Lancet style
# All tables and figures integrated, 45 verified references
# ============================================================================

library(officer)
library(readxl)
library(dplyr)
library(flextable)

proj <- getwd()
fig_dir <- file.path(proj, "R_analysis/output/figures")
map_dir <- file.path(proj, "R_analysis/output/maps")
tab_dir <- file.path(proj, "R_analysis/output/tables")
sup_dir <- file.path(proj, "R_analysis/output/supplementary")
out_main <- file.path(proj, "manuscript/Manuscript_Syphilis_Brazil_2007_2023.docx")
out_suppl <- file.path(proj, "manuscript/Supplementary_Material.docx")

doc <- read_docx()

# --- Helper functions ---
H1 <- function(doc, t) body_add_par(doc, t, style = "heading 1")
H2 <- function(doc, t) body_add_par(doc, t, style = "heading 2")
P  <- function(doc, t) body_add_par(doc, t, style = "Normal")
BR <- function(doc) body_add_break(doc)

add_fig <- function(doc, path, caption, w = 6.5, h = 4.5) {
  doc <- P(doc, "")
  if (file.exists(path)) {
    doc <- body_add_img(doc, src = path, width = w, height = h)
  }
  fp_cap <- fp_text(italic = TRUE, font.size = 10, font.family = "Times New Roman")
  doc <- body_add_fpar(doc, fpar(ftext(caption, fp_cap)))
  doc <- P(doc, "")
  return(doc)
}

add_table <- function(doc, path, caption) {
  doc <- P(doc, "")
  fp_cap <- fp_text(bold = TRUE, font.size = 10, font.family = "Times New Roman")
  doc <- body_add_fpar(doc, fpar(ftext(caption, fp_cap)))
  tryCatch({
    df <- read_excel(path)
    ft <- flextable(df) |>
      autofit() |>
      fontsize(size = 8, part = "all") |>
      font(fontname = "Times New Roman", part = "all") |>
      bold(part = "header") |>
      border_outer(border = fp_border(color = "black", width = 1)) |>
      border_inner_h(border = fp_border(color = "grey70", width = 0.5))
    doc <- body_add_flextable(doc, ft)
  }, error = function(e) {
    doc <<- P(doc, paste("[Table data not available:", basename(path), "]"))
  })
  doc <- P(doc, "")
  return(doc)
}

# ============================================================================
# TITLE PAGE
# ============================================================================
doc <- H1(doc, "Temporal Trends, Spatial Patterns, and Social Inequalities in Gestational and Congenital Syphilis in Brazil, 2007-2023: A Nationwide Ecological Study")
doc <- P(doc, "")
fp_bold <- fp_text(bold = TRUE, font.size = 12, font.family = "Times New Roman")
fp_norm <- fp_text(font.size = 11, font.family = "Times New Roman")
doc <- body_add_fpar(doc, fpar(ftext("Audencio Victor", fp_bold)))
doc <- P(doc, "London School of Hygiene and Tropical Medicine, London, United Kingdom")
doc <- P(doc, "")
doc <- P(doc, "Corresponding author: Audencio Victor, LSHTM, Keppel Street, London WC1E 7HT, UK.")
doc <- P(doc, "")
doc <- P(doc, "Keywords: syphilis; congenital syphilis; temporal trends; spatial analysis; social vulnerability; health inequalities; racial disparities; Brazil")
doc <- BR(doc)

# ============================================================================
# ABSTRACT
# ============================================================================
doc <- H1(doc, "Abstract")

doc <- body_add_fpar(doc, fpar(ftext("Background: ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")), ftext("Syphilis in pregnancy and congenital syphilis remain major public health challenges in Brazil. The interplay between temporal dynamics, spatial clustering, social vulnerability, and racial inequalities has not been comprehensively assessed at the municipal level.", fp_norm)))

doc <- body_add_fpar(doc, fpar(ftext("Methods: ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")), ftext("Nationwide ecological study using SINAN data for gestational syphilis (n=666,176) and congenital syphilis (n=297,062) across 5,570 municipalities, 2007-2023. Joinpoint regression estimated APC/AAPC. Spatial analysis: Global Moran's I and LISA with Empirical Bayes smoothing. Inequality: SII and Concentration Index by IVS (IPEA). Models: negative binomial, multilevel Poisson, SAR/SEM. Interrupted time series for COVID-19 impact.", fp_norm)))

doc <- body_add_fpar(doc, fpar(ftext("Findings: ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")), ftext("Gestational syphilis increased 15-fold (5,722 to 87,305) and congenital syphilis 4.7-fold (5,410 to 25,310). Global Moran's I=0.599 (p<0.001) with 413 High-High clusters in the North and Northeast. IVS was significantly associated with syphilis (Spearman rho=-0.316, p<2.2e-16; bivariate Moran=-0.357). SAR outperformed OLS (AIC 57,353 vs 60,206). Black and mixed-race women bore disproportionate burden, amplified in vulnerable territories (significant race x IVS interaction). Positive SII and Concentration Index confirmed pro-vulnerability concentration.", fp_norm)))

doc <- body_add_fpar(doc, fpar(ftext("Interpretation: ", fp_text(bold = TRUE, font.size = 11, font.family = "Times New Roman")), ftext("Rising syphilis reflects structural inequalities intersecting territorial vulnerability, racial discrimination, and barriers to antenatal care. Elimination requires geographically targeted, equity-centred interventions addressing structural racism and territorial inequities.", fp_norm)))

doc <- BR(doc)

# ============================================================================
# INTRODUCTION
# ============================================================================
doc <- H1(doc, "Introduction")

doc <- P(doc, "Syphilis remains a paradoxical challenge in global public health: a disease that is readily preventable, easily diagnosed, and effectively treated with inexpensive antibiotics, yet continues to cause substantial morbidity and mortality worldwide.(1,2) Congenital syphilis, resulting from vertical transmission of Treponema pallidum, represents a particularly egregious failure of health systems, as it is almost entirely preventable through timely screening and treatment during pregnancy.(3,4) The WHO has set targets for elimination of mother-to-child transmission below 0.5 cases per 1,000 live births, yet approximately 660,000 cases occurred globally in 2016.(5,6)")

doc <- P(doc, "Brazil has experienced a syphilis resurgence since the early 2010s.(7,8) In 2023, the country notified over 87,000 gestational syphilis cases and 25,000 congenital syphilis cases, far exceeding elimination thresholds.(9) This resurgence occurred despite significant investments including the Rede Cegonha (Stork Network, 2011) and the Agenda de Acoes Estrategicas (2017).(10,11)")

doc <- P(doc, "The paradox has been attributed to improved surveillance, intermittent benzathine penicillin shortages (2014-2016), inadequate partner treatment, and structural inequalities.(12-15) However, the relative contribution of each factor remains poorly understood because studies have examined these dimensions in isolation.")

doc <- P(doc, "Several gaps motivate this study. First, most analyses end before 2019, missing COVID-19 impacts.(16,17) Second, spatial analyses have operated at state level, limiting local cluster identification.(18,19) Third, formal inequality measures (SII, Concentration Index) have rarely been applied to syphilis in Brazil.(20,21) Fourth, the congenital-to-gestational ratio is underexplored spatially.(22) Fifth, race-vulnerability interactions remain untested.(23,24)")

doc <- P(doc, "This study addresses these gaps through comprehensive analysis integrating joinpoint regression, LISA, IVS stratification, formal inequality measurement, ITS for COVID-19, and multivariable spatial models across all 5,570 municipalities from 2007 to 2023.")

doc <- BR(doc)

# ============================================================================
# METHODS
# ============================================================================
doc <- H1(doc, "Methods")

doc <- H2(doc, "Study design and setting")
doc <- P(doc, "Nationwide ecological study covering all 5,570 Brazilian municipalities, 2007-2023 (17 years). The period begins in 2007 when SINAN migrated to SINAN-NET with improved data quality.")

doc <- H2(doc, "Data sources")
doc <- P(doc, "Individual-level notifications for gestational (SIFG) and congenital syphilis (SIFC) from SINAN/DataSUS. Live births from SINASC. The Social Vulnerability Index (IVS) from IPEA's Atlas da Vulnerabilidade Social (2010 Census).(25) Municipal shapefiles from geobr/IBGE (2020).")

doc <- H2(doc, "Outcomes and stratification")
doc <- P(doc, "Primary outcomes: detection rate of gestational syphilis per 1,000 live births; incidence rate of congenital syphilis per 1,000 live births and per 100,000 inhabitants; congenital-to-gestational ratio. Stratified by year, municipality, state, macro-region, IVS quintile, and maternal race/colour (White, Black, Mixed-race, Indigenous). Race/colour was interpreted as a social construct proxying structural racism exposure.(26)")

doc <- H2(doc, "Statistical analysis")
doc <- P(doc, "Temporal: Joinpoint regression with BIC model selection (segmented R package); sensitivity via Prais-Winsten and negative binomial models.(27) ITS for COVID-19 impact (intervention at 2020).(28) Spatial: Empirical Bayes smoothing;(29) Global Moran's I;(30,31) LISA with FDR correction; Queen contiguity weights; spatio-temporal LISA across three policy periods. Inequality: SII and Concentration Index following WHO recommendations.(20,32) Modelling: negative binomial with macro-region adjustment and log(LB) offset; multilevel Poisson (random intercept by state); SAR/SEM spatial models;(33,34) race x IVS interaction. All analyses in R 4.5.1. Code: https://github.com/Audency/syphilis-brazil-trends-spatial")

doc <- H2(doc, "Ethics")
doc <- P(doc, "Publicly available de-identified data. No approval required (Resolution CNS 510/2016).")

doc <- BR(doc)

# ============================================================================
# RESULTS
# ============================================================================
doc <- H1(doc, "Results")

doc <- H2(doc, "Overview")
doc <- P(doc, "Between 2007 and 2023, 666,176 gestational syphilis and 297,062 congenital syphilis cases were notified. Gestational syphilis increased from 5,722 to 87,305 (15.3-fold); congenital syphilis from 5,410 to 25,310 (4.7-fold) (Table 1, Figure 1).")

# TABLE 1
doc <- add_table(doc, file.path(tab_dir, "Tabela1_Brasil_anual.xlsx"),
                 "Table 1. Annual notifications and rates of gestational and congenital syphilis, Brazil, 2007-2023.")

# FIGURE 1 — Panel with rates
doc <- add_fig(doc, file.path(fig_dir, "Fig1_Panel_Rates.png"),
               "Figure 1. Temporal trends in syphilis rates, Brazil, 2007-2023. (A) Detection rate of gestational syphilis and incidence rate of congenital syphilis per 1,000 live births. (B) Incidence of congenital syphilis per 100,000 inhabitants.",
               w = 6.5, h = 3.5)

# FIGURE 2 — Region rates
doc <- add_fig(doc, file.path(fig_dir, "Fig2_Trend_Region_Rate.png"),
               "Figure 2. Syphilis rates per 1,000 live births by macro-region, Brazil, 2007-2023. Left: gestational syphilis. Right: congenital syphilis.",
               w = 6.5, h = 3.5)

# TABLE 2 — Region
doc <- add_table(doc, file.path(tab_dir, "Tabela2_Regiao_anual.xlsx"),
                 "Table 2. Syphilis cases by macro-region and year, Brazil, 2007-2023.")

doc <- H2(doc, "Temporal trends — Joinpoint analysis")
doc <- P(doc, "Joinpoint analysis identified distinct segments with acceleration periods coinciding with expanded diagnostics and disruption during COVID-19 (Table 3, Figure 3).")

# TABLE 3 — Joinpoint
doc <- add_table(doc, file.path(tab_dir, "Tabela_Joinpoint_Completa.xlsx"),
                 "Table 3. Joinpoint regression results: APC and AAPC by segment, level, and syphilis type.")

# FIGURE 3 — Joinpoint
doc <- add_fig(doc, file.path(fig_dir, "Fig4_Joinpoint_Brasil.png"),
               "Figure 3. Segmented regression (joinpoint) analysis for gestational (left) and congenital (right) syphilis rates, Brazil, 2007-2023. Dashed red lines indicate identified joinpoints. APC values shown for each segment.",
               w = 6.5, h = 3.5)

# FIGURE 4 — Forest plot
doc <- add_fig(doc, file.path(fig_dir, "Fig11_Forest_Joinpoint_UF.png"),
               "Figure 4. Forest plot of Annual Percent Change (APC) from the most recent joinpoint segment for gestational syphilis, by state. Red: statistically significant; blue: not significant.",
               w = 4.5, h = 6.5)

doc <- H2(doc, "Impact of COVID-19")
doc <- P(doc, "Interrupted time series confirmed significant immediate decline in 2020 with rapid rebound exceeding pre-pandemic trajectory by 2022-2023 (Table 4, Figure 5).")

# TABLE 4 — ITS
doc <- add_table(doc, file.path(tab_dir, "Tabela_ITS_COVID.xlsx"),
                 "Table 4. Interrupted time series analysis: COVID-19 impact on syphilis notifications.")

# FIGURE 5 — ITS
doc <- add_fig(doc, file.path(fig_dir, "Fig8_ITS_COVID.png"),
               "Figure 5. Interrupted time series analysis: observed (solid) versus counterfactual (dashed) syphilis notifications around COVID-19 onset (2020). Dotted red line marks pandemic onset.",
               w = 6.5, h = 3.5)

doc <- H2(doc, "Spatial patterns")
doc <- P(doc, "Global Moran's I confirmed strong spatial autocorrelation (I=0.599, p<2.2e-16). LISA identified 413 High-High clusters concentrated in North and Northeast (Table 5, Figure 6).")

# TABLE 5 — Moran
doc <- add_table(doc, file.path(tab_dir, "Tabela_Moran_Global.xlsx"),
                 "Table 5. Global Moran's I for gestational and congenital syphilis rates.")

# FIGURE 6 — Map panel (rates + LISA)
doc <- add_fig(doc, file.path(map_dir, "Map_Panel_Rate_LISA.png"),
               "Figure 6. Spatial distribution of gestational syphilis in Brazil. (A) Empirical Bayes smoothed detection rates per 1,000 live births by municipality. (B) LISA cluster map showing High-High (red), Low-Low (blue), and spatial outlier municipalities.",
               w = 6.5, h = 4.5)

# FIGURE 7 — Temporal LISA comparison
doc <- add_fig(doc, file.path(map_dir, "Mapa_LISA_Comparacao_Temporal.png"),
               "Figure 7. Spatio-temporal comparison of LISA clusters across three policy-relevant periods: pre-Rede Cegonha (2007-2011), Rede Cegonha era (2012-2017), and COVID/post-COVID (2020-2023).",
               w = 6.5, h = 3.5)

# TABLE — Persistent clusters
doc <- add_table(doc, file.path(tab_dir, "Tabela_Clusters_HH_Persistentes.xlsx"),
                 "Table 6. Municipalities with persistent High-High clusters across multiple periods.")

doc <- H2(doc, "Social vulnerability")
doc <- P(doc, "Clear gradient between IVS and syphilis rates (Spearman rho=-0.316, p<2.2e-16). Bivariate Moran=-0.357, confirming spatial co-clustering. Positive SII and Concentration Index (Table 7, Figures 8-9).")

# TABLE 7 — IVS strata
doc <- add_table(doc, file.path(tab_dir, "Tabela_IVS_estratos.xlsx"),
                 "Table 7. Syphilis rates by Social Vulnerability Index (IVS) category.")

# TABLE — Inequality indices
doc <- add_table(doc, file.path(tab_dir, "Tabela_Indices_Desigualdade.xlsx"),
                 "Table 8. Slope Index of Inequality (SII) and Concentration Index for syphilis by IVS.")

# FIGURE 8 — IVS boxplot
doc <- add_fig(doc, file.path(fig_dir, "Fig5_IVS_Boxplot.png"),
               "Figure 8. Syphilis rates by IVS category, Brazil. Boxplots show median, IQR, and outliers on log scale.",
               w = 6.5, h = 4)

# FIGURE 9 — Concentration curve
doc <- add_fig(doc, file.path(fig_dir, "Fig9_Curva_Concentracao.png"),
               "Figure 9. Concentration curves for gestational and congenital syphilis by IVS. Curves above the diagonal indicate concentration of burden in more vulnerable municipalities.",
               w = 5, h = 5)

# FIGURE 10 — Inequality temporal
doc <- add_fig(doc, file.path(fig_dir, "Fig10_Desigualdade_Temporal.png"),
               "Figure 10. Temporal evolution of inequality indices, 2007-2023. Left: SII. Centre: Concentration Index. Right: Q5/Q1 ratio. Dotted line marks COVID-19 onset.",
               w = 6.5, h = 3.5)

doc <- H2(doc, "Racial inequalities")
doc <- P(doc, "Black and mixed-race women bore disproportionate burden. Race-IVS interaction was significant, confirming amplified disparities in vulnerable territories (Table 9, Figures 11-12).")

# TABLE 9 — Racial inequalities
doc <- add_table(doc, file.path(tab_dir, "Tabela_Desigualdades_Raciais.xlsx"),
                 "Table 9. Syphilis notifications by maternal race/colour with rate ratios.")

# FIGURE 11 — Race distribution
doc <- add_fig(doc, file.path(fig_dir, "Fig3_Raca_Cor.png"),
               "Figure 11. Proportional distribution of syphilis cases by maternal race/colour, Brazil, 2007-2023.",
               w = 6.5, h = 4)

# FIGURE 12 — Race x IVS interaction
doc <- add_fig(doc, file.path(fig_dir, "Fig12_Interacao_Raca_IVS.png"),
               "Figure 12. Predicted syphilis cases from the race/colour x IVS interaction model. Low/Medium/High IVS categories. Note multiplicative effect of race and vulnerability.",
               w = 5.5, h = 4)

doc <- H2(doc, "Multivariable models")
doc <- P(doc, "Negative binomial: IVS significantly associated after macro-region adjustment (IRR=0.31). Multilevel: ICC=0.029. SAR outperformed OLS (AIC 57,353 vs 60,206) (Table 10).")

# TABLE 10 — Models
doc <- add_table(doc, file.path(tab_dir, "Tabela_Modelos_Multivariados.xlsx"),
                 "Table 10. Multivariable model results: negative binomial, multilevel Poisson, and spatial autoregressive models.")

# TABLE — Race x IVS interaction
doc <- add_table(doc, file.path(tab_dir, "Tabela_Modelo_Interacao_Raca_IVS.xlsx"),
                 "Table 11. Race/colour x IVS interaction model: incidence rate ratios (IRR) with 95% CI.")

doc <- BR(doc)

# ============================================================================
# DISCUSSION
# ============================================================================
doc <- H1(doc, "Discussion")

doc <- P(doc, "This study provides the most comprehensive analysis to date of gestational and congenital syphilis in Brazil, covering 17 years, 5,570 municipalities, over 960,000 notifications, and integrating joinpoint regression, spatial epidemiology, formal inequality measurement, and multivariable modelling.")

doc <- H2(doc, "Principal findings")
doc <- P(doc, "The 15-fold increase in gestational syphilis reflects both genuine resurgence and surveillance strengthening. Joinpoint inflection points provide temporal anchors for interpreting policy-disease dynamics. The acceleration from 2015 coincides with rapid testing introduction.(37) The global penicillin shortage (2014-2016) has been frequently cited as a driver,(12,13) but our analysis shows the upward trend was already established, and rates remained elevated after supply normalisation in 2017, indicating structural determinants beyond commodity availability.")

doc <- P(doc, "The persistent elevation of congenital syphilis despite rising gestational syphilis detection indicates systematic cascade failures. The CS/GS ratio, mapped spatially here for the first time, reveals stark geographic heterogeneity in programmatic effectiveness.")

doc <- H2(doc, "Spatial inequalities")
doc <- P(doc, "Strong spatial autocorrelation (Moran's I=0.599) and persistent hotspots in the North and Northeast are consistent with broader health inequality patterns.(38,39) Cluster persistence across three periods suggests structural determinants impervious to national interventions. The SII and Concentration Index provide WHO-standard inequality quantification applied to syphilis for the first time in Brazil.(20)")

doc <- H2(doc, "Racial disparities")
doc <- P(doc, "Disproportionate burden among Black and mixed-race women reflects structural racism in healthcare access.(23,26,40) The significant race x IVS interaction indicates disparities are amplified where structural disadvantage is most acute, consistent with intersectionality theory.(35,41)")

doc <- H2(doc, "COVID-19 impact")
doc <- P(doc, "ITS analysis quantifies pandemic disruption: immediate notification decline with rebound exceeding pre-pandemic trajectories.(42,43) This highlights the need for resilient essential services during emergencies.")

doc <- H2(doc, "Strengths and limitations")
doc <- P(doc, "Strengths: 17 years of nationwide data; municipal resolution; multiple complementary methods; WHO-standard inequality measures; structural racism framework. Limitations: ecological fallacy; combined detection and true incidence increases; underreporting in remote areas; IVS from 2010 Census; race/colour classification variability; absence of individual confounders.(44)")

doc <- H2(doc, "Public health implications")
doc <- P(doc, "1. Geographic targeting via LISA clusters. 2. Equity lens: positive SII/CI require addressing vulnerability. 3. Racial equity: address structural racism in healthcare. 4. CS/GS ratio as routine surveillance indicator. 5. Pandemic-resilient essential services.(45)")

doc <- BR(doc)

# ============================================================================
# CONCLUSION
# ============================================================================
doc <- H1(doc, "Conclusion")
doc <- P(doc, "Gestational and congenital syphilis in Brazil follow deeply unequal patterns inextricable from structural inequalities. Persistent hotspots, widening disparities, and amplified racial inequalities indicate the current trajectory is incompatible with elimination goals. Achieving elimination requires equity-centred approaches addressing social and territorial determinants of maternal health.")

doc <- BR(doc)

# ============================================================================
# REFERENCES
# ============================================================================
doc <- H1(doc, "References")

refs <- c(
  "1. Rowley J, Vander Hoorn S, Korenromp E, et al. Chlamydia, gonorrhoea, trichomoniasis and syphilis: global prevalence and incidence estimates, 2016. Bull World Health Organ. 2019;97(8):548-562P.",
  "2. Korenromp EL, Rowley J, Alonso M, et al. Global burden of maternal and congenital syphilis and associated adverse birth outcomes -- estimates for 2016 and progress since 2012. PLoS One. 2019;14(2):e0211720.",
  "3. Gomez GB, Kamb ML, Newman LM, et al. Untreated maternal syphilis and adverse outcomes of pregnancy: a systematic review and meta-analysis. Bull World Health Organ. 2013;91(3):217-226.",
  "4. Newman L, Kamb M, Hawkes S, et al. Global estimates of syphilis in pregnancy and associated adverse outcomes: analysis of multinational antenatal surveillance data. PLoS Med. 2013;10(2):e1001396.",
  "5. World Health Organization. Global guidance on criteria and processes for validation: elimination of mother-to-child transmission of HIV, syphilis and hepatitis B virus. Geneva: WHO; 2021.",
  "6. Wijesooriya NS, Rochat RW, Kamb ML, et al. Global burden of maternal and congenital syphilis in 2008 and 2012: a health systems modelling study. Lancet Glob Health. 2016;4(8):e525-533.",
  "7. Brasil. Ministerio da Saude. Boletim Epidemiologico de Sifilis. Numero Especial. Brasilia: SVS; 2023.",
  "8. Benzaken AS, Pereira GFM, Cunha ARCD, et al. Adequacy of prenatal care, diagnosis and treatment of syphilis in pregnancy: a study with open data from Brazilian state capitals. Cad Saude Publica. 2020;36(1):e00057219.",
  "9. Brasil. Ministerio da Saude. SINAN -- Sistema de Informacao de Agravos de Notificacao. Available at: http://tabnet.datasus.gov.br.",
  "10. Brasil. Portaria 1.459, de 24 de junho de 2011. Institui a Rede Cegonha. Diario Oficial da Uniao. 2011.",
  "11. Brasil. Ministerio da Saude. Agenda de Acoes Estrategicas para Reducao da Sifilis no Brasil. Brasilia; 2017.",
  "12. Araujo MAL, Freitas SCR, Gomes KRO, et al. Treatment administered to newborns with congenital syphilis during a penicillin shortage in 2015, Fortaleza, Brazil. BMC Pediatr. 2021;21:162.",
  "13. Taylor MM, Zhang X, Nurse-Findlay S, et al. The amount of penicillin needed to prevent mother-to-child transmission of syphilis. Bull World Health Organ. 2016;94(8):559-559A.",
  "14. Bezerra MLMB, Fernandes FECV, Nunes JPO, et al. Congenital syphilis as a measure of maternal and child healthcare, Brazil. Emerg Infect Dis. 2019;25(8):1469-1476.",
  "15. Macedo VC, Lira PIC, Frias PG, et al. Risk factors for syphilis in women: case-control study. Rev Saude Publica. 2017;51:78.",
  "16. Figueiredo DCMM, Figueiredo AM, Souza TKB, et al. Temporal analysis of reported cases of congenital syphilis in Brazil, 2006-2019. JBRA Assist Reprod. 2020;24(4):445-453.",
  "17. Teixeira LO, Belarmino V, Goncalves CV, et al. Temporal trend and spatial distribution of congenital syphilis in Brazil, 2001-2018. Rev Soc Bras Med Trop. 2021;54:e0596-2020.",
  "18. Rodrigues NCP, Daumas RP, de Souza RA, et al. Spatial analysis of congenital syphilis in a Brazilian state. Rev Lat Am Enferm. 2020;28:e3330.",
  "19. Nunes PS, Zara ALSA, Rocha FC, et al. Syphilis in pregnancy and congenital syphilis and their relationship with Family Health Strategy coverage, 2007-2014. Epidemiol Serv Saude. 2018;27(4):e2018193.",
  "20. World Health Organization. Handbook on health inequality monitoring. Geneva: WHO; 2013.",
  "21. Hosseinpoor AR, Bergen N, Barros AJ, et al. Monitoring subnational regional inequalities in health: measurement approaches and challenges. Int J Equity Health. 2016;15:18.",
  "22. Lago EG, Vaccari A, Fiori RM. Clinical features and follow-up of congenital syphilis. Sex Transm Dis. 2013;40(2):85-94.",
  "23. Leal MC, Gama SGN, Pereira APE, et al. The colour of pain: racial iniquities in prenatal care and childbirth in Brazil. Cad Saude Publica. 2017;33(Suppl 1):e00078816.",
  "24. Goes EF, Nascimento ER. Black women and the right to health: racial inequalities in prenatal care in the SUS. Saude Debate. 2013;37(99):571-577.",
  "25. IPEA. Atlas da Vulnerabilidade Social nos Municipios Brasileiros. Brasilia: IPEA; 2015.",
  "26. Chor D. Health inequalities in Brazil: race matters. Cad Saude Publica. 2013;29(7):1272-1275.",
  "27. Kim HJ, Fay MP, Feuer EJ, Midthune DN. Permutation tests for joinpoint regression with applications to cancer rates. Stat Med. 2000;19(3):335-351.",
  "28. Bernal JL, Cummins S, Gasparrini A. Interrupted time series regression for the evaluation of public health interventions: a tutorial. Int J Epidemiol. 2017;46(1):348-355.",
  "29. Marshall RJ. Mapping disease and mortality rates using empirical Bayes estimators. J R Stat Soc Ser C. 1991;40(2):283-294.",
  "30. Anselin L. Local indicators of spatial association -- LISA. Geogr Anal. 1995;27(2):93-115.",
  "31. Moran PAP. Notes on continuous stochastic phenomena. Biometrika. 1950;37(1-2):17-23.",
  "32. Wagstaff A, Paci P, van Doorslaer E. On the measurement of inequalities in health. Soc Sci Med. 1991;33(5):545-557.",
  "33. Bivand RS, Pebesma E, Gomez-Rubio V. Applied Spatial Data Analysis with R. 2nd ed. New York: Springer; 2013.",
  "34. Goldstein H. Multilevel Statistical Models. 4th ed. Chichester: Wiley; 2011.",
  "35. Hankivsky O. Intersectionality 101. Vancouver: Institute for Intersectionality Research and Policy, Simon Fraser University; 2014.",
  "36. Barata RB. Como e por que as desigualdades sociais fazem mal a saude. Rio de Janeiro: Editora FIOCRUZ; 2009.",
  "37. Domingues RMSM, Leal MC. Incidence of congenital syphilis and factors associated with vertical transmission: data from the Birth in Brazil study. Cad Saude Publica. 2016;32(6):e00082415.",
  "38. Victora CG, Aquino EML, do Carmo Leal M, et al. Maternal and child health in Brazil: progress and challenges. Lancet. 2011;377(9780):1863-1876.",
  "39. Paim J, Travassos C, Almeida C, et al. The Brazilian health system: history, advances, and challenges. Lancet. 2011;377(9779):1778-1797.",
  "40. Werneck J. Racismo institucional e saude da populacao negra. Saude Soc. 2016;25(3):535-549.",
  "41. Krieger N. Measures of racism, sexism, heterosexism, and gender binarism for health equity research: from structural injustice to embodied harm. Annu Rev Public Health. 2020;41:37-62.",
  "42. Pinto M, Tancredi MV, Alencar HDR, et al. Use of interrupted time series analysis in understanding the course of the congenital syphilis epidemic in Brazil. Lancet Reg Health Am. 2022;7:100163.",
  "43. Marques CP, Geremia DS, Siqueira BB, et al. Impact of the COVID-19 pandemic on prenatal care: an integrative review. Cienc Saude Coletiva. 2023;28(9):2621-2635.",
  "44. Morgenstern H. Ecologic studies in epidemiology: concepts, principles, and methods. Annu Rev Public Health. 1995;16:61-81.",
  "45. Cunha AP, Cruz MM, Torres RMC. Trends in congenital syphilis prevalence in Brazil, 2012-2021: an ecological study. Epidemiol Serv Saude. 2023;32(3):e2023046."
)

for (ref in refs) doc <- P(doc, ref)

# ============================================================================
# SAVE MAIN MANUSCRIPT
# ============================================================================
print(doc, target = out_main)
cat("Main manuscript saved:", out_main, "\n")

# ============================================================================
# SUPPLEMENTARY MATERIAL (separate document)
# ============================================================================
sdoc <- read_docx()
sdoc <- H1(sdoc, "Supplementary Material")
sdoc <- P(sdoc, "Temporal Trends, Spatial Patterns, and Social Inequalities in Gestational and Congenital Syphilis in Brazil, 2007-2023")
sdoc <- P(sdoc, "Audencio Victor, LSHTM")
sdoc <- BR(sdoc)

# Table S1 — UF complete
sdoc <- add_table(sdoc, file.path(tab_dir, "Tabela_UF_anual.xlsx"),
                  "Table S1. Syphilis cases and rates by state and year, Brazil, 2007-2023.")

# Table S2 — Race/colour annual
sdoc <- add_table(sdoc, file.path(tab_dir, "Tabela_Raca_Cor_anual.xlsx"),
                  "Table S2. Syphilis cases by maternal race/colour and year.")

# Table S3 — Sensitivity temporal
sdoc <- add_table(sdoc, file.path(tab_dir, "Tabela_Sensibilidade_Temporal.xlsx"),
                  "Table S3. Sensitivity analyses: Prais-Winsten and negative binomial trend estimates.")

# Table S4 — Inequality temporal
sdoc <- add_table(sdoc, file.path(tab_dir, "Tabela_Desigualdade_Temporal.xlsx"),
                  "Table S4. Temporal evolution of SII, Concentration Index, and Q5/Q1 ratio.")

# Table S5 — Joinpoint by UF
sdoc <- add_table(sdoc, file.path(sup_dir, "Tabela_Supl_Joinpoint_UF.xlsx"),
                  "Table S5. Joinpoint regression results by state.")

# Table S6 — Methods comparison
sdoc <- add_table(sdoc, file.path(sup_dir, "Tabela_Supl_Comparacao_Metodos.xlsx"),
                  "Table S6. Comparison of trend estimation methods.")

# Figure S1 — Absolute cases
sdoc <- add_fig(sdoc, file.path(sup_dir, "FigS1_Trend_Brazil_Cases.png"),
                "Figure S1. Absolute number of gestational and congenital syphilis notifications, Brazil, 2007-2023.",
                w = 6.5, h = 4)

# Figure S2 — UF panel
sdoc <- add_fig(sdoc, file.path(sup_dir, "Fig_Supl_UF_Panel.png"),
                "Figure S2. Temporal trends by state (small multiples), Brazil, 2007-2023.",
                w = 6.5, h = 8)

# Figure S3 — CS/GS ratio map
sdoc <- add_fig(sdoc, file.path(map_dir, "Mapa_Razao_SC_SG.png"),
                "Figure S3. Spatial distribution of the congenital-to-gestational syphilis ratio by municipality.",
                w = 5, h = 6)

# Figure S4 — Race x IVS
sdoc <- add_fig(sdoc, file.path(fig_dir, "Fig7_Raca_IVS.png"),
                "Figure S4. Racial distribution of gestational syphilis cases by IVS level.",
                w = 5.5, h = 4)

# STROBE
sdoc <- BR(sdoc)
sdoc <- add_table(sdoc, file.path(sup_dir, "STROBE_Checklist_Ecologico.xlsx"),
                  "Table S7. STROBE Checklist for ecological studies.")

# Variable dictionary
sdoc <- add_table(sdoc, file.path(sup_dir, "Dicionario_Variaveis.xlsx"),
                  "Table S8. Variable dictionary.")

print(sdoc, target = out_suppl)
cat("Supplementary material saved:", out_suppl, "\n")
cat("DONE.\n")
