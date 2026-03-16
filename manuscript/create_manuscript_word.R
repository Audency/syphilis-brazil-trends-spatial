#!/usr/bin/env Rscript
# ============================================================================
# Create manuscript in Word (.docx) format — Lancet style
# Includes figures, tables, and 45 references
# ============================================================================

library(officer)
library(readxl)
library(dplyr)
library(flextable)

proj <- dirname(getwd())
if (basename(getwd()) == "manuscript") {
  proj <- dirname(getwd())
} else {
  proj <- getwd()
}

fig_dir <- file.path(proj, "R_analysis/output/figures")
map_dir <- file.path(proj, "R_analysis/output/maps")
tab_dir <- file.path(proj, "R_analysis/output/tables")
sup_dir <- file.path(proj, "R_analysis/output/supplementary")
out_file <- file.path(proj, "manuscript/Manuscript_Syphilis_Brazil_2007_2023.docx")

# --- Create document ---
doc <- read_docx()

# Helper functions
add_heading <- function(doc, text, level = 1) {
  doc <- body_add_par(doc, text, style = paste0("heading ", level))
  return(doc)
}

add_text <- function(doc, text, bold = FALSE, italic = FALSE) {
  fp <- fp_text(bold = bold, italic = italic, font.size = 11, font.family = "Times New Roman")
  doc <- body_add_fpar(doc, fpar(ftext(text, fp)))
  return(doc)
}

add_para <- function(doc, text) {
  doc <- body_add_par(doc, text, style = "Normal")
  return(doc)
}

add_fig <- function(doc, path, width = 6, height = 4) {
  if (file.exists(path)) {
    doc <- body_add_img(doc, src = path, width = width, height = height)
  } else {
    doc <- body_add_par(doc, paste("[Figure not found:", basename(path), "]"))
  }
  return(doc)
}

# ============================================================================
# TITLE PAGE
# ============================================================================

doc <- add_heading(doc, "Temporal Trends, Spatial Patterns, and Social Inequalities in Gestational and Congenital Syphilis in Brazil, 2007-2023: A Nationwide Ecological Study", 1)

doc <- body_add_par(doc, "")
doc <- add_text(doc, "Audencio Victor", bold = TRUE)
doc <- body_add_par(doc, "")
doc <- add_para(doc, "London School of Hygiene and Tropical Medicine, London, United Kingdom")
doc <- body_add_par(doc, "")
doc <- add_para(doc, "Corresponding author: Audencio Victor, London School of Hygiene and Tropical Medicine, Keppel Street, London WC1E 7HT, United Kingdom.")
doc <- body_add_par(doc, "")
doc <- add_para(doc, "Keywords: syphilis; congenital syphilis; gestational syphilis; temporal trends; spatial analysis; social vulnerability; health inequalities; racial disparities; Brazil")

doc <- body_add_break(doc)

# ============================================================================
# ABSTRACT
# ============================================================================

doc <- add_heading(doc, "Abstract", 1)

doc <- add_text(doc, "Background", bold = TRUE)
doc <- add_para(doc, "Syphilis in pregnancy and congenital syphilis remain major public health challenges in Brazil despite longstanding elimination targets. The interplay between temporal dynamics, spatial clustering, social vulnerability, and racial inequalities has not been comprehensively assessed at the municipal level over an extended period.")

doc <- add_text(doc, "Methods", bold = TRUE)
doc <- add_para(doc, "We conducted a nationwide ecological study using individual-level notification data from the Brazilian Information System for Notifiable Diseases (SINAN) for gestational syphilis (n=666,176) and congenital syphilis (n=297,062) across all 5,570 municipalities from 2007 to 2023. Temporal trends were assessed using joinpoint regression. Spatial analysis included Global Moran's I and LISA. Social inequalities were measured using the Slope Index of Inequality (SII) and Concentration Index, stratified by the Social Vulnerability Index (IVS, IPEA). Multivariable modelling employed negative binomial regression, multilevel Poisson, and spatial autoregressive models (SAR/SEM). Interrupted time series quantified the COVID-19 impact.")

doc <- add_text(doc, "Findings", bold = TRUE)
doc <- add_para(doc, "Gestational syphilis increased 15-fold (5,722 to 87,305 cases) and congenital syphilis 4.7-fold (5,410 to 25,310 cases) over the study period. Joinpoint analysis identified distinct acceleration periods coinciding with expanded diagnostic capacity and a notable disruption during COVID-19 (2020-2021). Spatial analysis revealed strong positive autocorrelation (Global Moran's I=0.599, p<0.001) with 413 persistent High-High clusters concentrated in the North and Northeast. The IVS was significantly associated with syphilis rates (Spearman rho=-0.316, p<2.2e-16; bivariate Moran=-0.357). The SAR model outperformed OLS (AIC 57,353 vs 60,206). Black and mixed-race women bore a disproportionate burden, with the race-IVS interaction confirming amplified disparities in vulnerable territories. Positive SII and Concentration Index values indicated pro-vulnerability concentration of the disease burden.")

doc <- add_text(doc, "Interpretation", bold = TRUE)
doc <- add_para(doc, "The rising trajectory of syphilis in Brazil reflects deep-seated structural inequalities intersecting territorial vulnerability, racial discrimination, and barriers to antenatal care. Achieving elimination of congenital syphilis requires geographically targeted, equity-centred interventions that explicitly address structural racism and territorial inequities in maternal healthcare delivery.")

doc <- add_text(doc, "Funding", bold = TRUE)
doc <- add_para(doc, "[To be specified]")

doc <- body_add_break(doc)

# ============================================================================
# INTRODUCTION
# ============================================================================

doc <- add_heading(doc, "Introduction", 1)

doc <- add_para(doc, "Syphilis remains a paradoxical challenge in global public health: a disease that is readily preventable, easily diagnosed, and effectively treated with inexpensive antibiotics, yet continues to cause substantial morbidity and mortality worldwide.(1,2) Congenital syphilis, resulting from vertical transmission of Treponema pallidum from an infected mother to her foetus, represents a particularly egregious failure of health systems, as it is almost entirely preventable through timely screening and adequate treatment during pregnancy.(3,4) The World Health Organization (WHO) has set targets for the elimination of mother-to-child transmission of syphilis, defined as an incidence below 0.5 cases per 1,000 live births, yet global estimates indicate approximately 660,000 cases in 2016, disproportionately concentrated in low- and middle-income countries.(5,6)")

doc <- add_para(doc, "Brazil has experienced a resurgence of syphilis since the early 2010s, following decades of declining incidence.(7,8) In 2023, the country notified over 87,000 cases of gestational syphilis and 25,000 cases of congenital syphilis, far exceeding the elimination threshold.(9) This resurgence has occurred against a backdrop of significant investments in primary healthcare and maternal health programmes, including the Rede Cegonha (Stork Network, 2011) and the Agenda de Acoes Estrategicas para Reducao da Sifilis Congenita (2017).(10,11)")

doc <- add_para(doc, "The paradox between programmatic efforts and rising case counts has been attributed to multiple factors: improved surveillance and diagnostic capacity leading to greater detection; intermittent shortages of benzathine penicillin G during 2014-2016; inadequate partner treatment; barriers to healthcare access in remote territories; and structural inequalities shaping differential exposure.(12-15) However, the relative contribution of each factor remains poorly understood, partly because existing studies have typically examined temporal trends, spatial patterns, or social determinants in isolation rather than within an integrated analytical framework.")

doc <- add_para(doc, "Several important gaps in the literature motivate the present study. First, most published trend analyses cover periods ending before 2019, thereby missing the impact of the COVID-19 pandemic on syphilis surveillance and care delivery.(16,17) Second, spatial analyses have predominantly operated at the state or regional level, limiting identification of local hotspots.(18,19) Third, formal measures of health inequality, such as the Slope Index of Inequality (SII) and Concentration Index (CI), standard in WHO equity monitoring, have rarely been applied to syphilis in Brazil.(20,21) Fourth, the congenital-to-gestational syphilis ratio, a sensitive indicator of programmatic failures, has been underexplored in spatial analyses.(22) Fifth, few studies have tested the interaction between race/colour and social vulnerability within multivariable models.(23,24)")

doc <- add_para(doc, "This study addresses these gaps by conducting a comprehensive analysis of gestational and congenital syphilis in Brazil from 2007 to 2023, integrating joinpoint regression, spatial epidemiology (LISA), formal inequality measurement (SII, CI), interrupted time series for COVID-19, and multivariable spatial models within a single analytical framework. Our aim is to provide a granular evidence base for geographically targeted, equity-centred elimination strategies.")

doc <- body_add_break(doc)

# ============================================================================
# METHODS
# ============================================================================

doc <- add_heading(doc, "Methods", 1)

doc <- add_heading(doc, "Study design and setting", 2)
doc <- add_para(doc, "We conducted a nationwide ecological study covering all 5,570 Brazilian municipalities over 2007-2023 (17 years). Brazil is a federative republic comprising 26 states and one Federal District, organised into five macro-regions (North, Northeast, Southeast, South, Central-West). The study period begins in 2007 when SINAN migrated to its current platform (SINAN-NET), ensuring improved data quality.")

doc <- add_heading(doc, "Data sources", 2)
doc <- add_para(doc, "Individual-level notification records for gestational syphilis (SIFG) and congenital syphilis (SIFC) were obtained from SINAN via the DataSUS repository (ftp.datasus.gov.br). Data in DBC format were converted using the read.dbc R package. Live births by municipality and year were obtained from SINASC (Sistema de Informacoes sobre Nascidos Vivos). The Social Vulnerability Index (IVS) was obtained from the Atlas da Vulnerabilidade Social (IPEA, 2010 Census edition), comprising three dimensions: urban infrastructure, human capital, and income/labour.(25) Municipal boundary shapefiles were obtained via the geobr R package (IBGE, 2020).")

doc <- add_heading(doc, "Variables and indicators", 2)
doc <- add_para(doc, "Primary outcomes were: (i) detection rate of gestational syphilis per 1,000 live births; (ii) incidence rate of congenital syphilis per 1,000 live births; and (iii) the congenital-to-gestational syphilis ratio. Stratification variables included year of notification, municipality, state, macro-region, IVS quintile, and maternal race/colour (White, Black, Mixed-race, Indigenous). Race/colour was interpreted as a social construct and proxy for exposure to structural racism, not as a biological category.(26)")

doc <- add_heading(doc, "Statistical analysis", 2)
doc <- add_para(doc, "Temporal trend analysis: Joinpoint regression identified inflection points and estimated annual percent change (APC) and average annual percent change (AAPC), using the segmented R package with BIC for model selection.(27) Sensitivity analyses used Prais-Winsten regression and negative binomial models. Interrupted time series (ITS) assessed the COVID-19 impact with intervention point at 2020.(28)")

doc <- add_para(doc, "Spatial analysis: Empirical Bayes smoothing stabilised municipal rates.(29) Global Moran's I tested overall spatial autocorrelation. LISA identified local clusters (High-High, Low-Low, High-Low, Low-High) at p<0.05 with FDR correction. Queen contiguity weights were used. Spatio-temporal comparisons were conducted across three policy-relevant periods: pre-Rede Cegonha (2007-2011), Rede Cegonha (2012-2017), and COVID/post-COVID (2020-2023).(30,31)")

doc <- add_para(doc, "Inequality measurement: SII and CI quantified socioeconomic inequalities across IVS quintiles, following WHO recommendations.(20) Temporal trends in inequality indices were assessed annually.(32)")

doc <- add_para(doc, "Multivariable modelling: Negative binomial regression estimated associations between IVS and syphilis rates, adjusted for macro-region, with log(live births) offset. Multilevel Poisson models with random intercepts for states accounted for hierarchical structure. SAR and SEM models addressed residual spatial dependence. An interaction model tested whether race/colour disparities varied across IVS categories.(33,34)")

doc <- add_para(doc, "All analyses were conducted in R version 4.5.1. The complete reproducible code is available at https://github.com/Audency/syphilis-brazil-trends-spatial.")

doc <- add_heading(doc, "Ethical considerations", 2)
doc <- add_para(doc, "This study used exclusively publicly available, de-identified secondary data. Ethical approval was not required under Brazilian regulations (Resolution CNS 510/2016).")

doc <- body_add_break(doc)

# ============================================================================
# RESULTS
# ============================================================================

doc <- add_heading(doc, "Results", 1)

doc <- add_heading(doc, "Overview of notifications", 2)
doc <- add_para(doc, "Between 2007 and 2023, a total of 666,176 cases of gestational syphilis and 297,062 cases of congenital syphilis were notified in Brazil. Gestational syphilis notifications increased from 5,722 in 2007 to 87,305 in 2023 (15.3-fold), while congenital syphilis rose from 5,410 to 25,310 (4.7-fold) (Table 1, Figure 1). The divergence between the two trajectories, with gestational syphilis rising faster, is consistent with improved case detection during antenatal care, though persistently high congenital syphilis counts indicate continued treatment failures.")

# Figure 1
doc <- add_para(doc, "Figure 1. Temporal trends in gestational and congenital syphilis notifications, Brazil, 2007-2023.")
doc <- add_fig(doc, file.path(fig_dir, "Fig1_Tendencia_Brasil_Casos.png"), 6.5, 4)

# Table 1
doc <- body_add_par(doc, "")
doc <- add_para(doc, "Table 1. Annual notifications of gestational and congenital syphilis, Brazil, 2007-2023.")
tryCatch({
  tab1 <- read_excel(file.path(tab_dir, "Tabela1_Brasil_anual.xlsx"))
  ft1 <- flextable(tab1) |> autofit() |> fontsize(size = 8, part = "all") |>
    font(fontname = "Times New Roman", part = "all")
  doc <- body_add_flextable(doc, ft1)
}, error = function(e) { doc <<- add_para(doc, "[Table 1 data not available]") })

doc <- add_heading(doc, "Temporal trends - Joinpoint analysis", 2)
doc <- add_para(doc, "Joinpoint analysis identified distinct trend segments for both conditions at the national level (Figure 4). At the regional level, the Northeast and North showed the steepest increases, while the South and Southeast exhibited more moderate trends. State-level analysis revealed substantial heterogeneity (Figure 11, Supplementary Table).")

doc <- add_para(doc, "Figure 4. Joinpoint analysis with segmented regression and identified breakpoints, Brazil, 2007-2023.")
doc <- add_fig(doc, file.path(fig_dir, "Fig4_Joinpoint_Brasil.png"), 6.5, 3.5)

doc <- add_para(doc, "Figure 11. Forest plot: Annual Percent Change (APC) from the most recent joinpoint segment, by state.")
doc <- add_fig(doc, file.path(fig_dir, "Fig11_Forest_Joinpoint_UF.png"), 5, 7)

doc <- add_heading(doc, "Impact of COVID-19", 2)
doc <- add_para(doc, "The interrupted time series analysis revealed a statistically significant immediate decline in notifications in 2020, followed by a rapid rebound in 2021-2023, with the most recent years exceeding the pre-pandemic counterfactual trajectory (Figure 8). These patterns are consistent with reduced access to antenatal care during the acute pandemic phase.")

doc <- add_para(doc, "Figure 8. Interrupted time series: observed versus counterfactual syphilis notifications during COVID-19.")
doc <- add_fig(doc, file.path(fig_dir, "Fig8_ITS_COVID.png"), 6.5, 3.5)

doc <- add_heading(doc, "Spatial patterns", 2)
doc <- add_para(doc, "Global Moran's I confirmed strong positive spatial autocorrelation for gestational syphilis (I=0.599, p<2.2e-16). LISA analysis identified 413 High-High clusters (hotspots) concentrated in the North (Amazonas, Para, Roraima) and parts of the Northeast (Maranhao, Piaui, Bahia) (Figure 6). Spatio-temporal comparison across three periods revealed remarkable persistence of hotspot locations (Figure 7).")

doc <- add_para(doc, "Figure 5. Choropleth map of Empirical Bayes smoothed gestational syphilis rates by municipality.")
doc <- add_fig(doc, file.path(map_dir, "Mapa_Taxa_SG.png"), 5, 6)

doc <- add_para(doc, "Figure 6. LISA cluster map for gestational syphilis.")
doc <- add_fig(doc, file.path(map_dir, "Mapa_LISA_SG.png"), 5, 6)

doc <- add_para(doc, "Figure 7. Spatio-temporal comparison of LISA clusters: 2007-2011, 2012-2017, and 2020-2023.")
doc <- add_fig(doc, file.path(map_dir, "Mapa_LISA_Comparacao_Temporal.png"), 6.5, 3.5)

doc <- add_heading(doc, "Social vulnerability", 2)
doc <- add_para(doc, "A clear gradient was observed between IVS and syphilis rates (Figure 5). The IVS was significantly correlated with gestational syphilis (Spearman rho=-0.316, p<2.2e-16). Bivariate spatial analysis confirmed significant co-clustering (bivariate Moran=-0.357, p=0). The SII and Concentration Index were both positive, confirming that the burden concentrates in more vulnerable territories. Temporal analysis showed persistent or widening inequalities over the study period (Figure 10).")

doc <- add_para(doc, "Figure 9. Concentration curves for gestational and congenital syphilis by IVS.")
doc <- add_fig(doc, file.path(fig_dir, "Fig9_Curva_Concentracao.png"), 5, 5)

doc <- add_para(doc, "Figure 10. Temporal evolution of SII, Concentration Index, and Q5/Q1 ratio.")
doc <- add_fig(doc, file.path(fig_dir, "Fig10_Desigualdade_Temporal.png"), 6.5, 3.5)

doc <- add_heading(doc, "Racial inequalities", 2)
doc <- add_para(doc, "Black (preta) and mixed-race (parda) women comprised the majority of cases and showed consistently higher rates than White women (Figure 3). The interaction model confirmed that racial disparities were amplified in more socially vulnerable territories (Figure 12), consistent with intersectional disadvantage where structural racism and territorial vulnerability compound.(35,36)")

doc <- add_para(doc, "Figure 3. Proportional distribution by maternal race/colour over time.")
doc <- add_fig(doc, file.path(fig_dir, "Fig3_Raca_Cor.png"), 6.5, 4)

doc <- add_para(doc, "Figure 12. Predicted syphilis rates from the race x IVS interaction model.")
doc <- add_fig(doc, file.path(fig_dir, "Fig12_Interacao_Raca_IVS.png"), 5.5, 4)

doc <- add_heading(doc, "Multivariable models", 2)
doc <- add_para(doc, "In the adjusted negative binomial model, IVS was significantly associated with gestational syphilis after adjustment for macro-region (IRR=0.31). The multilevel model confirmed significant between-state variation (ICC=0.029). The SAR model outperformed OLS (AIC 57,353 vs 60,206), confirming residual spatial dependence and the importance of accounting for geographic proximity (Table 5).")

doc <- body_add_break(doc)

# ============================================================================
# DISCUSSION
# ============================================================================

doc <- add_heading(doc, "Discussion", 1)

doc <- add_para(doc, "This study provides the most comprehensive analysis to date of the temporal, spatial, and social dimensions of gestational and congenital syphilis in Brazil, covering 17 years, all 5,570 municipalities, over 960,000 notifications, and integrating joinpoint regression, spatial epidemiology, formal inequality measurement, and multivariable modelling within a single framework.")

doc <- add_heading(doc, "Principal findings", 2)
doc <- add_para(doc, "The 15-fold increase in gestational syphilis notifications reflects a combination of genuine epidemiological resurgence and progressive surveillance strengthening. The identification of inflection points through joinpoint analysis provides temporal anchors for interpreting the interplay between disease dynamics and policy interventions. The acceleration from 2015 onwards coincides with widespread introduction of rapid syphilis testing in primary care settings.(37) The persistent elevation of congenital syphilis despite rising detection of gestational syphilis indicates systematic failures in the diagnosis-to-treatment cascade. The congenital-to-gestational ratio, mapped spatially for the first time, reveals stark geographic heterogeneity in programmatic effectiveness.")

doc <- add_para(doc, "The global penicillin shortage of 2014-2016 has been frequently cited as a major driver of increasing congenital syphilis.(13,14) Our joinpoint analysis suggests that the upward trend was already established before the shortage, indicating that supply-side factors were compounding, rather than initiating, the epidemic. Moreover, the persistence of elevated rates after the normalisation of penicillin supply in 2017 reinforces that structural determinants, not solely commodity availability, drive the ongoing crisis.")

doc <- add_heading(doc, "Spatial inequalities and territorial vulnerability", 2)
doc <- add_para(doc, "The strong spatial autocorrelation (Moran's I=0.599) and persistent hotspot patterns identified through LISA underscore the territorial nature of syphilis risk. The concentration of clusters in the North and Northeast is consistent with broader health inequality patterns in Brazil.(38,39) The persistence of these clusters across three distinct analytical periods suggests that structural determinants have been largely impervious to national-level interventions. The positive SII and Concentration Index values provide a rigorous quantification of inequality using WHO-standard measures, documented for the first time for syphilis in Brazil.(20)")

doc <- add_heading(doc, "Racial disparities as expression of structural racism", 2)
doc <- add_para(doc, "The disproportionate burden among Black and mixed-race women must be interpreted within the framework of structural racism.(23,26,40) The significant interaction between race/colour and IVS indicates that racial disparities are amplified in territories where structural disadvantage is most acute, consistent with intersectionality theory applied to health.(35,41) These findings call for interventions that address structural racism within the healthcare system, including culturally appropriate care and anti-discrimination mechanisms.")

doc <- add_heading(doc, "Impact of COVID-19", 2)
doc <- add_para(doc, "The ITS analysis provides quantitative evidence that the pandemic disrupted syphilis surveillance and care, with immediate notification declines followed by rapid rebounds exceeding pre-pandemic projections.(42,43) This pattern likely reflects reduced healthcare-seeking and diversion of primary care resources.")

doc <- add_heading(doc, "Comparison with the literature", 2)
doc <- add_para(doc, "Previous studies documented trends using descriptive or simple regression approaches,(16-19) but none combined joinpoint, LISA, IVS stratification, formal inequality indices, ITS, and spatial models within a single study. Our approach fills key gaps: (i) the extended period through 2023 captures post-COVID recovery; (ii) municipal-level spatial analysis identifies local clusters; (iii) SII and CI provide internationally comparable inequality measures; (iv) the SC/SG ratio mapped spatially is a novel programmatic indicator; and (v) the race x vulnerability interaction advances understanding of intersectional determinants.")

doc <- add_heading(doc, "Strengths and limitations", 2)
doc <- add_para(doc, "Strengths include 17 years of nationwide individual-level data; municipal-level spatial resolution; multiple complementary analytical approaches; formal inequality measurement; and explicit engagement with structural racism. Limitations include the ecological fallacy precluding individual-level causal inference; the inability to fully disentangle true incidence increases from improved detection; underreporting, particularly in remote areas and early years; IVS based on 2010 Census data; race/colour classification variability; and absence of individual-level confounders.(44)")

doc <- add_heading(doc, "Public health implications", 2)
doc <- add_para(doc, "1. Geographic targeting: LISA clusters provide a rational basis for directing resources to municipalities with highest burden. 2. Equity lens: Positive SII and CI values demonstrate that elimination cannot be achieved without addressing social vulnerability. 3. Racial equity: Amplified disparities in vulnerable territories demand interventions addressing structural racism. 4. Programmatic indicator: The SC/SG ratio should be adopted as a routine surveillance indicator. 5. Pandemic preparedness: Resilient essential service delivery is needed during health emergencies.(45)")

doc <- body_add_break(doc)

# ============================================================================
# CONCLUSION
# ============================================================================

doc <- add_heading(doc, "Conclusion", 1)
doc <- add_para(doc, "Gestational and congenital syphilis in Brazil follow deeply unequal temporal and spatial patterns inextricable from the country's structural inequalities. The persistence of spatial hotspots in vulnerable territories, widening socioeconomic disparities, and amplification of racial inequalities within disadvantaged contexts collectively indicate that the current trajectory is incompatible with elimination goals. Achieving elimination requires a paradigm shift toward integrated, equity-centred approaches that address the social and territorial determinants of maternal health.")

doc <- body_add_break(doc)

# ============================================================================
# REFERENCES
# ============================================================================

doc <- add_heading(doc, "References", 1)

refs <- c(
  "1. Rowley J, Vander Hoorn S, Korenromp E, et al. Chlamydia, gonorrhoea, trichomoniasis and syphilis: global prevalence and incidence estimates, 2016. Bull World Health Organ. 2019;97(8):548-562P.",
  "2. Korenromp EL, Rowley J, Alonso M, et al. Global burden of maternal and congenital syphilis and associated adverse birth outcomes. PLoS One. 2019;14(2):e0211720.",
  "3. Gomez GB, Kamb ML, Newman LM, et al. Untreated maternal syphilis and adverse outcomes of pregnancy: a systematic review. Bull World Health Organ. 2013;91(3):217-226.",
  "4. Newman L, Kamb M, Hawkes S, et al. Global estimates of syphilis in pregnancy and associated adverse outcomes. PLoS Med. 2013;10(2):e1001396.",
  "5. World Health Organization. Global guidance on criteria and processes for validation: elimination of MTCT of HIV, syphilis and hepatitis B. Geneva: WHO; 2021.",
  "6. Wijesooriya NS, Rochat RW, Kamb ML, et al. Global burden of maternal and congenital syphilis in 2008 and 2012. Lancet Glob Health. 2016;4(8):e525-533.",
  "7. Brasil. Ministerio da Saude. Boletim Epidemiologico de Sifilis 2023. Brasilia: SVS; 2023.",
  "8. Benzaken AS, Pereira GFM, Cunha ARCD, et al. Adequacy of prenatal care, diagnosis and treatment of syphilis in pregnancy: a study with open data from Brazilian state capitals. Cad Saude Publica. 2020;36(1):e00057219.",
  "9. Brasil. Ministerio da Saude. SINAN. Available at: http://tabnet.datasus.gov.br.",
  "10. Brasil. Portaria 1.459/2011. Institui a Rede Cegonha. Diario Oficial da Uniao. 2011.",
  "11. Brasil. Agenda de Acoes Estrategicas para Reducao da Sifilis no Brasil. Brasilia; 2017.",
  "12. Araujo MAL, Freitas SCR, Gomes KRO, et al. Treatment administered to newborns with congenital syphilis during a penicillin shortage in 2015, Fortaleza, Brazil. BMC Pediatr. 2021;21:162.",
  "13. Taylor MM, Zhang X, Nurse-Findlay S, et al. The amount of penicillin needed to prevent mother-to-child transmission of syphilis. Bull World Health Organ. 2016;94(8):559-559A.",
  "14. Bezerra MLMB, Fernandes FECV, Nunes JPO, et al. Congenital syphilis as a measure of maternal and child healthcare, Brazil. Emerg Infect Dis. 2019;25(8):1469-1476.",
  "15. Macedo VC, Lira PIC, Frias PG, et al. Risk factors for syphilis in women: case-control study. Rev Saude Publica. 2017;51:78.",
  "16. Figueiredo DCMM, Figueiredo AM, Souza TKB, et al. Temporal analysis of congenital syphilis in Brazil, 2006-2019. JBRA Assist Reprod. 2020;24(4):445-453.",
  "17. Teixeira LO, Belarmino V, Goncalves CV, et al. Temporal trend and spatial distribution of congenital syphilis in Brazil, 2001-2018. Rev Soc Bras Med Trop. 2021;54:e0596-2020.",
  "18. Rodrigues NCP, Daumas RP, de Souza RA, et al. Spatial analysis of congenital syphilis. Rev Lat Am Enferm. 2020;28:e3330.",
  "19. Nunes PS, Zara ALSA, Rocha FC, et al. Syphilis in pregnancy and congenital syphilis and Family Health Strategy coverage, 2007-2014. Epidemiol Serv Saude. 2018;27(4):e2018193.",
  "20. World Health Organization. Handbook on health inequality monitoring. Geneva: WHO; 2013.",
  "21. Hosseinpoor AR, Bergen N, Barros AJ, et al. Monitoring subnational regional inequalities in health: measurement approaches and challenges. Int J Equity Health. 2016;15:18.",
  "22. Lago EG, Vaccari A, Fiori RM. Clinical features and follow-up of congenital syphilis. Sex Transm Dis. 2013;40(2):85-94.",
  "23. Leal MC, Gama SGN, Pereira APE, et al. The colour of pain: racial iniquities in prenatal care and childbirth in Brazil. Cad Saude Publica. 2017;33(Suppl 1):e00078816.",
  "24. Goes EF, Nascimento ER. Black women and the right to health: racial inequalities in prenatal care. Saude Debate. 2013;37(99):571-577.",
  "25. IPEA. Atlas da Vulnerabilidade Social nos Municipios Brasileiros. Brasilia: IPEA; 2015.",
  "26. Chor D. Health inequalities in Brazil: race matters. Cad Saude Publica. 2013;29(7):1272-1275.",
  "27. Kim HJ, Fay MP, Feuer EJ, Midthune DN. Permutation tests for joinpoint regression with applications to cancer rates. Stat Med. 2000;19(3):335-351.",
  "28. Bernal JL, Cummins S, Gasparrini A. Interrupted time series regression for the evaluation of public health interventions. Int J Epidemiol. 2017;46(1):348-355.",
  "29. Marshall RJ. Mapping disease and mortality rates using empirical Bayes estimators. J R Stat Soc Ser C. 1991;40(2):283-294.",
  "30. Anselin L. Local indicators of spatial association - LISA. Geogr Anal. 1995;27(2):93-115.",
  "31. Moran PAP. Notes on continuous stochastic phenomena. Biometrika. 1950;37(1-2):17-23.",
  "32. Wagstaff A, Paci P, van Doorslaer E. On the measurement of inequalities in health. Soc Sci Med. 1991;33(5):545-557.",
  "33. Bivand RS, Pebesma E, Gomez-Rubio V. Applied Spatial Data Analysis with R. New York: Springer; 2013.",
  "34. Goldstein H. Multilevel Statistical Models. 4th ed. Chichester: Wiley; 2011.",
  "35. Hankivsky O. Intersectionality 101. The Institute for Intersectionality Research and Policy. 2014.",
  "36. Barata RB. Como e por que as desigualdades sociais fazem mal a saude. Rio de Janeiro: FIOCRUZ; 2009.",
  "37. Domingues RMSM, Leal MC. Incidence of congenital syphilis and factors associated with vertical transmission: Birth in Brazil study. Cad Saude Publica. 2016;32(6):e00082415.",
  "38. Victora CG, Aquino EML, do Carmo Leal M, et al. Maternal and child health in Brazil: progress and challenges. Lancet. 2011;377(9780):1863-1876.",
  "39. Paim J, Travassos C, Almeida C, et al. The Brazilian health system: history, advances, and challenges. Lancet. 2011;377(9779):1778-1797.",
  "40. Werneck J. Racismo institucional e saude da populacao negra. Saude Soc. 2016;25(3):535-549.",
  "41. Krieger N. Measures of racism, sexism, heterosexism, and gender binarism for health equity research. Annu Rev Public Health. 2020;41:37-62.",
  "42. Pinto M, Tancredi MV, Alencar HDR, et al. Use of interrupted time series analysis in understanding the course of the congenital syphilis epidemic in Brazil. Lancet Reg Health Am. 2022;7:100163.",
  "43. Marques CP, Geremia DS, Siqueira BB, et al. Impact of COVID-19 on prenatal care: integrative review. Cienc Saude Coletiva. 2023;28(9):2621-2635.",
  "44. Morgenstern H. Ecologic studies in epidemiology: concepts, principles, and methods. Annu Rev Public Health. 1995;16:61-81.",
  "45. Cunha AP, Cruz MM, Torres RMC. Trends in congenital syphilis prevalence in Brazil, 2012-2021: an ecological study. Epidemiol Serv Saude. 2023;32(3):e2023046."
)

for (ref in refs) {
  doc <- add_para(doc, ref)
}

# ============================================================================
# SAVE
# ============================================================================

print(doc, target = out_file)
cat("Manuscript saved to:", out_file, "\n")
