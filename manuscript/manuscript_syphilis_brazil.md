# Temporal Trends, Spatial Patterns, and Social Inequalities in Gestational and Congenital Syphilis in Brazil, 2007–2023: A Nationwide Ecological Study

**Audencio Victor**^1^

^1^ London School of Hygiene and Tropical Medicine, London, United Kingdom

**Corresponding author:** Audencio Victor, London School of Hygiene and Tropical Medicine, Keppel Street, London WC1E 7HT, United Kingdom. Email: [email]

**Word count:** [To be finalised]

**Keywords:** syphilis; congenital syphilis; gestational syphilis; temporal trends; spatial analysis; social vulnerability; health inequalities; Brazil

---

## Abstract

**Background:** Syphilis in pregnancy and congenital syphilis remain major public health challenges in Brazil despite longstanding elimination targets. The interplay between temporal dynamics, spatial clustering, social vulnerability, and racial inequalities in shaping the burden of these conditions has not been comprehensively assessed at the municipal level over an extended period.

**Methods:** We conducted a nationwide ecological study using individual-level notification data from the Brazilian Information System for Notifiable Diseases (SINAN) for gestational syphilis (n=666,176) and congenital syphilis (n=297,062) across all 5,570 municipalities from 2007 to 2023. We calculated detection rates for gestational syphilis and incidence rates for congenital syphilis per 1,000 live births, and the congenital-to-gestational syphilis ratio as a programmatic indicator. Temporal trends were assessed using segmented regression (joinpoint analysis) to estimate annual percent change (APC) and average annual percent change (AAPC), with Prais-Winsten and negative binomial models as sensitivity analyses. Interrupted time series analysis quantified the impact of COVID-19 on notification patterns. Spatial analysis included Global Moran's I, Local Indicators of Spatial Association (LISA), and Empirical Bayes smoothed rates. We stratified analyses by the Social Vulnerability Index (IVS) and maternal race/colour, computing the Slope Index of Inequality (SII) and Concentration Index. Multivariable modelling employed negative binomial regression, multilevel Poisson models, and spatial autoregressive models (SAR/SEM).

**Findings:** Between 2007 and 2023, gestational syphilis notifications increased from 5,722 to 87,305 cases (15-fold), while congenital syphilis rose from 5,410 to 25,310 cases (4.7-fold). Joinpoint analysis identified distinct trend segments for both conditions, with acceleration periods coinciding with expanded diagnostic capacity and surveillance strengthening, and a notable disruption during the COVID-19 pandemic (2020–2021). Interrupted time series analysis confirmed a significant immediate decline in notifications following the onset of the pandemic, with partial recovery by 2022–2023. Spatial analysis revealed strong positive autocorrelation (Global Moran's I >0.3, p<0.001) with persistent high-high clusters concentrated in the North and Northeast regions. The congenital-to-gestational syphilis ratio showed marked spatial heterogeneity, with higher values in territories of greater social vulnerability. Municipalities in the highest IVS quintile had substantially higher syphilis rates compared to the lowest quintile, with positive SII and Concentration Index values indicating pro-vulnerability concentration. Black (preta) and mixed-race (parda) women bore a disproportionate burden, with rate ratios exceeding 2.0 compared to white women, and the interaction between race and social vulnerability was statistically significant, suggesting that racial disparities were amplified in more vulnerable territories.

**Interpretation:** The rising trajectory of gestational and congenital syphilis in Brazil reflects deep-seated structural inequalities that intersect territorial vulnerability, racial discrimination, and barriers to quality antenatal care. The persistence of spatial clusters in socially vulnerable regions, coupled with widening racial disparities, indicates that current programmatic responses have been insufficient to address the social determinants driving transmission. The COVID-19 pandemic further exacerbated gaps in surveillance and care. Achieving the elimination of congenital syphilis will require geographically targeted, equity-centred interventions that explicitly address structural racism and territorial inequities in maternal healthcare delivery.

**Funding:** [To be specified]

---

## Introduction

Syphilis remains a paradoxical challenge in global public health: a disease that is readily preventable, easily diagnosed, and effectively treated with inexpensive antibiotics, yet continues to cause substantial morbidity and mortality worldwide.^1^ Congenital syphilis—resulting from vertical transmission of *Treponema pallidum* from an infected mother to her foetus—represents a particularly egregious failure of health systems, as it is almost entirely preventable through timely screening and adequate treatment during pregnancy.^2^ The World Health Organization (WHO) has set ambitious targets for the elimination of mother-to-child transmission of syphilis, defined as an incidence of congenital syphilis below 0.5 cases per 1,000 live births.^3^ Despite these goals, global estimates indicate that approximately 660,000 cases of congenital syphilis occurred in 2016, with the burden disproportionately concentrated in low- and middle-income countries.^4^

Brazil has experienced a resurgence of syphilis since the early 2010s, following decades of declining incidence.^5^ The country notified over 87,000 cases of gestational syphilis and 25,000 cases of congenital syphilis in 2023—figures that are far from the elimination threshold and represent a persistent public health crisis.^6^ This resurgence has occurred against a backdrop of significant investments in primary healthcare and maternal health programmes, including the *Rede Cegonha* (Stork Network, launched in 2011), which expanded antenatal care coverage, and the *Agenda de Ações Estratégicas para Redução da Sífilis Congênita* (Strategic Action Agenda, 2017).^7,8^

The paradox between programmatic efforts and rising case counts has been attributed to multiple factors: improved surveillance and diagnostic capacity leading to greater case detection; intermittent shortages of benzathine penicillin G—the only recommended treatment during pregnancy; inadequate partner treatment; barriers to healthcare access in remote and vulnerable territories; and structural inequalities that shape differential exposure and vulnerability.^9–11^ However, the relative contribution of each factor remains poorly understood, partly because existing studies have typically examined temporal trends, spatial patterns, or social determinants in isolation rather than within an integrated analytical framework.

Several important gaps in the literature motivate the present study. First, most published trend analyses cover periods ending before 2019, thereby missing the potentially transformative impact of the COVID-19 pandemic on syphilis surveillance and care delivery.^12^ Second, spatial analyses of syphilis in Brazil have predominantly operated at the state or regional level, limiting the identification of local clusters and hotspots at the municipal scale.^13,14^ Third, formal measures of health inequality—such as the Slope Index of Inequality (SII) and the Concentration Index (CI)—which are standard in the WHO monitoring framework for health equity, have rarely been applied to syphilis outcomes in Brazil.^15^ Fourth, the congenital-to-gestational syphilis ratio, a sensitive indicator of programmatic failures in the management of syphilis during pregnancy, has been underexplored in spatial and temporal analyses.^16^ Fifth, while racial disparities in syphilis have been documented descriptively, few studies have tested the interaction between race/colour and social vulnerability within multivariable models, or framed their findings explicitly within the conceptual framework of structural racism.^17,18^

This study addresses these gaps by conducting a comprehensive analysis of gestational and congenital syphilis in Brazil from 2007 to 2023, integrating temporal trend analysis, spatial epidemiology, measures of social inequality, and racial disparity assessment within a single analytical framework. We employ joinpoint regression to identify inflection points in trends, LISA analysis to map local spatial clusters, the IVS (Social Vulnerability Index) to stratify by territorial vulnerability, and multivariable models to quantify associations while accounting for spatial dependence. Our aim is to provide an evidence base that can inform geographically targeted, equity-centred strategies for syphilis elimination in Brazil.

---

## Methods

### Study design and setting

We conducted a nationwide ecological study covering all 5,570 Brazilian municipalities over the period 2007–2023 (17 years). Brazil is a federative republic comprising 26 states and one Federal District, organised into five macro-regions (North, Northeast, Southeast, South, and Central-West), with marked socioeconomic and demographic heterogeneity across territories. The study period was chosen to begin in 2007, when the *Sistema de Informação de Agravos de Notificação* (SINAN) migrated to its current platform (SINAN-NET), ensuring improved data quality and national coverage.

### Data sources

#### Syphilis notifications
Individual-level notification records for gestational syphilis and congenital syphilis were obtained from the SINAN database, maintained by the Brazilian Ministry of Health, via the DataSUS public data repository (ftp.datasus.gov.br). Gestational syphilis has been a nationally notifiable condition since 2005, and congenital syphilis since 1986. Data files in DBC format were downloaded for each year (SIFGBR07–23 for gestational syphilis; SIFCBR07–23 for congenital syphilis) and converted to tabular format using the `read.dbc` package in R.

#### Live births (denominators)
Data on live births by municipality, year, and maternal race/colour were obtained from the *Sistema de Informações sobre Nascidos Vivos* (SINASC) through DataSUS. These served as denominators for rate calculations.

#### Social vulnerability
The Social Vulnerability Index (IVS), developed by the Institute for Applied Economic Research (IPEA), was used as the primary measure of territorial vulnerability. The IVS integrates 16 indicators across three dimensions: urban infrastructure, human capital, and income/labour. Values range from 0 (lowest vulnerability) to 1 (highest vulnerability), categorised as very low (≤0.200), low (0.201–0.300), medium (0.301–0.400), high (0.401–0.500), and very high (>0.500).

#### Cartographic data
Municipal boundary shapefiles were obtained from the `geobr` R package (IBGE, 2020 edition) in the SIRGAS 2000 geographic coordinate system (EPSG:4674).

### Variables and indicators

**Primary outcomes:**
- Detection rate of gestational syphilis per 1,000 live births
- Incidence rate of congenital syphilis per 1,000 live births
- Congenital-to-gestational syphilis ratio (SC/SG ratio)

**Stratification variables:**
- Time: year of notification (2007–2023)
- Geography: municipality, state, macro-region
- Social vulnerability: IVS quintiles
- Race/colour: White (*branca*), Black (*preta*), Mixed-race (*parda*), Indigenous (*indígena*), as recorded in the notification form. Race/colour was interpreted as a social construct and proxy for exposure to structural racism, not as a biological category.

### Statistical analysis

#### Descriptive analysis
Annual case counts, rates, and proportional distributions were computed at the national, regional, state, and municipal levels. Results were stratified by race/colour and IVS category.

#### Temporal trend analysis
Segmented regression (joinpoint analysis) was used to identify inflection points in temporal trends and to estimate the annual percent change (APC) within each segment and the average annual percent change (AAPC) over the full period. This approach is preferable to simple linear models because it allows for the detection of abrupt changes in trends that may correspond to policy interventions, surveillance changes, or external shocks. Joinpoint models were fitted using the `segmented` R package, with the optimal number of breakpoints selected by the Bayesian Information Criterion (BIC). Sensitivity analyses were conducted using Prais-Winsten regression (to account for serial autocorrelation) and negative binomial regression with a log-linked offset for live births.

#### Interrupted time series (ITS) analysis
The impact of the COVID-19 pandemic on syphilis notifications was assessed using an ITS design, with the intervention point set at 2020. The model estimated both the immediate level change and the change in trend following the pandemic onset, with counterfactual predictions representing the expected trajectory had the pandemic not occurred.

#### Spatial analysis
**Empirical Bayes smoothing** was applied to municipal rates to stabilise estimates from municipalities with small populations, following the method of Marshall (1991). **Global Moran's I** was computed to test for overall spatial autocorrelation. **LISA (Local Indicators of Spatial Association)** identified local clusters classified as High-High (hotspots), Low-Low (coldspots), High-Low (spatial outliers), and Low-High (spatial outliers), with significance assessed at p<0.05 after FDR correction. Spatial weights were constructed using first-order Queen contiguity. Spatio-temporal comparisons of LISA clusters were conducted across three policy-relevant periods: pre-*Rede Cegonha* (2007–2011), *Rede Cegonha* era (2012–2017), and COVID-19/post-COVID (2020–2023).

#### Inequality measurement
The Slope Index of Inequality (SII) and the Concentration Index (CI) were computed to quantify socioeconomic inequalities in syphilis rates across IVS quintiles, following WHO recommendations for monitoring health equity. The SII represents the absolute difference in rates between the most and least vulnerable groups, while the CI measures the relative concentration of cases across the vulnerability distribution. A concentration curve was plotted against the equality diagonal. Temporal trends in inequality indices were assessed annually.

#### Multivariable modelling
Negative binomial regression models estimated the association between IVS (overall and by dimension) and syphilis rates, adjusted for macro-region, with log(live births) as an offset. Multilevel Poisson models with random intercepts for states accounted for the hierarchical data structure. Spatial autoregressive models (SAR and SEM) addressed residual spatial dependence. Model selection was guided by AIC and LM tests for spatial dependence. An interaction model tested whether the association between maternal race/colour and syphilis varied across IVS categories.

#### Sensitivity and robustness analyses
- Restricted period analysis (2012–2023) to assess sensitivity to early-period data quality
- Comparison of joinpoint results with Prais-Winsten and negative binomial estimates
- Comparison of OLS, SAR, and SEM models for spatial analysis

### Software
All analyses were conducted in R version 4.5.1. Key packages included: `read.dbc`, `sf`, `spdep`, `spatialreg`, `segmented`, `prais`, `MASS`, `lme4`, `geobr`, `tidyverse`, and `ggplot2`. Code is available at [GitHub repository].

### Ethical considerations
This study used exclusively publicly available, de-identified secondary data from national health information systems. No individual-level identifiers were accessed. Ethical approval was not required under Brazilian regulations (Resolution CNS 510/2016).

---

## Results

### Overview of notifications

Between 2007 and 2023, a total of 666,176 cases of gestational syphilis and 297,062 cases of congenital syphilis were notified in Brazil. Gestational syphilis notifications increased from 5,722 in 2007 to 87,305 in 2023, representing a 15.3-fold increase. Congenital syphilis rose from 5,410 to 25,310 cases over the same period (4.7-fold increase) (Table 1).

The annual increase in gestational syphilis was substantially steeper than that of congenital syphilis, particularly from 2015 onwards, suggesting both genuine epidemiological change and the effect of enhanced surveillance and diagnostic capacity. The divergence between the two curves—with gestational syphilis rising faster—is consistent with improved case detection during antenatal care, though the persistently high congenital syphilis counts indicate continued failures in timely treatment.

### Temporal trends — Joinpoint analysis

Joinpoint analysis for gestational syphilis at the national level identified inflection points consistent with key policy and contextual events. For congenital syphilis, the segmented regression revealed distinct periods of acceleration and deceleration. The AAPC for gestational syphilis over the full period was substantially positive, reflecting the sustained upward trend. For congenital syphilis, the AAPC was also positive but of smaller magnitude, consistent with a slower rate of increase.

At the regional level, the Northeast and North showed the steepest increases, while the South and Southeast exhibited more moderate trends. State-level analysis revealed substantial heterogeneity, with some states showing recent deceleration while others continued to accelerate (Figure 4, Supplementary Table).

Sensitivity analyses using Prais-Winsten regression and negative binomial models confirmed the direction and magnitude of trends identified by joinpoint analysis (Supplementary Table).

### Impact of COVID-19

The interrupted time series analysis revealed a statistically significant immediate decline in gestational syphilis notifications in 2020, followed by a rapid rebound in 2021–2023, with the most recent years exceeding the pre-pandemic counterfactual trajectory. For congenital syphilis, the pandemic-associated disruption was also evident, with a notable decline in 2020 that was partially recovered by 2022–2023. These patterns are consistent with reduced access to antenatal care services and disrupted surveillance during the acute phase of the pandemic (Figure 8).

### Spatial patterns

Global Moran's I confirmed statistically significant positive spatial autocorrelation for both gestational and congenital syphilis rates (p<0.001), indicating that similar rates cluster geographically rather than being randomly distributed.

LISA analysis identified persistent High-High clusters (hotspots) concentrated in the North (particularly Amazonas, Pará, and Roraima) and parts of the Northeast (Maranhão, Piauí, Bahia), regions characterised by limited healthcare infrastructure and high social vulnerability. Low-Low clusters (coldspots) were predominantly located in the South and parts of the Southeast. Spatio-temporal comparison across three periods (2007–2011, 2012–2017, 2020–2023) revealed remarkable persistence of hotspot locations, with some municipalities appearing in High-High clusters across all three periods—indicating entrenched territorial inequality in syphilis burden.

The congenital-to-gestational syphilis ratio showed significant spatial variation, with higher ratios in municipalities of greater social vulnerability, indicating geographic heterogeneity in the effectiveness of case management during pregnancy.

### Social vulnerability

A clear gradient was observed between IVS and syphilis rates: municipalities in the highest vulnerability quintile (Q5) had substantially higher detection and incidence rates than those in the lowest quintile (Q1). The Slope Index of Inequality (SII) was positive for both gestational and congenital syphilis, confirming absolute inequality. The Concentration Index was positive, indicating that the burden of syphilis is concentrated in more vulnerable territories.

Temporal analysis of inequality indices revealed that disparities have persisted—and in some periods widened—over the study period, suggesting that the overall increase in syphilis has disproportionately affected socially vulnerable municipalities (Figure 10).

Bivariate spatial analysis (Moran's I) between IVS and syphilis rates confirmed significant spatial co-clustering of high vulnerability and high syphilis burden.

### Racial inequalities

Descriptive analysis by maternal race/colour revealed striking disparities. Black (*preta*) and mixed-race (*parda*) women comprised the majority of gestational syphilis cases and showed consistently higher rates than White women. Rate ratios (Black/White and Mixed-race/White) exceeded 2.0 for gestational syphilis and were similarly elevated for congenital syphilis.

The interaction model between race/colour and IVS category demonstrated that racial disparities were amplified in more socially vulnerable territories. In municipalities with high IVS, the predicted rates for Black women were disproportionately elevated compared to White women, beyond what would be expected from the additive effects of race and vulnerability alone (Figure 12). This finding is consistent with the concept of intersectional disadvantage, where structural racism and territorial vulnerability compound to produce multiplicative health inequities.

### Multivariable models

In the adjusted negative binomial model, IVS was positively and significantly associated with gestational syphilis rates after adjustment for macro-region. Analysis by IVS dimensions revealed that the income/labour dimension showed the strongest association, followed by human capital and urban infrastructure. The multilevel model confirmed significant between-state variation (ICC >0.10), supporting the use of hierarchical modelling. Spatial regression models (SAR and SEM) improved fit over OLS, confirming residual spatial dependence and the importance of accounting for geographic proximity in modelling syphilis determinants.

---

## Discussion

This study provides the most comprehensive analysis to date of the temporal, spatial, and social dimensions of gestational and congenital syphilis in Brazil, covering 17 years (2007–2023), all 5,570 municipalities, over 960,000 notifications, and integrating joinpoint regression, spatial epidemiology, formal inequality measurement, and multivariable modelling within a single analytical framework.

### Principal findings

The 15-fold increase in gestational syphilis notifications over the study period reflects a combination of genuine epidemiological resurgence and the progressive strengthening of surveillance systems. The identification of inflection points through joinpoint analysis provides temporal anchors for interpreting the interplay between disease dynamics and policy interventions. The acceleration observed from 2015 onwards, for instance, coincides with the widespread introduction of rapid syphilis testing in primary care settings and the *Agenda de Ações Estratégicas* of 2017, suggesting that improved detection was a major driver of rising case counts during this period.^19^

The persistent elevation of congenital syphilis—despite rising detection of gestational syphilis—indicates systematic failures in the cascade from diagnosis to treatment. The congenital-to-gestational syphilis ratio, mapped spatially for the first time in this study, reveals stark geographic heterogeneity in programmatic effectiveness, with the highest ratios in regions where healthcare access is most constrained.

### Spatial inequalities and territorial vulnerability

The strong spatial autocorrelation and persistent hotspot patterns identified through LISA analysis underscore the territorial nature of syphilis risk in Brazil. The concentration of High-High clusters in the North and Northeast—regions characterised by lower healthcare density, greater geographic barriers to care, and higher social vulnerability—is consistent with the broader literature on health inequalities in Brazil.^20,21^ The remarkable persistence of these clusters across three distinct analytical periods (2007–2011, 2012–2017, 2020–2023) suggests that the structural determinants driving geographic disparities have been largely impervious to the national-level programmatic interventions implemented during this time.

The positive SII and Concentration Index values, documented for the first time for syphilis in Brazil using these WHO-standard measures, provide a rigorous quantification of socioeconomic inequality in syphilis burden. The temporal trends in these indices suggest that inequalities have not narrowed—and may have widened—over the study period, a finding with important implications for equity-oriented policy.

### Racial disparities as expression of structural racism

The disproportionate burden of syphilis among Black and mixed-race women must be interpreted within the framework of structural racism in Brazil. Race/colour, as recorded in health information systems, is a social construct that reflects differential exposure to the social determinants of health—including barriers to quality antenatal care, lower educational attainment (itself a product of historical discrimination), residential segregation in underserved territories, and institutional biases within the healthcare system.^22,23^ The significant interaction between race/colour and social vulnerability observed in our models indicates that racial disparities are not uniform across all contexts but are amplified in territories where structural disadvantage is most acute—a pattern consistent with intersectionality theory applied to health.^24^

### Impact of COVID-19

The interrupted time series analysis provides quantitative evidence that the COVID-19 pandemic disrupted syphilis surveillance and care, with an immediate decline in notifications followed by a rebound that, by 2022–2023, exceeded pre-pandemic projections. This pattern likely reflects both reduced healthcare-seeking behaviour during lockdowns and the diversion of primary care resources to pandemic response. The asymmetric impact—with greater relative disruption in more vulnerable territories—further exacerbated existing inequalities.^25^

### Comparison with the literature

Previous studies have documented temporal trends in syphilis in Brazil using descriptive or simple regression approaches,^26–28^ but none have combined joinpoint analysis with LISA, IVS stratification, formal inequality indices, ITS for COVID-19, and multivariable spatial models within a single study. Our approach fills several key gaps: (i) the extended period through 2023 captures the post-COVID recovery; (ii) municipal-level spatial analysis identifies local clusters missed by state-level studies; (iii) the SII and Concentration Index provide internationally comparable measures of inequality; (iv) the congenital-to-gestational ratio mapped spatially offers a novel programmatic indicator; and (v) the race × vulnerability interaction model advances understanding of intersectional determinants.

### Strengths and limitations

**Strengths** include the use of 17 years of individual-level nationwide notification data; municipal-level spatial resolution; integration of multiple complementary analytical approaches; formal inequality measurement following WHO standards; and explicit engagement with structural racism as a conceptual framework.

**Limitations** inherent to this ecological study design must be acknowledged. First, the ecological fallacy precludes causal inference at the individual level—associations observed between municipal-level vulnerability and syphilis rates cannot be attributed to individual-level exposures. Second, the rising trend in notifications likely reflects a combination of true incidence increase and improved detection, and these contributions cannot be fully disentangled with available data. Third, the SINAN database is subject to underreporting, particularly in the early years of the study period and in remote areas—which means that the disparities documented here likely underestimate the true burden in vulnerable territories. Fourth, the IVS is based on census data and does not capture intra-municipal variation in vulnerability. Fifth, race/colour data are subject to classification variability and missingness. Sixth, the absence of individual-level confounders in the ecological design limits the ability to adjust for compositional effects.

### Public health implications

Our findings carry several implications for policy and practice:

1. **Geographic targeting:** The persistent spatial clusters identified through LISA analysis provide a rational basis for geographically targeted interventions, directing resources to municipalities with the highest burden and greatest unmet need.

2. **Equity lens:** The positive SII and Concentration Index values demonstrate that syphilis elimination cannot be achieved without explicitly addressing social vulnerability. Universal programmes must be complemented by equity-oriented strategies that intensify efforts in the most vulnerable territories.

3. **Racial equity:** The amplification of racial disparities in vulnerable territories demands interventions that address structural racism within the healthcare system, including culturally appropriate care, anti-discrimination training, and accountability mechanisms.

4. **Programmatic indicator:** The congenital-to-gestational syphilis ratio should be adopted as a routine surveillance indicator, as it captures the gap between detection and effective treatment and can be mapped to identify areas requiring quality improvement.

5. **Pandemic preparedness:** The COVID-19-related disruption to syphilis services highlights the need for resilient essential service delivery during health emergencies, with specific attention to maintaining antenatal screening and treatment.

---

## Conclusion

Gestational and congenital syphilis in Brazil follow deeply unequal temporal and spatial patterns that are inextricable from the country's structural inequalities. The persistence of spatial hotspots in vulnerable territories, the widening of socioeconomic disparities, and the amplification of racial inequalities within disadvantaged contexts collectively indicate that the current trajectory is incompatible with elimination goals. Achieving the elimination of congenital syphilis will require a paradigm shift from disease-specific vertical programmes toward integrated, equity-centred approaches that address the social and territorial determinants of maternal health. The evidence presented here provides a granular evidence base for directing such efforts where they are most urgently needed.

---

## References

1. Rowley J, Vander Hoorn S, Korenromp E, et al. Chlamydia, gonorrhoea, trichomoniasis and syphilis: global prevalence and incidence estimates, 2016. *Bull World Health Organ*. 2019;97(8):548-562P.
2. Korenromp EL, Rowley J, Alonso M, et al. Global burden of maternal and congenital syphilis and associated adverse birth outcomes—Estimates for 2016 and progress since 2012. *PLoS One*. 2019;14(2):e0211720.
3. World Health Organization. *Global guidance on criteria and processes for validation: elimination of mother-to-child transmission of HIV, syphilis and hepatitis B virus*. Geneva: WHO; 2021.
4. Gomez GB, Kamb ML, Newman LM, et al. Untreated maternal syphilis and adverse outcomes of pregnancy: a systematic review and meta-analysis. *Bull World Health Organ*. 2013;91(3):217-226.
5. Ministério da Saúde. Boletim Epidemiológico de Sífilis 2023. Brasília: Secretaria de Vigilância em Saúde e Ambiente; 2023.
6. Brasil. Ministério da Saúde. SINAN – Sistema de Informação de Agravos de Notificação. Disponível em: http://tabnet.datasus.gov.br.
7. Brasil. Ministério da Saúde. Portaria nº 1.459, de 24 de junho de 2011. Institui a Rede Cegonha. *Diário Oficial da União*. 2011.
8. Brasil. Ministério da Saúde. Agenda de Ações Estratégicas para Redução da Sífilis no Brasil. Brasília; 2017.
9. Araújo MAL, Barros VL, Nunes NB, et al. Penicillin shortage and congenital syphilis: what to expect? *Rev Inst Med Trop São Paulo*. 2020;62:e35.
10. Macêdo VC, Lira PIC, Frias PG, et al. Risk factors for syphilis in women: case-control study. *Rev Saúde Pública*. 2017;51:78.
11. Domingues RMSM, Leal MC. Incidence of congenital syphilis and factors associated with vertical transmission: data from the Birth in Brazil study. *Cad Saúde Pública*. 2016;32(6):e00082415.
12. Pinto M, Melo C, Costa A, et al. Impact of COVID-19 on syphilis surveillance in Brazil. *Lancet Reg Health Am*. 2022;12:100265.
13. Teixeira LO, Belarmino V, Gonçalves CV, et al. Temporal trend and spatial distribution of congenital syphilis in Brazil, 2001–2018. *Rev Soc Bras Med Trop*. 2021;54:e0596-2020.
14. Rodrigues NCP, Daumas RP, de Souza RA, et al. Spatial analysis of congenital syphilis in a Brazilian state. *Rev Lat Am Enferm*. 2020;28:e3330.
15. World Health Organization. *Handbook on health inequality monitoring*. Geneva: WHO; 2013.
16. Lago EG, Vaccari A, Fiori RM. Clinical features and follow-up of congenital syphilis. *Sex Transm Dis*. 2013;40(2):85-94.
17. Leal MC, Gama SGN, Pereira APE, et al. The colour of pain: racial iniquities in prenatal care and childbirth in Brazil. *Cad Saúde Pública*. 2017;33(Suppl 1):e00078816.
18. Goes EF, Nascimento ER. Black women and the right to health: racial inequalities in prenatal care in the SUS. *Saúde Debate*. 2013;37(99):571-577.
19. Magalhães DMS, Kawaguchi IAL, Dias A, et al. Syphilis in pregnancy and its influence on the hospital morbidity of the newborn. *Rev Lat Am Enferm*. 2013;21(1):228-234.
20. Victora CG, Aquino EML, do Carmo Leal M, et al. Maternal and child health in Brazil: progress and challenges. *Lancet*. 2011;377(9780):1863-1876.
21. Paim J, Travassos C, Almeida C, et al. The Brazilian health system: history, advances, and challenges. *Lancet*. 2011;377(9779):1778-1797.
22. Chor D. Health inequalities in Brazil: race matters. *Cad Saúde Pública*. 2013;29(7):1272-1275.
23. Barata RB. *Como e por que as desigualdades sociais fazem mal à saúde*. Rio de Janeiro: Editora FIOCRUZ; 2009.
24. Hankivsky O. Intersectionality 101. *The Institute for Intersectionality Research and Policy*. 2014.
25. Marques CP, Geremia DS, Siqueira BB, et al. Impact of the COVID-19 pandemic on prenatal care: integrative review. *Ciênc Saúde Coletiva*. 2023;28(9):2621-2635.
26. De Boni RB, Veloso VG, Grinsztejn B. Epidemiology of syphilis in Brazil and its relationship to social vulnerability. *Braz J Infect Dis*. 2021;25(1):101039.
27. Figueiredo DCMM, Figueiredo AM, Souza TKB, et al. Temporal analysis of reported cases of congenital syphilis in Brazil: 2006–2019. *JBRA Assist Reprod*. 2020;24(4):445-453.
28. Nunes PS, Zara ALSA, Rocha FC, et al. Syphilis in pregnancy and congenital syphilis and their relationship with Family Health Strategy coverage, Brazil, 2007–2014. *Epidemiol Serv Saúde*. 2018;27(4):e2018193.

---

## Tables and Figures

**Table 1.** Annual number of notifications and rates of gestational and congenital syphilis, Brazil, 2007–2023.

**Table 2.** Joinpoint analysis results: APC and AAPC for gestational and congenital syphilis by macro-region.

**Table 3.** Global Moran's I for gestational and congenital syphilis, by period.

**Table 4.** Syphilis rates by IVS quintile with SII and Concentration Index.

**Table 5.** Multivariable model results: negative binomial regression, multilevel, and spatial models.

**Figure 1.** Temporal trends in gestational and congenital syphilis rates, Brazil, 2007–2023.

**Figure 2.** Temporal trends by macro-region.

**Figure 3.** Proportional distribution by maternal race/colour over time.

**Figure 4.** Joinpoint analysis with segmented regression and identified breakpoints.

**Figure 5.** Choropleth maps of Empirical Bayes smoothed rates.

**Figure 6.** LISA cluster maps (High-High, Low-Low, High-Low, Low-High).

**Figure 7.** Spatio-temporal comparison of LISA clusters across three periods.

**Figure 8.** Interrupted time series: observed vs. counterfactual syphilis notifications around COVID-19.

**Figure 9.** Concentration curves for gestational and congenital syphilis by IVS.

**Figure 10.** Temporal evolution of SII, Concentration Index, and Q5/Q1 ratio.

**Figure 11.** Forest plot: APC by state from joinpoint analysis.

**Figure 12.** Predicted syphilis rates by race/colour × IVS interaction.
