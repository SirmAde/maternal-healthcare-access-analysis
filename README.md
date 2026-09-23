# Maternal Healthcare Access and Under-Five Child Mortality in Nigeria

## Overview

This project investigates the relationship between maternal antenatal care (ANC) attendance and under-five child mortality in Nigeria.

The analysis examines whether adequate maternal ANC attendance, defined as **four or more ANC visits (≥4)**, is associated with lower under-five mortality. The study uses nationally representative data from the **Nigeria Demographic and Health Survey (DHS)** and applies statistical analysis in **R**.

The project focuses on the relationship between maternal healthcare access and child survival while considering the broader socioeconomic and demographic factors that may influence both ANC utilisation and child mortality.

---

## Research Question

**Is adequate maternal antenatal care attendance (≥4 visits) associated with lower under-five child mortality in Nigeria?**

---

## Statement of the Problem

Nigeria accounts for a disproportionate share of global under-five deaths. Despite policy commitments to Universal Health Coverage and continued investment in primary healthcare infrastructure, reductions in child mortality have progressed more slowly than projected under the Sustainable Development Goals (SDGs).

A key gap in the existing evidence concerns the relationship between ANC adequacy and child survival outcomes. Maternal ANC utilisation is closely associated with socioeconomic and demographic characteristics, including maternal education, household wealth, place of delivery, and type of residence.

Consequently, an observed association between ANC attendance and child survival may partly reflect differences in socioeconomic and demographic circumstances rather than ANC attendance alone.

This study examines the relationship between ANC adequacy and under-five child mortality using individual-level birth records from the Nigeria Demographic and Health Survey.

---

## Research Hypothesis

The study is based on the following hypothesis:

### Null Hypothesis (H₀)

Adequate maternal ANC attendance (≥4 visits) has no association with under-five child mortality in Nigeria.

### Alternative Hypothesis (H₁)

Adequate maternal ANC attendance (≥4 visits) is associated with lower under-five child mortality in Nigeria.

---

## Data Source

The study uses data from the:

**Nigeria Demographic and Health Survey (DHS) – Birth Recode File (NGBR8BFL.DTA)**

The DHS provides nationally representative demographic and health information covering areas such as fertility, maternal health, healthcare utilisation, and child survival.

The original Birth Recode dataset contained:

**104,557 birth records**

After data cleaning and selection of records with complete information on the key variables required for the analysis, the final analytical sample consisted of:

**14,040 birth records**

The data preparation and statistical analysis were conducted using **R**.

> The raw DHS dataset is not included in this repository.

---

## Key Variables

The analysis focuses primarily on:

| Variable | Description |
|---|---|
| ANC Adequacy | Whether the mother attended ≥4 ANC visits |
| Under-Five Mortality | Whether the child died before reaching age five |
| Maternal Education | Mother's educational attainment |
| Household Wealth | Household wealth status |
| Place of Delivery | Location where the child was delivered |
| Residence Type | Urban or rural residence |

These variables provide a basis for examining the relationship between maternal healthcare access and child survival.

---

## Methodology

The analysis was conducted using **R** and included the following stages:

1. Data preparation
2. Data cleaning
3. Selection of relevant birth records
4. Handling of missing observations
5. Creation of the ANC adequacy variable
6. Construction of the under-five mortality outcome
7. Descriptive analysis
8. Cross-tabulation of ANC adequacy and child mortality
9. Chi-square test of association
10. Interpretation of statistical results

### Statistical Test

A **Pearson's Chi-square test of independence** was used to examine whether there was a statistically significant association between ANC adequacy and under-five child mortality.

---

## Results

### Chi-Square Test

The chi-square analysis produced the following result:

- **χ² = 17.614**
- **Degrees of freedom = 1**
- **p < 0.001**
- **Significance level (α) = 0.05**

Since the p-value is below the 0.05 significance threshold, the null hypothesis was rejected.

The analysis therefore provides statistical evidence of an association between ANC adequacy and under-five child mortality in the study sample.

---

## Cross-Tabulation: Child Survival by ANC Adequacy

The cross-tabulation showed differences in mortality between children whose mothers had inadequate and adequate ANC attendance.

| ANC Attendance | Under-Five Mortality |
|---|---:|
| Inadequate ANC (<4 visits) | 5.84% |
| Adequate ANC (≥4 visits) | 4.28% |

The observed difference in mortality was:

**1.56 percentage points**

The mortality percentage was lower among children whose mothers had adequate ANC attendance.

---

## Interpretation

The findings indicate that adequate maternal ANC attendance (≥4 visits) is statistically associated with under-five child survival in the analysed dataset.

Children whose mothers had adequate ANC attendance had a lower observed under-five mortality rate than children whose mothers had fewer than four ANC visits.

However, the chi-square test establishes an **association**, not causation. Therefore, the findings should not be interpreted as demonstrating that ANC attendance alone causes the reduction in child mortality.

Socioeconomic and demographic characteristics may influence both ANC utilisation and child survival. Further multivariable or multilevel modelling would be appropriate for assessing whether the observed relationship remains after controlling for potential confounding factors.

---

## Key Finding

The analysis found a statistically significant association between maternal ANC adequacy and under-five child mortality:

> **χ²(1) = 17.614, p < 0.001**

Under-five mortality was **4.28%** among children whose mothers had adequate ANC attendance compared with **5.84%** among those whose mothers had fewer than four ANC visits.

---

## Tools and Technologies

- **R**
- RStudio
- Statistical analysis
- Data cleaning
- Data transformation
- Cross-tabulation
- Chi-square testing
- Data visualisation

---

## Repository Structure

```text
maternal-healthcare-access-analysis/
│
├── data/
│   └── README.md
│
├── R/
│   └── maternal_healthcare_analysis.R
│
├── results/
│   └── figures/
│
├── report/
│   └── maternal_healthcare_analysis.pdf
│
├── README.md
└── .gitignore
