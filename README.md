# MIMIC-IV BigQuery Analysis

This project provides R scripts to extract, clean, and analyze **MIMIC-IV** data from Google BigQuery. It focuses on admissions with cirrhosis, MELD-related labs, liver transplants, and dialysis procedures, preparing datasets for survival and calibration analysis.

---

## Features
- **BigQuery setup & authentication** via `gargle` and `bigrquery`
- **SQL queries** for:
  - All admissions
  - Cirrhosis-related admissions (ICD-9/10)
  - MELD-relevant labs (bilirubin, creatinine, sodium, INR, albumin)
  - Liver transplant procedures
  - Dialysis procedures
- **Helper utilities** to:
  - Run queries safely
  - Save results to CSV
  - Enrich missing ethnicity data
  - Select optimal lab sets per admission
  - Compute AUROCs, confidence intervals, and p-values
  - Generate calibration plots
- **Data preparation pipeline**:
  - Clean and filter admissions
  - Merge labs and outcomes
  - Add MELD, MELD-Na, and MELD-3.0 scores
  - Add outcomes (transplant, death, dialysis)
  - Save ready-to-analyze CSVs
- **Analysis outputs**:
  - Table 1 summary by ethnicity
  - ROC curves, AUROC with CIs
  - Hosmer-Lemeshow tests
  - Calibration plots
  - Predicted mortality comparisons

---