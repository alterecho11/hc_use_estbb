[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19615879.svg)](https://doi.org/10.5281/zenodo.19615878)

## Overview

This repository contains the analysis pipeline for a longitudinal descriptive study of hormonal contraceptive (HC) use among female participants in the Estonian Biobank (206K data freeze).

This repository does not contain real individual-level data. Dummy data provided here was created manually to mirror the structure of the real input, with no statistical properties derived from the real data. In certain examples, supplementary information tables (S3 Appendix) provided with the manuscript may be used. For access to real data, see the Data availability section.

---

## Repository structure

```
├── README.md         # project overview and instructions
├── hc-estbb.Rproj    # RStudio project file
├── code/
│   ├── main.R        # full analysis pipeline
│   └── core.js       # convert purchases to usage periods
└── session_info.txt  # R session snapshot
```

---

## How to run

1. Clone this repository
2. Open `hc-estbb.Rproj` in RStudio
3. Install dependencies (see `session_info.txt` for the exact R and package versions)
4. Follow `code/main.R`, it documents statistical analysis step by step, corresponding to the respective manuscript sections

Optional:

5. Read through `code/core.js` to understand the input data structure, algorithm logic, and known limitations before running; `code/core.js` documents the algorithm used to infer HC usage periods from purchase records, including dummy test scenarios
6. Install [Node.js](https://nodejs.org)
7. Run `node core.js` in Terminal to execute the test suite and understand the output

---

## How to cite


If you use the R code in your research, please cite the manuscript:

Citation: Džigurski J, Möls M, Läll K, Currant H, Eltermaa M, Estonian Biobank Research Team, et al. (2026) Prescribed hormonal contraceptive use trends in the Estonian Biobank: A longitudinal observational study. PLoS Med 23(5): e1005086.
DOI: https://doi.org/10.1371/journal.pmed.1005086

If you specifically use or adapt the HC usage period reconstruction 
algorithm JS code, please also cite the code repository:

Jelisaveta Džigurski. (2026). alterecho11/hc_use_estbb: v1.0.0 (v1.0.0). Zenodo. 
https://doi.org/10.5281/zenodo.19615879

---

## Data availability

The analyses carried out in the study included individual-level data, which cannot be made publicly available due to legal and ethical restrictions. To access the data, the approval must be obtained from the Scientific Advisory Committee of the Estonian Biobank and the Estonian Committee on Bioethics and Human Research. The inquiry for data should be sent via e-mail to releases@ut.ee. For more details, please see the Data Access section at https://genomics.ut.ee/en/content/estonian-biobank#dataaccess.

---

## Correspondence

Code: jelisaveta.dzigurski@ut.ee (if anything is unclear, please write; im happy to help or think along)
<br>
Data: releases@ut.ee
