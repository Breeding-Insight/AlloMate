# AlloMate — Genetic Mate Allocation and Breeding Optimization App

AlloMate is a **Shiny web application** developed by the Breeding Insight team to support **genetic mate allocation and breeding optimization**. The app integrates pedigree-based kinship, estimated breeding values (EBVs), and **Optimum Contribution Selection (OCS)** to help breeding programs balance genetic gain while controlling inbreeding.

This repository follows a **golem-based application structure**.

---

## Overview

Modern breeding programs must optimize selection decisions while managing inbreeding and long-term genetic diversity. AlloMate provides an interactive and reproducible framework for:

- Evaluating genetic relatedness among breeding candidates  
- Combining multiple EBV traits using user-defined weights  
- Optimizing individual contributions via Optimum Contribution Selection  
- Producing feasible mating plans under kinship constraints  

The application is designed to be species-agnostic and adaptable to a wide range of breeding program designs.

---

## Key Features

### Breeding and Optimization

- Pedigree-based kinship matrix calculation  
- Support for multiple EBV traits with configurable weights (weights must sum to 1)  
- Optimum Contribution Selection (OCS) to balance gain and inbreeding  
- Kinship threshold filtering to exclude unfavorable crosses  
- Automated generation of mating plans  

### Technical Highlights

- Implemented in **R and Shiny**  
- Uses the `optiSel` package when available  
- Includes a **pure-R OCS fallback implementation** for environments where `optiSel` cannot be installed  
- Designed for modularity and extensibility  
- Exportable results for downstream analysis and reporting  

---

## Installation and Running the App

AlloMate uses a **golem application structure**, allowing it to be installed like a standard R package.

### Install from GitHub

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("Breeding-Insight/AlloMate")
```

### Run AlloMate

```r
AlloMate::run_app()
```

---

## Dependencies

Key R packages used by AlloMate include:

- shiny
- dplyr
- tidyr
- kinship2
- quadprog
- DT
- openxlsx

**Optional:**

- optiSel (used when available for Optimum Contribution Selection)

---

## Usage Summary

1. **Upload required input files:**
   - Candidate list (IDs and sex)
   - Pedigree file (individual, sire, dam)
   - EBV files for one or more traits

2. **Configure analysis parameters:**
   - Trait weights (must sum to 1)
   - Kinship thresholds
   - OCS constraints

3. **Review outputs:**
   - Kinship matrices and summaries
   - Combined EBV scores
   - Optimal contributions and mating plans

4. **Export results:**
   - Download reports containing all outputs and diagnostics

---

## Citation

If you use AlloMate in research or breeding analyses, please cite it as:

Chinchilla-Vargas, J., Ackerman, A. J., & Taniguti, C. H.  
AlloMate: Genetic Mate Allocation and Breeding Optimization App.  
RRID: SCR_027115

