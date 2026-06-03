# AlloMate — Genetic Mate Allocation and Breeding Optimization App

[![Development Status](https://img.shields.io/badge/status-active%20development-yellow)](https://github.com/Breeding-Insight/AlloMate)
[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Web%20Application-blueviolet)](https://shiny.posit.co/)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![GitHub issues](https://img.shields.io/github/issues/Breeding-Insight/AlloMate)](https://github.com/Breeding-Insight/AlloMate/issues)
[![GitHub pull requests](https://img.shields.io/github/issues-pr/Breeding-Insight/AlloMate)](https://github.com/Breeding-Insight/AlloMate/pulls)
[![GitHub Release](https://img.shields.io/github/v/release/Breeding-Insight/AlloMate?include_prereleases)](https://github.com/Breeding-Insight/AlloMate/releases/latest)

AlloMate is a Shiny web application developed by the Breeding Insight team
to support genetic mate allocation and breeding optimization. The app integrates
pedigree-based kinship, estimated breeding values (EBVs), and Optimum Contribution
Selection (OCS) to help breeding programs balance genetic gain while controlling
inbreeding. It includes built-in pedigree validation and cleaning tools to ensure
data integrity before analysis.

This repository follows a golem-based application structure.

---

## Overview

Modern breeding programs must optimize selection decisions while managing inbreeding
and long-term genetic diversity. AlloMate provides an interactive and reproducible
framework for:

- Validating and cleaning pedigree records prior to analysis
- Evaluating genetic relatedness among breeding candidates
- Combining multiple EBV traits using user-defined weights
- Optimizing individual contributions via Optimum Contribution Selection
- Producing feasible mating plans under kinship constraints

The application is designed to be species-agnostic and adaptable to a wide range
of breeding program designs.

---

## Key Features

### Pedigree Cleaning

- Detection of structural pedigree errors (e.g., missing parents, duplicate IDs,
  sex conflicts)
- Flagging of individuals with inconsistent or incomplete pedigree records
- Interactive review and resolution of pedigree issues before downstream analysis
- Summary reports of identified issues and applied corrections

### Breeding and Optimization

- Pedigree-based kinship matrix calculation
- Support for multiple EBV traits with configurable weights (weights must sum to 1)
- Optimum Contribution Selection (OCS) to balance gain and inbreeding
- Kinship threshold filtering to exclude unfavorable crosses
- Automated generation of mating plans

### Technical Highlights

- Implemented in R and Shiny
- Uses the optiSel package when available
- Includes a pure-R OCS fallback implementation for environments where optiSel
  cannot be installed
- Designed for modularity and extensibility
- Exportable results for downstream analysis and reporting

---

## Installation and Running the App

AlloMate uses a golem application structure, allowing it to be installed like
a standard R package.

### Install from GitHub
```{r}
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("Breeding-Insight/AlloMate")
```
### Run AlloMate
```{r}
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

Optional:

- optiSel (used when available for Optimum Contribution Selection)

---

## Usage Summary

1. Upload required input files:
   - Candidate list (IDs and sex)
   - Pedigree file (id, male_parent, female_parent)
   - EBV files for one or more traits

2. Run pedigree cleaning:
   - Review flagged structural errors and inconsistencies
   - Apply or export corrections before proceeding with analysis

3. Configure analysis parameters:
   - Trait weights (must sum to 1)
   - Kinship thresholds
   - OCS constraints

4. Review outputs:
   - Kinship matrices and summaries
   - Combined EBV scores
   - Optimal contributions and mating plans

5. Export results:
   - Download Excel reports containing all outputs and diagnostics

---

## Citation

If you use AlloMate in research or breeding analyses, please cite it as:

Chinchilla-Vargas, J., Ackerman, A. J., Taniguti, C. H., & Sandercock, A. M.  
AlloMate: Genetic Mate Allocation and Breeding Optimization App.
  - RRID: SCR_027115

---

## Contributing

Contributions are welcome. Please:

- Follow existing coding and naming conventions
- Clearly document new functions and modules
- Test changes before submitting pull requests

Submit issues and pull requests via GitHub.

---

## License

AlloMate is released under the Apache License, Version 2.0.
See the LICENSE file or https://www.apache.org/licenses/LICENSE-2.0 for details.

---

## Acknowledgments

AlloMate is developed as part of the Breeding Insight initiative
(https://www.breedinginsight.org) to provide open-source, data-driven tools
for modern breeding programs.
