## Overview

This repository provides data documentation and analysis code for the
manuscript:

**“Measuring Intersectoral Coordination Dynamics: Psychometric
Properties of the German Relational Coordination Survey”**

The repository follows the **TIER Documentation Protocol 3.0**  
(see:
<https://www.projecttier.org/tier-protocol/tier-protocol-version-history/specifications-3-0/#overview-of-the-documentation>)

------------------------------------------------------------------------

## Repository Structure

The repository is organized as follows:

-   **`README.md`**  
    Overview and guidance for reproducing the analysis

-   **`eliPfad-data-publications.Rpoject`**  
    RStudio project file for the analysis

-   **Reproducible R Environment**

    -   `renv.lock` – Snapshot of package dependencies

-   **`original-data/`** *(restricted access)*

    -   **`metadata/`**
        -   `metadata_guide.md` – Description of original data sources
            and structure  
        -   **`supplements/`**
            -   `questionnaire_ger_final.pdf` – Final German
                questionnaire

-   **`processing-and-analysis/`**

    -   **`command-files/`**
        -   `eliPfad_data_tidy.R` – Data cleaning and preprocessing  
        -   `Relational_Coordination_analysis.R` – Statistical analysis
    -   **`analysis-data/`**
        -   `rl_data_psych.Rdata` – Cleaned dataset used for analysis  
        -   `data_appendix.Rmd` – English codebook  
        -   `codebook_german.pdf` – Full German codebook
    -   **`results/`**
        -   Output files (tables, model results, figures)

------------------------------------------------------------------------

## Reproducibility

This project uses the **`renv`** package to ensure a reproducible
computational environment.

-   **R version:** 4.2.2  
-   All package dependencies are specified in `renv.lock`

To restore the environment:

    install.packages("renv")
    renv::restore()

## How to Reproduce the Analysis

1.  Clone the repository or download it as a ZIP file
2.  Open the \`eliPfad-data-publications.Rpoject in R / RStudio
3.  Restore the reproducible environment using renv::restore()
4.  Navigate to: `**processing-and-analysis/command-files/`\*\*
5.  Run the analysis script: `Relational_Coordination_analysis.R`

-&gt; The script: - loads the cleaned dataset from analysis-data/ -
performs all statistical analyses (CFA, scale validation) - saves
outputs to the `processing-and-analysis/results/` folder

## Data Availability

The original raw data are not included in this repository due to ongoing
analyses and planned follow-up publications.

Detailed information on the raw data is provided in:
original-data/metadata/metadata\_guide.md Access to the original dataset
is available upon reasonable request - contact <kai.keller@ukbonn.de>
