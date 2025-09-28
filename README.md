# semScreenR

Deterministic, rule-based data screening for structural equation modeling (SEM) in R. Provides ethical guardrails including preregistered caps on row/item removal, validation checks, and complete audit trails.

## Key Features

- **Preset configurations**: Conservative, balanced, and aggressive screening approaches
- **Safety constraints**: Caps on row and item removal; minimum N and indicators per factor  
- **Careless response detection**: Longstring, IRV, Mahalanobis distance, and response time screening
- **Cross-validation gates**: K-fold validation on ΔCFI, ΔTLI, ΔRMSEA, ΔSRMR to prevent overfitting
- **Professional reporting**: Pre/post APA-style tables and optional path diagrams (semPlot)
- **Export capabilities**: HTML report generation; optional DOCX table export (officer + flextable)
- **Hierarchical model support**: Automatic detection and comparison of higher-order vs lower-order factor models

## Installation

### From GitHub

```r
# install.packages("devtools")
devtools::install_github("MohamedaliS/semScreenR", upgrade = "never")
```

## Quick Start Example

```r
library(semScreenR)
library(lavaan)

# Load example data
data("HolzingerSwineford1939", package = "lavaan")
dat <- HolzingerSwineford1939

# Define your measurement model
model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

# Configure screening parameters
cfg  <- triage_rules("balanced")                              # Use balanced preset
plan <- triage_plan(dat, model, id_cols = "id", config = cfg) # Create screening plan
res  <- triage_apply(dat, model, plan)                        # Execute screening

# Generate outputs
sem_compare_table(dat, model, res)                            # APA-style fit table
export_sem_report(dat, model, res, file = "sem_report.html") # HTML report

# Optional: Create path diagrams (requires semPlot)
# install.packages("semPlot")
sem_paths_prepost(dat, model, res, file_base = "sem_paths", type = "png")
```

## Ethical Research Framework

semScreenR promotes transparent and reproducible research practices through:

- **Rule-based decisions**: All screening follows predetermined criteria, not post-hoc optimization
- **Transparent audit trails**: Complete logging of all actions taken during screening
- **Validation safeguards**: K-fold cross-validation prevents overfitting to sample-specific patterns
- **Theory preservation**: Maintains original model structure without automatic modifications
- **Preregistration support**: Configurable limits encourage researchers to specify screening parameters in advance

This framework helps researchers conduct rigorous, defensible data screening while maintaining scientific integrity.

## Dependencies

### Required

- R (>= 4.1)

### Suggested  

- **lavaan**: Structural equation modeling framework
- **semPlot**: Path diagram visualization  
- **gt**: Modern table formatting
- **officer** + **flextable**: DOCX export capabilities
- **rmarkdown** + **knitr**: Report generation
- **testthat**: Unit testing framework

## Citation

If you use semScreenR in published work, please cite the package using the information in `CITATION.cff`.
