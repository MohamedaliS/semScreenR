# semScreenR

Deterministic, rule-based data screening for structural equation modeling (SEM) in R.  
Ethical guardrails include preregistered caps on row/item removal, validation checks, and full audit trails.

## Key features
- Presets: conservative, balanced, aggressive
- Caps on row and item removal; minimum N and minimum indicators per factor
- Careless-responding screening (longstring, IRV, Mahalanobis, too-fast)
- K-fold validation gates on ΔCFI, ΔTLI, ΔRMSEA, ΔSRMR
- Pre/post APA-style tables; optional path diagrams (semPlot)
- HTML report export; optional DOCX table export (officer + flextable)

## Installation
# From a local checkout
```r
# install.packages("devtools")
devtools::install("semScreenR", upgrade = "never")
```
## Quick Start
```r
library(semScreenR)
library(lavaan)

data("HolzingerSwineford1939", package = "lavaan")
dat <- HolzingerSwineford1939

model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

cfg  <- triage_rules("balanced")
plan <- triage_plan(dat, model, id_cols = "id", config = cfg)
res  <- triage_apply(dat, model, plan)

# APA-style fit table (gt if available)
sem_compare_table(dat, model, res)

# HTML report
export_sem_report(dat, model, res, file = "sem_report.html")

# Optional: path diagrams
# install.packages(c("semPlot"))
sem_paths_prepost(dat, model, res, file_base = "sem_paths", type = "png")
```
## Ethics

semScreenR is not a tool for data snooping. All removals are rule-based, capped, validated, and fully logged. The package favors theory integrity: no automatic cross-loadings or residual correlations are added.

suggests: lavaan, semPlot, gt, officer, flextable, rmarkdown, knitr, testthat (>= 3.0.0)

## Citation

If you use semScreenR in published work, please cite the package and your preregistered thresholds.
::contentReference[oaicite:0]{index=0}

