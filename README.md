# semScreenR

**semScreenR** provides a deterministic, ethically-guarded data screening workflow for structural equation modeling (SEM) in R.

## Key features
- Pre-specified rulesets (conservative, balanced, aggressive)
- Caps on row/item deletion; minimum-N and minimum-indicator safeguards
- Conditional careless-responding screening using multiple indicators
- Cross-validated improvement gates (ΔCFI, ΔTLI, ΔRMSEA, ΔSRMR) and AIC/BIC checks
- Audit trail + pre/post APA-style tables and path diagrams (via `semPlot`)

## Install (prototype bundle)
```r
devtools::install_local('semScreenR_0.3.1.zip', upgrade = 'never')
library(semScreenR)
```

## Minimal example
```r
library(lavaan)
data('HolzingerSwineford1939', package='lavaan')
dat <- HolzingerSwineford1939
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
cfg  <- triage_rules('balanced')
plan <- triage_plan(dat, model, id_cols='id', config=cfg)
res  <- triage_apply(dat, model, plan)
sem_compare_table(dat, model, res)
```

> semScreenR is not a tool for data fishing. It enforces preregistered thresholds, validation, and full reporting.
