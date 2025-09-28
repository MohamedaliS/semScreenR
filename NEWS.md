# semScreenR News

## 0.0.1
- Initial scaffold: deterministic screening workflow with presets and caps
- Careless-responding signals (longstring, IRV, Mahalanobis, too-fast)  
- K-fold validation gates on ΔCFI, ΔTLI, ΔRMSEA, ΔSRMR
- Pre/post APA-style comparison table (gt fallback to data.frame)
- HTML report export; Shiny app skeleton
- Path diagram export (semPlot), PNG/SVG
- **NEW: Hierarchical model detection and comparison** 
  - `is_higher_order_model()` detects hierarchical factor structure
  - `lower_order_equivalent()` generates lower-order model alternatives  
  - `sem_hierarchy_compare()` compares models with no-material-harm guardrails
  - `sem_maybe_hierarchy()` provides conditional analysis pipeline
  - Automatic integration into HTML reports when hierarchical models detected
  - Comprehensive comparison tables with fit measures and recommendations
- Enhanced exports: `apa_fit_table()`, `one_line_delta()`, `config_from_yaml()`
- Improved error handling and validation throughout
- Comprehensive test suite including hierarchical model tests
- ASCII-compatible output for cross-platform reliability