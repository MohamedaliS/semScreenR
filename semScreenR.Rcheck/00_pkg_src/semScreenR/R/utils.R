#' Internal helpers and light-weight utilities
#' @name utils-internal
#' @keywords internal
#' @importFrom stats setNames
#' @importFrom utils head
NULL

# Declare global variables to avoid R CMD check warnings
# These are column names from lavaan functions
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("op", "abs_est"))
}

# Safe NULL coalescing (keep only here; remove duplicates elsewhere)
# Null coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# Standardized formatting functions for consistent decimal places
#' @title Format fit indices with appropriate precision
#' @param value numeric value to format
#' @param measure character indicating measure type for context-appropriate formatting
#' @return formatted character string
#' @noRd
format_fit_measure <- function(value, measure = "default") {
  if (is.null(value) || is.na(value)) {
    return("--")
  }
  
  # Context-appropriate decimal places
  decimals <- switch(measure,
    "cfi" = 3,
    "tli" = 3, 
    "rmsea" = 3,
    "srmr" = 3,
    "aic" = 1,
    "bic" = 1,
    "chisq" = 2,
    "pvalue" = 4,
    "loading" = 3,
    "improvement" = 3,
    3  # default
  )
  
  # Format with appropriate decimal places
  if (abs(value) < 0.001 && measure %in% c("rmsea", "srmr", "improvement")) {
    return("< .001")
  } else if (abs(value) > 9999 && measure %in% c("aic", "bic", "chisq")) {
    # Use scientific notation for very large values
    return(formatC(value, format = "e", digits = 2))
  } else {
    formatted <- sprintf(paste0("%.", decimals, "f"), value)
    # Remove leading zero for fit indices < 1
    if (measure %in% c("cfi", "tli", "rmsea", "srmr", "loading") && abs(value) < 1) {
      formatted <- gsub("^0\\.", ".", formatted)
      formatted <- gsub("^-0\\.", "-.", formatted)
    }
    return(formatted)
  }
}

#' @title Format change/improvement values with +/- signs
#' @param value numeric change value
#' @param measure character indicating measure type
#' @return formatted character string with appropriate sign
#' @noRd
format_change <- function(value, measure = "default") {
  if (is.null(value) || is.na(value)) {
    return("--")
  }
  
  formatted <- format_fit_measure(abs(value), measure)
  sign <- if (value >= 0) "+" else "-"
  
  # Remove leading dot if present and add sign
  if (startsWith(formatted, ".")) {
    return(paste0(sign, formatted))
  } else if (formatted == "< .001") {
    return(paste0(sign, "< .001"))
  } else {
    return(paste0(sign, formatted))
  }
}

# Is a package installed and loadable?
.pkg_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' Extract manifest variables from lavaan model syntax
#' @param model_syntax A character string containing the lavaan model.
#' @return A character vector of unique manifest variable names.
#' @noRd
extract_model_variables <- function(model_syntax) {
  # Use lavaan's own parser to get the parameter table
  param_table <- try(lavaan::lavaanify(model_syntax, fixed.x = FALSE), silent = TRUE)
  if (inherits(param_table, "try-error")) {
    # Fallback to regex if lavaanify fails (e.g., syntax error)
    # This is less robust but better than nothing.
    # Extract variable names using regex
    all_vars <- unlist(strsplit(model_syntax, "\\s*(=~|~|~~)\\s*"))
    # Clean up and get unique names
    manifest_vars <- gsub("[^[:alnum:]_.]", " ", all_vars)
    manifest_vars <- unlist(strsplit(manifest_vars, "\\s+"))
    return(unique(manifest_vars[manifest_vars != ""]))
  }
  
  # Get all unique names appearing in the table
  all_names <- unique(c(param_table$lhs, param_table$rhs))
  
  # Identify latent variables (those on the LHS of "=~")
  latent_vars <- unique(param_table$lhs[param_table$op == "=~"])
  
  # Return manifest variables only for data validation purposes
  manifest_vars <- all_names[!(all_names %in% latent_vars) & all_names != ""]
  
  return(manifest_vars)
}

# Extract all variables (including latent) for testing purposes
extract_all_model_variables <- function(model_syntax) {
  param_table <- try(lavaan::lavaanify(model_syntax, fixed.x = FALSE), silent = TRUE)
  if (inherits(param_table, "try-error")) {
    # Fallback to regex if lavaanify fails (e.g., syntax error)
    all_vars <- unlist(strsplit(model_syntax, "\\s*(=~|~|~~)\\s*"))
    # Clean up and get unique names
    manifest_vars <- gsub("[^[:alnum:]_.]", " ", all_vars)
    manifest_vars <- unlist(strsplit(manifest_vars, "\\s+"))
    return(unique(manifest_vars[manifest_vars != ""]))
  }
  
  # Get all unique names appearing in the table (both manifest and latent)
  all_names <- unique(c(param_table$lhs, param_table$rhs))
  
  # Return all unique variable names
  return(all_names[all_names != ""])
}

# Fit a CFA/SEM model safely and return NULL on error
.safe_fit <- function(model, dat) {
  if (!.pkg_available("lavaan")) return(NULL)
  
  model_vars <- extract_model_variables(model)
  if (nrow(dat) < length(model_vars)) {
    warning("Skipping fit: data has fewer rows than model variables.")
    return(NULL)
  }
  
  args <- list(model = model, data = dat, std.lv = TRUE)
  
  fit <- suppressWarnings(try(do.call(lavaan::cfa, args), silent = TRUE))
  
  if (inherits(fit, "try-error") || !lavaan::lavInspect(fit, "converged")) {
    return(NULL)
  }
  
  fit
}

# Extract a consistent set of fit measures as a named list
.fit_measures <- function(fit) {
  if (is.null(fit)) return(list())
  keep <- c(
    "cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper",
    "srmr","aic","bic","chisq","df","pvalue","npar","nobs"
  )
  out <- try(lavaan::fitMeasures(fit, keep), silent = TRUE)
  if (inherits(out, "try-error")) return(list())
  as.list(out)
}

# Modify model syntax to remove a specific indicator
modify_model_syntax <- function(model_syntax, drop_item) {
  if (is.null(model_syntax) || !is.character(model_syntax) || length(model_syntax) == 0) {
    return(NULL)
  }
  if (is.null(drop_item) || !is.character(drop_item) || length(drop_item) == 0) {
    return(model_syntax)
  }
  
  lines <- strsplit(model_syntax, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  modified_lines <- character()
  for (line in lines) {
    if (grepl("=~", line)) {
      # This is a factor definition line
      parts <- strsplit(line, "=~")[[1]]
      if (length(parts) == 2) {
        factor_name <- trimws(parts[1])
        indicators_text <- trimws(parts[2])
        
        # Split indicators by + and clean them
        indicators <- trimws(strsplit(indicators_text, "\\+")[[1]])
        # Remove coefficient specifications for comparison
        clean_indicators <- gsub("\\*.*$", "", indicators)
        clean_indicators <- gsub("\\s+", "", clean_indicators)
        
        # Remove the drop_item
        keep_mask <- clean_indicators != drop_item
        indicators <- indicators[keep_mask]
        
        if (length(indicators) == 0) {
          # If no indicators left, skip this factor (problematic!)
          warning(sprintf("Factor '%s' would have no indicators after dropping '%s'", 
                         factor_name, drop_item))
          return(NULL)
        }
        
        new_line <- paste(factor_name, "=~", paste(indicators, collapse = " + "))
        modified_lines <- c(modified_lines, new_line)
      } else {
        modified_lines <- c(modified_lines, line)
      }
    } else {
      modified_lines <- c(modified_lines, line)
    }
  }
  
  if (length(modified_lines) == 0) {
    return(NULL)
  }
  
  paste(modified_lines, collapse = "\n")
}

# Human-readable one-liner summarizing change in fit (UTF-8)
#' @title One-line delta summary
#' @description Human-readable one-liner summarizing change in fit measures
#' @param pre list or lavaan object for pre-screening fit measures
#' @param post list or lavaan object for post-screening fit measures
#' @return character string summarizing fit changes
#' @export
one_line_delta <- function(pre, post) {
  cfi_pre <- format_fit_measure(pre$cfi %||% NA, "cfi")
  cfi_post <- format_fit_measure(post$cfi %||% NA, "cfi")
  tli_pre <- format_fit_measure(pre$tli %||% NA, "tli") 
  tli_post <- format_fit_measure(post$tli %||% NA, "tli")
  rmsea_pre <- format_fit_measure(pre$rmsea %||% NA, "rmsea")
  rmsea_post <- format_fit_measure(post$rmsea %||% NA, "rmsea")
  srmr_pre <- format_fit_measure(pre$srmr %||% NA, "srmr")
  srmr_post <- format_fit_measure(post$srmr %||% NA, "srmr")
  
  sprintf(
    "CFI %s \u2192 %s; TLI %s \u2192 %s; RMSEA %s \u2192 %s; SRMR %s \u2192 %s",
    cfi_pre, cfi_post, tli_pre, tli_post, rmsea_pre, rmsea_post, srmr_pre, srmr_post
  )
}

# ASCII fallback if needed
one_line_delta_ascii <- function(pre, post) {
  cfi_pre <- format_fit_measure(pre$cfi %||% NA, "cfi")
  cfi_post <- format_fit_measure(post$cfi %||% NA, "cfi")
  tli_pre <- format_fit_measure(pre$tli %||% NA, "tli") 
  tli_post <- format_fit_measure(post$tli %||% NA, "tli")
  rmsea_pre <- format_fit_measure(pre$rmsea %||% NA, "rmsea")
  rmsea_post <- format_fit_measure(post$rmsea %||% NA, "rmsea")
  srmr_pre <- format_fit_measure(pre$srmr %||% NA, "srmr")
  srmr_post <- format_fit_measure(post$srmr %||% NA, "srmr")
  
  sprintf(
    "CFI %s -> %s; TLI %s -> %s; RMSEA %s -> %s; SRMR %s -> %s",
    cfi_pre, cfi_post, tli_pre, tli_post, rmsea_pre, rmsea_post, srmr_pre, srmr_post
  )
}

# Detect hierarchical model structure
#' @title Detect hierarchical model structure
#' @description Analyzes lavaan model syntax to detect hierarchical factor models
#' @param model character string with lavaan syntax
#' @return list with hierarchy information
#' @noRd
detect_model_hierarchy <- function(model) {
  if (!is.character(model) || length(model) == 0) {
    return(list(is_hierarchical = FALSE, levels = 1, factors = character(), higher_order = character()))
  }
  
  lines <- strsplit(model, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != "" & !grepl("^#", lines)]
  
  # Extract factor definitions (=~) and other relationships
  factor_lines <- lines[grepl("=~", lines)]
  
  # Parse factors and their indicators
  factors <- list()
  all_variables <- character()
  
  for (line in factor_lines) {
    parts <- strsplit(line, "=~")[[1]]
    if (length(parts) == 2) {
      factor_name <- trimws(parts[1])
      indicators_text <- trimws(parts[2])
      
      # Split indicators and clean them
      indicators <- trimws(strsplit(indicators_text, "\\+")[[1]])
      clean_indicators <- gsub("\\*.*$|\\s+", "", indicators)
      
      factors[[factor_name]] <- clean_indicators
      all_variables <- c(all_variables, clean_indicators)
    }
  }
  
  # Detect hierarchy: factors that load on other factors
  factor_names <- names(factors)
  higher_order_factors <- character()
  first_order_factors <- character()
  
  # Check if any factor's indicators are actually other factors
  for (factor_name in factor_names) {
    indicators <- factors[[factor_name]]
    # Check if indicators are other factor names
    factor_indicators <- intersect(indicators, factor_names)
    
    if (length(factor_indicators) > 0) {
      # This is a higher-order factor
      higher_order_factors <- c(higher_order_factors, factor_name)
      # The indicators that are factors become lower-order
      first_order_factors <- c(first_order_factors, factor_indicators)
    }
  }
  
  # Determine hierarchy levels
  is_hierarchical <- length(higher_order_factors) > 0
  levels <- if (is_hierarchical) 2 else 1
  
  # If we have higher-order factors, we might have multiple levels
  if (is_hierarchical) {
    # Check for third-order factors (factors of factors of factors)
    third_order <- character()
    for (ho_factor in higher_order_factors) {
      ho_indicators <- factors[[ho_factor]]
      if (any(ho_indicators %in% higher_order_factors)) {
        third_order <- c(third_order, ho_factor)
        levels <- max(levels, 3)
      }
    }
  }
  
  # Detect bifactor models (general factor + specific factors)
  is_bifactor <- FALSE
  general_factor <- NULL
  
  if (length(factor_names) > 1) {
    # Check if one factor loads on all or most indicators
    indicator_counts <- sapply(factors, length)
    max_indicators <- max(indicator_counts)
    
    # Potential general factor: loads on many indicators
    potential_general <- names(indicator_counts)[indicator_counts == max_indicators]
    
    if (length(potential_general) == 1) {
      general_indicators <- factors[[potential_general]]
      
      # Check if other factors share indicators with potential general factor
      shared_count <- 0
      for (other_factor in setdiff(factor_names, potential_general)) {
        other_indicators <- factors[[other_factor]]
        shared <- intersect(general_indicators, other_indicators)
        if (length(shared) > 0) shared_count <- shared_count + 1
      }
      
      # If general factor shares indicators with most other factors, it's bifactor
      if (shared_count >= length(factor_names) - 1) {
        is_bifactor <- TRUE
        general_factor <- potential_general
      }
    }
  }
  
  # Return comprehensive hierarchy information
  list(
    is_hierarchical = is_hierarchical,
    is_bifactor = is_bifactor,
    levels = levels,
    factors = factor_names,
    higher_order = higher_order_factors,
    first_order = setdiff(factor_names, higher_order_factors),
    general_factor = general_factor,
    factor_structure = factors,
    total_factors = length(factor_names),
    total_indicators = length(unique(all_variables))
  )
}

# Build an APA-style table (returns a gt table if gt is available, else data.frame)
#' @title APA-style fit comparison table
#' @description Create an APA-style table comparing pre and post screening fit measures
#' @param pre list or lavaan object for pre-screening fit measures
#' @param post list or lavaan object for post-screening fit measures  
#' @return gt table if gt package available, otherwise data.frame
#' @export
apa_fit_table <- function(pre, post) {
  # Check if pre and post are valid lavaan objects or lists of fit measures
  is_valid_fit <- function(fit) {
    (inherits(fit, "lavaan") && lavaan::lavInspect(fit, "converged")) ||
      (is.list(fit) && !is.null(fit$cfi))
  }
  
  # Define measures to extract - consistent order
  measures <- c("cfi", "tli", "rmsea", "srmr", "aic", "bic", "chisq", "df", "pvalue")
  
  # Extract measures for one stage, ensuring consistent structure
  extract_measures <- function(fit, stage_name) {
    if (is.null(fit) || length(fit) == 0 || !is_valid_fit(fit)) {
      # Return NA for all measures if fit object is empty/invalid
      result <- setNames(rep(NA, length(measures)), measures)
    } else if (is.list(fit) && !is.null(fit$cfi)) {
      # Already a list of fit measures - extract what we need
      result <- sapply(measures, function(m) fit[[m]] %||% NA, simplify = TRUE)
    } else {
      # Extract fit measures from lavaan object
      result <- tryCatch({
        as.list(lavaan::fitMeasures(fit, measures))
      }, error = function(e) {
        setNames(rep(NA, length(measures)), measures)
      })
      result <- sapply(measures, function(m) result[[m]] %||% NA, simplify = TRUE)
    }
    
    # Create a single-row data frame
    row_data <- c(Stage = stage_name, as.list(result))
    data.frame(row_data, stringsAsFactors = FALSE, check.names = FALSE)
  }
  
  # Create consistent rows
  df_list <- list()
  df_list[["pre"]] <- extract_measures(pre, "Pre-screening")
  df_list[["post"]] <- extract_measures(post, "Post-screening")
  
  # Combine into final data frame
  df <- do.call(rbind, df_list)
  
  # If both stages have all NA values, return a message
  if (all(is.na(df[, measures]))) {
    return(data.frame(
      Stage = "Pre- and Post-screening",
      Note = "Model fitting failed or did not converge for both stages.",
      stringsAsFactors = FALSE
    ))
  }

  if (.pkg_available("gt")) {
    tb <- gt::gt(df)
    tb <- gt::tab_header(tb, title = "Pre vs Post Screening: Model Fit (APA-style)")
    return(tb)
  }
  df
}

# Export a gt table to DOCX (if officer/flextable available) or return FALSE
export_table_docx <- function(gt_or_df, file = "fit_table.docx") {
  if (.pkg_available("gt") && inherits(gt_or_df, "gt_tbl")) {
    if (.pkg_available("officer") && .pkg_available("flextable")) {
      # gt >= 0.10.0 provides as_flex_table()
      if (!is.function(gt::as_flex_table)) {
        message("gt::as_flex_table() not available; update 'gt' or export skipped.")
        return(FALSE)
      }
      ft <- gt::as_flex_table(gt_or_df)
      doc <- officer::read_docx()
      doc <- officer::body_add_flextable(doc, ft)
      print(doc, target = file)
      return(normalizePath(file))
    } else {
      message("officer/flextable not installed; cannot export DOCX.")
      return(FALSE)
    }
  } else {
    message("Input is not a gt table; export skipped.")
    return(FALSE)
  }
}

# Open a graphics device, call a plotting function, and save to disk
# type = 'png' or 'svg'
with_device <- function(filename, type = c("png","svg"), width = 8, height = 6, res = 150, code) {
  type <- match.arg(type)
  if (type == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = res)
  } else {
    grDevices::svg(filename, width = width, height = height)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
  normalizePath(filename)
}

# Deep merge of two nested lists
deep_merge <- function(x, y) {
  if (is.list(x) && is.list(y)) {
    nm <- union(names(x), names(y))
    setNames(lapply(nm, function(k) deep_merge(x[[k]], y[[k]])), nm)
  } else if (!is.null(y)) y else x
}

# Load a YAML config and merge with defaults from triage_rules()
#' @title Load configuration from YAML file
#' @description Load a YAML config and merge with defaults from triage_rules()
#' @param path character path to YAML config file
#' @return list of merged configuration parameters
#' @export
config_from_yaml <- function(path) {
  stopifnot(file.exists(path))
  if (!requireNamespace("yaml", quietly = TRUE))
    stop("Please install the 'yaml' package.")
  y <- yaml::read_yaml(path)
  base <- triage_rules(preset = (y$preset %||% "balanced"))
  y$preset <- NULL
  deep_merge(base, y)
}

# ============================================================================
# HIERARCHICAL MODEL FUNCTIONS
# ============================================================================

#' @title Detect higher-order model structure
#' @description Check if a lavaan model contains higher-order factors
#' @param model character lavaan model syntax
#' @param dat data.frame (optional, for additional validation)
#' @return logical indicating if model contains higher-order factors
#' @export
is_higher_order_model <- function(model, dat = NULL) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan package required for hierarchical model detection")
  }
  
  param_table <- try(lavaan::lavaanify(model, fixed.x = FALSE), silent = TRUE)
  if (inherits(param_table, "try-error")) {
    return(FALSE)
  }
  
  # Get all measurement relations (=~)
  measurement_relations <- subset(param_table, op == "=~")
  if (nrow(measurement_relations) == 0) {
    return(FALSE)
  }
  
  # Extract all latent variables (lhs of =~ relations)
  all_latents <- unique(measurement_relations$lhs)
  
  # Check if any latent variable appears as rhs in another =~ relation
  # This indicates a higher-order structure
  higher_order_indicators <- measurement_relations$rhs %in% all_latents
  
  any(higher_order_indicators)
}

#' @title Create lower-order equivalent model
#' @description Convert higher-order model to lower-order equivalent by removing higher-order factors
#' @param model character lavaan model syntax for higher-order model
#' @param dat data.frame (optional, for validation)
#' @return character lavaan syntax for lower-order equivalent model
#' @export
lower_order_equivalent <- function(model, dat = NULL) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan package required for hierarchical model processing")
  }
  
  param_table <- try(lavaan::lavaanify(model, fixed.x = FALSE), silent = TRUE)
  if (inherits(param_table, "try-error")) {
    stop("Invalid model syntax")
  }
  
  # Get measurement relations
  measurement_relations <- subset(param_table, op == "=~")
  all_latents <- unique(measurement_relations$lhs)
  
  # Identify higher-order relations (where rhs is a latent variable)
  higher_order_rows <- measurement_relations$rhs %in% all_latents
  
  # Remove higher-order relations, keep only first-order measurement relations
  first_order_measurement <- measurement_relations[!higher_order_rows, ]
  
  # Keep other relations (regressions, correlations, etc.)
  other_relations <- subset(param_table, op != "=~")
  
  # Filter out any relations involving higher-order factors
  higher_order_factors <- unique(measurement_relations$lhs[measurement_relations$rhs %in% all_latents])
  
  # Remove relations involving higher-order factors
  other_relations <- other_relations[
    !(other_relations$lhs %in% higher_order_factors) & 
    !(other_relations$rhs %in% higher_order_factors), 
  ]
  
  # Reconstruct model syntax
  lower_order_syntax <- character()
  
  # Add first-order measurement relations
  for (factor in unique(first_order_measurement$lhs)) {
    indicators <- first_order_measurement$rhs[first_order_measurement$lhs == factor]
    lower_order_syntax <- c(lower_order_syntax, 
                           paste0("  ", factor, " =~ ", paste(indicators, collapse = " + ")))
  }
  
  # Add other relations (regressions, correlations)
  if (nrow(other_relations) > 0) {
    for (i in 1:nrow(other_relations)) {
      rel <- other_relations[i, ]
      if (rel$op == "~") {
        lower_order_syntax <- c(lower_order_syntax, paste0("  ", rel$lhs, " ~ ", rel$rhs))
      } else if (rel$op == "~~" && rel$lhs != rel$rhs) {
        lower_order_syntax <- c(lower_order_syntax, paste0("  ", rel$lhs, " ~~ ", rel$rhs))
      }
    }
  }
  
  paste(lower_order_syntax, collapse = "\n")
}

#' @title Compare hierarchical vs lower-order models
#' @description Compare higher-order model against its lower-order equivalent
#' @param dat data.frame containing the data
#' @param model_lower character lavaan syntax for lower-order model
#' @param model_higher character lavaan syntax for higher-order model
#' @param config list of configuration parameters
#' @param estimator character estimator to use (default "MLR")
#' @param validate logical whether to perform cross-validation
#' @return list containing comparison results
#' @export
sem_hierarchy_compare <- function(dat, model_lower, model_higher, config, estimator = "MLR", validate = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan package required for hierarchical model comparison")
  }
  
  # Fit both models
  fit_lower <- try(lavaan::cfa(model_lower, data = dat, estimator = estimator), silent = TRUE)
  fit_higher <- try(lavaan::cfa(model_higher, data = dat, estimator = estimator), silent = TRUE)
  
  # Check convergence
  if (inherits(fit_lower, "try-error") || !lavaan::lavInspect(fit_lower, "converged")) {
    return(list(
      status = "lower_order_failed",
      recommendation = "Lower-order model failed to converge",
      comparison_table = NULL
    ))
  }
  
  if (inherits(fit_higher, "try-error") || !lavaan::lavInspect(fit_higher, "converged")) {
    return(list(
      status = "higher_order_failed", 
      recommendation = "Higher-order model failed to converge",
      comparison_table = NULL
    ))
  }
  
  # Extract fit measures
  measures <- c("cfi", "tli", "rmsea", "srmr", "aic", "bic")
  fit_lower_measures <- lavaan::fitMeasures(fit_lower, measures)
  fit_higher_measures <- lavaan::fitMeasures(fit_higher, measures)
  
  # Calculate deltas
  delta_cfi <- fit_higher_measures["cfi"] - fit_lower_measures["cfi"]
  delta_tli <- fit_higher_measures["tli"] - fit_lower_measures["tli"] 
  delta_rmsea <- fit_higher_measures["rmsea"] - fit_lower_measures["rmsea"]
  delta_srmr <- fit_higher_measures["srmr"] - fit_lower_measures["srmr"]
  delta_bic <- fit_higher_measures["bic"] - fit_lower_measures["bic"]
  
  # Apply no-material-harm tolerance
  # Higher-order should not harm fit by more than these tolerances
  harm_tolerance <- list(
    cfi = -0.005,    # CFI should not drop more than 0.005
    tli = -0.005,    # TLI should not drop more than 0.005  
    rmsea = 0.005,   # RMSEA should not increase more than 0.005
    srmr = 0.005     # SRMR should not increase more than 0.005
  )
  
  # Check if higher-order model passes harm tolerance
  no_material_harm <- (delta_cfi >= harm_tolerance$cfi) &&
                      (delta_tli >= harm_tolerance$tli) &&
                      (delta_rmsea <= harm_tolerance$rmsea) &&
                      (delta_srmr <= harm_tolerance$srmr)
  
  # Determine recommendation
  if (!no_material_harm) {
    recommendation <- "Use lower-order model (higher-order causes material harm to fit)"
    preferred_model <- "lower_order"
  } else if (delta_bic < 0) {
    recommendation <- "Use higher-order model (better parsimony, no material harm)"
    preferred_model <- "higher_order"
  } else {
    recommendation <- "Use lower-order model (equivalent fit, better parsimony)"
    preferred_model <- "lower_order" 
  }
  
  # Create comparison table
  comparison_df <- data.frame(
    Model = c("Lower-Order", "Higher-Order", "Delta (Higher - Lower)"),
    CFI = c(format_fit_measure(fit_lower_measures["cfi"], "cfi"),
            format_fit_measure(fit_higher_measures["cfi"], "cfi"),
            format_change(delta_cfi, "cfi")),
    TLI = c(format_fit_measure(fit_lower_measures["tli"], "tli"),
            format_fit_measure(fit_higher_measures["tli"], "tli"),
            format_change(delta_tli, "tli")),
    RMSEA = c(format_fit_measure(fit_lower_measures["rmsea"], "rmsea"),
              format_fit_measure(fit_higher_measures["rmsea"], "rmsea"),
              format_change(delta_rmsea, "rmsea")),
    SRMR = c(format_fit_measure(fit_lower_measures["srmr"], "srmr"),
             format_fit_measure(fit_higher_measures["srmr"], "srmr"),
             format_change(delta_srmr, "srmr")),
    BIC = c(format_fit_measure(fit_lower_measures["bic"], "bic"),
            format_fit_measure(fit_higher_measures["bic"], "bic"),
            format_change(delta_bic, "bic")),
    stringsAsFactors = FALSE
  )
  
  # Format as gt table if available
  if (requireNamespace("gt", quietly = TRUE)) {
    comparison_table <- gt::gt(comparison_df)
    comparison_table <- gt::tab_header(comparison_table, 
                                      title = "Hierarchical Model Comparison")
  } else {
    comparison_table <- comparison_df
  }
  
  # Cross-validation if requested
  cv_result <- NULL
  if (validate && nrow(dat) >= 100) {  # Only if sufficient sample size
    cv_result <- "Cross-validation skipped (not implemented in this version)"
  }
  
  list(
    status = "comparison_complete",
    preferred_model = preferred_model,
    recommendation = recommendation,
    comparison_table = comparison_table,
    no_material_harm = no_material_harm,
    delta_measures = list(
      cfi = delta_cfi,
      tli = delta_tli, 
      rmsea = delta_rmsea,
      srmr = delta_srmr,
      bic = delta_bic
    ),
    cv_result = cv_result
  )
}

#' @title Conditional hierarchical model analysis
#' @description Analyze hierarchical model structure if detected, otherwise return NULL
#' @param dat data.frame containing the data
#' @param model character lavaan model syntax
#' @param config list of configuration parameters
#' @param estimator character estimator to use (default "MLR")
#' @param validate logical whether to perform cross-validation
#' @return list of hierarchical analysis results or NULL if not higher-order
#' @export
sem_maybe_hierarchy <- function(dat, model, config, estimator = "MLR", validate = TRUE) {
  # Check if model is higher-order
  if (!is_higher_order_model(model, dat)) {
    return(NULL)
  }
  
  # Generate lower-order equivalent
  model_lower <- try(lower_order_equivalent(model, dat), silent = TRUE)
  if (inherits(model_lower, "try-error")) {
    return(list(
      status = "lower_order_generation_failed",
      recommendation = "Could not generate lower-order equivalent"
    ))
  }
  
  # Compare models
  comparison <- sem_hierarchy_compare(dat, model_lower, model, config, estimator, validate)
  
  # Add model specifications to result
  comparison$model_higher <- model
  comparison$model_lower <- model_lower
  
  comparison
}