#' Internal helpers and light-weight utilities
#' @keywords internal
NULL

# Safe NULL coalescing (keep only here; remove duplicates elsewhere)
`%||%` <- function(a, b) if (is.null(a)) b else a

# Is a package installed and loadable?
.pkg_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

# Fit a CFA/SEM model safely and return NULL on error
.safe_fit <- function(model, data, estimator = NULL) {
  if (!.pkg_available("lavaan")) {
    message("lavaan is not installed; returning NULL fit.")
    return(NULL)
  }
  args <- list(model = model, data = data, warn = FALSE)
  if (!is.null(estimator)) args$estimator <- estimator
  fit <- try(do.call(lavaan::cfa, args), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
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

# Extract variable names from lavaan model syntax
extract_model_variables <- function(model_syntax) {
  if (is.null(model_syntax) || !is.character(model_syntax) || length(model_syntax) == 0) {
    return(character(0))
  }
  
  lines <- strsplit(model_syntax, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[lines != "" & !startsWith(lines, "#")]
  
  vars <- character()
  for (line in lines) {
    if (grepl("=~", line)) {
      # Factor definition
      parts <- strsplit(line, "=~")[[1]]
      if (length(parts) == 2) {
        indicators <- trimws(strsplit(parts[2], "\\+")[[1]])
        vars <- c(vars, indicators)
      }
    } else if (grepl("~", line) && !grepl("=~", line)) {
      # Regression
      parts <- strsplit(line, "~")[[1]]
      if (length(parts) == 2) {
        lhs <- trimws(parts[1])
        rhs <- trimws(strsplit(parts[2], "\\+")[[1]])
        vars <- c(vars, lhs, rhs)
      }
    }
  }
  
  # Clean variable names (remove coefficients, constraints, etc.)
  vars <- gsub("\\*.*$", "", vars)  # Remove coefficient specifications
  vars <- gsub("\\s+", "", vars)   # Remove whitespace
  vars <- vars[vars != ""]
  unique(vars)
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
one_line_delta <- function(pre, post) {
  sprintf(
    "CFI %.3f \u2192 %.3f; TLI %.3f \u2192 %.3f; RMSEA %.3f \u2192 %.3f; SRMR %.3f \u2192 %.3f",
    pre$cfi %||% NA, post$cfi %||% NA,
    pre$tli %||% NA, post$tli %||% NA,
    pre$rmsea %||% NA, post$rmsea %||% NA,
    pre$srmr %||% NA, post$srmr %||% NA
  )
}

# ASCII fallback if needed
one_line_delta_ascii <- function(pre, post) {
  sprintf(
    "CFI %.3f -> %.3f; TLI %.3f -> %.3f; RMSEA %.3f -> %.3f; SRMR %.3f -> %.3f",
    pre$cfi %||% NA, post$cfi %||% NA,
    pre$tli %||% NA, post$tli %||% NA,
    pre$rmsea %||% NA, post$rmsea %||% NA,
    pre$srmr %||% NA, post$srmr %||% NA
  )
}

# Build an APA-style table (returns a gt table if gt is available, else data.frame)
apa_fit_table <- function(pre, post) {
  df <- data.frame(
    Stage = c("Pre-screening", "Post-screening"),
    CFI   = c(pre$cfi %||% NA,   post$cfi %||% NA),
    TLI   = c(pre$tli %||% NA,   post$tli %||% NA),
    RMSEA = c(pre$rmsea %||% NA, post$rmsea %||% NA),
    `RMSEA 90% CI` = c(
      if (!is.null(pre$rmsea.ci.lower) && !is.null(pre$rmsea.ci.upper))
        sprintf("[%.3f, %.3f]", pre$rmsea.ci.lower, pre$rmsea.ci.upper) else NA,
      if (!is.null(post$rmsea.ci.lower) && !is.null(post$rmsea.ci.upper))
        sprintf("[%.3f, %.3f]", post$rmsea.ci.lower, post$rmsea.ci.upper) else NA
    ),
    SRMR  = c(pre$srmr %||% NA,  post$srmr %||% NA),
    AIC   = c(pre$aic %||% NA,   post$aic %||% NA),
    BIC   = c(pre$bic %||% NA,   post$bic %||% NA),
    Chisq = c(pre$chisq %||% NA, post$chisq %||% NA),
    df    = c(pre$df %||% NA,    post$df %||% NA),
    p     = c(pre$pvalue %||% NA,post$pvalue %||% NA),
    check.names = FALSE
  )

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
config_from_yaml <- function(path) {
  stopifnot(file.exists(path))
  if (!requireNamespace("yaml", quietly = TRUE))
    stop("Please install the 'yaml' package.")
  y <- yaml::read_yaml(path)
  base <- triage_rules(preset = (y$preset %||% "balanced"))
  y$preset <- NULL
  deep_merge(base, y)
}