#' Internal helpers and light-weight utilities
#' @keywords internal
NULL

# Safe NULL coalescing (keep only here; remove duplicates elsewhere)
`%||%` <- function(a, b) if (is.null(a)) b else a

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
    # It finds words on the right side of =~, ~, or ~~ operators.
    all_vars <- unlist(strsplit(model_syntax, "\\s*(=~|~|~~)\\s*"))
    # Remove the latent variable definitions
    latent_vars <- unique(param_table$lhs[param_table$op == "=~"])
    manifest_vars <- all_vars[!all_vars %in% latent_vars]
    # Clean up and get unique names
    manifest_vars <- gsub("[^[:alnum:]_.]", " ", manifest_vars)
    manifest_vars <- unlist(strsplit(manifest_vars, "\\s+"))
    return(unique(manifest_vars[manifest_vars != ""]))
  }
  
  # Get all unique names appearing in the table
  all_names <- unique(c(param_table$lhs, param_table$rhs))
  
  # Identify latent variables (those on the LHS of "=~")
  latent_vars <- unique(param_table$lhs[param_table$op == "=~"])
  
  # Manifest variables are all names that are not latent variables
  manifest_vars <- all_names[!(all_names %in% latent_vars) & all_names != ""]
  
  return(manifest_vars)
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
#' @return data.frame of fit measures
#' @noRd
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
config_from_yaml <- function(path) {
  stopifnot(file.exists(path))
  if (!requireNamespace("yaml", quietly = TRUE))
    stop("Please install the 'yaml' package.")
  y <- yaml::read_yaml(path)
  base <- triage_rules(preset = (y$preset %||% "balanced"))
  y$preset <- NULL
  deep_merge(base, y)
}