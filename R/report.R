#' Export a compact HTML report with pre/post fit and action log
#'
#' If gt is available, includes an APA-like fit table. Otherwise plain HTML.
#'
#' @param dat data
#' @param model model syntax
#' @param res result from triage_apply
#' @param file output .html file path
#' @return normalized file path (invisible)
#' @export
export_sem_report <- function(dat, model, res, file = "sem_report.html") {
  if (!inherits(res, "semScreen_result")) {
    stop("res must be a semScreen_result object from triage_apply()")
  }
  pre  <- res$fit$pre  %||% list()
  post <- res$fit$post %||% list()

  # Generate fit table HTML with robust fallback
  table_html <- ""
  try({
    tb <- apa_fit_table(pre, post)
    df <- as.data.frame(tb)
    if (nrow(df) > 0) {
      # Build HTML table rows and header
      rows <- apply(df, 1, function(r) {
        cells <- sapply(r, function(x) if (is.na(x)) "--" else format(x, digits = 3))
        paste0("<tr>", paste0(sprintf("<td>%s</td>", cells), collapse=""), "</tr>")
      })
      header <- paste0("<tr>", paste0(sprintf("<th>%s</th>", names(df)), collapse=""), "</tr>")
      table_html <- paste0(
        "<table border='1' style='border-collapse: collapse; width: 100%;'>",
        header,
        paste0(rows, collapse=""),
        "</table>"
      )
    } else {
      table_html <- "<p>No fit measures available.</p>"
    }
  }, silent = TRUE)
  # Ensure fallback if any error occurs
  if (table_html == "" || grepl("Error generating fit table", table_html, fixed = TRUE)) {
    table_html <- "<p>No fit measures available.</p>"
  }

  # Generate action log
  hist_html <- tryCatch({
    if (length(res$history) == 0) {
      "<p>No actions were taken during screening.</p>"
    } else {
      hist_items <- sapply(res$history, function(x) {
        paste0(
          "<li><strong>Step ", x$step, ":</strong> ",
          x$type, 
          if (!is.null(x$item)) paste0(" (", x$item, ")") else "",
          " - ", x$note %||% "No details",
          "</li>"
        )
      })
      paste0("<ul>", paste0(hist_items, collapse=""), "</ul>")
    }
  }, error = function(e) {
    paste0("<p>Error generating action log: ", e$message, "</p>")
  })
  
  # Generate summary statistics
  summary_html <- tryCatch({
    components <- c(
      "<ul>",
      paste0("<li><strong>Status:</strong> ", res$status, "</li>"),
      paste0("<li><strong>Original sample size:</strong> ", nrow(dat), "</li>"),
      paste0("<li><strong>Final sample size:</strong> ", nrow(res$data_final), "</li>"),
      paste0("<li><strong>Actions taken:</strong> ", length(res$history), "</li>"),
      if (length(pre) > 0 && !is.null(pre$cfi)) {
        paste0("<li><strong>Pre-screening CFI:</strong> ", 
               format_fit_measure(pre$cfi %||% NA, "cfi"), "</li>")
      } else "",
      if (length(post) > 0 && !is.null(post$cfi)) {
        paste0("<li><strong>Post-screening CFI:</strong> ", 
               format_fit_measure(post$cfi %||% NA, "cfi"), "</li>")
      } else "",
      "</ul>"
    )
    paste0(components, collapse = "")
  }, error = function(e) {
    paste0("<p>Error generating summary: ", e$message, "</p>")
  })

  # Check for hierarchical model structure and generate conditional section
  hierarchy_html <- tryCatch({
    # Apply hierarchical model analysis if applicable
    hierarchy_result <- sem_maybe_hierarchy(dat, model, 
                                           config = triage_rules("balanced"), 
                                           estimator = "MLR", 
                                           validate = TRUE)
    
    if (!is.null(hierarchy_result) && hierarchy_result$status == "comparison_complete") {
      # Generate hierarchical model section HTML
      section_html <- paste0(
        "<h2>Hierarchical Model Analysis</h2>",
        "<div class='alert alert-info'>",
        "<strong>Higher-Order Model Detected:</strong> Your model contains hierarchical factor structure. ",
        "semScreenR automatically compared your higher-order model against a lower-order equivalent.",
        "</div>",
        
        "<h3>Model Comparison</h3>"
      )
      
      # Add comparison table
      if (!is.null(hierarchy_result$comparison_table)) {
        comparison_df <- as.data.frame(hierarchy_result$comparison_table)
        if (nrow(comparison_df) > 0) {
          comp_rows <- apply(comparison_df, 1, function(r) {
            cells <- sapply(r, function(x) if (is.na(x)) "--" else as.character(x))
            paste0("<tr>", paste0(sprintf("<td>%s</td>", cells), collapse=""), "</tr>")
          })
          comp_header <- paste0("<tr>", paste0(sprintf("<th>%s</th>", names(comparison_df)), collapse=""), "</tr>")
          comp_table_html <- paste0(
            "<table border='1' style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
            comp_header,
            paste0(comp_rows, collapse=""),
            "</table>"
          )
          section_html <- paste0(section_html, comp_table_html)
        }
      }
      
      # Add recommendation
      paste0(
        section_html,
        "<h3>Recommendation</h3>",
        "<div class='alert ", 
        if (hierarchy_result$preferred_model == "higher_order") "alert-success" else "alert-warning",
        "'>",
        "<strong>Recommendation:</strong> ", hierarchy_result$recommendation,
        "</div>",
        
        "<h3>Interpretation Guidelines</h3>",
        "<ul>",
        "<li><strong>No Material Harm:</strong> ", 
        if (hierarchy_result$no_material_harm) "[PASS] Passed" else "[FAIL] Failed",
        " (Higher-order model does not substantially worsen fit)</li>",
        "<li><strong>Model Complexity:</strong> Higher-order models are more parsimonious but may obscure factor-specific relationships</li>",
        "<li><strong>Theoretical Fit:</strong> Choose the model that best matches your theoretical framework</li>",
        "</ul>"
      )
    } else {
      ""
    }
  }, error = function(e) {
    # If hierarchical analysis fails, continue without it
    ""
  })

  # Ensure all components are character before combining
  summary_html <- as.character(summary_html)
  table_html <- as.character(table_html)
  hist_html <- as.character(hist_html)
  hierarchy_html <- as.character(hierarchy_html)

  # Create complete HTML document
  html <- c(
    "<!DOCTYPE html>",
    "<html><head>",
    "<meta charset='utf-8'>",
    "<title>semScreenR Report</title>",
    "<style>",
    "body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; margin: 2rem; }",
    "h1, h2 { margin: 1rem 0 0.5rem 0; color: #333; }",
    "table { border-collapse: collapse; width: 100%; margin: 1rem 0; }",
    "th, td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }",
    "th { background: #f5f5f5; font-weight: bold; }",
    "ul { margin: 0.5rem 0; }",
    "li { margin: 0.25rem 0; }",
    ".alert { padding: 1rem; margin: 1rem 0; border-radius: 4px; }",
    ".alert-info { background: #d1ecf1; border: 1px solid #bee5eb; color: #0c5460; }",
    ".alert-success { background: #d4edda; border: 1px solid #c3e6cb; color: #155724; }",
    ".alert-warning { background: #fff3cd; border: 1px solid #ffeaa7; color: #856404; }",
    "</style>",
    "</head><body>",
    "<h1>semScreenR Analysis Report</h1>",
    
    "<h2>Summary</h2>",
    summary_html,
    
    "<h2>Model Fit Comparison</h2>",
    table_html,
    
    "<h2>Action Log</h2>",
    hist_html,
    
    # Add hierarchical model section if present
    if (nchar(hierarchy_html) > 0) hierarchy_html else "",
    
    "<div class='alert alert-info'>",
    "<strong>Note:</strong> This report was generated by semScreenR. ",
    "All screening decisions were made using pre-registered rules and validation procedures. ",
    "For reproducibility, please cite your screening parameters in any publications.",
    "</div>",
    
    "<hr>",
    as.character(paste0("<p><small>Generated on ", as.character(Sys.time()), " using semScreenR version ", 
           as.character(utils::packageVersion("semScreenR") %||% "unknown"), "</small></p>")),
    
    "</body></html>"
  )

  # Write file with error handling
  tryCatch({
    writeLines(html, file)
    message("Report exported to: ", normalizePath(file))
  }, error = function(e) {
    stop("Failed to write report file: ", e$message)
  })
  
  invisible(normalizePath(file))
}
