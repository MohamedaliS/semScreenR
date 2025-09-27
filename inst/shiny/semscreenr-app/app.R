library(shiny)

ui <- fluidPage(
  titlePanel("semScreenR"),
  
  # Add error display
  conditionalPanel(
    condition = "output.show_error",
    div(class = "alert alert-danger", 
        strong("Error: "), textOutput("error_message", inline = TRUE))
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Check for required packages
      conditionalPanel(
        condition = "!output.packages_available",
        div(class = "alert alert-warning",
            p("Required packages not available:"),
            p("Please install: lavaan, semScreenR"),
            p("Optional: gt (for better tables)"))
      ),
      
      conditionalPanel(
        condition = "output.packages_available",
        textAreaInput("model", "lavaan model:", value = "
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
", rows = 8),
        selectInput("preset", "Rules preset", c("balanced","conservative","aggressive")),
        textInput("idcol", "ID column (optional)", value = "id"),
        numericInput("loading_min", "Minimum loading", value = 0.40, min = 0.1, max = 1.0, step = 0.05),
        checkboxInput("use_validation", "Use k-fold validation", value = TRUE),
        actionButton("run", "Run triage", class = "btn-primary")
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.packages_available",
        tabsetPanel(
          tabPanel("Results",
                   h3("Status"),
                   verbatimTextOutput("status"),
                   
                   h3("Model Fit Comparison"),
                   tableOutput("fit"),
                   
                   h3("Action History"),
                   tableOutput("history")
          ),
          
          tabPanel("Data Info",
                   h3("Dataset Information"),
                   verbatimTextOutput("data_info")
          ),
          
          tabPanel("Help",
                   h3("How to Use semScreenR"),
                   tags$div(
                     p("1. Enter your lavaan model syntax in the text area"),
                     p("2. Choose a preset (conservative/balanced/aggressive)"),
                     p("3. Optionally specify an ID column and minimum loading threshold"),
                     p("4. Click 'Run triage' to execute the screening"),
                     p("The app will show fit improvements and what items were removed."),
                     h4("Presets:"),
                     tags$ul(
                       tags$li("Conservative: Strict limits, min N=300, max 5% removals"),
                       tags$li("Balanced: Moderate limits, min N=200, max 10% removals"),
                       tags$li("Aggressive: Relaxed limits, min N=150, max 15% removals")
                     )
                   ))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Check package availability
  packages_ok <- reactive({
    semscreenr_ok <- requireNamespace("semScreenR", quietly = TRUE)
    lavaan_ok <- requireNamespace("lavaan", quietly = TRUE)
    list(semscreenr = semscreenr_ok, lavaan = lavaan_ok, 
         all_ok = semscreenr_ok && lavaan_ok)
  })
  
  output$packages_available <- reactive({
    packages_ok()$all_ok
  })
  outputOptions(output, "packages_available", suspendWhenHidden = FALSE)
  
  # Load data
  dat <- reactive({
    if (!packages_ok()$lavaan) return(NULL)
    tryCatch({
      data("HolzingerSwineford1939", package = "lavaan")
      HolzingerSwineford1939
    }, error = function(e) NULL)
  })
  
  output$data_info <- renderPrint({
    d <- dat()
    if (is.null(d)) {
      "No data available - lavaan package required"
    } else {
      cat("Dataset: HolzingerSwineford1939\n")
      cat("Dimensions:", nrow(d), "rows,", ncol(d), "columns\n")
      cat("Variables:", paste(names(d), collapse = ", "), "\n")
      cat("\nFirst few rows:\n")
      print(head(d))
    }
  })
  
  # Error handling
  error_state <- reactiveVal(NULL)
  
  output$show_error <- reactive({
    !is.null(error_state())
  })
  outputOptions(output, "show_error", suspendWhenHidden = FALSE)
  
  output$error_message <- renderText({
    error_state()
  })
  
  # Main analysis
  res <- eventReactive(input$run, {
    error_state(NULL)  # Clear previous errors
    
    if (!packages_ok()$all_ok) {
      error_state("Required packages not available")
      return(NULL)
    }
    
    d <- dat()
    if (is.null(d)) {
      error_state("Data not available")
      return(NULL)
    }
    
    tryCatch({
      cfg <- semScreenR::triage_rules(
        preset = input$preset,
        loading_min = input$loading_min
      )
      
      # Update validation setting
      cfg$gates$use_validation <- input$use_validation
      
      id_cols <- if (nzchar(input$idcol)) input$idcol else NULL
      plan <- semScreenR::triage_plan(d, input$model, id_cols = id_cols, config = cfg)
      
      semScreenR::triage_apply(d, input$model, plan)
    }, error = function(e) {
      error_state(paste("Analysis failed:", e$message))
      NULL
    })
  })
  
  output$status <- renderPrint({
    r <- res()
    if (is.null(r)) return("No results")
    cat("Status:", r$status, "\n")
    cat("Final sample size:", nrow(r$data_final), "\n")
    cat("Actions taken:", length(r$history), "\n")
  })
  
  output$fit <- renderTable({
    r <- res()
    if (is.null(r)) return(data.frame())
    
    pre <- r$fit$pre
    post <- r$fit$post
    
    # Handle empty lists
    if (length(pre) == 0) pre <- list(cfi = NA, tli = NA, rmsea = NA, srmr = NA)
    if (length(post) == 0) post <- list(cfi = NA, tli = NA, rmsea = NA, srmr = NA)
    
    data.frame(
      Stage = c("Pre-screening", "Post-screening"),
      CFI = c(pre$cfi %||% NA, post$cfi %||% NA),
      TLI = c(pre$tli %||% NA, post$tli %||% NA),
      RMSEA = c(pre$rmsea %||% NA, post$rmsea %||% NA),
      SRMR = c(pre$srmr %||% NA, post$srmr %||% NA),
      AIC = c(pre$aic %||% NA, post$aic %||% NA),
      BIC = c(pre$bic %||% NA, post$bic %||% NA)
    )
  }, digits = 3, na = "—")
  
  output$history <- renderTable({
    r <- res()
    if (is.null(r)) return(data.frame())
    
    h <- r$history
    if (length(h) == 0) {
      return(data.frame(Message = "No actions taken"))
    }
    
    history_df <- do.call(rbind, lapply(h, function(x) {
      data.frame(
        Step = x$step,
        Type = x$type,
        Item = x$item %||% "—",
        Kept = x$kept %||% "—",
        Note = substr(x$note %||% "", 1, 100)  # Truncate long notes
      )
    }))
    
    history_df
  }, striped = TRUE)
}

# Define the null coalescing operator for this context
`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)