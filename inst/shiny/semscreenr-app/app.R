library(shiny)

ui <- fluidPage(
  titlePanel("semScreenR"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("model", "lavaan model:", value = "
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
", rows = 8),
      selectInput("preset", "Rules preset", c("balanced","conservative","aggressive")),
      textInput("idcol", "ID column (optional)", value = "id"),
      actionButton("run", "Run triage")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      tableOutput("fit"),
      tableOutput("history")
    )
  )
)

server <- function(input, output, session) {
  dat <- NULL
  observe({
    if (requireNamespace("lavaan", quietly = TRUE)) {
      data("HolzingerSwineford1939", package = "lavaan")
      dat <<- HolzingerSwineford1939
    }
  })
  res <- eventReactive(input$run, {
    req(dat)
    cfg  <- triage_rules(input$preset)
    plan <- triage_plan(dat, input$model, id_cols = input$idcol, config = cfg)
    triage_apply(dat, input$model, plan)
  })
  output$status  <- renderPrint({ req(res()); res()$status })
  output$fit     <- renderTable({
    req(res())
    pre <- res()$fit$pre %||% list(); post <- res()$fit$post %||% list()
    data.frame(
      Stage = c("Pre","Post"),
      CFI = c(pre$cfi %||% NA, post$cfi %||% NA),
      TLI = c(pre$tli %||% NA, post$tli %||% NA),
      RMSEA = c(pre$rmsea %||% NA, post$rmsea %||% NA),
      SRMR = c(pre$srmr %||% NA, post$srmr %||% NA)
    )
  })
  output$history <- renderTable({
    req(res())
    h <- res()$history
    if (length(h) == 0) return(data.frame())
    do.call(rbind, lapply(h, function(x) {
      data.frame(step = x$step, type = x$type, item = x$item %||% NA, note = x$note)
    }))
  })
}

shinyApp(ui, server)