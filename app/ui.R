ui <- fluidPage(
  useShinyjs(),   # enables shinyjs functions
  
  ## ─── Flex container with banner and AlloMate hex ───────────────────────────
  div(
    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px;",
    
    # AlloMate hex (right)
    tags$img(
      src = "allomate.png",
      height = "120px",
      style = "margin-left: 20px;"
    ),
    # Logos banner (left)
    tags$img(
      src = "logos.png",
      style = "width: 70%; height: auto;"

    )
  ),
    sidebarLayout(
    sidebarPanel(
      h3("Estimate progeny genetic merit"),
      
      fileInput("candidate_file", "Upload list of candidates",
                accept = c(".csv", ".txt")),
      
      h4("Calculate kinship matrix"),
      fileInput("pedigree_file", "Upload pedigree file", accept = ".txt"),
      
      h4("Traits (EBVs and weights)"),
      uiOutput("trait_inputs"),
      
      fluidRow(
        column(6, actionButton("add_trait", "➕ Add trait")),
        column(6, actionButton("remove_trait", "➖ Remove trait"))
      ),
      
      br(),
      
      h4("Select max kinship allowed between mates"),
      numericInput("thresh", "Kinship threshold:",
                   value = 1, min = 0, max = 1, step = 0.1),
      
      h4("Export results"),
      downloadButton("download1", label = "Download Results"),
      br(), br()
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Kinship and EBV",
                 h5("User feedback for calculating kinship:"),
                 verbatimTextOutput("message1"),
                 
                 h5("User feedback for calculating EBVs:"),
                 verbatimTextOutput("message2"),
                 
                 DTOutput("quadrants_table"),
                 DTOutput("matrix")
        )
      )
    )
  )
)