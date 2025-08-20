library(shiny)
library(shinyjs)
library(DT)

ui <- function(request) {
  fluidPage(
    useShinyjs(),   # enables shinyjs functions
    
    # Custom CSS for better help content styling
    tags$head(
      tags$style(HTML("
        .help-content h1 {
          color: #2c3e50;
          border-bottom: 3px solid #3498db;
          padding-bottom: 10px;
          margin-top: 30px;
          margin-bottom: 20px;
        }
        
        .help-content h2 {
          color: #34495e;
          border-left: 4px solid #3498db;
          padding-left: 15px;
          margin-top: 25px;
          margin-bottom: 15px;
        }
        
        .help-content h3 {
          color: #2c3e50;
          margin-top: 20px;
          margin-bottom: 10px;
        }
        
        .help-content h4, .help-content h5, .help-content h6 {
          color: #34495e;
          margin-top: 15px;
          margin-bottom: 8px;
        }
        
        .help-content p {
          margin-bottom: 12px;
          text-align: justify;
        }
        
        .help-content ul, .help-content ol {
          margin-bottom: 15px;
          padding-left: 25px;
        }
        
        .help-content li {
          margin-bottom: 5px;
        }
        
        .help-content code {
          background-color: #f8f9fa;
          color: #e74c3c;
          padding: 2px 6px;
          border-radius: 3px;
          font-family: 'Courier New', monospace;
          font-size: 0.9em;
        }
        
        .help-content pre {
          background-color: #2c3e50;
          color: #ecf0f1;
          padding: 15px;
          border-radius: 5px;
          overflow-x: auto;
          margin: 15px 0;
        }
        
        .help-content pre code {
          background-color: transparent;
          color: inherit;
          padding: 0;
        }
        
        .help-content a {
          color: #3498db;
          text-decoration: none;
          border-bottom: 1px solid transparent;
          transition: border-bottom 0.3s ease;
        }
        
        .help-content a:hover {
          border-bottom: 1px solid #3498db;
        }
        
        .help-content hr {
          border: none;
          border-top: 2px solid #bdc3c7;
          margin: 25px 0;
        }
        
        .help-content blockquote {
          border-left: 4px solid #3498db;
          padding-left: 15px;
          margin: 15px 0;
          font-style: italic;
          color: #7f8c8d;
        }
        
        .help-content .emoji {
          font-size: 1.2em;
          margin-right: 5px;
        }
        
        .toc-container {
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 8px;
          padding: 20px;
          margin-bottom: 25px;
        }
        
        .toc-container h3 {
          margin-top: 0;
          color: #495057;
          border-bottom: 2px solid #007bff;
          padding-bottom: 10px;
        }
        
        .toc-link {
          color: #495057;
          text-decoration: none;
          display: block;
          padding: 3px 0;
          transition: color 0.3s ease;
        }
        
        .toc-link:hover {
          color: #007bff;
          text-decoration: underline;
        }
        
        .help-content {
          scroll-behavior: smooth;
        }
      "))
    ),
    
    # JavaScript for smooth scrolling and TOC functionality
    tags$script(HTML("
      $(document).ready(function() {
        // Smooth scrolling for TOC links
        $('.toc-link').on('click', function(e) {
          e.preventDefault();
          var target = $(this).attr('href');
          var $target = $(target);
          if ($target.length) {
            $('.help-content').animate({
              scrollTop: $target.offset().top - 100
            }, 800);
          }
        });
      });
    ")),
    
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
        
        # Dynamic startup guide and feedback
        div(
          id = "startup_guide",
          style = "background-color: #ffffff; border: 1px solid #dee2e6; padding: 10px; margin-bottom: 15px; border-radius: 5px;",
          h4("🚀 Getting Started"),
          htmlOutput("dynamic_guide"),
          verbatimTextOutput("package_status_text"),
          conditionalPanel(
            condition = "output.webr_detected",
            div(
              style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 8px; margin-top: 10px; border-radius: 3px;",
              p("🌐 WebR environment detected. Custom OCS fallback will be used since optiSel is not available in WebR.")
            )
          ),
          div(
            style = "text-align: center; margin-top: 15px; padding-top: 10px; border-top: 1px solid #dee2e6;",
            actionButton("help_btn", "❓ Help", 
                        style = "background-color: #007bff; color: white; border: none; padding: 8px 16px; border-radius: 5px;")
          )
        ),
        
        wellPanel(
          style = "background-color: #e3f2fd; border: 2px solid #2196f3; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          h4("🧬 Core Data Inputs", style = "color: #1565c0; margin-bottom: 15px; border-bottom: 1px solid #2196f3; padding-bottom: 8px;"),
          p("These inputs are used by both Index Generation and OCS calculations:", style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"),
          
          h5("Estimate progeny genetic merit"),
          
          fileInput("candidate_file", "Upload list of candidates",
                    accept = c(".csv", ".txt")),
          
          h5("Calculate kinship matrix"),
          fileInput("pedigree_file", "Upload pedigree file", accept = ".txt"),
          
          h5("Set kinship threshold"),
          numericInput("thresh", "Max kinship allowed between mates:",
                      value = 1, min = 0, max = 1, step = 0.1),
        ),
        
        wellPanel(
          style = "background-color: #ffebee; border: 2px solid #f44336; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          h4("⚖️ Weighted EBVs", style = "color: #c62828; margin-bottom: 15px; border-bottom: 1px solid #f44336; padding-bottom: 8px;"),
          p("Define traits and their relative importance for breeding decisions:", style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"),
          
          h5("Traits (EBVs and weights)"),
          uiOutput("trait_inputs"),
          
          fluidRow(
            column(6, actionButton("add_trait", "➕ Add trait")),
            column(6, actionButton("remove_trait", "➖ Remove trait"))
          ),
          p("💡 Note: Adding or removing traits will require re-uploading files.", 
            style = "color: #6c757d; font-size: 11px; font-style: italic; margin-top: 8px;"),
        ),
        
        wellPanel(
          style = "background-color: #fff3cd; border: 2px solid #ffeaa7; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          h4("🎯 Optimum Contribution Selection", style = "color: #856404; margin-bottom: 15px; border-bottom: 1px solid #ffeaa7; padding-bottom: 8px;"),
          p("Configure breeding objectives and constraints:", style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"),
          
          h5("Breeding Objectives"),
          numericInput("inbreeding_rate", "Desired Inbreeding Rate", 
                       value = 0.05, min = 0.01, max = 0.2, step = 0.01),
          numericInput("num_offspring", "Number of Offspring", 
                       value = 100, min = 10, step = 1),
          
          actionButton("run_ocs_btn", "Run OCS", 
                       style = "margin-top: 15px; width: 100%; background-color: #856404; color: white; border: none; padding: 10px; border-radius: 5px;")
        ),
        
        wellPanel(
          style = "background-color: #d4edda; border: 2px solid #c3e6cb; padding: 15px; margin-bottom: 20px; border-radius: 8px;",
          h4("📊 Export Results", style = "color: #155724; margin-bottom: 15px; border-bottom: 1px solid #c3e6cb; padding-bottom: 8px;"),
          p("Download all results in a single Excel file with multiple tabs:", style = "color: #6c757d; font-size: 12px; margin-bottom: 15px;"),
          downloadButton("download_all_results", "📥 Export All Results", 
                         style = "width: 100%; background-color: #28a745; color: white; border: none; padding: 10px; border-radius: 5px;")
        )
      ),
      mainPanel(
        tabsetPanel(
          id = "main_tabs",
          tabPanel("Kinship and EBV",
                   verbatimTextOutput("message1"),
                   
                   verbatimTextOutput("message2"),
                   
                   DTOutput("quadrants_table"),
                   DTOutput("matrix")
          ),
          tabPanel("Optimum Contribution Selection",
                   DTOutput("ocs_candidate_table"),
                   br(),
                   DTOutput("ocs_mating_table")
          ),
          tabPanel("Help",
                   div(
                     style = "padding: 20px; background-color: #f8f9fa; border-radius: 8px; margin: 10px 0; max-height: 80vh; overflow-y: auto; position: relative;",
                     div(
                       style = "background-color: white; padding: 25px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                       div(
                         style = "text-align: center; margin-bottom: 25px; padding-bottom: 15px; border-bottom: 2px solid #007bff;",
                         h2("📚 AlloMate Documentation", style = "color: #007bff; margin-bottom: 10px;"),
                         p("Complete user guide and technical documentation", style = "color: #666; font-size: 16px;")
                       ),
                       div(
                         style = "line-height: 1.6; font-size: 14px;",
                         htmlOutput("help_content")
                       ),
                       div(
                         style = "text-align: center; margin-top: 30px; padding-top: 20px; border-top: 1px solid #dee2e6;",
                         actionButton("back_to_top", "⬆️ Back to Top", 
                                     style = "background-color: #6c757d; color: white; border: none; padding: 8px 16px; border-radius: 5px;")
                       )
                     )
                   )
          )
        )
      )
    )
  )
}