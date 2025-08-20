# All packages are loaded in global.R
# optiSel availability is checked in global.R

# All functions are now loaded from the organized functions folder
# See functions/load_all_functions.R for details

#### Server Function ####

server <- function(input, output, session) {
  
  # Package status output
  output$package_status_text <- renderText({
    generate_package_status()
  })
  
  # WebR detection output
  output$webr_detected <- renderText({
    if (is_webr_environment()) {
      "WebR detected"
    } else {
      ""
    }
  })
  outputOptions(output, "webr_detected", suspendWhenHidden = FALSE)
  
  # Help button functionality
  observeEvent(input$help_btn, {
    updateTabsetPanel(session, "main_tabs", selected = "Help")
  })
  
  # Back to top functionality
  observeEvent(input$back_to_top, {
    runjs("document.querySelector('.help-content').scrollTop = 0;")
  })
  
  # Enhanced markdown to HTML conversion function
  markdown_to_html <- function(markdown_text) {
    # Split into lines for processing
    lines <- strsplit(markdown_text, "\n")[[1]]
    html_lines <- character(length(lines))
    in_code_block <- FALSE
    in_list <- FALSE
    list_type <- ""
    
    for (i in seq_along(lines)) {
      line <- lines[i]
      
      # Handle code blocks
      if (grepl("^```", line)) {
        if (!in_code_block) {
          html_lines[i] <- "<pre><code class='language-markdown'>"
          in_code_block <- TRUE
        } else {
          html_lines[i] <- "</code></pre>"
          in_code_block <- FALSE
        }
        next
      }
      
      if (in_code_block) {
        html_lines[i] <- paste0("<span>", line, "</span>")
        next
      }
      
      # Handle headers
      if (grepl("^# ", line)) {
        level <- nchar(gsub("^(#+).*", "\\1", line))
        content <- gsub("^#+ ", "", line)
        # Add emoji support for main headers
        if (level == 1) {
          content <- paste0("🧬 ", content)
        } else if (level == 2) {
          content <- paste0("🚀 ", content)
        } else if (level == 3) {
          content <- paste0("📁 ", content)
        } else if (level == 4) {
          content <- paste0("🛠️ ", content)
        } else if (level == 5) {
          content <- paste0("📊 ", content)
        } else if (level == 6) {
          content <- paste0("🔧 ", content)
        }
        # Create anchor ID for TOC linking
        anchor <- tolower(gsub("[^a-zA-Z0-9\\s]", "", content))
        anchor <- gsub("\\s+", "-", anchor)
        
        html_lines[i] <- paste0("<h", level, " id='", anchor, "'>", content, "</h", level, ">")
        next
      }
      
      # Handle bold and italic
      line <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", line)
      line <- gsub("\\*(.+?)\\*", "<em>\\1</em>", line)
      
      # Handle inline code
      line <- gsub("`(.+?)`", "<code>\\1</code>", line)
      
      # Handle links
      line <- gsub("\\[(.+?)\\]\\((.+?)\\)", "<a href='\\2' target='_blank'>\\1</a>", line)
      
      # Handle lists
      if (grepl("^[*-] ", line)) {
        content <- gsub("^[*-] ", "", line)
        if (!in_list) {
          html_lines[i] <- paste0("<ul><li>", content, "</li>")
          in_list <- TRUE
          list_type <- "ul"
        } else {
          html_lines[i] <- paste0("<li>", content, "</li>")
        }
        next
      }
      
      # Handle numbered lists
      if (grepl("^\\d+\\. ", line)) {
        content <- gsub("^\\d+\\. ", "", line)
        if (!in_list) {
          html_lines[i] <- paste0("<ol><li>", content, "</li>")
          in_list <- TRUE
          list_type <- "ol"
        } else {
          html_lines[i] <- paste0("<li>", content, "</li>")
        }
        next
      }
      
      # Close lists when we encounter a non-list line
      if (in_list && !grepl("^[*-] ", line) && !grepl("^\\d+\\. ", line) && nchar(trimws(line)) > 0) {
        html_lines[i-1] <- paste0(html_lines[i-1], paste0("</", list_type, ">"))
        in_list <- FALSE
        list_type <- ""
      }
      
      # Handle horizontal rules
      if (grepl("^---$", line)) {
        html_lines[i] <- "<hr>"
        next
      }
      
      # Handle empty lines (paragraph breaks)
      if (nchar(trimws(line)) == 0) {
        html_lines[i] <- "</p><p>"
        next
      }
      
      # Regular text
      if (i == 1 || (i > 1 && nchar(trimws(html_lines[i-1])) == 0)) {
        html_lines[i] <- paste0("<p>", line)
      } else {
        html_lines[i] <- line
      }
    }
    
    # Close any open lists
    if (in_list) {
      html_lines[length(html_lines)] <- paste0(html_lines[length(html_lines)], paste0("</", list_type, ">"))
    }
    
    # Close any open paragraphs
    if (length(html_lines) > 0 && grepl("^<p>", html_lines[1])) {
      html_lines[1] <- paste0(html_lines[1], "</p>")
    }
    
    # Combine and clean up
    html_content <- paste(html_lines, collapse = "\n")
    html_content <- gsub("</p><p></p><p>", "</p><p>", html_content)
    html_content <- gsub("^</p>", "", html_content)
    html_content <- gsub("</p>$", "", html_content)
    
    return(html_content)
  }
  
  # Generate table of contents function
  generate_toc <- function(markdown_text) {
    lines <- strsplit(markdown_text, "\n")[[1]]
    toc_items <- character(0)
    
    for (line in lines) {
      if (grepl("^# ", line)) {
        level <- nchar(gsub("^(#+).*", "\\1", line))
        content <- gsub("^#+ ", "", line)
        
        # Create anchor for linking
        anchor <- tolower(gsub("[^a-zA-Z0-9\\s]", "", content))
        anchor <- gsub("\\s+", "-", anchor)
        
        # Indent based on header level
        indent <- paste(rep("&nbsp;&nbsp;&nbsp;&nbsp;", level - 1), collapse = "")
        
        # Add to TOC
        toc_items <- c(toc_items, 
          paste0('<div style="margin: 5px 0;"><a href="#', anchor, '" class="toc-link">', 
                 indent, '• ', content, '</a></div>')
        )
      }
    }
    
    if (length(toc_items) > 0) {
      return(paste(toc_items, collapse = ""))
    } else {
      return("<p>No headers found for table of contents.</p>")
    }
  }
  
  # Help content (README)
  output$help_content <- renderUI({
    # Try multiple possible paths for README.md
    possible_paths <- c(
      "../README.md",  # If running from app directory
      "README.md",     # If running from project root
      "../../README.md" # If running from deeper subdirectory
    )
    
    readme_path <- NULL
    for (path in possible_paths) {
      if (file.exists(path)) {
        readme_path <- path
        break
      }
    }
    
    if (!is.null(readme_path)) {
      tryCatch({
        readme_content <- readLines(readme_path, warn = FALSE, encoding = "UTF-8")
        readme_text <- paste(readme_content, collapse = "\n")
        
        # Enhanced markdown to HTML conversion
        html_content <- markdown_to_html(readme_text)
        
        # Generate table of contents
        toc <- generate_toc(readme_text)
        
        # Combine TOC and content
        full_content <- paste0(
          '<div class="toc-container">',
          '<h3>📋 Table of Contents</h3>',
          toc,
          '</div>',
          '<hr>',
          html_content
        )
        
        # Wrap in div with CSS class for styling
        styled_html <- paste0('<div class="help-content">', full_content, '</div>')
        
        HTML(styled_html)
      }, error = function(e) {
        HTML(paste0(
          "<h2>Help Documentation</h2>",
          "<p>Error reading README.md file: ", e$message, "</p>",
          "<p>Please ensure the README.md file exists and is readable.</p>"
        ))
      })
    } else {
      HTML(paste0(
        "<h2>Help Documentation</h2>",
        "<p>README.md file not found. Please ensure the documentation file exists in the project root.</p>"
      ))
    }
  })
  
  trait_counter <- reactiveVal(1)
  ocs_results_reactive <- reactiveVal()  # For OCS results
  ebv_results_reactive <- reactiveVal()  # For EBV results
  error_message <- reactiveVal("")  # For tracking errors
  
  # Dynamic startup guide
  output$dynamic_guide <- renderUI({
    # Check for errors first
    current_error <- error_message()
    if (current_error != "") {
      return(HTML(paste0(
        "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; border-radius: 4px; margin-bottom: 10px;'>",
        "<p style='color: #721c24; margin: 0;'><strong>❌ Error occurred:</strong></p>",
        "<p style='color: #721c24; margin: 5px 0; font-family: monospace; font-size: 11px;'>", current_error, "</p>",
        "<p style='color: #721c24; margin: 5px 0 0 0; font-size: 12px;'>💡 Need help? Check the Help tab for detailed documentation.</p>",
        "</div>"
      )))
    }
    
    # Check current state
    has_candidates <- !is.null(input$candidate_file)
    has_pedigree <- !is.null(input$pedigree_file)
    has_kinship_threshold <- !is.null(input$thresh) && input$thresh < 1
    has_traits <- trait_counter() > 0 && any(sapply(1:trait_counter(), function(i) !is.null(input[[paste0("trait_file_", i)]])))
    has_ocs_results <- !is.null(ocs_results_reactive())
    
    # Build step-by-step guide
    steps <- c()
    
    # Step 1: Candidates
    if (has_candidates) {
      steps <- c(steps, "<p><strong>✅ Step 1:</strong> Upload your candidate list to begin the analysis</p>")
    } else {
      steps <- c(steps, "<p><strong>Step 1:</strong> Upload your candidate list to begin the analysis</p>")
    }
    
    # Step 2: Pedigree
    if (has_pedigree) {
      steps <- c(steps, "<p><strong>✅ Step 2:</strong> Upload your pedigree file for kinship calculations</p>")
    } else {
      steps <- c(steps, "<p><strong>Step 2:</strong> Upload your pedigree file for kinship calculations</p>")
    }
    
    # Step 3: Kinship threshold
    if (has_pedigree) {
      steps <- c(steps, "<p><strong>⚙️ Step 3:</strong> Set your kinship threshold (optional)</p>")
    } else {
      steps <- c(steps, "<p><strong>Step 3:</strong> Set your kinship threshold (optional)</p>")
    }
    
    # Step 4: Traits
    if (has_traits) {
      steps <- c(steps, "<p><strong>✅ Step 4:</strong> Add trait files and weights for breeding value analysis</p>")
    } else {
      steps <- c(steps, "<p><strong>Step 4:</strong> Add trait files and weights for breeding value analysis</p>")
    }
    
    # Step 5: OCS
    if (has_ocs_results) {
      steps <- c(steps, "<p><strong>✅ Step 5:</strong> Configure OCS parameters and run analysis</p>")
    } else {
      steps <- c(steps, "<p><strong>Step 5:</strong> Configure OCS parameters and run analysis</p>")
    }
    
    # Add completion message if all steps are done
    if (has_ocs_results) {
      steps <- c(steps, 
        "<p><strong>🎉 Analysis Complete!</strong></p>",
        "<p><strong>Final Step:</strong> Export your results using the download button below</p>",
        "<p style='color: #28a745; font-weight: bold;'>Great job! Results are ready for review and export</p>"
      )
    }
    
    HTML(paste(steps, collapse = ""))
  })
  
  observeEvent(input$add_trait, {
    trait_counter(trait_counter() + 1)
  })
  
  observeEvent(input$remove_trait, {
    if (trait_counter() > 1) {
      trait_counter(trait_counter() - 1)
    }
  })
  
  output$trait_inputs <- renderUI({
    create_trait_inputs(trait_counter())
  })
  
  # OCS trait inputs
  output$ocs_trait_inputs <- renderUI({
    req(input$ocs_trait_counter)
    create_ocs_trait_inputs(input$ocs_trait_counter)
  })
  
  candidates_data <- reactive({
    req(input$candidate_file)
    read_candidates(input$candidate_file)
  })
  
  pedigree_data <- reactiveVal(NULL)
  
  observeEvent(input$pedigree_file, {
    req(candidates_data())
    males <- candidates_data()$males
    females <- candidates_data()$females
    
    tryCatch({
      raw_ped <- readr::read_table(input$pedigree_file$datapath)
      final_ped <- clean_pedigree(raw_ped)
      kinship_res <- compute_kinship_matrix(final_ped, males, females)
      
      output$quadrants_table <- DT::renderDT({
        datatable(kinship_res$quads, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
          formatStyle(colnames(kinship_res$quads), styleEqual(
            kinship_res$quads[1, ], c("lightgreen", "yellow", "orange", "coral")
          ))
      })
      
      pedigree_data(list(results = kinship_res$results, quads = kinship_res$quads))
      
      error_message("")  # Clear any previous errors
      output$message1 <- renderText("✅ Kinship matrix generated successfully.")
    }, error = function(e) {
      error_message(paste0("Error processing pedigree: ", e$message))
      output$message1 <- renderText(
        paste0("❌ Error processing pedigree: Make sure your pedigree is clean and valid.\n",
               "Original error: ", e$message)
      )
    })
  })
  
  observe({
    req(candidates_data())
    ebv_res <- process_ebvs(trait_counter(), input)
    req(ebv_res)
    
    joint_ebvs <- ebv_res$joint_ebvs
    rel_weights <- ebv_res$rel_weights
    weight_total <- ebv_res$weight_total
    
    ebv_cols <- paste0("EBV.", seq_along(rel_weights))
    # Ensure EBV columns are numeric for matrix multiplication
    joint_ebvs[ebv_cols] <- lapply(joint_ebvs[ebv_cols], as.numeric)
    joint_ebvs$index_val <- as.vector(as.matrix(joint_ebvs[ebv_cols]) %*% rel_weights)
    
    cands <- candidates_data()$candidates
    males <- candidates_data()$males
    females <- candidates_data()$females
    
    cand_ebv <- left_join(cands, joint_ebvs, by = c("id" = "ID")) %>%
      select(id, sex, index_val)
    
    m_ebv <- filter(cand_ebv, id %in% males) %>% select(id, index_val)
    f_ebv <- filter(cand_ebv, id %in% females) %>% select(id, index_val)
    
    ebv_matrix <- outer(m_ebv$index_val, f_ebv$index_val, function(x, y) round((x + y) / 2, 2))
    rownames(ebv_matrix) <- m_ebv$id
    colnames(ebv_matrix) <- f_ebv$id
    
    ebv_quads <- tibble(
      Data = "EBV",
      Q25 = quantile(ebv_matrix, 0.25),
      Q50 = quantile(ebv_matrix, 0.50),
      Q75 = quantile(ebv_matrix, 0.75),
      Q100 = quantile(ebv_matrix, 1.00)
    ) %>% column_to_rownames("Data")
    
    quads_combined <- if (!is.null(pedigree_data())) {
      bind_rows(pedigree_data()$quads, ebv_quads)
    } else {
      ebv_quads
    }
    
    output$quadrants_table <- DT::renderDT({
      datatable(quads_combined, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
        formatStyle("Q25", backgroundColor = "coral") %>%
        formatStyle("Q50", backgroundColor = "orange") %>%
        formatStyle("Q75", backgroundColor = "yellow") %>%
        formatStyle("Q100", backgroundColor = "lightgreen")
    })
    
    ebv_results <- as_tibble(ebv_matrix, rownames = "Male") %>%
      pivot_longer(-Male, names_to = "Female", values_to = "EBV")
    
    full_results <- if (!is.null(pedigree_data())) {
      left_join(pedigree_data()$results, ebv_results, by = c("Female", "Male"))
    } else {
      relocate(mutate(ebv_results, Kinship = NA), Kinship, .after = EBV)
    }
    
    # For table: filter out crosses with EBV <=0 or kinship >= threshold
    filt_results_table <- full_results %>%
      filter(EBV > 0, (is.na(Kinship) | Kinship < input$thresh))
    
    # For Excel: mask EBVs for invalid crosses as NA (blank in Excel)
    filt_results_matrix <- full_results %>%
      mutate(EBV = ifelse(EBV <= 0 | (!is.na(Kinship) & Kinship >= input$thresh), NA, EBV))
    
    # Store results for download handler
    ebv_results_reactive(list(
      filt_results_table = filt_results_table,
      filt_results_matrix = filt_results_matrix,
      full_results = full_results,
      ebv_quads = ebv_quads
    ))
    
    output$matrix <- DT::renderDT({
      datatable(filt_results_table, rownames = TRUE) %>%
        formatStyle("EBV", styleInterval(
          unlist(ebv_quads[1, c("Q25", "Q50", "Q75")]),
          c("coral", "orange", "yellow", "lightgreen")
        ))
    })
    
    output$message2 <- renderText(
      if (abs(weight_total - 1) > 1e-6)
        paste("⚠️ Warning: Trait weights do not sum to 1 (total =", round(weight_total, 4), ")")
      else
        "✅ EBV matrix generated successfully."
    )
    
    output$download_all_results <- downloadHandler(
      filename = function() {
        paste0("AlloMate_Complete_Results-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        
        # Add comprehensive README worksheet
        openxlsx::addWorksheet(wb, "README")
        readme_text <- c(
          "📊 AlloMate Complete Results Report",
          "",
          "This Excel file contains all results from your AlloMate analysis:",
          "",
          "📋 Worksheets included:",
          "1. README - This overview and explanation",
          "2. Filtered Results - Crosses meeting criteria (positive EBVs, kinship below threshold)",
          "3. EBV Matrix - Complete matrix view with masked values",
          "4. OCS Candidates - Selected candidates from Optimum Contribution Selection",
          "5. Mating Plan - Recommended mating pairs from OCS",
          "6. Parameters - Analysis parameters used",
          "",
          "🔍 Data Details:",
          "- Filtered Results: Only crosses with positive EBVs and kinship below threshold",
          "- EBV Matrix: All possible crosses with values masked for failed criteria",
          "- OCS Results: Available only after running Optimum Contribution Selection",
          "",
          "📅 Generated on:", as.character(Sys.Date())
        )
        openxlsx::writeData(wb, "README", readme_text)
        
        # Add kinship/EBV results if available
        ebv_results <- ebv_results_reactive()
        if (!is.null(ebv_results)) {
          openxlsx::addWorksheet(wb, "Filtered Results")
          openxlsx::writeData(wb, "Filtered Results", ebv_results$filt_results_table, rowNames = TRUE)
          
          openxlsx::addWorksheet(wb, "EBV Matrix")
          m_ids <- unique(ebv_results$full_results$Male)
          f_ids <- unique(ebv_results$full_results$Female)
          mat_for_excel <- matrix(NA_real_, nrow = length(m_ids), ncol = length(f_ids),
                                  dimnames = list(m_ids, f_ids))
          
          for (i in seq_len(nrow(ebv_results$filt_results_matrix))) {
            m <- ebv_results$filt_results_matrix$Male[i]
            f <- ebv_results$filt_results_matrix$Female[i]
            val <- ebv_results$filt_results_matrix$EBV[i]
            mat_for_excel[m, f] <- val
          }
          openxlsx::writeData(wb, "EBV Matrix", mat_for_excel, rowNames = TRUE)
        }
        
        # Add OCS results if available
        if (exists("ocs_results_reactive") && !is.null(ocs_results_reactive())) {
          results <- ocs_results_reactive()
          formatted_results <- format_ocs_results(results)
          
          openxlsx::addWorksheet(wb, "OCS Candidates")
          openxlsx::writeData(wb, "OCS Candidates", formatted_results$candidate_table, rowNames = FALSE)
          
          openxlsx::addWorksheet(wb, "Mating Plan")
          openxlsx::writeData(wb, "Mating Plan", formatted_results$mating_table, rowNames = FALSE)
        }
        
        # Add parameters worksheet
        openxlsx::addWorksheet(wb, "Parameters")
        params_data <- data.frame(
          Parameter = c("Kinship Threshold", "Desired Inbreeding Rate", "Number of Offspring", "Analysis Date"),
          Value = c(
            as.character(input$thresh),
            as.character(input$inbreeding_rate),
            as.character(input$num_offspring),
            as.character(Sys.Date())
          )
        )
        openxlsx::writeData(wb, "Parameters", params_data, rowNames = FALSE)
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
  
  #### OCS Server Logic ####
  
  observeEvent(input$run_ocs_btn, {
    req(input$pedigree_file, input$candidate_file)
    
    # Check if any OCS implementation is available (optiSel or custom fallback)
    if ((!exists("optisel_available") || !optisel_available) && 
        (!exists("custom_ocs_available") || !custom_ocs_available)) {
      showModal(modalDialog(
        title = "OCS Functionality Not Available",
        "Neither optiSel nor the custom OCS fallback could be loaded. 
        Please check that all required packages are installed and restart the app.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    tryCatch({
      ped_data <- read.table(input$pedigree_file$datapath, header = TRUE)
      candidates <- read.table(input$candidate_file$datapath, header = TRUE)
      
      final_ped <- clean_pedigree(ped_data)
      kinship_matrix <- if (exists("kinship2_available") && kinship2_available) {
        kinship2::kinship(final_ped)
      } else {
        fallback_kinship(final_ped)
      }
      
      ebv_result <- process_ebvs(trait_counter(), input)
      if (is.null(ebv_result) || abs(ebv_result$weight_total - 1) > 1e-6) {
        showModal(modalDialog(
          title = "Invalid Weights",
          "Weights must sum to 1.",
          easyClose = TRUE
        ))
        return(NULL)
      }
      
      joint_ebvs <- calculate_index(ebv_result$joint_ebvs, ebv_result$rel_weights)
      candidates <- left_join(candidates, joint_ebvs, by = c("id" = "ID"))
      
      results <- run_ocs(
        candidates_df = candidates,
        kinship_matrix = kinship_matrix,
        ebv_index = candidates$index_val,
        desired_inbreeding_rate = input$inbreeding_rate,
        num_offspring = input$num_offspring
      )
      
      ocs_results_reactive(results)
      
      error_message("")  # Clear any previous errors
      
      # Format results for display
      formatted_results <- format_ocs_results(results)
      
      output$ocs_candidate_table <- DT::renderDT({
        formatted_results$candidate_table
      })
      
      # Switch to OCS tab to show results
      updateTabsetPanel(session, "main_tabs", selected = "Optimum Contribution Selection")
      
    }, error = function(e) {
      error_message(paste0("Error running OCS: ", e$message))
      showModal(modalDialog(
        title = "Error",
        paste("❌ Error running OCS:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  output$ocs_mating_table <- DT::renderDT({
    req(ocs_results_reactive())
    results <- ocs_results_reactive()
    formatted_results <- format_ocs_results(results)
    formatted_results$mating_table %>%
      datatable(
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
  })
  

}