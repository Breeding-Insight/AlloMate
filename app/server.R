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
  
  # View R Code button functionality
  observeEvent(input$view_r_code_btn, {
    updateTabsetPanel(session, "main_tabs", selected = "R Code")
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
  candidate_status <- reactiveVal(list(ok = FALSE, error = NULL))
  ocs_results_reactive <- reactiveVal()  # For OCS results
  ebv_results_reactive <- reactiveVal()  # For EBV results
  error_message <- reactiveVal("")  # For tracking errors
  pedigree_validation_stats <- reactiveVal(NULL)
  
  # Export: Prebuild workbook so Chrome downloads reliably
  export_cache <- reactiveVal(NULL)

  generate_export_xlsx <- function(dest_file) {
    use_openxlsx <- exists("openxlsx_available") && isTRUE(openxlsx_available)
    safe_char <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)

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

    ebv_results <- ebv_results_reactive()
    mat_for_excel <- NULL
    filtered_results_df <- NULL
    ebv_matrix_df <- NULL

    if (!is.null(ebv_results)) {
      m_ids <- unique(ebv_results$full_results$Male)
      f_ids <- unique(ebv_results$full_results$Female)
      mat_for_excel <- matrix(NA_real_, nrow = length(m_ids), ncol = length(f_ids),
                              dimnames = list(m_ids, f_ids))

      if (nrow(ebv_results$filt_results_matrix) > 0) {
        for (i in seq_len(nrow(ebv_results$filt_results_matrix))) {
          m <- ebv_results$filt_results_matrix$Male[i]
          f <- ebv_results$filt_results_matrix$Female[i]
          val <- ebv_results$filt_results_matrix$EBV[i]
          if (!is.na(m) && !is.na(f) &&
              m %in% rownames(mat_for_excel) && f %in% colnames(mat_for_excel)) {
            mat_for_excel[m, f] <- val
          }
        }
      }

      filtered_results_df <- as.data.frame(ebv_results$filt_results_table)

      row_labels <- rownames(mat_for_excel)
      if (is.null(row_labels)) row_labels <- seq_len(nrow(mat_for_excel))
      ebv_matrix_df <- data.frame(
        Male = row_labels,
        mat_for_excel,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    formatted_results <- NULL
    if (!is.null(ocs_results_reactive())) {
      formatted_results <- tryCatch(
        format_ocs_results(ocs_results_reactive()),
        error = function(e) {
          message("format_ocs_results error: ", e$message)
          NULL
        }
      )
    }

    params_data <- data.frame(
      Parameter = c("Kinship Threshold", "Desired Inbreeding Rate", "Number of Offspring", "Analysis Date"),
      Value = c(
        safe_char(input$thresh),
        safe_char(input$inbreeding_rate),
        safe_char(input$num_offspring),
        as.character(Sys.Date())
      ),
      stringsAsFactors = FALSE
    )

    ok <- FALSE
    tryCatch({
      if (use_openxlsx) {
        wb <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(wb, "README")
        openxlsx::writeData(wb, "README", readme_text)

        if (!is.null(ebv_results)) {
          openxlsx::addWorksheet(wb, "Filtered Results")
          openxlsx::writeData(wb, "Filtered Results", filtered_results_df, rowNames = TRUE)

          openxlsx::addWorksheet(wb, "EBV Matrix")
          openxlsx::writeData(wb, "EBV Matrix", ebv_matrix_df, rowNames = FALSE)
        }

        if (!is.null(formatted_results)) {
          openxlsx::addWorksheet(wb, "OCS Candidates")
          openxlsx::writeData(wb, "OCS Candidates", formatted_results$candidate_table, rowNames = FALSE)

          openxlsx::addWorksheet(wb, "Mating Plan")
          openxlsx::writeData(wb, "Mating Plan", formatted_results$mating_table, rowNames = FALSE)
        }

        openxlsx::addWorksheet(wb, "Parameters")
        openxlsx::writeData(wb, "Parameters", params_data, rowNames = FALSE)

        openxlsx::saveWorkbook(wb, dest_file, overwrite = TRUE)
      } else {
        sheets <- list(
          README = data.frame(Text = readme_text, stringsAsFactors = FALSE),
          Parameters = params_data
        )
        if (!is.null(filtered_results_df)) sheets$`Filtered Results` <- filtered_results_df
        if (!is.null(ebv_matrix_df)) sheets$`EBV Matrix` <- ebv_matrix_df
        if (!is.null(formatted_results)) {
          sheets$`OCS Candidates` <- as.data.frame(formatted_results$candidate_table)
          sheets$`Mating Plan` <- as.data.frame(formatted_results$mating_table)
        }
        write_xlsx_pure(dest_file, sheets)
      }
      ok <- TRUE
    }, error = function(e) {
      message("Export error: ", e$message)
      writeLines(paste("Export failed:", e$message), dest_file, useBytes = TRUE)
    })
    ok
  }

  observeEvent({
    list(
      ebv_results_reactive(),
      ocs_results_reactive(),
      input$thresh,
      input$inbreeding_rate,
      input$num_offspring
    )
  }, {
    tmp <- tempfile(pattern = "allomate_export_", fileext = ".xlsx")
    if (generate_export_xlsx(tmp) && file.exists(tmp) && file.info(tmp)$size > 0) {
      old <- export_cache()
      export_cache(tmp)
      if (!is.null(old) && file.exists(old)) unlink(old, force = TRUE)
    } else if (file.exists(tmp)) {
      unlink(tmp, force = TRUE)
    }
  }, ignoreNULL = FALSE)

  session$onSessionEnded(function() {
    cache <- export_cache()
    if (!is.null(cache) && file.exists(cache)) unlink(cache, force = TRUE)
  })

  output$download_all_results <- downloadHandler(
    filename = function() {
      paste0("AlloMate_Complete_Results-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      cache <- export_cache()
      if (!is.null(cache) && file.exists(cache)) {
        file.copy(cache, file, overwrite = TRUE)
      } else {
        generate_export_xlsx(file)
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  outputOptions(output, "download_all_results", suspendWhenHidden = FALSE)

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

    # Determine error type from error message, if not working properly, update the grep logic
    pedigree_error <- current_error != "" && grepl("error|pedigree|kinship", current_error, ignore.case = TRUE)
    trait_error <- current_error != "" && grepl("error|ebv|trait|weight", current_error, ignore.case = TRUE)
    ocs_error <- current_error != "" && grepl("error|ocs", current_error, ignore.case = TRUE)

    cs <- candidate_status()
    candidate_ready <- isTRUE(cs$ok)
    candidate_error_flag <- !is.null(cs$error)

    # Helper to decide icon per step
    get_step_icon <- function(has_item, error_flag) {
      if (error_flag) {
        "❌"
      } else if (has_item) {
        "✅"
      } else {
        "⬜"
      }
    }
    
    # Build step-by-step guide
    steps <- c()
    
    # Step 1: Candidates
    step1_icon <- get_step_icon(candidate_ready, candidate_error_flag)
    steps <- c(steps, sprintf("<p><strong>%s Step 1:</strong> Upload your candidate list to begin the analysis</p>", step1_icon))
    
    # Step 2: Pedigree
    step2_icon <- get_step_icon(has_pedigree, pedigree_error)
    steps <- c(steps, sprintf("<p><strong>%s Step 2:</strong> Upload your pedigree file for kinship calculations</p>", step2_icon))
    
    # Step 3: Kinship threshold
    if (has_pedigree) {
      steps <- c(steps, "<p><strong>⚙️ Step 3:</strong> Set your kinship threshold (optional)</p>")
    } else {
      steps <- c(steps, "<p><strong>Step 3:</strong> Set your kinship threshold (optional)</p>")
    }
    
    # Step 4: Traits
    step4_icon <- get_step_icon(has_traits, trait_error)
    steps <- c(steps, sprintf("<p><strong>%s Step 4:</strong> Add trait files and weights for breeding value analysis</p>", step4_icon))
    
    # Step 5: OCS
    step5_icon <- get_step_icon(has_ocs_results, ocs_error)
    steps <- c(steps, sprintf("<p><strong>%s Step 5:</strong> Configure OCS parameters and run analysis</p>", step5_icon))
    
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
  
  # File status display
  output$file_status_display <- renderUI({
    # Check what files/data are ready
    has_candidates <- !is.null(input$candidate_file)
    has_pedigree <- !is.null(input$pedigree_file)
    has_ebv <- !is.null(ebv_results_reactive())
    has_ocs <- !is.null(ocs_results_reactive())
    
    # Check for errors
    current_error <- error_message()
    has_error <- current_error != ""
    
    cs <- candidate_status()
    candidate_ready <- isTRUE(cs$ok)
    candidate_error_flag <- !is.null(cs$error)

    # Determine error type from error message
    pedigree_error <- has_error && grepl("pedigree|kinship", current_error, ignore.case = TRUE)
    ebv_error <- has_error && grepl("ebv|trait|weight", current_error, ignore.case = TRUE)
    ocs_error <- has_error && grepl("ocs", current_error, ignore.case = TRUE)
    
    # Helper function to determine status icon and text
    get_status <- function(has_data, uploaded, has_specific_error, ready_text = "Ready", pending_text = "Not uploaded", error_text = "Error") {
      if (has_specific_error) {
        list(icon = "❌", text = paste0("<span style='color: #dc3545;'>", error_text, "</span>"))
      } else if (has_data) {
        list(icon = "✅", text = paste0("<span style='color: #28a745;'>", ready_text, "</span>"))
      } else if (uploaded) {
        list(icon = "⬜", text = paste0("<span style='color: #ffc107;'>Processing...</span>"))
      } else {
        list(icon = "⬜", text = paste0("<span style='color: #6c757d;'>", pending_text, "</span>"))
      }
    }
    
    # Candidate status with explicit tracking
    candidate_status_ui <- if (candidate_error_flag) {
      list(icon = "❌", text = "<span style='color: #dc3545;'>Error</span>")
    } else if (candidate_ready) {
      list(icon = "✅", text = "<span style='color: #28a745;'>Ready</span>")
    } else if (has_candidates) {
      list(icon = "⬜", text = "<span style='color: #ffc107;'>Processing...</span>")
    } else {
      list(icon = "⬜", text = "<span style='color: #6c757d;'>Not uploaded</span>")
    }
    
    # Pedigree status
    pedigree_status <- get_status(has_pedigree, has_pedigree, pedigree_error, 
                                  "Ready", "Not uploaded", "Error")
    
    # EBV status - can error if uploaded but processing failed
    ebv_status <- get_status(has_ebv, 
                            has_candidates && has_pedigree, 
                            ebv_error, 
                            "Ready", "Pending", "Error")
    
    # OCS status
    ocs_status <- get_status(has_ocs, FALSE, ocs_error, 
                            "Ready", "Not run", "Error")
    
    # Create status lines with emojis
    status_lines <- c(
      paste0(
        candidate_status_ui$icon,
        " <strong>Candidate List:</strong> ",
        candidate_status_ui$text
      ),
      paste0(
        pedigree_status$icon,
        " <strong>Pedigree Data:</strong> ",
        pedigree_status$text
      ),
      paste0(
        ebv_status$icon,
        " <strong>EBV Matrix:</strong> ",
        ebv_status$text
      ),
      paste0(
        ocs_status$icon,
        " <strong>OCS Results:</strong> ",
        ocs_status$text
      )
    )
    
    # Check if all files are ready for download
    all_ready <- candidate_ready && has_pedigree && has_ebv
    
    download_status <- if (has_error) {
      "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; margin-top: 10px; border-radius: 3px;'>
        <p style='color: #721c24; margin: 0; font-size: 12px;'><strong>⚠️ Error detected.</strong> Check the startup guide above for details.</p>
      </div>"
    } else if (all_ready) {
      "<div style='background-color: #d4edda; border: 1px solid #c3e6cb; padding: 8px; margin-top: 10px; border-radius: 3px;'>
        <p style='color: #155724; margin: 0; font-size: 12px;'><strong>✅ Download ready!</strong> All core data is available.</p>
      </div>"
    } else {
      "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 8px; margin-top: 10px; border-radius: 3px;'>
        <p style='color: #6c757d; margin: 0; font-size: 12px;'>Upload required files to enable download.</p>
      </div>"
    }
    
    HTML(paste0(
      "<div style='font-size: 12px; line-height: 1.8;'>",
      paste(status_lines, collapse = "<br>"),
      "</div>",
      download_status
    ))
  })
  
  output$pedigree_status_display <- renderUI({
    stats <- pedigree_validation_stats()
    if (is.null(stats)) {
      return(NULL)
    }

    get_count <- function(val) {
      if (is.null(val) || is.na(val)) {
        0L
      } else {
        as.integer(val)
      }
    }

    format_count <- function(val) {
      format(get_count(val), big.mark = ",", scientific = FALSE)
    }

    records <- format_count(stats$records_loaded)
    unknown_count <- get_count(stats$unknown_parent_count)
    circular_count <- get_count(stats$circular_reference_count)
    missing_count <- get_count(stats$missing_candidates)
    duplicates <- get_count(stats$duplicates_removed)

    green_box <- paste0(
      "<div style='background-color: #d4edda; border: 1px solid #c3e6cb; padding: 8px; border-radius: 3px; margin-top: 10px; font-size: 12px;'>",
      "✅ ", records, " records loaded",
      "</div>"
    )

    # Build yellow box warnings only for non-zero counts
    yellow_warnings <- c()
    if (unknown_count > 0) {
      yellow_warnings <- c(yellow_warnings, 
        paste0("<p style='margin: ", if(length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>⚠️ ", 
               format_count(stats$unknown_parent_count), 
               " individuals with unknown parent(s) (treated as founders)</p>"))
    }
    if (circular_count > 0) {
      yellow_warnings <- c(yellow_warnings,
        paste0("<p style='margin: ", if(length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>⚠️ ", 
               format_count(stats$circular_reference_count),
               " circular references detected and broken at earliest generation</p>"))
    }
    if (missing_count > 0) {
      yellow_warnings <- c(yellow_warnings,
        paste0("<p style='margin: ", if(length(yellow_warnings) == 0) "0" else "4px 0 0", ";'>⚠️ ", 
               format_count(stats$missing_candidates),
               " selection candidates missing from pedigree</p>"))
    }
    
    yellow_box <- if (length(yellow_warnings) > 0) {
      paste0(
        "<div style='background-color: #fff3cd; border: 1px solid #ffeeba; padding: 8px; border-radius: 3px; margin-top: 6px; font-size: 12px;'>",
        paste(yellow_warnings, collapse = ""),
        "</div>"
      )
    } else {
      ""
    }

    red_box <- if (duplicates > 0) {
      paste0(
        "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; border-radius: 3px; margin-top: 6px; font-size: 12px;'>",
        "❌ ", format(duplicates, big.mark = ",", scientific = FALSE), " duplicates removed",
        "</div>"
      )
    } else {
      ""
    }

    HTML(paste0(green_box, yellow_box, red_box))
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

    tryCatch({
      res <- read_candidates(input$candidate_file)
      candidate_status(list(ok = TRUE, error = NULL))

      current_err <- error_message()
      if (current_err != "" && grepl("^Error processing candidates", current_err)) {
        error_message("")
      }

      res
    }, error = function(e) {
      candidate_status(list(ok = FALSE, error = e$message))
      error_message(paste0("Error processing candidates: ", e$message))
      validate(need(FALSE, e$message))
      NULL
    })
  })
  
  pedigree_data <- reactiveVal(NULL)
  
  observeEvent(input$pedigree_file, {
    req(candidates_data())
    males <- candidates_data()$males
    females <- candidates_data()$females
    
    tryCatch({
      raw_ped <- readr::read_table(input$pedigree_file$datapath)
      cleaned_ped <- clean_pedigree(raw_ped, return_stats = TRUE)
      final_ped <- cleaned_ped$pedigree
      
      # Check for candidates missing from pedigree
      candidate_ids <- candidates_data()$candidates$id
      pedigree_ids <- as.character(raw_ped$id)
      missing_candidates <- sum(!candidate_ids %in% pedigree_ids)
      
      # Add missing candidates count to stats
      cleaned_ped$stats$missing_candidates <- missing_candidates
      
      kinship_res <- compute_kinship_matrix(final_ped, males, females)
      
      output$quadrants_table <- DT::renderDT({
        datatable(kinship_res$quads, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
          formatStyle(colnames(kinship_res$quads), styleEqual(
            kinship_res$quads[1, ], c("lightgreen", "yellow", "orange", "coral")
          ))
      })
      
      pedigree_data(list(results = kinship_res$results, quads = kinship_res$quads))
      pedigree_validation_stats(cleaned_ped$stats)
      
      error_message("")  # Clear any previous errors
      output$message1 <- renderText("✅ Kinship matrix generated successfully.")
    }, error = function(e) {
      error_message(paste0("Error processing pedigree: ", e$message))
      pedigree_validation_stats(NULL)
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
    
    # download handler defined at top-level above
  })
  
  #### OCS Server Logic ####
  
  observeEvent(input$force_greedy_mating, {
    options(allomate.force_greedy_mating = isTRUE(input$force_greedy_mating))
  }, ignoreNULL = FALSE)
  
  observeEvent(input$force_qp_greedy, {
    options(allomate.force_qp_greedy = isTRUE(input$force_qp_greedy))
  }, ignoreNULL = FALSE)
  
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
    
    options(allomate.force_greedy_mating = isTRUE(input$force_greedy_mating))
    options(allomate.force_qp_greedy = isTRUE(input$force_qp_greedy))
    
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
      
      # Set seed for reproducibility when testing optiSel vs fallback
      set.seed(42)
      
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
  
  output$ocs_solver_note <- renderUI({
    req(ocs_results_reactive())
    formatted_results <- format_ocs_results(ocs_results_reactive())
    info <- formatted_results$summary_stats$mating_info
    if (is.null(info) || is.na(info) || info == "") {
      return(NULL)
    }
    div(
      style = "margin: 10px 0; padding: 10px; background-color: #fff8e1; border-left: 4px solid #ffb300; font-size: 13px;",
      tags$strong("Solver note: "), info
    )
  })
  
  #### R Code Display and Download ####
  
  # HTML escape function for code display
  htmlEscape <- function(text) {
    text <- gsub("&", "&amp;", text)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text <- gsub('"', "&quot;", text)
    text <- gsub("'", "&#39;", text)
    return(text)
  }
  
  # Function to read and format R code files
  format_r_code_content <- function() {
    # Define the R files to include
    r_files <- c(
      "global.R" = "Global Setup and Package Loading",
      "R/load_functions.R" = "Function Loading Logic", 
      "R/utils.R" = "Data Processing Functions",
      "R/ocs_helpers.R" = "OCS Calculation Functions",
      "R/ui_helpers.R" = "UI Helper Functions",
      "R/optsel_fallback.R" = "Custom OCS Implementation"
    )
    
    # Determine the base path
    base_path <- if (app_dir) "." else "app"
    
    # Read and format each file
    formatted_sections <- list()
    
    for (file_path in names(r_files)) {
      full_path <- file.path(base_path, file_path)
      
      if (file.exists(full_path)) {
        tryCatch({
          file_content <- readLines(full_path, warn = FALSE, encoding = "UTF-8")
          file_content <- paste(file_content, collapse = "\n")
          
          # Create formatted section
          section_html <- paste0(
            '<div class="code-section">',
            '<h4>📁 ', r_files[file_path], '</h4>',
            '<div class="file-info">',
            '<strong>File:</strong> ', file_path, '<br>',
            '<strong>Lines:</strong> ', length(strsplit(file_content, "\n")[[1]]), '<br>',
            '<strong>Description:</strong> ', r_files[file_path],
            '</div>',
            '<pre><code class="language-r">', 
            htmlEscape(file_content),
            '</code></pre>',
            '</div>'
          )
          
          formatted_sections[[file_path]] <- section_html
        }, error = function(e) {
          formatted_sections[[file_path]] <- paste0(
            '<div class="code-section">',
            '<h4>❌ Error reading file</h4>',
            '<p>Could not read file: ', file_path, '</p>',
            '<p>Error: ', e$message, '</p>',
            '</div>'
          )
        })
      } else {
        formatted_sections[[file_path]] <- paste0(
          '<div class="code-section">',
          '<h4>❌ File not found</h4>',
          '<p>File not found: ', file_path, '</p>',
          '</div>'
        )
      }
    }
    
    # Create setup instructions
    setup_instructions <- paste0(
      '<div class="setup-instructions">',
      '<h4>🚀 Setup Instructions</h4>',
      '<p><strong>To run this analysis independently in R:</strong></p>',
      '<ol>',
      '<li><strong>Install required packages:</strong><br>',
      '<code>install.packages(c("tidyverse", "shiny", "DT", "openxlsx", "quadprog", "kinship2", "optiSel"))</code></li>',
      '<li><strong>Load the code files:</strong> Copy the code sections above into separate .R files</li>',
      '<li><strong>Prepare your data:</strong> Ensure your input files match the expected format</li>',
      '<li><strong>Run the analysis:</strong> Execute the functions in the order shown above</li>',
      '</ol>',
      '<p><strong>Note:</strong> The custom OCS fallback will be used if optiSel is not available.</p>',
      '</div>'
    )
    
    # Combine all sections
    full_content <- paste0(
      '<div class="r-code-content">',
      '<h1>🧬 AlloMate R Code Implementation</h1>',
      '<p>This page contains all the R code needed to implement the AlloMate analysis independently. ',
      'The code is organized by function and includes all necessary data processing, kinship calculations, ',
      'and optimum contribution selection algorithms.</p>',
      setup_instructions,
      paste(formatted_sections, collapse = ""),
      '</div>'
    )
    
    return(full_content)
  }
  
  # Render R code content
  output$r_code_content <- renderUI({
    HTML(format_r_code_content())
  })
  
  # Download R code functionality
  output$download_r_code <- downloadHandler(
    filename = function() {
      paste0("allomate_complete_script_", format(Sys.Date(), "%Y%m%d"), ".R")
    },
    content = function(file) {
      # Define the R files to include
      r_files <- c(
        "global.R" = "# Global Setup and Package Loading",
        "R/load_functions.R" = "# Function Loading Logic", 
        "R/utils.R" = "# Data Processing Functions",
        "R/ocs_helpers.R" = "# OCS Calculation Functions",
        "R/ui_helpers.R" = "# UI Helper Functions",
        "R/optsel_fallback.R" = "# Custom OCS Implementation"
      )
      
      # Determine the base path
      base_path <- if (app_dir) "." else "app"
      
      # Create the complete script
      script_lines <- c(
        "# AlloMate Complete R Script",
        "# Generated on:", as.character(Sys.Date()),
        "# This script contains all functions needed to run AlloMate analysis independently",
        "",
        "# =============================================================================",
        "# SETUP INSTRUCTIONS",
        "# =============================================================================",
        "# 1. Install required packages:",
        "#    install.packages(c('tidyverse', 'shiny', 'DT', 'openxlsx', 'quadprog', 'kinship2', 'optiSel'))",
        "# 2. Load required libraries:",
        "#    library(tidyverse)",
        "#    library(openxlsx)",
        "#    library(quadprog)",
        "#    library(kinship2)",
        "#    library(optiSel)",
        "# 3. Run this script to load all functions",
        "# 4. Use the functions as demonstrated in the comments",
        "",
        "# =============================================================================",
        "# FUNCTION DEFINITIONS",
        "# =============================================================================",
        ""
      )
      
      for (file_path in names(r_files)) {
        full_path <- file.path(base_path, file_path)
        
        if (file.exists(full_path)) {
          tryCatch({
            file_content <- readLines(full_path, warn = FALSE, encoding = "UTF-8")
            
            # Add section header
            script_lines <- c(script_lines, 
                             paste0("# ", "=", strrep("=", 70)),
                             r_files[file_path],
                             paste0("# ", "=", strrep("=", 70)),
                             "",
                             file_content,
                             "",
                             ""
            )
          }, error = function(e) {
            script_lines <- c(script_lines,
                             paste0("# Error reading file: ", file_path),
                             paste0("# ", e$message),
                             "",
                             ""
            )
          })
        } else {
          script_lines <- c(script_lines,
                           paste0("# File not found: ", file_path),
                           "",
                           ""
          )
        }
      }
      
      # Add usage example
      script_lines <- c(script_lines,
                        "# =============================================================================",
                        "# USAGE EXAMPLE",
                        "# =============================================================================",
                        "# ",
                        "# # Load your data",
                        "# candidates <- read.table('your_candidates.txt', header = TRUE)",
                        "# pedigree <- read.table('your_pedigree.txt', header = TRUE)",
                        "# ",
                        "# # Process data",
                        "# candidates_data <- read_candidates(list(datapath = 'your_candidates.txt'))",
                        "# final_ped <- clean_pedigree(pedigree)",
                        "# kinship_matrix <- compute_kinship_matrix(final_ped, candidates_data$males, candidates_data$females)",
                        "# ",
                        "# # Run OCS analysis",
                        "# results <- run_ocs(candidates_df = candidates, kinship_matrix = kinship_matrix$results,",
                        "#                    ebv_index = your_ebv_values, desired_inbreeding_rate = 0.05, num_offspring = 100)",
                        "# ",
                        "# # View results",
                        "# print(results)",
                        ""
      )
      
      # Write to file
      writeLines(script_lines, file)
    }
  )
  

}