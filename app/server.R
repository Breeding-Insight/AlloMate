server <- function(input, output, session) {
  # Dynamic-trait UI helpers 
  trait_counter <- reactiveVal(1)
  
  observeEvent(input$add_trait,  trait_counter(trait_counter() + 1))
  observeEvent(input$remove_trait, {
    if (trait_counter() > 1) trait_counter(trait_counter() - 1)
  })
  
  observeEvent(trait_counter(), {
    n <- trait_counter()
    lapply(seq_len(n), function(i) {
      updateNumericInput(session, paste0("trait_weight_", i), value = round(1 / n, 3))
    })
  })
  
  output$trait_inputs <- renderUI({
    n <- trait_counter()
    tagList(lapply(seq_len(n), function(i) {
      wellPanel(
        fileInput(paste0("trait_file_", i), paste("EBVs for trait", i)),
        numericInput(paste0("trait_weight_", i), paste("Relative weight for Trait", i),
                     value = round(1 / n, 3), min = 0, max = 1, step = 0.01)
      )
    }))
  })
  
  # Main observe
  observe({
    req(input$candidate_file)
    candidates <- read_table(input$candidate_file$datapath)
    males_to_select   <- candidates %>% filter(sex == "M") %>% pull(id)
    females_to_select <- candidates %>% filter(sex == "F") %>% pull(id)
    
    if (!is.null(input$pedigree_file)) {
      tryCatch({
        raw_ped <- read_table(input$pedigree_file$datapath) %>%
          mutate(id = as.factor(id), sire = as.factor(sire), dam = as.factor(dam))
        
        sex_ped <- raw_ped %>% mutate(sex = case_when(
          id %in% sire ~ 0,
          id %in% dam ~ 1,
          TRUE ~ 2
        ))
        
        messy_parents <- intersect(sex_ped$sire, sex_ped$dam) %>%
          setdiff(0) %>% as.data.frame() %>% rename(id = 1)
        
        parents_fixed <- sex_ped
        parents_fixed$sire[parents_fixed$sire %in% messy_parents$id] <- 0
        parents_fixed$dam[parents_fixed$dam %in% messy_parents$id] <- 0
        
        doubled <- parents_fixed %>% count(id, name = "freq") %>% filter(freq > 1) %>% pull(id)
        nodup <- parents_fixed %>% filter(!id %in% doubled)
        
        circdep <- nodup %>% mutate(across(c(id, sire, dam), as.character)) %>%
          filter(id == sire | id == dam)
        
        clean_ped <- anti_join(nodup, circdep, by = "id")
        ready_ped <- with(clean_ped, fixParents(id, sire, dam, sex, missid = "0"))
        final_ped <- with(ready_ped, pedigree(id, dadid, momid, sex, missid = "0"))
        kinship_matrix <- kinship(final_ped)
        kin_mat_sel <- kinship_matrix[males_to_select, females_to_select]
        
        kin_quads <- tibble(
          Data = "Kinship",
          Q25 = quantile(kin_mat_sel, 0.25),
          Q50 = quantile(kin_mat_sel, 0.50),
          Q75 = quantile(kin_mat_sel, 0.75),
          Q100 = quantile(kin_mat_sel, 1.00)
        ) %>% column_to_rownames("Data")
        
        output$quadrants_table <- renderDT({
          datatable(kin_quads, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
            formatStyle(colnames(kin_quads), backgroundColor = styleEqual(
              kin_quads[1, ], c("lightgreen", "yellow", "orange", "coral")
            ))
        })
        
        kinship_results <<- as_tibble(kin_mat_sel, rownames = "Male") %>%
          pivot_longer(-Male, names_to = "Female", values_to = "Kinship")
        
        output$matrix <- renderDT({
          datatable(kinship_results %>% arrange(Kinship), rownames = TRUE) %>%
            formatStyle("Kinship", backgroundColor = styleInterval(
              unlist(kin_quads[1, c("Q25", "Q50", "Q75")]),
              c("lightgreen", "yellow", "orange", "coral")
            ))
        })
        
        output$message1 <- renderText("Kinship matrix generated successfully.")
      }, error = function(e) {
        output$message1 <- renderText(paste("Pedigree error:", e$message))
      })
    }
    
    # EBV calculation
    ebv_inputs <- list()
    for (i in seq_len(trait_counter())) {
      file_i <- input[[paste0("trait_file_", i)]]
      weight_i <- input[[paste0("trait_weight_", i)]]
      if (!is.null(file_i) && !is.null(weight_i)) {
        df_raw <- read_table(file_i$datapath)
        if (!"ID" %in% names(df_raw)) names(df_raw)[1] <- "ID"
        if (!"EBV" %in% names(df_raw)) names(df_raw)[2] <- "EBV"
        ebv_inputs <- append(ebv_inputs, list(df_raw %>% select(ID, EBV), weight_i))
      }
    }
    
    if (length(ebv_inputs) >= 2 && length(ebv_inputs) %% 2 == 0) {
      rel_weights <- unlist(ebv_inputs[seq(2, length(ebv_inputs), by = 2)])
      weight_total <- sum(rel_weights)
      ebv_dfs <- ebv_inputs[seq(1, length(ebv_inputs), by = 2)]
      ebv_dfs <- purrr::imap(ebv_dfs, ~ rename(.x, !!paste0("EBV.", .y) := EBV))
      
      joint_ebvs <- reduce(ebv_dfs, full_join, by = "ID") %>%
        mutate(across(starts_with("EBV."), ~ replace_na(.x, 0)))
      
      ebv_cols <- paste0("EBV.", seq_along(rel_weights))
      joint_ebvs$index_val <- as.vector(as.matrix(joint_ebvs[ebv_cols]) %*% rel_weights)
      
      cand_ebv <- candidates %>%
        left_join(joint_ebvs, by = c("id" = "ID")) %>%
        select(id, sex, index_val)
      
      m_ebv <- cand_ebv %>% filter(id %in% males_to_select) %>% select(id, index_val)
      f_ebv <- cand_ebv %>% filter(id %in% females_to_select) %>% select(id, index_val)
      
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
      
      quad_combined <- if (exists("kin_quads")) bind_rows(kin_quads, ebv_quads) else ebv_quads
      
      output$quadrants_table <- renderDT({
        datatable(quad_combined, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
          formatStyle("Q25", backgroundColor = "lightgreen") %>%
          formatStyle("Q50", backgroundColor = "yellow") %>%
          formatStyle("Q75", backgroundColor = "orange") %>%
          formatStyle("Q100", backgroundColor = "coral")
      })
      
      ebv_results <- as_tibble(ebv_matrix, rownames = "Male") %>%
        pivot_longer(-Male, names_to = "Female", values_to = "EBV")
      
      full_results <- if (exists("kinship_results")) {
        left_join(kinship_results, ebv_results, by = c("Female", "Male"))
      } else {
        ebv_results %>% mutate(Kinship = NA) %>% relocate(Kinship, .after = EBV)
      }
      
      filt_results <- full_results %>%
        filter((is.na(Kinship) | Kinship < input$thresh) & EBV > 0)
      
      output$matrix <- renderDT({
        datatable(filt_results, rownames = TRUE) %>%
          formatStyle("EBV", backgroundColor = styleInterval(
            unlist(ebv_quads[1, c("Q25", "Q50", "Q75")]),
            c("coral", "orange", "yellow", "lightgreen")
          ))
      })
      
      output$message2 <- renderText(
        if (abs(weight_total - 1) > 1e-6)
          paste("Warning: trait weights do not sum to 1 (total =", round(weight_total, 4), ")")
        else
          "EBV matrix generated successfully."
      )
      
      # Download handler
      output$download1 <- downloadHandler(
        filename = function() {
          paste0("trout_app_results-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          wb <- createWorkbook()
          addWorksheet(wb, "Mate Selection")
          writeData(wb, "Mate Selection", filt_results, rowNames = TRUE)
          addWorksheet(wb, "EBV Matrix")
          writeData(wb, "EBV Matrix", ebv_matrix, rowNames = TRUE)
          saveWorkbook(wb, file, overwrite = TRUE)
        }
      )
    }
  })
}