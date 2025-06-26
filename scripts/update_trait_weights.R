library(dplyr)
library(tidyr)
library(openxlsx)
library(DT)

update_trait_weights <- function(input, output, session, candidates, males_to_select, females_to_select, trait_counter, kinship_results = NULL) {
  
  observeEvent(trait_counter(), {
    n <- trait_counter()
    lapply(seq_len(n), function(i) {
      updateNumericInput(session, paste0("trait_weight_", i), value = round(1 / n, 3))
    })
  })
  
  observe({
    ebv_inputs <- list()
    for (i in seq_len(trait_counter())) {
      file_i <- input[[paste0("trait_file_", i)]]
      weight_i <- input[[paste0("trait_weight_", i)]]
      if (!is.null(file_i) && !is.null(weight_i)) {
        df_raw <- readr::read_table(file_i$datapath)
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
      
      quad_combined <- if (!is.null(kinship_results)) bind_rows(
        kinship_results %>% select(Q25, Q50, Q75, Q100) %>% mutate(Data = "Kinship"),
        ebv_quads %>% mutate(Data = "EBV")
      ) else {
        ebv_quads
      }
      
      output$quadrants_table <- DT::renderDT({
        datatable(quad_combined, options = list(ordering = FALSE, dom = "t"), rownames = TRUE) %>%
          formatStyle("Q25", backgroundColor = "lightgreen") %>%
          formatStyle("Q50", backgroundColor = "yellow") %>%
          formatStyle("Q75", backgroundColor = "orange") %>%
          formatStyle("Q100", backgroundColor = "coral")
      })
      
      ebv_results <- as_tibble(ebv_matrix, rownames = "Male") %>%
        pivot_longer(-Male, names_to = "Female", values_to = "EBV")
      
      full_results <- if (!is.null(kinship_results)) {
        left_join(kinship_results, ebv_results, by = c("Female", "Male"))
      } else {
        ebv_results %>% mutate(Kinship = NA) %>% relocate(Kinship, .after = EBV)
      }
      
      filt_results <- full_results %>%
        filter((is.na(Kinship) | Kinship < input$thresh) & EBV > 0)
      
      output$matrix <- DT::renderDT({
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
