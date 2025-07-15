library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(kinship2)
library(DT)
library(tibble)
library(openxlsx)

#### Helper Functions ####

read_candidates <- function(file) {
  df <- readr::read_table(file$datapath)
  list(
    candidates = df,
    males = filter(df, sex == "M") %>% pull(id),
    females = filter(df, sex == "F") %>% pull(id)
  )
}

clean_pedigree <- function(ped) {
  ped <- ped %>%
    mutate(
      id = as.factor(id),
      sire = as.factor(sire),
      dam = as.factor(dam)
    )
  
  sex_ped <- ped %>%
    mutate(
      sex = case_when(
        id %in% sire ~ 0,
        id %in% dam ~ 1,
        TRUE ~ 2
      )
    )
  
  messy_parents <- setdiff(intersect(sex_ped$sire, sex_ped$dam), 0) %>%
    as.data.frame() %>%
    rename(id = 1)
  
  parents_fixed <- sex_ped
  parents_fixed$sire[parents_fixed$sire %in% messy_parents$id] <- 0
  parents_fixed$dam[parents_fixed$dam %in% messy_parents$id] <- 0
  
  doubled <- parents_fixed %>%
    count(id, name = "freq") %>%
    filter(freq > 1) %>%
    pull(id)
  
  nodup <- filter(parents_fixed, !id %in% doubled)
  
  circdep <- nodup %>%
    mutate(across(c(id, sire, dam), as.character)) %>%
    filter(id == sire | id == dam)
  
  clean_ped <- anti_join(nodup, circdep, by = "id")
  
  ready_ped <- with(clean_ped, kinship2::fixParents(id, sire, dam, sex, missid = "0"))
  final_ped <- with(ready_ped, kinship2::pedigree(id, dadid, momid, sex, missid = "0"))
  
  final_ped
}

compute_kinship_matrix <- function(ped, males, females) {
  kinship_matrix <- kinship2::kinship(ped)
  kin_mat_sel <- kinship_matrix[males, females]
  
  kin_quads <- tibble(
    Data = "Kinship",
    Q25 = quantile(kin_mat_sel, 0.25),
    Q50 = quantile(kin_mat_sel, 0.50),
    Q75 = quantile(kin_mat_sel, 0.75),
    Q100 = quantile(kin_mat_sel, 1.00)
  ) %>% column_to_rownames("Data")
  
  kinship_results <- as_tibble(kin_mat_sel, rownames = "Male") %>%
    pivot_longer(-Male, names_to = "Female", values_to = "Kinship")
  
  list(results = kinship_results, quads = kin_quads, matrix = kin_mat_sel)
}

process_ebvs <- function(trait_counter, input) {
  ebv_inputs <- list()
  for (i in seq_len(trait_counter)) {
    file_i <- input[[paste0("trait_file_", i)]]
    weight_i <- input[[paste0("trait_weight_", i)]]
    if (!is.null(file_i) && !is.null(weight_i)) {
      df_raw <- readr::read_table(file_i$datapath)
      if (!"ID" %in% names(df_raw)) names(df_raw)[1] <- "ID"
      if (!"EBV" %in% names(df_raw)) names(df_raw)[2] <- "EBV"
      ebv_inputs <- append(ebv_inputs, list(select(df_raw, ID, EBV), weight_i))
    }
  }
  
  if (length(ebv_inputs) >= 2 && length(ebv_inputs) %% 2 == 0) {
    rel_weights <- unlist(ebv_inputs[seq(2, length(ebv_inputs), by = 2)])
    weight_total <- sum(rel_weights)
    ebv_dfs <- ebv_inputs[seq(1, length(ebv_inputs), by = 2)]
    ebv_dfs <- purrr::imap(ebv_dfs, ~ rename(.x, !!paste0("EBV.", .y) := EBV))
    
    joint_ebvs <- purrr::reduce(ebv_dfs, full_join, by = "ID") %>%
      mutate(across(starts_with("EBV."), ~ tidyr::replace_na(.x, 0)))
    
    list(joint_ebvs = joint_ebvs, rel_weights = rel_weights, weight_total = weight_total)
  } else {
    NULL
  }
}

#### Server Function ####

server <- function(input, output, session) {
  
  trait_counter <- reactiveVal(1)
  
  observeEvent(input$add_trait, {
    trait_counter(trait_counter() + 1)
  })
  
  observeEvent(input$remove_trait, {
    if (trait_counter() > 1) trait_counter(trait_counter() - 1)
  })
  
  output$trait_inputs <- renderUI({
    n <- trait_counter()
    tagList(
      lapply(seq_len(n), function(i) {
        wellPanel(
          fileInput(paste0("trait_file_", i), paste("EBVs for trait", i)),
          numericInput(paste0("trait_weight_", i), paste("Relative weight for Trait", i),
                       value = round(1 / n, 3), min = 0, max = 1, step = 0.01)
        )
      })
    )
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
      
      output$message1 <- renderText("✅ Kinship matrix generated successfully.")
    }, error = function(e) {
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
    
    output$download1 <- downloadHandler(
      filename = function() {
        paste0("AlloMate_results-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()
        
        # Add README worksheet with explanation
        addWorksheet(wb, "README")
        readme_text <- c(
          "This Excel file contains two data sheets generated by the app:",
          "",
          "1. Filtered Results - Table:",
          "   - Only crosses with positive EBVs and kinship below the selected threshold are included.",
          "   - Crosses failing these criteria are completely removed from this table.",
          "",
          "2. EBV Matrix - Masked:",
          "   - Shows all possible male-female crosses with EBV values.",
          "   - EBVs for crosses with negative values or kinship above the threshold are blank (hidden) in the matrix.",
          "",
          "This distinction allows detailed matrix views while keeping the filtered table clean for analysis."
        )
        writeData(wb, "README", readme_text)
        
        addWorksheet(wb, "Filtered Results")
        writeData(wb, "Filtered Results", filt_results_table, rowNames = TRUE)
        
        addWorksheet(wb, "EBV Matrix")
        
        m_ids <- unique(full_results$Male)
        f_ids <- unique(full_results$Female)
        mat_for_excel <- matrix(NA_real_, nrow = length(m_ids), ncol = length(f_ids),
                                dimnames = list(m_ids, f_ids))
        
        for (i in seq_len(nrow(filt_results_matrix))) {
          m <- filt_results_matrix$Male[i]
          f <- filt_results_matrix$Female[i]
          val <- filt_results_matrix$EBV[i]
          mat_for_excel[m, f] <- val
        }
        
        writeData(wb, "EBV Matrix", mat_for_excel, rowNames = TRUE)
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}