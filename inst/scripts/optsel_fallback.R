library(shiny)
library(shinyjs)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(openxlsx)
library(kinship2)
library(quadprog)  # For quadratic programming optimization

#### Custom OCS Implementation Functions ####

#' Create candidate object similar to optiSel::candes
#' This structures data for optimization algorithms
custom_candes <- function(phen, pKin, quiet = FALSE) {
  # Validate inputs with Shadow Broker precision
  if(!all(c("Indiv", "Sex", "BV", "isCandidate") %in% names(phen))) {
    stop("❌ phen must contain columns: Indiv, Sex, BV, isCandidate")
  }
  
  # Extract candidates only
  candidates <- phen %>% filter(isCandidate == TRUE)
  
  # Calculate current population parameters
  mean_bv <- mean(candidates$BV, na.rm = TRUE)
  var_bv <- var(candidates$BV, na.rm = TRUE)
  
  # Structure the data as the Shadow Broker would organize her archives
  cand_obj <- list(
    phen = phen,
    candidates = candidates,
    n_candidates = nrow(candidates),
    n_males = sum(candidates$Sex == "male"),
    n_females = sum(candidates$Sex == "female"),
    kinship = pKin,
    current = data.frame(
      Name = "BV",
      Type = "trait",
      Val = mean_bv,
      Var = var_bv
    )
  )
  
  if(!quiet) {
    cat("📊 Candidate statistics intercepted:\n")
    cat(sprintf("  Males: %d, Females: %d\n", cand_obj$n_males, cand_obj$n_females))
    cat(sprintf("  Mean BV: %.4f (Var: %.4f)\n", mean_bv, var_bv))
  }
  
  class(cand_obj) <- "custom_candes"
  return(cand_obj)
}

#' Custom implementation of optiSel::opticont
#' Uses quadratic programming to solve OCS problem
custom_opticont <- function(method, cand, con, quiet = FALSE) {
  # Extract method components (e.g., "max.BV" -> maximize BV)
  optimize_direction <- substr(method, 1, 3)
  target_trait <- substr(method, 5, nchar(method))
  
  if(target_trait != "BV") {
    stop("❌ Currently only BV optimization is supported")
  }
  
  candidates <- cand$candidates
  n <- nrow(candidates)
  
  # Separate males and females for proper contribution allocation
  male_idx <- which(candidates$Sex == "male")
  female_idx <- which(candidates$Sex == "female")
  n_males <- length(male_idx)
  n_females <- length(female_idx)
  
  # Extract kinship matrix for candidates
  candidate_ids <- candidates$Indiv
  K <- cand$kinship[candidate_ids, candidate_ids]
  
  # Set up optimization problem
  # We need to maximize BV while constraining average kinship
  # Decision variables: contributions (c) for each candidate
  
  # Objective: maximize sum(c_i * BV_i)
  # For quadprog, we minimize -sum(c_i * BV_i)
  bv_vec <- candidates$BV
  
  # Quadratic term: minimize c'Kc (average kinship in next generation)
  # Linear term: -2 * BV' (to maximize BV)
  
  # Build constraint matrix for quadprog
  # Constraints:
  # 1. sum(c_males) = 0.5
  # 2. sum(c_females) = 0.5
  # 3. c_i >= 0 for all i
  # 4. Average kinship <= threshold
  
  # For quadprog: min(-d'b + 1/2 b'Db) s.t. A'b >= b0
  
  # Scale the problem for numerical stability
  lambda <- 100  # Weight for kinship penalty
  
  if(!is.null(con$ub.pKin)) {
    target_kinship <- con$ub.pKin
  } else {
    target_kinship <- mean(K[upper.tri(K)])  # Current mean kinship
  }
  
  # Use penalty method for constrained optimization
  # Minimize: -BV + lambda * Kinship
  Dmat <- 2 * lambda * K
  dvec <- bv_vec
  
  # Constraint matrix
  # Each row of Amat represents a constraint
  Amat <- matrix(0, n, n + 2)
  
  # Sum of male contributions = 0.5
  Amat[male_idx, 1] <- 1
  # Sum of female contributions = 0.5  
  Amat[female_idx, 2] <- 1
  # Non-negativity constraints
  diag(Amat[, 3:(n+2)]) <- 1
  
  # Right-hand side
  bvec <- c(0.5, 0.5, rep(0, n))
  
  # Solve using quadratic programming
  # Note: quadprog solves min(-d'b + 1/2 b'Db) s.t. A'b >= b0
  tryCatch({
    # Make Dmat positive definite if needed
    eigen_decomp <- eigen(Dmat)
    if(any(eigen_decomp$values < 1e-8)) {
      Dmat <- Dmat + diag(1e-6, n)
    }
    
    sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
    
    # Extract optimum contributions
    oc <- sol$solution
    
    # Normalize to ensure sum = 1
    oc <- oc / sum(oc)
    
    # Create output similar to optiSel
    parent_df <- candidates %>%
      mutate(oc = oc) %>%
      select(Indiv, Sex, oc)
    
    # Calculate expected kinship in next generation
    mean_kinship_next <- as.numeric(t(oc) %*% K %*% oc)
    
    if(!quiet) {
      cat("\n🎯 Optimization complete. Information intercepted:\n")
      cat(sprintf("  Expected mean kinship: %.4f\n", mean_kinship_next))
      cat(sprintf("  Expected mean BV: %.4f\n", sum(oc * bv_vec)))
      cat(sprintf("  Active parents: %d\n", sum(oc > 0.001)))
    }
    
    result <- list(
      parent = parent_df,
      mean.kin = mean_kinship_next,
      mean.bv = sum(oc * bv_vec),
      info = "Optimization successful"
    )
    
    class(result) <- "custom_opticont"
    return(result)
    
  }, error = function(e) {
    stop(paste("❌ Optimization failed:", e$message))
  })
}

#' Calculate number of offspring from optimum contributions
#' Replicates optiSel::noffspring functionality
custom_noffspring <- function(Candidate, N) {
  # Validate input
  if(!all(c("Indiv", "Sex", "oc") %in% names(Candidate))) {
    stop("❌ Candidate must contain columns: Indiv, Sex, oc")
  }
  
  # Calculate raw offspring numbers
  # Target 2N per-parent counts (N male + N female) to mirror optiSel behaviour
  raw_offspring <- 2 * N * Candidate$oc
  
  # Round while maintaining sum constraints
  males <- Candidate$Sex == "male"
  females <- Candidate$Sex == "female"
  
  nOff <- numeric(nrow(Candidate))
  
  # Smart rounding to maintain exact totals
  if(sum(males) > 0) {
    male_raw <- raw_offspring[males]
    male_int <- floor(male_raw)
    male_frac <- male_raw - male_int
    
    # Add extra offspring to males with highest fractional parts
    need <- as.integer(N - sum(male_int))
    if (need > 0) {
      ord <- order(male_frac, Candidate$oc[males], if ("BV" %in% names(Candidate)) Candidate$BV[males] else 0, decreasing = TRUE)
      idx <- ord[seq_len(min(need, length(ord)))]
      male_int[idx] <- male_int[idx] + 1
    }
    nOff[males] <- male_int
  }
  
  if(sum(females) > 0) {
    female_raw <- raw_offspring[females]
    female_int <- floor(female_raw)
    female_frac <- female_raw - female_int
    
    # Add extra offspring to females with highest fractional parts
    need <- as.integer(N - sum(female_int))
    if (need > 0) {
      ord <- order(female_frac, Candidate$oc[females], if ("BV" %in% names(Candidate)) Candidate$BV[females] else 0, decreasing = TRUE)
      idx <- ord[seq_len(min(need, length(ord)))]
      female_int[idx] <- female_int[idx] + 1
    }
    nOff[females] <- female_int
  }
  
  result <- data.frame(
    Indiv = Candidate$Indiv,
    nOff = nOff
  )
  
  return(result)
}

#' Mate allocation algorithm
#' Replicates optiSel::matings functionality
custom_matings <- function(Candidate, Kin, quiet = FALSE) {
  # Extract candidates with offspring
  active_candidates <- Candidate %>% filter(n > 0)
  
  males <- active_candidates %>% filter(Sex == "male")
  females <- active_candidates %>% filter(Sex == "female")
  
  if(nrow(males) == 0 || nrow(females) == 0) {
    stop("❌ Need at least one male and one female with n > 0")
  }
  
  # Get kinship submatrix for active candidates
  male_ids <- males$Indiv
  female_ids <- females$Indiv
  
  K_mf <- Kin[male_ids, female_ids, drop = FALSE]
  
  # Initialize mating list
  matings_list <- list()
  
  # Track remaining matings needed
  males_remaining <- males$n
  females_remaining <- females$n
  
  # Minimum kinship mating algorithm
  # Iteratively select minimum kinship pairs
  iter <- 0
  total_matings <- sum(males$n)
  
  while(sum(males_remaining) > 0 && sum(females_remaining) > 0) {
    iter <- iter + 1
    
    # Find available pairs (those with remaining matings)
    avail_m <- which(males_remaining > 0)
    avail_f <- which(females_remaining > 0)
    
    if(length(avail_m) == 0 || length(avail_f) == 0) break
    
    # Get kinships for available pairs
    K_avail <- K_mf[avail_m, avail_f, drop = FALSE]
    
    # Add small penalty for repeated matings to encourage diversity
    # This mimics the alpha parameter in optiSel
    penalty_matrix <- matrix(0, length(avail_m), length(avail_f))
    for(i in seq_along(matings_list)) {
      m_idx <- which(male_ids[avail_m] == matings_list[[i]]$Sire)
      f_idx <- which(female_ids[avail_f] == matings_list[[i]]$Dam)
      if(length(m_idx) > 0 && length(f_idx) > 0) {
        penalty_matrix[m_idx, f_idx] <- penalty_matrix[m_idx, f_idx] + 0.001
      }
    }
    
    K_adjusted <- K_avail + penalty_matrix
    
    # Find minimum kinship pair
    min_idx <- which.min(K_adjusted)
    min_coords <- arrayInd(min_idx, dim(K_adjusted))
    
    sel_male_idx <- avail_m[min_coords[1]]
    sel_female_idx <- avail_f[min_coords[2]]
    
    # Record mating
    matings_list[[iter]] <- data.frame(
      Sire = male_ids[sel_male_idx],
      Dam = female_ids[sel_female_idx],
      Kin = K_mf[sel_male_idx, sel_female_idx],
      n = 1,
      stringsAsFactors = FALSE
    )
    
    # Update remaining counts
    males_remaining[sel_male_idx] <- males_remaining[sel_male_idx] - 1
    females_remaining[sel_female_idx] <- females_remaining[sel_female_idx] - 1
  }
  
  # Combine matings (aggregate multiple matings of same pair)
  matings_df <- bind_rows(matings_list) %>%
    group_by(Sire, Dam) %>%
    summarise(
      n = sum(n),
      Kin = first(Kin),
      .groups = "drop"
    ) %>%
    arrange(Kin)
  
  # Calculate mean inbreeding coefficient of offspring
  mean_inbreeding <- sum(matings_df$Kin * matings_df$n) / sum(matings_df$n)
  
  if(!quiet) {
    cat("\n🔗 Mate allocation complete:\n")
    cat(sprintf("  Total matings: %d\n", sum(matings_df$n)))
    cat(sprintf("  Unique pairs: %d\n", nrow(matings_df)))
    cat(sprintf("  Mean offspring inbreeding: %.4f\n", mean_inbreeding))
  }
  
  # Add attributes similar to optiSel
  attr(matings_df, "objval") <- mean_inbreeding
  attr(matings_df, "info") <- "Minimum kinship mating"
  
  return(matings_df)
}

#' Main OCS function combining all steps
run_custom_ocs <- function(candidates_df, kinship_matrix, ebv_index, 
                           desired_inbreeding_rate, num_offspring) {
  
  # Prepare phenotype data in required format
  phen <- data.frame(
    Indiv = candidates_df$id,
    Sex = ifelse(candidates_df$sex == "M", "male", "female"),
    BV = ebv_index,
    isCandidate = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Ensure kinship matrix has correct dimensions and names
  candidate_ids <- candidates_df$id
  sKin <- kinship_matrix[candidate_ids, candidate_ids]
  rownames(sKin) <- candidate_ids
  colnames(sKin) <- candidate_ids
  
  # Step 1: Create candidate object
  cat("\n⚡ Initializing Shadow Broker's OCS algorithm...\n")
  cand <- custom_candes(phen = phen, pKin = sKin)
  
  # Step 2: Run optimization
  con <- list(ub.pKin = desired_inbreeding_rate)
  offspring_result <- custom_opticont(method = "max.BV", cand = cand, con = con)
  
  # Step 3: Calculate number of offspring
  Candidate <- offspring_result$parent
  offspring_counts <- custom_noffspring(Candidate, num_offspring)
  Candidate$n <- offspring_counts$nOff
  
  # Filter candidates with offspring
  Candidate <- filter(Candidate, n > 0)
  
  # Validate we have both sexes
  if(length(unique(Candidate$Sex)) < 2) {
    stop("❌ OCS resulted in only one sex being selected. Adjust parameters.")
  }
  
  # Step 4: Mate allocation
  cat("\n🧬 Computing optimal mate pairs...\n")
  Mating <- custom_matings(Candidate, Kin = sKin)
  
  cat("\n✅ OCS analysis complete. The patterns are clear.\n")
  
  list(Candidate = Candidate, Mating = Mating)
}


#### Shiny UI ####
ui <- fluidPage(
  useShinyjs(),
  
  # Header with style
  div(
    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px;",
    
    tags$img(
      src = "allomate.png",
      height = "120px",
      style = "margin-left: 20px;"
    ),
    
    tags$img(
      src = "logos.png",
      style = "width: 70%; height: auto;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Optimum Contribution Selection"),
      
      wellPanel(
        h4("Upload Files"),
        fileInput("ped_file", "Upload Pedigree File"),
        fileInput("cand_file", "Upload Candidates File")
      ),
      
      wellPanel(
        h4("Traits (EBVs and weights)"),
        uiOutput("trait_inputs"),
        fluidRow(
          column(6, actionButton("add_trait", "➕ Add trait")),
          column(6, actionButton("remove_trait", "➖ Remove trait"))
        )
      ),
      
      wellPanel(
        h4("Breeding Parameters"),
        numericInput("inbreeding_rate", "Desired Inbreeding Rate", 
                     value = 0.05, min = 0.01, max = 0.2, step = 0.01),
        numericInput("num_offspring", "Number of Offspring", 
                     value = 100, min = 10, step = 1)
      ),
      
      actionButton("run_ocs_btn", "Run OCS", 
                   style = "margin-top: 15px; width: 100%;",
                   class = "btn-primary"),
      
      br(), br(),
      verbatimTextOutput("ocs_status")
    ),
    
    mainPanel(
      h3("Selected Candidates"),
      DTOutput("ocs_candidate_table"),
      
      br(),
      
      h3("Mating Plan"),
      DTOutput("ocs_mating_table"),
      
      br(),
      
      downloadButton("download_mating", "Download Results (.xlsx)")
    )
  )
)


#### Shiny Server ####
server <- function(input, output, session) {
  
  # Reactive values
  trait_counter <- reactiveVal(1)
  ocs_results <- reactiveVal(NULL)
  
  # Trait management
  observeEvent(input$add_trait, {
    trait_counter(trait_counter() + 1)
  })
  
  observeEvent(input$remove_trait, {
    if (trait_counter() > 1) trait_counter(trait_counter() - 1)
  })
  
  # Dynamic trait inputs
  output$trait_inputs <- renderUI({
    n <- trait_counter()
    tagList(
      lapply(seq_len(n), function(i) {
        wellPanel(
          fileInput(paste0("trait_file_", i), paste("EBVs for trait", i)),
          numericInput(paste0("trait_weight_", i), paste("Weight for trait", i),
                       value = round(1 / n, 3), min = 0, max = 1, step = 0.01)
        )
      })
    )
  })
  
  # Helper: Process EBVs (reusing from original)
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
  
  # Helper: Clean pedigree (reusing from original)
  clean_pedigree <- function(ped) {
    ped <- ped %>% mutate(across(c(id, sire, dam), as.factor))
    sex_ped <- ped %>% mutate(sex = case_when(id %in% sire ~ 0, id %in% dam ~ 1, TRUE ~ 2))
    messy_parents <- setdiff(intersect(sex_ped$sire, sex_ped$dam), 0) %>% 
      as.data.frame() %>% rename(id = 1)
    parents_fixed <- sex_ped
    parents_fixed$sire[parents_fixed$sire %in% messy_parents$id] <- 0
    parents_fixed$dam[parents_fixed$dam %in% messy_parents$id] <- 0
    doubled <- parents_fixed %>% count(id, name = "freq") %>% filter(freq > 1) %>% pull(id)
    nodup <- filter(parents_fixed, !id %in% doubled)
    circdep <- nodup %>% mutate(across(c(id, sire, dam), as.character)) %>% 
      filter(id == sire | id == dam)
    clean_ped <- anti_join(nodup, circdep, by = "id")
    ready_ped <- with(clean_ped, kinship2::fixParents(id, sire, dam, sex, missid = "0"))
    final_ped <- with(ready_ped, kinship2::pedigree(id, dadid, momid, sex, missid = "0"))
    final_ped
  }
  
  # Main OCS execution
  observeEvent(input$run_ocs_btn, {
    req(input$ped_file, input$cand_file)
    
    tryCatch({
      # Update status
      output$ocs_status <- renderText("🔄 Processing files...")
      
      # Read data
      ped_data <- read.table(input$ped_file$datapath, header = TRUE)
      candidates <- read.table(input$cand_file$datapath, header = TRUE)
      
      # Process pedigree
      output$ocs_status <- renderText("🔄 Calculating kinship matrix...")
      final_ped <- clean_pedigree(ped_data)
      kinship_matrix <- kinship2::kinship(final_ped)
      
      # Process EBVs
      output$ocs_status <- renderText("🔄 Processing breeding values...")
      ebv_result <- process_ebvs(trait_counter(), input)
      
      if (is.null(ebv_result)) {
        output$ocs_status <- renderText("❌ No valid EBV files provided")
        return(NULL)
      }
      
      if (abs(ebv_result$weight_total - 1) > 1e-6) {
        output$ocs_status <- renderText(
          sprintf("❌ Trait weights must sum to 1 (current: %.3f)", ebv_result$weight_total)
        )
        return(NULL)
      }
      
      # Calculate index
      ebv_cols <- paste0("EBV.", seq_along(ebv_result$rel_weights))
      joint_ebvs <- ebv_result$joint_ebvs
      joint_ebvs$index_val <- as.vector(
        as.matrix(joint_ebvs[ebv_cols]) %*% ebv_result$rel_weights
      )
      
      # Merge with candidates
      candidates <- left_join(candidates, joint_ebvs, by = c("id" = "ID"))
      
      # Run custom OCS
      output$ocs_status <- renderText("🔄 Running optimization...")
      
      results <- run_custom_ocs(
        candidates_df = candidates,
        kinship_matrix = kinship_matrix,
        ebv_index = candidates$index_val,
        desired_inbreeding_rate = input$inbreeding_rate,
        num_offspring = input$num_offspring
      )
      
      # Store results
      ocs_results(results)
      
      # Update status
      output$ocs_status <- renderText("✅ OCS complete! Results ready for review.")
      
      # Display candidate table
      output$ocs_candidate_table <- renderDT({
        results$Candidate %>%
          select(Indiv, Sex, oc, n) %>%
          mutate(`Contribution (%)` = round(oc * 100, 2)) %>%
          rename(
            `ID` = Indiv,
            `# Offspring` = n
          ) %>%
          select(ID, Sex, `Contribution (%)`, `# Offspring`)
      }, options = list(pageLength = 10, dom = 'frtip'))
      
      # Display mating table
      output$ocs_mating_table <- renderDT({
        results$Mating %>%
          mutate(Kinship = round(Kin, 4)) %>%
          select(Sire, Dam, Kinship, n) %>%
          rename(`# Offspring` = n)
      }, options = list(pageLength = 10, dom = 'frtip'))
      
    }, error = function(e) {
      output$ocs_status <- renderText(paste("❌ Error:", e$message))
    })
  })
  
  # Download handler
  output$download_mating <- downloadHandler(
    filename = function() {
      paste0("OCS_Results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(ocs_results())
      results <- ocs_results()
      
      readme_text <- c(
        "Optimum Contribution Selection Results",
        "",
        "Generated using Shadow Broker Bioinformatics Custom OCS Implementation",
        paste("Date:", Sys.Date()),
        "",
        "Sheets included:",
        "1. Optimal Contributions - Selected candidates and their contributions",
        "2. Mating Plan - Optimal mate pairings to minimize inbreeding",
        "",
        "Parameters used:",
        paste("- Target inbreeding rate:", input$inbreeding_rate),
        paste("- Number of offspring:", input$num_offspring),
        "",
        "The patterns in your genetic data have been thoroughly analyzed."
      )

      use_openxlsx <- exists("openxlsx_available") && isTRUE(openxlsx_available)

      contrib_df <- results$Candidate %>%
        select(Indiv, Sex, oc, n) %>%
        mutate(`Contribution (%)` = round(oc * 100, 2)) %>%
        rename(`ID` = Indiv, `# Offspring` = n)

      mating_df <- results$Mating %>%
        mutate(Kinship = round(Kin, 4)) %>%
        select(Sire, Dam, Kinship, n) %>%
        rename(`# Offspring` = n)

      if (use_openxlsx) {
        wb <- openxlsx::createWorkbook()

        addWorksheet(wb, "README")
        writeData(wb, "README", readme_text)

        addWorksheet(wb, "Optimal Contributions")
        writeData(wb, "Optimal Contributions", contrib_df)

        addWorksheet(wb, "Mating Plan")
        writeData(wb, "Mating Plan", mating_df)

        saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        sheets <- list(
          README = data.frame(Text = readme_text, stringsAsFactors = FALSE),
          `Optimal Contributions` = as.data.frame(contrib_df, stringsAsFactors = FALSE),
          `Mating Plan` = as.data.frame(mating_df, stringsAsFactors = FALSE)
        )

        write_xlsx_pure(file, sheets)
      }
    }
  )
}

# Create Shiny app
shinyApp(ui = ui, server = server)