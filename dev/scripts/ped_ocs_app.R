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
library(optiSel)


#### Helper Functions ####
read_candidates <- function(file) {
  df <- readr::read_table(file$datapath)
  males <- filter(df, sex == "M") %>% pull(id)
  females <- filter(df, sex == "F") %>% pull(id)
  list(candidates = df, males = males, females = females)
}


clean_pedigree <- function(ped) {
  ped <- ped %>% mutate(across(c(id, sire, dam), as.factor))
  sex_ped <- ped %>% mutate(sex = case_when(id %in% sire ~ 0, id %in% dam ~ 1, TRUE ~ 2))
  messy_parents <- setdiff(intersect(sex_ped$sire, sex_ped$dam), 0) %>% as.data.frame() %>% rename(id = 1)
  parents_fixed <- sex_ped
  parents_fixed$sire[parents_fixed$sire %in% messy_parents$id] <- 0
  parents_fixed$dam[parents_fixed$dam %in% messy_parents$id] <- 0
  doubled <- parents_fixed %>% count(id, name = "freq") %>% filter(freq > 1) %>% pull(id)
  nodup <- filter(parents_fixed, !id %in% doubled)
  circdep <- nodup %>% mutate(across(c(id, sire, dam), as.character)) %>% filter(id == sire | id == dam)
  clean_ped <- anti_join(nodup, circdep, by = "id")
  ready_ped <- with(clean_ped, kinship2::fixParents(id, sire, dam, sex, missid = "0"))
  final_ped <- with(ready_ped, kinship2::pedigree(id, dadid, momid, sex, missid = "0"))
  final_ped
}

process_ebvs <- function(trait_counter, input, prefix = "") {
  ebv_inputs <- list()
  for (i in seq_len(trait_counter)) {
    file_i <- input[[paste0(prefix, "trait_file_", i)]]
    weight_i <- input[[paste0(prefix, "trait_weight_", i)]]
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

calculate_index <- function(joint_ebvs, rel_weights) {
  joint_ebvs$index_val <- rowSums(as.matrix(joint_ebvs[grep("^EBV\\.", names(joint_ebvs))]) %*% rel_weights)
  joint_ebvs
}

run_ocs <- function(candidates_df, kinship_matrix, ebv_index, desired_inbreeding_rate, num_offspring) {
  phen <- data.frame(
    Indiv = candidates_df$id,
    Sex = ifelse(candidates_df$sex == "M", "male", "female"),
    BV = ebv_index,
    isCandidate = TRUE
  )
  candidate_ids <- candidates_df$id
  sKin <- kinship_matrix[candidate_ids, candidate_ids]
  cand <- candes(phen = phen, pKin = sKin)
  con <- list(ub.pKin = desired_inbreeding_rate)
  Offspring <- opticont(method = "max.BV", cand = cand, con = con)
  Candidate <- Offspring$parent[, c("Indiv", "Sex", "oc")]
  Candidate$n <- noffspring(Candidate, num_offspring)$nOff
  Candidate <- filter(Candidate, n > 0)
  if (length(unique(Candidate$Sex)) < 2) {
    stop("❌ OCS resulted in only one sex being selected. Cannot generate mating pairs.")
  }
  Mating <- matings(Candidate, Kin = sKin)
  list(Candidate = Candidate, Mating = Mating)
}

#### UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Optimum Contribution Selection App"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Upload Files"),
        fluidRow(
          column(6, fileInput("ped_file", "Upload Pedigree File")),
          column(6, fileInput("cand_file", "Upload Candidates File"))
        )
      ),
      wellPanel(
        h4("Traits"),
        numericInput("trait_counter", "Number of Traits", value = 2, min = 1, step = 1),
        uiOutput("trait_inputs")
      ),
      wellPanel(
        h4("Breeding Parameters"),
        fluidRow(
          column(6, numericInput("inbreeding_rate", "Desired Inbreeding Rate", value = 0.05, min = 0.01, max = 0.2, step = 0.01)),
          column(6, numericInput("num_offspring", "Number of Offspring", value = 100, min = 10, step = 1))
        )
      ),
      actionButton("run_btn", "Run OCS", style = "margin-top: 15px; width: 100%;")
    ),
    mainPanel(
      h3("Selected Candidates"),
      DTOutput("candidate_table"),
      br(),
      h3("Mating Plan"),
      DTOutput("mating_table"),
      downloadButton("download_mating", "Download Mating Plan")
    )
  )
)

#### Server ####
server <- function(input, output, session) {
  results_reactive <- reactiveVal()
  
  output$trait_inputs <- renderUI({
    req(input$trait_counter)
    lapply(1:input$trait_counter, function(i) {
      fluidRow(
        column(6, fileInput(paste0("trait_file_", i), paste("Upload EBV File", i))),
        column(6, numericInput(paste0("trait_weight_", i), paste("Weight for Trait", i),
                               value = round(1 / input$trait_counter, 2), min = 0, max = 1, step = 0.01))
      )
    })
  })
  
  observeEvent(input$run_btn, {
    req(input$ped_file, input$cand_file)
    
    ped_data <- read.table(input$ped_file$datapath, header = TRUE)
    candidates <- read.table(input$cand_file$datapath, header = TRUE)
    
    final_ped <- clean_pedigree(ped_data)
    kinship_matrix <- kinship(final_ped)
    
    ebv_result <- process_ebvs(input$trait_counter, input)
    if (is.null(ebv_result) || abs(ebv_result$weight_total - 1) > 1e-6) {
      showModal(modalDialog("Weights must sum to 1.", easyClose = TRUE))
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
    
    results_reactive(results)
    
    output$candidate_table <- renderDT({
      results$Candidate %>%
        select(Indiv, Sex, oc, n) %>%
        mutate(`Optimal Contribution (%)` = round(oc * 100, 1)) %>%
        rename(`ID` = Indiv, `# of offspring` = n) %>%
        select(ID, Sex, `Optimal Contribution`, `# of offspring`)
    })
  })
  
  output$mating_table <- renderDT({
    req(results_reactive())
    results <- results_reactive()
    results$Mating %>%
      rename(
        Male = Sire,
        Female = Dam,
        Kinship = Kin
      ) %>%
      mutate_all(as.character) %>%
      datatable(
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
  })
  
  output$download_mating <- downloadHandler(
    filename = function() {
      "mating_plan.xlsx"
    },
    content = function(file) {
      results <- results_reactive()
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "Optimal Contributions")
      contrib_df <- results$Candidate %>%
        select(Indiv, Sex, oc, n) %>%
        mutate(`Optimal Contribution (%)` = round(oc * 100, 1)) %>%
        rename(`ID` = Indiv, `# of offspring` = n)
      openxlsx::writeData(wb, sheet = "Optimal Contributions", contrib_df)
      
      openxlsx::addWorksheet(wb, "Optimal Matings")
      openxlsx::writeData(wb, sheet = "Optimal Matings", results$Mating)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)