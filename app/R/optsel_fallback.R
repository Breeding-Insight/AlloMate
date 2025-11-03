# Custom OCS Implementation Functions
# Note: All required packages are loaded in global.R

#### Custom OCS Implementation Functions ####

#' Fallback optimization when quadprog is not available
#' Uses a simple gradient-based approach to find optimal contributions
fallback_optimization <- function(bv_vec, K, male_idx, female_idx, target_kinship, lambda) {
  n <- length(bv_vec)
  
  # Initialize with equal contributions
  oc <- rep(1/n, n)
  
  # Ensure sex balance constraints
  oc[male_idx] <- oc[male_idx] * 0.5 / sum(oc[male_idx])
  oc[female_idx] <- oc[female_idx] * 0.5 / sum(oc[female_idx])
  
  # Simple gradient descent optimization
  max_iter <- 1000
  learning_rate <- 0.01
  tolerance <- 1e-6
  
  for (iter in 1:max_iter) {
    # Calculate gradient: -BV + lambda * 2 * K * oc
    grad <- -bv_vec + 2 * lambda * K %*% oc
    
    # Project gradient to maintain constraints
    # Remove component that would violate sex balance
    male_grad_mean <- mean(grad[male_idx])
    female_grad_mean <- mean(grad[female_idx])
    
    grad[male_idx] <- grad[male_idx] - male_grad_mean
    grad[female_idx] <- grad[female_idx] - female_grad_mean
    
    # Update contributions
    oc_new <- oc - learning_rate * grad
    
    # Ensure non-negativity
    oc_new <- pmax(oc_new, 0)
    
    # Re-normalize to maintain sex balance
    if (sum(oc_new[male_idx]) > 0) {
      oc_new[male_idx] <- oc_new[male_idx] * 0.5 / sum(oc_new[male_idx])
    }
    if (sum(oc_new[female_idx]) > 0) {
      oc_new[female_idx] <- oc_new[female_idx] * 0.5 / sum(oc_new[female_idx])
    }
    
    # Check convergence
    if (max(abs(oc_new - oc)) < tolerance) {
      break
    }
    
    oc <- oc_new
  }
  
  return(oc)
}

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
  
  # Try to use quadprog if available, otherwise use fallback optimization
  tryCatch({
    if (requireNamespace("quadprog", quietly = TRUE)) {
      # Use quadprog if available
      # Make Dmat positive definite if needed
      eigen_decomp <- eigen(Dmat)
      if(any(eigen_decomp$values < 1e-8)) {
        Dmat <- Dmat + diag(1e-6, n)
      }
      
      sol <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
      oc <- sol$solution
    } else {
      # Fallback: Simple gradient-based optimization
      oc <- fallback_optimization(bv_vec, K, male_idx, female_idx, target_kinship, lambda)
    }
    
    # Normalize to ensure sum = 1
    oc <- oc / sum(oc)
    
    # Create output similar to optiSel
    parent_df <- candidates %>%
      mutate(oc = oc) %>%
      select(Indiv, Sex, oc)
    
    # Calculate expected kinship in next generation
    mean_kinship_next <- as.numeric(t(oc) %*% K %*% oc)
    
    if(!quiet) {
      
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
  # Each individual contributes to N * oc offspring
  raw_offspring <- N * Candidate$oc
  
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
    n_extra_males <- N/2 - sum(male_int)
    if(n_extra_males > 0) {
      top_males <- order(male_frac, decreasing = TRUE)[1:min(n_extra_males, length(male_frac))]
      male_int[top_males] <- male_int[top_males] + 1
    }
    nOff[males] <- male_int
  }
  
  if(sum(females) > 0) {
    female_raw <- raw_offspring[females]
    female_int <- floor(female_raw)
    female_frac <- female_raw - female_int
    
    # Add extra offspring to females with highest fractional parts
    n_extra_females <- N/2 - sum(female_int)
    if(n_extra_females > 0) {
      top_females <- order(female_frac, decreasing = TRUE)[1:min(n_extra_females, length(female_frac))]
      female_int[top_females] <- female_int[top_females] + 1
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
  Mating <- custom_matings(Candidate, Kin = sKin)
  
  list(Candidate = Candidate, Mating = Mating)
}
