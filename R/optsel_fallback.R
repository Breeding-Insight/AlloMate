# Custom OCS Implementation Functions
# Note: All required packages are loaded in global.R

#' Create candidate object similar to candes
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' 
#' @param phen Data frame with columns Indiv, Sex, BV, isCandidate
#' @param pKin Kinship matrix
#' @param quiet Logical, suppress messages if TRUE
#' 
#' @return Object of class 'custom_candes'
custom_candes <- function(phen, pKin, quiet = FALSE) {
  # Validate inputs
  if(!all(c("Indiv", "Sex", "BV", "isCandidate") %in% names(phen))) {
    stop("❌ phen must contain columns: Indiv, Sex, BV, isCandidate")
  }
  
  # Extract candidates only
  candidates <- phen %>% filter(isCandidate == TRUE)
  
  # Calculate current population parameters
  mean_bv <- mean(candidates$BV, na.rm = TRUE)
  var_bv <- var(candidates$BV, na.rm = TRUE)
  
  # Structure the data
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

#' Custom implementation of opticont
#' @importFrom dplyr mutate select arrange desc
#' @importFrom magrittr %>%
#' @importFrom quadprog solve.QP
#' 
#' @param method Optimization method, e.g., "max.BV"
#' @param cand Object returned by custom_candes
#' @param con List containing constraints
#' @param quiet Logical, suppress messages if TRUE
#' @return Object of class 'custom_opticont'
custom_opticont <- function(method, cand, con, quiet = FALSE) {
  target_trait <- substr(method, 5, nchar(method))

  if (target_trait != "BV") {
    stop("❌ Currently only BV optimization is supported")
  }

  # Check if quadprog is available
  if (!requireNamespace("quadprog", quietly = TRUE)) {
    stop("❌ quadprog package is required for OCS fallback. Please install it with: install.packages('quadprog')")
  }

  if (!quiet) {
    cat("--- Optimization Diagnostics ---\n")
  }

  candidates <- cand$candidates
  n <- nrow(candidates)

  male_idx <- which(candidates$Sex == "male")
  female_idx <- which(candidates$Sex == "female")

  candidate_ids <- candidates$Indiv
  K <- cand$kinship[candidate_ids, candidate_ids, drop = FALSE]
  K <- 0.5 * (K + t(K))
  K <- as.matrix(K)
  storage.mode(K) <- "double"
  bv_vec <- as.numeric(candidates$BV)

  target_kinship <- if (!is.null(con$ub.pKin)) {
    con$ub.pKin
  } else {
    kin_vals <- K[upper.tri(K)]
    if (length(kin_vals) == 0) {
      mean(diag(K))
    } else {
      mean(kin_vals)
    }
  }

  kin_tolerance <- 1e-5

  enforce_sex_balance <- function(oc) {
    oc <- pmax(oc, 0)
    if (length(male_idx) > 0) {
      total_male <- sum(oc[male_idx])
      if (total_male > 0) {
        oc[male_idx] <- oc[male_idx] * 0.5 / total_male
      } else {
        oc[male_idx] <- rep(0.5 / length(male_idx), length(male_idx))
      }
    }
    if (length(female_idx) > 0) {
      total_female <- sum(oc[female_idx])
      if (total_female > 0) {
        oc[female_idx] <- oc[female_idx] * 0.5 / total_female
      } else {
        oc[female_idx] <- rep(0.5 / length(female_idx), length(female_idx))
      }
    }
    oc
  }

  if (isTRUE(getOption("allomate.force_qp_greedy", FALSE))) {
    if (!quiet) {
      cat("Solver used      : heuristic (quadprog bypassed)\n")
    }
    shifted_bv <- bv_vec - min(bv_vec, na.rm = TRUE)
    oc_raw <- if (all(shifted_bv <= 1e-12)) {
      rep(1 / n, n)
    } else {
      shifted_bv
    }
    oc_raw <- oc_raw / sum(oc_raw)
    oc <- enforce_sex_balance(oc_raw)

    parent_df <- candidates %>%
      mutate(oc = oc, BV = bv_vec) %>%
      select(Indiv, Sex, oc, BV)

    mean_kinship_next <- as.numeric(t(oc) %*% K %*% oc)

    if (!quiet) {
      top_df <- parent_df %>% arrange(desc(oc)) %>% head(5)
      cat("\n--- OCS Optimization Diagnostics ---\n")
      cat(sprintf("Solver used      : heuristic (quadprog bypassed)\n"))
      cat(sprintf("Target kinship   : %.5f\n", target_kinship))
      cat(sprintf("Achieved kinship : %.5f\n", mean_kinship_next))
      cat(sprintf("Mean BV          : %.5f\n", sum(oc * bv_vec)))
      cat("Top contributions:\n")
      print(top_df)
      cat("-------------------------------------\n\n")
    }

    result <- list(
      parent = parent_df,
      mean.kin = mean_kinship_next,
      mean.bv = sum(oc * bv_vec),
      info = "Heuristic contribution (quadprog bypassed)",
      solver = "heuristic"
    )

    class(result) <- "custom_opticont"
    return(result)
  }

  tryCatch({
    best <- local({
      solve_qp_for_lambda <- function(lambda) {
        Dmat <- 2 * lambda * K
        dvec <- as.numeric(bv_vec)

        male_col <- numeric(n)
        female_col <- numeric(n)
        if (length(male_idx) > 0) male_col[male_idx] <- 1
        if (length(female_idx) > 0) female_col[female_idx] <- 1

        if (sum(male_col) == 0 || sum(female_col) == 0) {
          stop("❌ Need at least one male and one female among candidates to enforce sex-balance constraints.")
        }

        Amat <- cbind(male_col, female_col, diag(1, n))
        bvec <- c(0.5, 0.5, rep(0, n))

        storage.mode(Dmat) <- "double"
        storage.mode(Amat) <- "double"
        dvec <- as.numeric(dvec)
        bvec <- as.numeric(bvec)

        eigen_values <- eigen(Dmat, symmetric = TRUE, only.values = TRUE)$values
        if (any(eigen_values < 1e-8)) {
          Dmat <- Dmat + diag(1e-6, n)
        }

        sol <- tryCatch(
          solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 2),
          error = function(e) stop(paste("QP solve failed:", e$message))
        )
        oc_qp <- enforce_sex_balance(sol$solution)
        kin <- as.numeric(t(oc_qp) %*% K %*% oc_qp)
        list(oc = oc_qp, kin = kin)
      }

      low <- 1e-6
      high <- 1e6
      best_local <- NULL

      for (iter in seq_len(30)) {
        lambda <- exp((log(low) + log(high)) / 2)
        current <- solve_qp_for_lambda(lambda)

        if (is.null(best_local) || abs(current$kin - target_kinship) < abs(best_local$kin - target_kinship)) {
          best_local <- current
        }

        if (abs(current$kin - target_kinship) < kin_tolerance) {
          best_local <- current
          break
        }

        if (current$kin > target_kinship) {
          low <- lambda
        } else {
          high <- lambda
        }
      }

      best_local
    })

    oc <- enforce_sex_balance(best$oc)

    parent_df <- candidates %>%
      mutate(oc = oc, BV = bv_vec) %>%
      select(Indiv, Sex, oc, BV)

    mean_kinship_next <- as.numeric(t(oc) %*% K %*% oc)
    
    if (!quiet) {
      top_df <- parent_df %>% arrange(desc(oc)) %>% head(5)
      cat("\n--- OCS Optimization Diagnostics ---\n")
      cat(sprintf("Solver used      : quadprog\n"))
      cat(sprintf("Target kinship   : %.5f\n", target_kinship))
      cat(sprintf("Achieved kinship : %.5f\n", mean_kinship_next))
      cat(sprintf("Mean BV          : %.5f\n", sum(oc * bv_vec)))
      cat("Top contributions:\n")
      print(top_df)
      cat("-------------------------------------\n\n")
    }

    result <- list(
      parent = parent_df,
      mean.kin = mean_kinship_next,
      mean.bv = sum(oc * bv_vec),
      info = "Optimization successful (quadprog)",
      solver = "quadprog"
    )

    class(result) <- "custom_opticont"
    return(result)
  }, error = function(e) {
    stop(paste("❌ Optimization failed:", e$message))
  })
}

#' Calculate number of offspring from optimum contributions
#' @importFrom stats xtabs
#' @param Candidate Data frame with columns Indiv, Sex, oc, optionally BV
#' @param N Total number of offspring
#' @return Data frame with Indiv and nOff
custom_noffspring <- function(Candidate, N) {
  if(!all(c("Indiv", "Sex", "oc") %in% names(Candidate))) {
    stop("❌ Candidate must contain columns: Indiv, Sex, oc")
  }

  has_bv <- "BV" %in% names(Candidate)

  # Target 2N per-parent counts (N male + N female) to mirror optiSel behaviour
  raw_offspring <- 2 * N * Candidate$oc

  males <- Candidate$Sex == "male"
  females <- Candidate$Sex == "female"

  nOff <- numeric(nrow(Candidate))

  adjust_counts <- function(raw_vals, oc_vals, bv_vals, ids_vals) {
    counts <- floor(raw_vals + 1e-9)
    frac <- raw_vals - counts
    id_key <- xtfrm(ids_vals)
    target_total <- N
    diff <- target_total - sum(counts)

    if (diff > 0) {
      ord <- order(-frac, -oc_vals, -bv_vals, id_key, na.last = TRUE)
      take <- head(ord, min(diff, length(ord)))
      counts[take] <- counts[take] + 1
    } else if (diff < 0) {
      ord <- order(frac, oc_vals, bv_vals, -id_key, na.last = TRUE)
      take <- head(ord, min(abs(diff), length(ord)))
      counts[take] <- pmax(0, counts[take] - 1)
    }
    counts
  }

  if(sum(males) > 0) {
    male_raw <- raw_offspring[males]
    male_oc <- Candidate$oc[males]
    male_bv <- if (has_bv) Candidate$BV[males] else rep(0, sum(males))
    male_ids <- Candidate$Indiv[males]
    male_counts <- adjust_counts(male_raw, male_oc, male_bv, male_ids)
    nOff[males] <- male_counts
  }

  if(sum(females) > 0) {
    female_raw <- raw_offspring[females]
    female_oc <- Candidate$oc[females]
    female_bv <- if (has_bv) Candidate$BV[females] else rep(0, sum(females))
    female_ids <- Candidate$Indiv[females]
    female_counts <- adjust_counts(female_raw, female_oc, female_bv, female_ids)
    nOff[females] <- female_counts
  }

  data.frame(
    Indiv = Candidate$Indiv,
    nOff = nOff
  )
}

#' Mate allocation algorithm
#' @importFrom dplyr filter arrange
#' @importFrom lpSolve lp.transport
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' 
#' @param Candidate Data frame with columns Indiv, Sex, n
#' @param Kin Kinship matrix
#' @param max_pair_kinship Optional numeric threshold
#' @param quiet Logical, suppress messages if TRUE
#' @return Data frame of matings with Sire, Dam, n, Kin
custom_matings <- function(Candidate, Kin, max_pair_kinship = NULL, quiet = FALSE) {
  active_candidates <- Candidate %>% filter(n > 0)

  males <- active_candidates %>% filter(Sex == "male")
  females <- active_candidates %>% filter(Sex == "female")

  if (nrow(males) == 0 || nrow(females) == 0) {
    stop("❌ Need at least one male and one female with n > 0")
  }

  male_ids <- males$Indiv
  female_ids <- females$Indiv
  supply <- as.integer(males$n)
  demand <- as.integer(females$n)

  K_mf <- Kin[male_ids, female_ids, drop = FALSE]
  costs <- as.matrix(K_mf)
  storage.mode(costs) <- "double"

  allowed_mask <- !is.na(costs)
  finite_costs <- costs[allowed_mask]
  if (length(finite_costs) == 0) {
    stop("❌ Kinship matrix does not contain any finite male/female pairings.")
  }

  enforce_threshold <- function() {
    stop(
      sprintf(
        "❌ Unable to find a mating plan where every pair has kinship < %.4f. Try increasing the per-pair limit or reducing offspring.",
        max_pair_kinship
      )
    )
  }

  if (!is.null(max_pair_kinship)) {
    allowed_mask <- allowed_mask & (costs < max_pair_kinship)
    if (!any(allowed_mask)) {
      enforce_threshold()
    }
    if (any(rowSums(allowed_mask) == 0) || any(colSums(allowed_mask) == 0)) {
      enforce_threshold()
    }
  }

  platform_tag <- tolower(R.version$platform)
  force_greedy <- isTRUE(getOption("allomate.force_greedy_mating", FALSE))
  detected_webr <- grepl("emscripten|wasm", platform_tag)
  # Always respect the checkbox setting - user choice takes precedence
  # In webR, the checkbox defaults to TRUE in global.R, but user can uncheck it
  use_greedy <- force_greedy
  lp_available <- requireNamespace("lpSolve", quietly = TRUE)

  announce_algorithm <- function(label) {
    if (!quiet) cat("Mating algorithm:", label, "\n")
  }

  run_greedy <- function(info_label) {
    announce_algorithm(info_label)

    n_m <- length(supply)
    n_f <- length(demand)
    X <- matrix(0L, nrow = n_m, ncol = n_f)

    pairs <- expand.grid(m = seq_len(n_m), f = seq_len(n_f),
                         KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    pairs$allowed <- allowed_mask[cbind(pairs$m, pairs$f)]
    pairs <- pairs[pairs$allowed, , drop = FALSE]
    if (nrow(pairs) == 0) {
      if (!is.null(max_pair_kinship)) enforce_threshold()
      stop("❌ No feasible mating pairs available.")
    }
    pairs$cost <- costs[cbind(pairs$m, pairs$f)]
    pairs <- pairs[order(pairs$cost, pairs$m, pairs$f), , drop = FALSE]

    remaining_supply <- supply
    remaining_demand <- demand

    for (k in seq_len(nrow(pairs))) {
      i <- pairs$m[k]
      j <- pairs$f[k]
      if (remaining_supply[i] <= 0L || remaining_demand[j] <= 0L) next
      assignable <- min(remaining_supply[i], remaining_demand[j])
      if (assignable > 0L) {
        X[i, j] <- X[i, j] + as.integer(assignable)
        remaining_supply[i] <- remaining_supply[i] - assignable
        remaining_demand[j] <- remaining_demand[j] - assignable
        if (sum(remaining_supply) == 0L || sum(remaining_demand) == 0L) break
      }
    }

    if (sum(remaining_supply) != 0L || sum(remaining_demand) != 0L) {
      if (!is.null(max_pair_kinship)) {
        enforce_threshold()
      }
      stop("❌ Greedy mating allocation incomplete. Check supply/demand consistency.")
    }

    idx <- which(X > 0, arr.ind = TRUE)
    if (nrow(idx) == 0) {
      stop("❌ No mating assignments produced.")
    }

    matings_df <- tibble(
      Sire = male_ids[idx[, 1]],
      Dam = female_ids[idx[, 2]],
      n = as.integer(X[idx]),
      Kin = costs[idx]
    ) %>%
      arrange(Kin)

    mean_inbreeding <- sum(matings_df$Kin * matings_df$n) / sum(matings_df$n)
    attr(matings_df, "objval") <- mean_inbreeding
    attr(matings_df, "info") <- info_label
    if (!is.null(max_pair_kinship)) {
      attr(matings_df, "info") <- paste0(
        attr(matings_df, "info"),
        sprintf(" | per-pair kinship < %.4f", max_pair_kinship)
      )
    }

    if (!quiet) {
      cat("\n--- Mating Diagnostics ---\n")
      cat(sprintf("Total matings: %d\n", sum(matings_df$n)))
      cat(sprintf("Mean offspring kinship: %.5f\n", mean_inbreeding))
      cat("--------------------------\n\n")
    }

    matings_df
  }

  greedy_label <- if (force_greedy && detected_webr) {
    "Greedy allocation (user-selected, webR environment)"
  } else if (force_greedy) {
    "Greedy allocation (user-selected)"
  } else {
    "Greedy allocation (lpSolve unavailable)"
  }

  if (use_greedy || !lp_available) {
    return(run_greedy(greedy_label))
  }

  announce_algorithm("lpSolve transportation")

  disallowed_mask <- !allowed_mask
  solver_costs <- costs
  solver_costs[is.na(solver_costs)] <- 0
  if (any(disallowed_mask)) {
    max_allowed_cost <- max(costs[allowed_mask], na.rm = TRUE)
    if (!is.finite(max_allowed_cost)) {
      max_allowed_cost <- 1
    }
    penalty_cost <- max_allowed_cost + max(1, max_allowed_cost)
    solver_costs[disallowed_mask] <- penalty_cost
  }

  enforce_integer_transport <- function(solution_matrix) {
    X_round <- round(solution_matrix)
    X_round[X_round < 0] <- 0
    storage.mode(X_round) <- "integer"

    row_check <- supply - rowSums(X_round)
    col_check <- demand - colSums(X_round)

    if (any(row_check != 0) || any(col_check != 0)) {
      return(NULL)
    }

    X_round
  }

  sol <- tryCatch(
    lp.transport(
      cost.mat = solver_costs,
      direction = "min",
      row.signs = rep("=", length(supply)),
      row.rhs = supply,
      col.signs = rep("=", length(demand)),
      col.rhs = demand
    ),
    error = function(e) {
      if (!quiet) {
        cat("lpSolve error:", e$message, "\nFalling back to greedy mating.\n")
      }
      NULL
    }
  )

  if (is.null(sol) || is.null(sol$status) || sol$status != 0) {
    if (!quiet && !is.null(sol) && !is.null(sol$status) && sol$status != 0) {
      cat("lpSolve returned non-zero status:", sol$status, "→ fallback to greedy mating.\n")
    }
    return(run_greedy("Greedy allocation (lpSolve failure fallback)"))
  }

  X <- matrix(sol$solution, nrow = length(supply), ncol = length(demand), byrow = TRUE)

  if (!is.null(max_pair_kinship)) {
    used_disallowed <- which(X > 0 & disallowed_mask, arr.ind = TRUE)
    if (nrow(used_disallowed) > 0) {
      enforce_threshold()
    }
  }

  X_integer <- enforce_integer_transport(X)
  if (is.null(X_integer)) {
    if (!quiet) {
      cat("Rounded lpSolve transport solution inconsistent with supply/demand totals. Falling back to greedy mating.\n")
    }
    return(run_greedy("Greedy allocation (lpSolve rounding fallback)"))
  }

  if (!is.null(max_pair_kinship)) {
    used_disallowed <- which(X_integer > 0 & disallowed_mask, arr.ind = TRUE)
    if (nrow(used_disallowed) > 0) {
      enforce_threshold()
    }
  }

  idx <- which(X_integer > 0, arr.ind = TRUE)
  matings_df <- tibble(
    Sire = male_ids[idx[, 1]],
    Dam = female_ids[idx[, 2]],
    n = as.integer(X_integer[idx]),
    Kin = costs[idx]
  ) %>%
    arrange(Kin)

  mean_inbreeding <- sum(matings_df$Kin * matings_df$n) / sum(matings_df$n)
  attr(matings_df, "objval") <- mean_inbreeding
  attr(matings_df, "info") <- "Minimum-cost transportation mating (lpSolve)"
  if (!is.null(max_pair_kinship)) {
    attr(matings_df, "info") <- paste0(
      attr(matings_df, "info"),
      sprintf(" | per-pair kinship < %.4f", max_pair_kinship)
    )
  }

  if (!quiet) {
    cat("\n--- Mating Diagnostics ---\n")
    cat(sprintf("Total matings: %d\n", sum(matings_df$n)))
    cat(sprintf("Mean offspring kinship: %.5f\n", mean_inbreeding))
    cat("--------------------------\n\n")
  }

  matings_df
}

#' Main OCS function combining all steps
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' 
#' @param candidates_df Data frame of candidate IDs and sex
#' @param kinship_matrix Kinship matrix
#' @param ebv_index Vector of estimated breeding values
#' @param desired_inbreeding_rate Numeric, upper limit of mean kinship
#' @param num_offspring Number of offspring to produce
#' @param per_pair_kinship_limit Optional per-pair kinship threshold
#' @return List with Candidate and Mating data frames
run_custom_ocs <- function(candidates_df, kinship_matrix, ebv_index,
                           desired_inbreeding_rate, num_offspring,
                           per_pair_kinship_limit = NULL) {
  
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
  Mating <- custom_matings(
    Candidate,
    Kin = sKin,
    max_pair_kinship = per_pair_kinship_limit
  )
  
  if (nrow(Mating) == 0) {
    stop("❌ No feasible mating plan produced. Consider relaxing constraints or reducing offspring targets.")
  }

  validate_mating_consistency(Candidate, Mating)

  list(Candidate = Candidate, Mating = Mating)
}
