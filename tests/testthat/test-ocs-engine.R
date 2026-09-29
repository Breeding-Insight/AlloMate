testdata_path <- function(...) {
  system.file("agh_testdata", ..., package = "AlloMate")
}

# 8 candidates (4 M / 4 F) with kinship from the bundled A matrix. They are
# closely related: the lowest mean kinship any set of contributions can reach
# is ~0.197, and unconstrained max-BV selection lands at ~0.3125.
load_ocs_inputs <- function() {
  skip_if(identical(testdata_path("candidate_A_matrix.csv"), ""), "agh_testdata not installed")
  cands <- read_candidates(testdata_path("candidates.txt"))$candidates
  ebvs  <- read_uploaded_table(testdata_path("length_ebvs.txt"))
  cands$index_val <- ebvs$EBV[match(cands$id, ebvs$ID)]
  kin <- resolve_kinship_input(
    "upload",
    matrix_file = testdata_path("candidate_A_matrix.csv")
  )$kinship_matrix
  list(cands = cands, kin = kin)
}

# The engine prints solver diagnostics; keep test output clean
run_test_ocs <- function(rate, num_offspring = 20, per_pair = NULL) {
  inputs <- load_ocs_inputs()
  invisible(utils::capture.output(
    res <- run_ocs(inputs$cands, inputs$kin, inputs$cands$index_val,
                   desired_inbreeding_rate = rate,
                   num_offspring           = num_offspring,
                   per_pair_kinship_limit  = per_pair)
  ))
  res
}

opticont_at <- function(rate) {
  inputs <- load_ocs_inputs()
  phen <- data.frame(
    Indiv = inputs$cands$id,
    Sex = ifelse(inputs$cands$sex == "M", "male", "female"),
    BV = inputs$cands$index_val,
    isCandidate = TRUE
  )
  custom_opticont("max.BV", custom_candes(phen, inputs$kin), list(ub.pKin = rate), quiet = TRUE)
}

test_that("custom_opticont splits contributions 0.5/0.5 between the sexes", {
  parent <- opticont_at(0.25)$parent
  expect_equal(sum(parent$oc[parent$Sex == "male"]), 0.5, tolerance = 1e-8)
  expect_equal(sum(parent$oc[parent$Sex == "female"]), 0.5, tolerance = 1e-8)
  expect_true(all(parent$oc >= 0))
})

test_that("custom_opticont hits a reachable kinship target", {
  expect_equal(opticont_at(0.25)$mean.kin, 0.25, tolerance = 1e-4)
})

test_that("relaxing the inbreeding rate never lowers genetic merit", {
  bv <- vapply(c(0.2, 0.25, 0.3), function(r) opticont_at(r)$mean.bv, numeric(1))
  expect_true(all(diff(bv) >= -1e-8))
})

test_that("run_ocs returns a plan consistent with the offspring target", {
  res <- run_test_ocs(0.25, num_offspring = 20)
  expect_named(res, c("Candidate", "Mating", "kinship"))
  expect_equal(sum(res$Mating$n), 20)
  expect_equal(sum(res$Candidate$n[res$Candidate$Sex == "male"]), 20)
  expect_equal(sum(res$Candidate$n[res$Candidate$Sex == "female"]), 20)
  expect_true(res$kinship$target_met)
  expect_lte(res$kinship$achieved, 0.25 + 1e-4)
})

test_that("an unreachable inbreeding rate returns the lowest-kinship plan and flags it", {
  res <- run_test_ocs(0.05)
  expect_false(res$kinship$target_met)
  expect_equal(res$kinship$target, 0.05)
  # Same minimum-kinship solution regardless of how far below it the target is
  expect_equal(res$kinship$achieved, run_test_ocs(0.01)$kinship$achieved, tolerance = 1e-6)
  expect_gt(res$kinship$achieved, 0.19)
  expect_equal(sum(res$Mating$n), 20)
})

test_that("format_ocs_results carries the kinship target through to summary_stats", {
  stats <- format_ocs_results(run_test_ocs(0.05))$summary_stats
  expect_false(stats$kinship_target_met)
  expect_equal(stats$target_kinship, 0.05)
  expect_gt(stats$achieved_kinship, 0.05)
})

test_that("mate allocation uses the lpSolve transport solution, not the greedy backup", {
  for (per_pair in list(NULL, 0.1)) {
    info <- attr(run_test_ocs(0.25, per_pair = per_pair)$Mating, "info")
    expect_match(info, "Minimum-cost transportation mating (lpSolve)", fixed = TRUE)
  }
})

test_that("run_ocs respects a feasible per-pair kinship limit", {
  res <- run_test_ocs(0.25, per_pair = 0.1)
  expect_true(all(res$Mating$Kin < 0.1))
})

test_that("run_ocs errors clearly when no plan satisfies the per-pair limit", {
  expect_error(run_test_ocs(0.25, per_pair = 0.05), "every pair has kinship < 0.0500")
})
