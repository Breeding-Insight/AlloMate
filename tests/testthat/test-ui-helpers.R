test_that("dynamic trait inputs use the provided namespace", {
  ns <- function(id) paste0("allomate-", id)

  trait_html <- as.character(AlloMate:::create_trait_inputs(2, ns = ns))
  expect_true(grepl("allomate-trait_file_1", trait_html, fixed = TRUE))
  expect_true(grepl("allomate-trait_weight_1", trait_html, fixed = TRUE))
  expect_true(grepl("allomate-trait_file_2", trait_html, fixed = TRUE))
  expect_true(grepl("allomate-trait_weight_2", trait_html, fixed = TRUE))

  ocs_html <- as.character(shiny::tagList(AlloMate:::create_ocs_trait_inputs(2, ns = ns)))
  expect_true(grepl("allomate-ocs_trait_file_1", ocs_html, fixed = TRUE))
  expect_true(grepl("allomate-ocs_trait_weight_1", ocs_html, fixed = TRUE))
  expect_true(grepl("allomate-ocs_trait_file_2", ocs_html, fixed = TRUE))
  expect_true(grepl("allomate-ocs_trait_weight_2", ocs_html, fixed = TRUE))
})
