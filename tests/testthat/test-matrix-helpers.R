testdata_path <- function(...) {
  system.file("agh_testdata", ..., package = "AlloMate")
}

read_test_pedigree <- function() {
  utils::read.delim(testdata_path("pedigree.txt"), colClasses = "character")
}

read_test_genotypes <- function() {
  geno <- utils::read.csv(testdata_path("genotypes.csv"), check.names = FALSE, row.names = 1)
  as.matrix(geno)
}

skip_without_aghmatrix <- function() {
  skip_if_not_installed("AGHmatrix")
  skip_if(identical(testdata_path("pedigree.txt"), ""), "agh_testdata not installed")
}

test_that("validate_relationship_matrix rejects non-matrix input", {
  expect_error(validate_relationship_matrix(NULL), "not a matrix")
  expect_error(validate_relationship_matrix(list()), "not a matrix")
})

test_that("validate_relationship_matrix rejects malformed matrices", {
  square <- matrix(1, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_true(validate_relationship_matrix(square))

  expect_error(validate_relationship_matrix(matrix(numeric(0), nrow = 0, ncol = 0)),
               "no individuals")
  expect_error(validate_relationship_matrix(matrix(1, nrow = 2, ncol = 3)), "square")
  expect_error(validate_relationship_matrix(matrix(1, nrow = 2, ncol = 2)), "missing individual IDs")
})

test_that("pedigree_to_df handles the id/male_parent/female_parent shape", {
  ped <- data.frame(
    id            = c("a", "b"),
    male_parent   = c("0", "a"),
    female_parent = c("0", "0"),
    stringsAsFactors = FALSE
  )
  out <- pedigree_to_df(ped)
  expect_named(out, c("Ind", "Sire", "Dam"))
  expect_identical(out$Ind, c("a", "b"))
  expect_identical(out$Sire, c("0", "a"))
})

test_that("pedigree_to_df handles the fallback_pedigree list shape", {
  out <- pedigree_to_df(list(id = c("a", "b"), dadid = c("0", "a"), momid = c("0", "0")))
  expect_identical(out$Ind, c("a", "b"))
  expect_identical(out$Sire, c("0", "a"))
})

test_that("matrix_to_kinship halves relationship matrices but passes kinship through", {
  m <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  expect_equal(matrix_to_kinship(m), m / 2)
  expect_identical(matrix_to_kinship(m, already_kinship = TRUE), m)
})

test_that("build_relationship_matrix rejects H methods AGHmatrix cannot run", {
  skip_without_aghmatrix()
  ped  <- read_test_pedigree()
  geno <- read_test_genotypes()

  # These were previously offered in the UI dropdown but are Gmatrix methods.
  # Hmatrix() silently returns NULL for them, so they must be rejected up front.
  for (bad in c("Legarra", "YangAll", "YangSingle", "Munoz")) {
    expect_error(
      build_relationship_matrix("H", pedigree = ped, genotypes = geno, h_method = bad),
      "not supported"
    )
  }
})

test_that("build_relationship_matrix validates blend_weight", {
  skip_without_aghmatrix()
  ped  <- read_test_pedigree()
  geno <- read_test_genotypes()

  for (bad in list(0, -1, 1.5, NA_real_, "0.95", c(0.5, 0.5))) {
    expect_error(
      build_relationship_matrix("H", pedigree = ped, genotypes = geno, blend_weight = bad),
      "blend_weight"
    )
  }
})

test_that("build_relationship_matrix builds an A matrix covering the whole pedigree", {
  skip_without_aghmatrix()
  ped <- read_test_pedigree()

  A <- suppressMessages(build_relationship_matrix("A", pedigree = ped))
  expect_true(validate_relationship_matrix(A))
  expect_equal(nrow(A), nrow(ped))
})

test_that("build_relationship_matrix builds a G matrix over the genotyped individuals", {
  skip_without_aghmatrix()
  geno <- read_test_genotypes()

  G <- suppressMessages(build_relationship_matrix("G", genotypes = geno))
  expect_true(validate_relationship_matrix(G))
  expect_equal(nrow(G), nrow(geno))
})

test_that("an unblended VanRaden G is singular, which is why H needs blending", {
  skip_without_aghmatrix()
  geno <- read_test_genotypes()

  G <- suppressMessages(build_relationship_matrix("G", genotypes = geno))
  # Column-centring by sample allele frequency puts the ones vector in the null
  # space, so G is rank deficient no matter how many markers are supplied.
  expect_lt(rcond(G), 1e-15)
  expect_lt(max(abs(G %*% rep(1, nrow(G)))), 1e-8)
})

test_that("build_relationship_matrix builds an H matrix spanning pedigree and genotypes", {
  skip_without_aghmatrix()
  ped  <- read_test_pedigree()
  geno <- read_test_genotypes()

  H <- suppressMessages(build_relationship_matrix("H", pedigree = ped, genotypes = geno))

  expect_true(validate_relationship_matrix(H))
  # 18 pedigree individuals, 13 of them genotyped: H covers the union.
  expect_equal(nrow(H), nrow(ped))
  expect_true(all(rownames(geno) %in% rownames(H)))
  expect_false(any(is.na(H)))
  expect_equal(H, t(H), tolerance = 1e-8)
})

test_that("H matrix construction is robust across blend weights", {
  skip_without_aghmatrix()
  ped  <- read_test_pedigree()
  geno <- read_test_genotypes()

  for (w in c(0.5, 0.8, 0.95, 0.99)) {
    H <- suppressMessages(
      build_relationship_matrix("H", pedigree = ped, genotypes = geno, blend_weight = w)
    )
    expect_true(validate_relationship_matrix(H))
    expect_equal(nrow(H), nrow(ped))
  }
})

test_that("build_relationship_matrix errors clearly when genotyped IDs are absent from the pedigree", {
  skip_without_aghmatrix()
  ped  <- read_test_pedigree()
  geno <- read_test_genotypes()

  rownames(geno)[1] <- "not_in_pedigree"
  expect_error(
    suppressMessages(build_relationship_matrix("H", pedigree = ped, genotypes = geno)),
    "missing from the pedigree"
  )

  rownames(geno) <- paste0("ghost_", seq_len(nrow(geno)))
  expect_error(
    suppressMessages(build_relationship_matrix("H", pedigree = ped, genotypes = geno)),
    "No genotyped individuals"
  )
})

test_that("build_relationship_matrix requires the inputs each matrix type needs", {
  skip_without_aghmatrix()
  expect_error(build_relationship_matrix("A"), "pedigree file is required")
  expect_error(build_relationship_matrix("G"), "genotype/marker file is required")
  expect_error(build_relationship_matrix("Z"), "'arg' should be one of")
})
