# Relationship Matrix Helpers
# Functions to build pedigree-based (A), genomic (G), and combined (H)
# relationship matrices via the AGHmatrix package, and to read/interpret
# relationship matrices supplied directly by the user (e.g. in the Mate
# Allocation module's "upload precomputed matrix" path).

#' Read an uploaded genotype/dosage matrix
#' @param file Uploaded Shiny file object or file path
#' @return numeric matrix, individuals in rows (rownames = IDs), markers in columns
read_genotype_matrix <- function(file) {
  df <- read_uploaded_table(file, file_type = "GENOTYPES")
  if (ncol(df) < 2) {
    stop("GENOTYPES: file must have an ID column followed by one column per marker.")
  }
  ids <- as.character(df[[1]])
  if (anyDuplicated(ids) > 0) {
    stop("GENOTYPES: duplicate individual IDs found in the first column.")
  }
  mat <- as.matrix(df[, -1, drop = FALSE])
  storage.mode(mat) <- "numeric"
  rownames(mat) <- ids
  mat
}

#' Read a precomputed relationship/kinship matrix uploaded by the user
#' @param file Uploaded Shiny file object or file path
#' @return numeric matrix with rownames/colnames = individual IDs, aligned to the same order
read_relationship_matrix_upload <- function(file) {
  path <- if (is.character(file)) file else file$datapath
  name <- if (is.character(file)) basename(file) else file$name
  if (is.null(path) || !file.exists(path)) {
    stop("MATRIX: uploaded file is missing or unavailable.")
  }
  df <- tryCatch(
    utils::read.csv(path, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) stop(sprintf("MATRIX: could not read %s as a CSV with row names in the first column (%s).", name, e$message))
  )
  mat <- as.matrix(df)
  storage.mode(mat) <- "numeric"
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    stop("MATRIX: uploaded matrix is empty.")
  }
  if (nrow(mat) != ncol(mat)) {
    stop("MATRIX: uploaded matrix must be square (same individuals as rows and columns).")
  }
  if (is.null(rownames(mat)) || is.null(colnames(mat))) {
    stop("MATRIX: uploaded matrix must have individual IDs as both row and column names.")
  }
  if (!setequal(rownames(mat), colnames(mat))) {
    stop("MATRIX: row and column names must reference the same set of individual IDs.")
  }
  # Align column order to row order in case the matrix was saved reordered
  mat[rownames(mat), rownames(mat), drop = FALSE]
}

#' Convert a relationship matrix to a kinship matrix
#'
#' optiSel::candes()/opticont() expect kinship coefficients (diagonal
#' approx. 0.5 * (1 + F)), the same convention produced by kinship2::kinship().
#' A/G/H matrices built by AGHmatrix are on the relationship scale (diagonal
#' approx. 1 + F) and must be halved before use.
#'
#' @param mat Relationship or kinship matrix
#' @param already_kinship If TRUE, mat is returned unchanged. If FALSE (default),
#'   mat is treated as a relationship matrix and divided by 2.
#' @return kinship matrix suitable for run_ocs()/optiSel::candes(pKin = ...)
matrix_to_kinship <- function(mat, already_kinship = FALSE) {
  if (isTRUE(already_kinship)) mat else mat / 2
}

#' Validate a relationship/kinship matrix before use
#' @param mat Matrix to validate
#' @return TRUE (invisibly) if valid, otherwise throws an error
validate_relationship_matrix <- function(mat) {
  if (!is.matrix(mat)) {
    stop("Resulting object is not a matrix.")
  }
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    stop("Relationship matrix has no individuals. Check that your input files share matching IDs.")
  }
  if (nrow(mat) != ncol(mat)) {
    stop("Relationship matrix must be square.")
  }
  if (is.null(rownames(mat)) || is.null(colnames(mat))) {
    stop("Relationship matrix is missing individual IDs (row/column names).")
  }
  invisible(TRUE)
}

#' Convert a pedigree representation into an AGHmatrix-ready Ind/Sire/Dam data.frame
#'
#' Handles the three pedigree shapes that can reach this point:
#'   1. A raw/cleaned data.frame with id, male_parent, female_parent columns.
#'   2. The fallback_pedigree() list (id, dadid, momid) used when kinship2 is unavailable.
#'   3. A kinship2::pedigree() object, which stores parents as findex/mindex row
#'      pointers into its own id vector rather than dadid/momid columns.
#'
#' @param pedigree Pedigree object/list/data.frame as described above
#' @return data.frame(Ind, Sire, Dam) with "0" for unknown parents
#' @noRd
pedigree_to_df <- function(pedigree) {
  if (is.null(pedigree)) {
    stop("A pedigree file is required to build this matrix.")
  }

  if (is.data.frame(pedigree) &&
      all(c("id", "male_parent", "female_parent") %in% names(pedigree))) {
    return(data.frame(
      Ind  = as.character(pedigree$id),
      Sire = as.character(pedigree$male_parent),
      Dam  = as.character(pedigree$female_parent),
      stringsAsFactors = FALSE
    ))
  }

  if (!is.null(pedigree$id) && !is.null(pedigree$dadid) && !is.null(pedigree$momid)) {
    return(data.frame(
      Ind  = as.character(pedigree$id),
      Sire = as.character(pedigree$dadid),
      Dam  = as.character(pedigree$momid),
      stringsAsFactors = FALSE
    ))
  }

  # kinship2::pedigree object: parents are stored as findex/mindex, integer
  # positions into pedigree$id (0 = unknown/founder), not as id strings.
  if (!is.null(pedigree$id) && !is.null(pedigree$findex) && !is.null(pedigree$mindex)) {
    ids  <- as.character(pedigree$id)
    sire <- ifelse(pedigree$findex == 0, "0", ids[pedigree$findex])
    dam  <- ifelse(pedigree$mindex == 0, "0", ids[pedigree$mindex])
    return(data.frame(Ind = ids, Sire = sire, Dam = dam, stringsAsFactors = FALSE))
  }

  stop("Pedigree must contain id and parent columns (male_parent/female_parent, dadid/momid, or a kinship2 pedigree object).")
}

#' Build a pedigree-based (A), genomic (G), or combined (H) relationship
#' matrix using the AGHmatrix package.
#'
#' @param matrix_type One of "A", "G", "H"
#' @param pedigree A pedigree object/list with id, dadid, momid (e.g. as
#'   returned by clean_pedigree()), or a data.frame with columns id,
#'   male_parent, female_parent. Required for "A" and "H".
#' @param genotypes A numeric matrix of marker dosages, individuals in rows
#'   (rownames = IDs), markers in columns. Required for "G" and "H".
#' @param ploidy Ploidy level (default 2)
#' @param g_method Method passed to AGHmatrix::Gmatrix (default "VanRaden")
#' @param h_method Method passed to AGHmatrix::Hmatrix (default "Martini")
#' @return numeric relationship matrix (relationship scale, not kinship/0.5 scale)
build_relationship_matrix <- function(matrix_type,
                                      pedigree  = NULL,
                                      genotypes = NULL,
                                      ploidy    = 2,
                                      g_method  = "VanRaden",
                                      h_method  = "Martini") {
  matrix_type <- match.arg(matrix_type, c("A", "G", "H"))
  if (!requireNamespace("AGHmatrix", quietly = TRUE)) {
    stop("The AGHmatrix package is required to build A/G/H matrices. Install it with install.packages(\"AGHmatrix\").")
  }

  build_A <- function() {
    ped_df <- pedigree_to_df(pedigree)
    AGHmatrix::Amatrix(data = ped_df, ploidy = ploidy)
  }

  build_G <- function() {
    if (is.null(genotypes)) {
      stop("A genotype/marker file is required to build a G matrix.")
    }
    AGHmatrix::Gmatrix(SNPmatrix = genotypes, method = g_method, ploidy = ploidy, missingValue = NA)
  }

  switch(
    matrix_type,
    A = build_A(),
    G = build_G(),
    H = {
      A <- build_A()
      G <- build_G()
      AGHmatrix::Hmatrix(A = A, G = G, ploidy = ploidy, method = h_method)
    }
  )
}

#' Resolve a kinship matrix from either a pedigree upload or a precomputed
#' relationship matrix upload (A/G/H) supplied by the user.
#'
#' Centralizes the logic shared by the Kinship/EBV preview and the Run OCS
#' handler in the Mate Allocation module, so both stay in sync regardless of
#' which relationship matrix source the user chooses.
#'
#' @param source "pedigree" (default) or "upload"
#' @param pedigree_file Shiny file input for the pedigree (used when source == "pedigree")
#' @param matrix_file Shiny file input for a precomputed matrix (used when source == "upload")
#' @param matrix_is_kinship If TRUE, matrix_file values are treated as already on the
#'   kinship (0.5) scale. If FALSE (default), they are treated as a relationship
#'   matrix (A/G/H) and halved via matrix_to_kinship().
#' @return list(kinship_matrix, pedigree, stats, raw_ped)
resolve_kinship_input <- function(source,
                                  pedigree_file     = NULL,
                                  matrix_file       = NULL,
                                  matrix_is_kinship = FALSE) {
  if (identical(source, "upload")) {
    if (is.null(matrix_file)) {
      stop("Please upload a relationship matrix file (A/G/H).")
    }
    mat <- read_relationship_matrix_upload(matrix_file)
    validate_relationship_matrix(mat)
    kinship_matrix <- matrix_to_kinship(mat, already_kinship = isTRUE(matrix_is_kinship))
    return(list(kinship_matrix = kinship_matrix, pedigree = NULL, stats = NULL, raw_ped = NULL))
  }

  if (is.null(pedigree_file)) {
    stop("Please upload a pedigree file.")
  }
  raw_ped <- read_uploaded_table(pedigree_file, file_type = "PEDIGREE")
  names(raw_ped) <- tolower(names(raw_ped))
  required_cols <- c("id", "male_parent", "female_parent")
  missing_cols  <- setdiff(required_cols, colnames(raw_ped))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Missing required column(s): ", paste(missing_cols, collapse = ", "),
      ". File must contain: id, male_parent, female_parent."
    ))
  }
  cleaned_ped    <- clean_pedigree(raw_ped, return_stats = TRUE)
  final_ped      <- cleaned_ped$pedigree
  kinship_matrix <- if (requireNamespace("kinship2", quietly = TRUE)) {
    kinship2::kinship(final_ped)
  } else {
    fallback_kinship(final_ped)
  }
  list(
    kinship_matrix = kinship_matrix,
    pedigree       = final_ped,
    stats          = cleaned_ped$stats,
    raw_ped        = raw_ped
  )
}
