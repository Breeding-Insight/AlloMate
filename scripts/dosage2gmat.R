compute_relationship_matrix <- function(mat,
                                        ploidy,
                                        method = "VanRaden",
                                        impute = TRUE) {
  ### Prepare matrix
  #Remove any SNPs that are all NAs
  mat <- mat[, colSums(is.na(mat)) < nrow(mat)]
  #impute the missing values with the mean of the column
  if (impute) {
    mat <- data.frame(mat, check.names = FALSE, check.rows = FALSE) %>%
      mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
      as.matrix()
  }
  
  if (method == "VanRaden") {
    # Calculate the additive relationship matrix using VanRaden's method
    
    #Calculate allele frequencies
    p <- apply(mat, 2, mean, na.rm = TRUE)
    q <- 1 - p
    #Calculate the denominator
    #Note: need to use 1/ploidy for ratios, where ploidy should be used for dosages.
    #This is because the ratios are from 0-1, which is what you get when dosage/ploidy, whereas dosages are from 0 to ploidy
    denominator <- (1/as.numeric(ploidy))*sum(p * q, na.rm = TRUE)
    #Get the numerator
    Z <- scale(mat, center = TRUE, scale = FALSE)
    ZZ <- (Z %*% t(Z))
    #Calculate the additive relationship matrix
    A.mat <- ZZ / denominator
    
    return(A.mat)
    
  } 
}
