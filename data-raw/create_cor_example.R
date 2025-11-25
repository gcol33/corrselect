# Generate cor_example dataset for vignette examples
# This script creates a 20x20 correlation matrix with known block structure

set.seed(20250125)

# Create block structure:
# Block 1 (vars 1-5): High correlation (0.75-0.95)
# Block 2 (vars 6-10): Moderate correlation (0.5-0.7)
# Block 3 (vars 11-15): Low correlation (0.2-0.4)
# Block 4 (vars 16-20): Minimal correlation (0.0-0.15)
# Between-block correlations: Low (0.0-0.3)

n_vars <- 20
cor_example <- matrix(0, n_vars, n_vars)
diag(cor_example) <- 1

# Block 1: High correlation cluster (vars 1-5)
block1 <- c(1:5)
for (i in block1) {
  for (j in block1) {
    if (i < j) {
      cor_example[i, j] <- cor_example[j, i] <- runif(1, 0.75, 0.95)
    }
  }
}

# Block 2: Moderate correlation cluster (vars 6-10)
block2 <- c(6:10)
for (i in block2) {
  for (j in block2) {
    if (i < j) {
      cor_example[i, j] <- cor_example[j, i] <- runif(1, 0.5, 0.7)
    }
  }
}

# Block 3: Low correlation cluster (vars 11-15)
block3 <- c(11:15)
for (i in block3) {
  for (j in block3) {
    if (i < j) {
      cor_example[i, j] <- cor_example[j, i] <- runif(1, 0.2, 0.4)
    }
  }
}

# Block 4: Minimal correlation (vars 16-20)
block4 <- c(16:20)
for (i in block4) {
  for (j in block4) {
    if (i < j) {
      cor_example[i, j] <- cor_example[j, i] <- runif(1, 0.0, 0.15)
    }
  }
}

# Between-block correlations: Low (0.0-0.3)
all_blocks <- list(block1, block2, block3, block4)
for (b1_idx in 1:(length(all_blocks) - 1)) {
  for (b2_idx in (b1_idx + 1):length(all_blocks)) {
    block_a <- all_blocks[[b1_idx]]
    block_b <- all_blocks[[b2_idx]]
    for (i in block_a) {
      for (j in block_b) {
        val <- runif(1, 0.0, 0.3)
        cor_example[i, j] <- cor_example[j, i] <- val
      }
    }
  }
}

# Ensure matrix is positive definite
# Use nearest positive definite matrix if needed
eig <- eigen(cor_example)
if (any(eig$values < 0)) {
  # Force positive definiteness
  eig$values[eig$values < 0] <- 1e-6
  cor_example <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  # Re-standardize to correlation matrix
  D <- diag(1 / sqrt(diag(cor_example)))
  cor_example <- D %*% cor_example %*% D
  diag(cor_example) <- 1
}

# Add variable names
colnames(cor_example) <- rownames(cor_example) <- paste0("V", 1:20)

# Ensure exact symmetry (eliminate floating point errors)
cor_example <- (cor_example + t(cor_example)) / 2
diag(cor_example) <- 1

# Verify structure
cat("Block 1 (V1-V5) mean correlation:",
    mean(cor_example[1:5, 1:5][upper.tri(cor_example[1:5, 1:5])]), "\n")
cat("Block 2 (V6-V10) mean correlation:",
    mean(cor_example[6:10, 6:10][upper.tri(cor_example[6:10, 6:10])]), "\n")
cat("Block 3 (V11-V15) mean correlation:",
    mean(cor_example[11:15, 11:15][upper.tri(cor_example[11:15, 11:15])]), "\n")
cat("Block 4 (V16-V20) mean correlation:",
    mean(cor_example[16:20, 16:20][upper.tri(cor_example[16:20, 16:20])]), "\n")

# Check positive definiteness
cat("All eigenvalues positive:", all(eigen(cor_example)$values > 0), "\n")

# Save
usethis::use_data(cor_example, overwrite = TRUE)
