# Script to check for bullet point formatting issues in vignettes

library(stringr)

vignettes <- list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE)

for (vignette in vignettes) {
  content <- readLines(vignette, warn = FALSE)

  cat("\n=== Checking:", basename(vignette), "===\n")

  issues <- 0

  for (i in 2:(length(content)-1)) {
    line <- content[i]
    prev_line <- content[i-1]
    next_line <- content[i+1]

    # Check 1: Text ending with colon, followed immediately by bullet (no blank line)
    if (str_detect(prev_line, ":\\s*$") && !str_detect(prev_line, "^```") &&
        prev_line != "" && str_detect(line, "^\\s*-\\s")) {
      cat(sprintf("Line %d: Missing blank line before bullet list\n", i))
      cat(sprintf("  %d: %s\n", i-1, prev_line))
      cat(sprintf("  %d: %s\n", i, line))
      issues <- issues + 1
    }

    # Check 2: Text ending with colon but with bullet on same line
    if (str_detect(line, ":\\s+-\\s")) {
      cat(sprintf("Line %d: Bullet on same line as colon\n", i))
      cat(sprintf("  %d: %s\n", i, line))
      issues <- issues + 1
    }

    # Check 3: Bullet list without blank line before it (general case)
    if (str_detect(prev_line, "[a-z]\\s*$") && prev_line != "" &&
        !str_detect(prev_line, "^```") && !str_detect(prev_line, "^#") &&
        str_detect(line, "^\\s*-\\s") && !str_detect(prev_line, "^\\s*-\\s")) {
      cat(sprintf("Line %d: Possibly missing blank line before bullet\n", i))
      cat(sprintf("  %d: %s\n", i-1, prev_line))
      cat(sprintf("  %d: %s\n", i, line))
      issues <- issues + 1
    }
  }

  if (issues == 0) {
    cat("✓ No bullet formatting issues found\n")
  } else {
    cat(sprintf("\n⚠ Found %d potential issues\n", issues))
  }
}
