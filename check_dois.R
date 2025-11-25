#!/usr/bin/env Rscript
# Script to verify all DOIs in the corrselect package

library(httr)

# Extract all DOIs from the package
dois <- c(
  "10.18637/jss.v028.i05",
  "10.18637/jss.v036.i11",
  "10.18637/jss.v033.i01",
  "10.1002/0471725153",
  "10.1007/978-3-642-17517-6_36",
  "10.1145/362342.362367",
  "10.1016/j.tcs.2006.06.015",
  "10.1145/2402.322385",
  "10.1137/0206036",
  "10.1007/s11135-006-9018-6",
  "10.2307/1267205",
  "10.2307/1412159",
  "10.2307/2332226",
  "10.1016/S0167-5060(13)71063-X",
  "10.1007/BF02760024",
  "10.1111/j.1600-0587.2012.07348.x",
  "10.1093/bioinformatics/btm344"
)

cat("Checking", length(dois), "DOIs...\n\n")

results <- data.frame(
  DOI = character(),
  Status = character(),
  HTTP_Code = integer(),
  stringsAsFactors = FALSE
)

for (doi in dois) {
  url <- paste0("https://doi.org/", doi)

  tryCatch({
    response <- HEAD(url, timeout(10))
    status_code <- status_code(response)

    if (status_code >= 200 && status_code < 400) {
      status <- "OK"
      cat(sprintf("✓ %s [%d]\n", doi, status_code))
    } else {
      status <- "FAILED"
      cat(sprintf("✗ %s [%d]\n", doi, status_code))
    }

    results <- rbind(results, data.frame(
      DOI = doi,
      Status = status,
      HTTP_Code = status_code
    ))

  }, error = function(e) {
    cat(sprintf("✗ %s [ERROR: %s]\n", doi, e$message))
    results <<- rbind(results, data.frame(
      DOI = doi,
      Status = "ERROR",
      HTTP_Code = NA
    ))
  })

  Sys.sleep(0.5)  # Be polite to doi.org
}

cat("\n=== SUMMARY ===\n")
cat(sprintf("Total DOIs checked: %d\n", nrow(results)))
cat(sprintf("OK: %d\n", sum(results$Status == "OK")))
cat(sprintf("FAILED: %d\n", sum(results$Status == "FAILED")))
cat(sprintf("ERROR: %d\n", sum(results$Status == "ERROR")))

if (any(results$Status != "OK")) {
  cat("\nProblematic DOIs:\n")
  print(results[results$Status != "OK", ])
}
