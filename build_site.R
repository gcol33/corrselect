# build_site.R - Build pkgdown site with SVG post-processing
#
# Usage: source("build_site.R") or Rscript build_site.R

#' Replace white fills with transparent in an SVG file
#'
#' @param svg_path Path to SVG file
#' @param verbose Print status
#' @return Invisible logical indicating if changes were made
fix_svg_white_fills <- function(svg_path, verbose = TRUE) {
  svg_text <- readLines(svg_path, warn = FALSE)
  svg_content <- paste(svg_text, collapse = "\n")
  original <- svg_content

  # Base R svg() uses rgb(100%, 100%, 100%) for white
  svg_content <- gsub(
    'fill="rgb\\(100%,\\s*100%,\\s*100%\\)"',
    'fill="none"',
    svg_content
  )

  # Also handle style attributes
  svg_content <- gsub(
    'fill:\\s*rgb\\(100%,\\s*100%,\\s*100%\\)',
    'fill:none',
    svg_content
  )

  # Handle hex notation (just in case)
  svg_content <- gsub('fill="#FFFFFF"', 'fill="none"', svg_content, ignore.case = TRUE)
  svg_content <- gsub('fill="#FFF"', 'fill="none"', svg_content, ignore.case = TRUE)
  svg_content <- gsub('fill="white"', 'fill="none"', svg_content, ignore.case = TRUE)

  changed <- !identical(original, svg_content)

  if (changed) {
    writeLines(svg_content, svg_path)
  }

  if (verbose) {
    cat(sprintf("  %s: %s\n",
                basename(svg_path),
                if (changed) "fixed" else "unchanged"))
  }

  invisible(changed)
}

#' Fix white fills in pkgdown SVG files
#'
#' Recursively finds all SVG files in pkgdown output and replaces
#' white fills with transparent fills for proper dark mode support.
#'
#' @param docs_dir Path to pkgdown docs directory (default: "docs")
#' @param verbose Print progress messages
#' @return Invisible vector of modified file paths
fix_pkgdown_svgs <- function(docs_dir = "docs", verbose = TRUE) {
  svg_files <- list.files(
    docs_dir,
    pattern = "\\.svg$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(svg_files) == 0) {
    if (verbose) cat("No SVG files found in", docs_dir, "\n")
    return(invisible(character(0)))
  }

  if (verbose) {
    cat(sprintf("Found %d SVG files in %s\n", length(svg_files), docs_dir))
  }

  modified <- character(0)

  for (svg_path in svg_files) {
    changed <- fix_svg_white_fills(svg_path, verbose = verbose)
    if (changed) {
      modified <- c(modified, svg_path)
    }
  }

  if (verbose) {
    cat(sprintf("\nModified %d of %d files\n", length(modified), length(svg_files)))
  }

  invisible(modified)
}

# Main execution
if (!interactive() || TRUE) {
  cat("=== Building pkgdown site ===\n\n")
  pkgdown::build_site()

  cat("\n=== Post-processing SVG files ===\n")
  fix_pkgdown_svgs("docs")

  cat("\n=== Done! ===\n")
}
